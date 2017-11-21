%%%-------------------------------------------------------------------
%%% @author khanhhua
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Nov 2017 5:37 PM
%%%-------------------------------------------------------------------
-module(erlbot_bus).
-author("khanhhua").

-behaviour(gen_server).

%% API
-export([
  start_link/0,
  estimate/3]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(API_BUS_STOPS, "http://datamall2.mytransport.sg/ltaodataservice/BusStops").
-define(API_BUS_ROUTES, "http://datamall2.mytransport.sg/ltaodataservice/BusRoutes"). %% ?skip=N
-define(API_BUS_ARRIVAL, "http://datamall2.mytransport.sg/ltaodataservice/BusArrivalv2"). %% ?BusStopCode=83139

-record(state, { bus_stops, bus_routes }).
-record(bus_stop, {bus_stop_code, service_nos, next_stops}). %% next_stops :: [ {bus_stop_code, [service_no1, service_no2...]}, ... ]
-record(bus_route, {service_no, forward_bus_stop_codes, reverse_bus_stop_codes}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Estimate bus traveling time from PointA to Point B, preferably via
%% the given Bus
%% PointA :: BusStopCode
%% PointB :: BusStopCode
%% Bus :: ServiceNo
%%
%% Reference: http://datamall2.mytransport.sg/ltaodataservice/BusStops
%% @end
%%--------------------------------------------------------------------
estimate(PointA,PointB,Bus) ->
  gen_server:call(?SERVER, {estimate, PointA, PointB, Bus}).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  io:format("Updating bus stops database from DataMall SG (r)...~n"),
  Headers = [
    {"AccountKey", "fATyZro1T0qJr07ERUf5IA=="}
  ],

  Url = ?API_BUS_STOPS,

  io:format("Requesting ~p~n", [Url]),

  {ok, {{_Version, 200, _ReasonPhrase}, _NewHeaders, Body}} =
    httpc:request(get, {Url, Headers}, [], []),

  Data = jsx:decode(list_to_binary(Body), [return_maps]),
  BusStops = lists:map(fun (Item) ->
                        #bus_stop{
                          bus_stop_code = binary_to_list(maps:get(<<"BusStopCode">>, Item)),
                          service_nos = [],
                          next_stops = []
                        }
                      end, maps:get(<<"value">>, Data)),
  io:format("DONE Updating bus stops database from DataMall SG (r). Total stops: ~w...~n", [length(BusStops)]),

  io:format("Updating bus routes database from DataMall SG (r)...~n"),
  {ok, BusRoutesRaw} = file:read_file("/Users/khanhhua/dev/erlbot/priv/bus-data/bus_routes-20171020.csv"),
  [_ | BusRoutesLines] = binary:split(BusRoutesRaw, [<<"\n">>], [global]),
  {BusRoutes, BusStops2} = lists:foldl(
    fun (<<"">>, Acc0) -> Acc0; %% EOF
    (Line, {Routes0, BusStops0}) ->
      %% csv columns :: BusStopCode ServiceNo   Direction    StopSequence

      [BusStopCode_, ServiceNo_, Direction, _StopSequence] = binary:split(Line, [<<"\t">>], [global]),
      BusStopCode = binary_to_list(BusStopCode_),
      ServiceNo = binary_to_list(ServiceNo_),

      {Route0, Routes1} = case maps:is_key(ServiceNo, Routes0) of
        true ->
          {maps:get(ServiceNo, Routes0), Routes0};
        _ ->
          NewRoute = #bus_route{
            service_no = ServiceNo,
            forward_bus_stop_codes = [],
            reverse_bus_stop_codes = []
          },
          {NewRoute, maps:put(ServiceNo, NewRoute, Routes0)}
      end,

      UpdateBusStops = fun ([], BusStops_) -> BusStops_; (BusStopCodes, BusStops_) ->
        LastBusStopCode = lists:last(BusStopCodes),
        io:format("LastBusStopCode: ~p~n", [LastBusStopCode]),

        case lists:keyfind(LastBusStopCode, #bus_stop.bus_stop_code, BusStops_) of
          false ->
            io:format("Could not find ~p in BusStops~n", [LastBusStopCode]),
            BusStops_;
          LastBusStop -> case lists:keyfind(BusStopCode, 1, LastBusStop#bus_stop.next_stops) of
            {BusStopCode, ServiceNos} ->
              io:format("LastBusStop: ~p~n", [LastBusStop]),
              io:format("ServiceNos: ~p~n", [ServiceNos]),
              LastBusStop#bus_stop{
                next_stops = lists:keyreplace(BusStopCode, 1, LastBusStop#bus_stop.next_stops, {BusStopCode, [ServiceNo|ServiceNos]})
              };
            false -> {BusStopCode, [ServiceNo]}
          end,
          lists:keyreplace(LastBusStopCode, #bus_stop.bus_stop_code, BusStops_, LastBusStop)
        end
      end,

      {Route1, BusStops1} = case Direction of
         <<"1">> ->
           {
             Route0#bus_route{forward_bus_stop_codes = lists:append(Route0#bus_route.forward_bus_stop_codes, [BusStopCode])},
             UpdateBusStops(Route0#bus_route.forward_bus_stop_codes, BusStops0)
           };
         <<"2">> ->
           {
             Route0#bus_route{reverse_bus_stop_codes = lists:append(Route0#bus_route.reverse_bus_stop_codes, [BusStopCode])},
             UpdateBusStops(Route0#bus_route.reverse_bus_stop_codes, BusStops0)
           }
      end,
      {
        maps:put(ServiceNo, Route1, Routes1),
        BusStops1
      }
    end, {#{}, BusStops}, BusRoutesLines),

  io:format("DONE Updating bus routes database from DataMall SG (r). Total routes: ~w...~n", [maps:size(BusRoutes)]),
  io:format("Bus stops: ~p~n", [BusStops2]),

  {ok, #state{
    bus_stops = BusStops2,
    bus_routes = BusRoutes
  }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call(Request, _From, State) ->
  case Request of
    {estimate, PointA, PointB, Bus} ->
      {ok, Estimation} = handle_estimate(PointA, PointB, Bus),
      {reply, {ok, Estimation}, State};
    _ -> {reply, ok, State}
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Query bus arrival time at PointA (BusStopCode) then BAT for pointB.
%% The sum of both should be the total estimation to travel to pointB
%% by bus.
%% Only one estimation is returned
%% @spec handle_estimate(PointA, PointB, Bus) -> {ok, Estimation}
%% @end
%%--------------------------------------------------------------------
-spec(handle_estimate(PointA :: string(), PointB :: string(), Bus :: string()) ->
  {ok, Estimation :: term()}).
handle_estimate(PointA, PointB, _Bus) ->
  Headers = [
    {"AccountKey", "fATyZro1T0qJr07ERUf5IA=="}
  ],

  {ok, {{_Version, 200, _ReasonPhrase}, _NewHeaders, Body}} =
    httpc:request(get, {?API_BUS_ARRIVAL ++ "?BusStopCode=" ++ PointA, Headers}, [], []),
  PointAData = jsx:decode(list_to_binary(Body), [returns_map]),
  BusesToPointA = lists:foldl(
    fun (Item, Acc0) ->
      [#{
        service_no => maps:get("ServiceNo", Item),
        eta => maps:get("EstimatedArrival", Item)
      } | Acc0]
    end, [], maps:get("Services", PointAData)),

  {ok, {{_Version, 200, _ReasonPhrase}, _NewHeaders, Body}} =
    httpc:request(get, {?API_BUS_ARRIVAL ++ "?BusStopCode=" ++ PointB, Headers}, [], []),
  PointBData = jsx:decode(list_to_binary(Body), [returns_map]),
  BusesToPointB = lists:foldl(
    fun (Item, Acc0) ->


      [#{
        service_no => maps:get("ServiceNo", Item),
        eta => maps:get("EstimatedArrival", Item)
      } | Acc0]
    end, [], maps:get("Services", PointBData)),

  ok.