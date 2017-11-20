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
-record(bus_stop, {bus_stop_code, service_nos}).
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
                          service_nos = []
                        }
                      end, maps:get(<<"value">>, Data)),
  io:format("DONE Updating bus stops database from DataMall SG (r). Total stops: ~w...~n", [length(BusStops)]),

  io:format("Updating bus routes database from DataMall SG (r)...~n"),
  {ok, BusRoutesRaw} = file:read_file("/Users/khanhhua/dev/erlbot/priv/bus-data/bus_routes-20171020.csv"),
  [_ | BusRoutesLines] = binary:split(BusRoutesRaw, [<<"\n">>], [global]),
  BusRoutes = lists:foldl(
    fun (<<"">>, Acc0) -> Acc0; %% EOF
    (Line, Acc0) ->
      %% csv columns :: BusStopCode ServiceNo   Direction    StopSequence

      [BusStopCode, ServiceNo, Direction, _StopSequence] = binary:split(Line, [<<"\t">>], [global]),

      {Route0, Acc1} = case maps:is_key(ServiceNo, Acc0) of
        true ->
          {maps:get(ServiceNo, Acc0), Acc0};
        _ ->
          NewRoute = #bus_route{
            service_no = binary_to_list(ServiceNo),
            forward_bus_stop_codes = [],
            reverse_bus_stop_codes = []
          },
          {NewRoute, maps:put(binary_to_list(ServiceNo), NewRoute, Acc0)}
      end,

      Route1 = case Direction of
                 <<"1">> -> Route0#bus_route{forward_bus_stop_codes = lists:append(Route0#bus_route.forward_bus_stop_codes, [binary_to_list(BusStopCode)])};
                 <<"2">> -> Route0#bus_route{reverse_bus_stop_codes = lists:append(Route0#bus_route.reverse_bus_stop_codes, [binary_to_list(BusStopCode)])}
               end,

      maps:put(binary_to_list(ServiceNo), Route1, Acc1)
    end, #{}, BusRoutesLines),

  BusStops2 = lists:map(
    fun (BusStop) ->
      lists:foldl(
        fun (Route, BusStop0) ->
          case lists:member(BusStop#bus_stop.bus_stop_code, Route#bus_route.forward_bus_stop_codes) of
            true -> BusStop0#bus_stop{service_nos = [ Route#bus_route.service_no | BusStop0#bus_stop.service_nos]};
            _ -> BusStop0
          end
        end, BusStop, maps:values(BusRoutes))
    end, BusStops),

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