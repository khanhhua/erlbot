%% @author khanhhua
%% @doc @todo Add description to erlbot_bot.


-module(erlbot_bot).
-behaviour(gen_fsm).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([greeting/2, greeting/3, listening/2, listening/3, guiding/2, guiding/3, idle/2]).

-include("headers.hrl").

-define(SERVER, ?MODULE).
%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1, stop/1, tell/2, subscribe/2, unsubscribe/2]).



%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% start_link/1
%% ========== 
start_link(Username) ->
  % Query database for the backing conversation
  {ok, Pid} = gen_fsm:start_link(?MODULE, #conversation{id=Username, username=Username}, []),
  io:format("Registering ~p by the name ~p~n", [Pid, Username]),
  global:register_name(Username, Pid),
  {ok, Pid}.

stop(BotPid) ->
  gen_fsm:stop(BotPid).

tell(BotPid, TextMessage) ->
  gen_fsm:send_event(BotPid, #message{who=user, text=TextMessage}).

subscribe(BotPid, From) ->
  io:format("Subscribe ~p to the bot ~p~n", [From, BotPid]),
  gen_fsm:send_all_state_event(BotPid, {subscription, From}).

unsubscribe(BotPid, From) ->
  io:format("Unsubscribe ~p to the bot ~p~n", [From, BotPid]),
  gen_fsm:send_all_state_event(BotPid, {unsubscription, From}).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:init-1">gen_fsm:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, StateName, StateData}
			| {ok, StateName, StateData, Timeout}
			| {ok, StateName, StateData, hibernate}
			| {stop, Reason}
			| ignore,
	StateName :: atom(),
	StateData :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
init(Conversation) ->
  process_flag(trap_exit, true),
  io:format("Michelle has waken up. Will greet in 30s...~n"),
  {ok, greeting, Conversation, 10000}.


%% greeting/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:StateName-2">gen_fsm:StateName/2</a>
-spec greeting(Event :: timeout | term(), StateData :: term()) -> Result when
	Result :: {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, Timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, NewStateData},
	NextStateName :: atom(),
	NewStateData :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
% @todo implement actual conversation
greeting(Message, Conversation) when is_record(Message, message) ->
  io:format("Michelle is greeting...~n"),

  Text = "Michelle is here",
  Conversation2 = reply(Text, Conversation),

  Conversation3 = pick_up_message(Message, Conversation),
  io:format("Conversation is now ~p...~n", [Conversation3]),
  {next_state, listening, Conversation2, 20000};

greeting(Message, Conversation) ->
  {next_state, listening, Conversation, 20000}.

%% greeting/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:StateName-3">gen_fsm:StateName/3</a>
-spec greeting(Event :: term(), From :: {pid(), Tag :: term()}, StateData :: term()) -> Result when
	Result :: {reply, Reply, NextStateName, NewStateData}
			| {reply, Reply, NextStateName, NewStateData, Timeout}
			| {reply, Reply, NextStateName, NewStateData, hibernate}
			| {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, Timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, Reply, NewStateData}
			| {stop, Reason, NewStateData},
	Reply :: term(),
	NextStateName :: atom(),
	NewStateData :: atom(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: normal | term().
%% ====================================================================
greeting(Event, From, StateData) ->
    io:format("Michelle is greeting...~n"),
    Reply = ok,
    {reply, Reply, listening, StateData, 5000}.

%% listening/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:StateName-3">gen_fsm:StateName/3</a>
-spec listening(Event :: term(), From :: {pid(), Tag :: term()}, StateData :: term()) -> Result when
    Result :: {reply, Reply, NextStateName, NewStateData}
            | {reply, Reply, NextStateName, NewStateData, Timeout}
            | {reply, Reply, NextStateName, NewStateData, hibernate}
            | {next_state, NextStateName, NewStateData}
            | {next_state, NextStateName, NewStateData, Timeout}
            | {next_state, NextStateName, NewStateData, hibernate}
            | {stop, Reason, Reply, NewStateData}
            | {stop, Reason, NewStateData},
    Reply :: term(),
    NextStateName :: atom(),
    NewStateData :: atom(),
    Timeout :: non_neg_integer() | infinity,
    Reason :: normal | term().
%% ====================================================================
listening(Event, From, StateData) ->
    Reply = ok,
    {reply, Reply, listening, StateData}.

%% listening/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:StateName-2">gen_fsm:StateName/2</a>
-spec listening(Event :: timeout | term(), StateData :: term()) -> Result when
    Result :: {next_state, NextStateName, NewStateData}
            | {next_state, NextStateName, NewStateData, Timeout}
            | {next_state, NextStateName, NewStateData, hibernate}
            | {stop, Reason, NewStateData},
    NextStateName :: atom(),
    NewStateData :: term(),
    Timeout :: non_neg_integer() | infinity,
    Reason :: term().
%% ====================================================================
%% @todo implement actual conversation
listening(timeout, Conversation) ->
  Text = "Sorry but are you still there...",
  Conversation2 = reply(Text, Conversation),
  io:format("~p~n", [Text]),
  {next_state, listening, Conversation2, 30000};
listening(Message, Conversation) when is_record(Message, message)->
    io:format("Michelle is listening to your request...~n"),
    io:format("listening: ~p ~p ~n", [Message, Conversation#conversation.messages]),
    % If conversation has one reference to "bus", then begin to guide

    #message{who=user, text=Text} = Message,
    Conversation1 = Conversation#conversation{session = io_lib:format("~s-~p", [Conversation#conversation.id, os:system_time()])},
    {ok, Intent} = erlbot_ai:query(Text, [{sessionId, Conversation1#conversation.session}]),

    case Intent of
        #intent{action = "find_bus"} ->
          Conversation2 = if
            Conversation1#conversation.context =:= bus ->
              Flow = Conversation1#conversation.flow,
              reply("I am working on it", Conversation1#conversation{context = bus});
            true ->
              {ok, Flow} = erlbot_flow:get_flow("bus"),
              reply("Lemme see....", Conversation1#conversation{context = bus, flow = Flow})
          end,
          Parameters = Intent#intent.parameters,
          EntityNames = erlbot_flow:get_entity_names(Flow),

          Entities = lists:foldl(
            fun (EntityName, Entities0) ->
              case proplists:get_value(EntityName, Parameters) of
                undefined ->
                  Entities0;
                Value ->
                  [#entity{name = EntityName, value = Value} | Entities0]
              end
            end,
            [], EntityNames),

          io:format("Updating flow with entities: ~p~n", [Entities]),
          Flow2 = erlbot_flow:update_flow(Flow, Entities),
          Conversation3 = Conversation2#conversation{flow = Flow2},
          io:format("Updated flow: ~p~n", [Flow2]),

          FlowItem = erlbot_flow:get_current_flow_item(Flow2),
          if
            is_record(FlowItem, flow_item_interactive) ->
              Question = FlowItem#flow_item_interactive.question,
              Conversation4 = pick_up_message(Message, Conversation3),
              ConversationOut = reply(Question, Conversation4),
              {next_state, guiding, ConversationOut, 30000};
            true ->
              Response = erlbot_flow:get_current_answer(Flow2),
              %% Conversation4 = pick_up_message(Message, Conversation3)
              ConversationOut = reply(Response, Conversation2#conversation{flow = Flow2}),
              {next_state, guiding, ConversationOut, 30000}
          end;
        #intent{action = "terminate_conversation"} ->
          io:format("erbot_bot:listening - terminating flow"),
          ConversationOut = reply("Bye now!", Conversation1#conversation{context = undefined, flow = undefined}),
          {next_state, greeting, ConversationOut};
        _ -> {next_state, listening, Conversation#conversation{context = undefined, messages = []}, 5000}
    end.
    

%% guiding/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:StateName-3">gen_fsm:StateName/2</a>
-spec guiding(Event :: term(), StateData :: term()) -> Result when
    Result :: {next_state, NextStateName, NewStateData}
            | {next_state, NextStateName, NewStateData, Timeout}
            | {next_state, NextStateName, NewStateData, hibernate}
            | {stop, Reason, NewStateData},
    NextStateName :: atom(),
    NewStateData :: term(),
    Timeout :: non_neg_integer() | infinity,
    Reason :: term().
%% ====================================================================
guiding(timeout, StateData) ->
    io:format("While so long!!!!~n"),
    {next_state, listening, StateData, 30000};
guiding(Message, Conversation) when is_record(Message, message) ->
  Conversation2 = pick_up_message(Message, Conversation),

  Flow = Conversation#conversation.flow,
  Text = Message#message.text,
  {ok, Flow2} = case erlbot_ai:query(Text, [{sessionId, Conversation#conversation.session}]) of
    {ok, #intent{action = "terminate_conversation"}} ->
      io:format("erbot_bot:guiding - terminating flow"),
      {ok, terminated};
    {ok, #intent{action = "declare_current_location", parameters = Parameters}} ->
      io:format("erbot_bot:guiding - update_flow flow"),
      Entity = #entity{
        name = "current_location",
        value = proplists:get_value("current_location", Parameters)},
      erlbot_flow:update_flow(Flow, Entity);
    {ok, #intent{action = "declare_destination", parameters = Parameters}} ->
      io:format("erbot_bot:guiding - update_flow flow"),
      Entity = #entity{
        name = "destination",
        value = proplists:get_value("destination", Parameters)},
      erlbot_flow:update_flow(Flow, Entity);
    _ ->
      io:format("erbot_bot:guiding - flow remains the same"),
      reply("Sorry but I could not understand...", Conversation),
      {ok, Flow}
  end,

  if
    Flow2 =:= terminated ->
      ConversationOut = reply("Bye!", Conversation2#conversation{flow = Flow2}),
      io:format("Bot saying bye bye...~n"),
      {next_state, listening, ConversationOut#conversation{context = undefined, flow = undefined}};
    %% Else
    true -> CurrentFlowItem = erlbot_flow:get_current_flow_item(Flow2),
      if
        is_record(CurrentFlowItem, flow_item_interactive) ->
          Response = CurrentFlowItem#flow_item_interactive.question,
          Conversation3 = reply(Response, Conversation2#conversation{flow = Flow2}),
          io:format("Thank you for your information...~p~n", [Conversation3#conversation.messages]),
          {next_state, guiding, Conversation3, 30000};
        true ->
          Response = erlbot_flow:get_current_answer(Flow2),
          Conversation3 = reply(Response, Conversation2#conversation{flow = Flow2}),
          {next_state, listening, Conversation3#conversation{context = undefined, session = undefined, messages = []}, 30000}
      end
  end.

%% guiding/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:StateName-3">gen_fsm:StateName/3</a>
-spec guiding(Event :: term(), From :: {pid(), Tag :: term()}, StateData :: term()) -> Result when
    Result :: {reply, Reply, NextStateName, NewStateData}
            | {reply, Reply, NextStateName, NewStateData, Timeout}
            | {reply, Reply, NextStateName, NewStateData, hibernate}
            | {next_state, NextStateName, NewStateData}
            | {next_state, NextStateName, NewStateData, Timeout}
            | {next_state, NextStateName, NewStateData, hibernate}
            | {stop, Reason, Reply, NewStateData}
            | {stop, Reason, NewStateData},
    Reply :: term(),
    NextStateName :: atom(),
    NewStateData :: atom(),
    Timeout :: non_neg_integer() | infinity,
    Reason :: normal | term().
%% ====================================================================
guiding(Event, From, StateData) ->
    Reply = ok,
    {reply, Reply, listening, StateData}.

%% idle/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:StateName-3">gen_fsm:StateName/2</a>
-spec idle(Event :: term(), StateData :: term()) -> Result when
  Result :: {next_state, NextStateName, NewStateData}
  | {next_state, NextStateName, NewStateData, Timeout}
  | {next_state, NextStateName, NewStateData, hibernate}
  | {stop, Reason, NewStateData},
  NextStateName :: atom(),
  NewStateData :: term(),
  Timeout :: non_neg_integer() | infinity,
  Reason :: term().
%% ====================================================================
idle(timeout, StateData) ->
  io:format("Bot idle for too long. Shutting down...~n"),
  {stop, shutdown, StateData}.

%% handle_event/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:handle_event-3">gen_fsm:handle_event/3</a>
-spec handle_event(Event :: term(), StateName :: atom(), StateData :: term()) -> Result when
	Result :: {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, Timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, NewStateData},
	NextStateName :: atom(),
	NewStateData :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_event(Event, StateName, StateData) ->
  io:format("Handle event ~p ~p ~n", [Event, StateName]),
  case Event of
    {subscription, From} ->
      NextState = if
                    StateName =:= idle -> listening;
                    true -> StateName
                  end,
      {next_state, NextState, StateData#conversation{subscribers = StateData#conversation.subscribers ++ [From] }};
    {unsubscription, From} ->
      NextSubscribers = lists:filter(
        fun (X) -> X =/= From end,
        StateData#conversation.subscribers),
      if
        length(NextSubscribers) =:= 0 -> {next_state, idle, StateData#conversation{subscribers = NextSubscribers }, 30000};
        true -> {next_state, StateName, StateData#conversation{subscribers = NextSubscribers }}
      end;
    _ ->
      {next_state, StateName, StateData}
  end.



%% handle_sync_event/4
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:handle_sync_event-4">gen_fsm:handle_sync_event/4</a>
-spec handle_sync_event(Event :: term(), From :: {pid(), Tag :: term()}, StateName :: atom(), StateData :: term()) -> Result when
	Result :: {reply, Reply, NextStateName, NewStateData}
			| {reply, Reply, NextStateName, NewStateData, Timeout}
			| {reply, Reply, NextStateName, NewStateData, hibernate}
			| {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, Timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, Reply, NewStateData}
			| {stop, Reason, NewStateData},
	Reply :: term(),
	NextStateName :: atom(),
	NewStateData :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_sync_event(Event, From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.


%% handle_info/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:handle_info-3">gen_fsm:handle_info/3</a>
-spec handle_info(Info :: term(), StateName :: atom(), StateData :: term()) -> Result when
	Result :: {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, Timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, NewStateData},
	NextStateName :: atom(),
	NewStateData :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: normal | term().
%% ====================================================================
handle_info(Info, StateName, StateData) ->
    {next_state, StateName, StateData}.


%% terminate/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:terminate-3">gen_fsm:terminate/3</a>
-spec terminate(Reason, StateName :: atom(), StateData :: term()) -> Result :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(shutdown, StateName, #conversation{id = Username}) ->
  io:format("~p: Terminating bot... ~p [~p]~n", [StateName, Username, self()]),
  ok;
terminate(Reason, StateName, StatData) ->
    ok.


%% code_change/4
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:code_change-4">gen_fsm:code_change/4</a>
-spec code_change(OldVsn, StateName :: atom(), StateData :: term(), Extra :: term()) -> {ok, NextStateName :: atom(), NewStateData :: term()} when
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(OldVsn, StateName, StateData, Extra) ->
    {ok, StateName, StateData}.


%% ====================================================================
%% Internal functions
%% ====================================================================

pick_up_message(Message, Conversation) ->
  Messages = Conversation#conversation.messages ++ [Message],
  Conversation#conversation{messages = Messages}.

reply(Text, Conversation) ->
  Subscribers = Conversation#conversation.subscribers,
  lists:foreach(
    fun (S) ->
      catch S!{reply, Text}
    end,
    Subscribers),
  pick_up_message(#message{who=bot, text=Text}, Conversation).