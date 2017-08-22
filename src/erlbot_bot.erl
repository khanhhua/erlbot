%% @author khanhhua
%% @doc @todo Add description to erlbot_bot.


-module(erlbot_bot).
-behaviour(gen_fsm).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([greeting/2, greeting/3, listening/2, listening/3, guiding/2, guiding/3]).

-include("headers.hrl").

-define(SERVER, ?MODULE).
%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1, stop/1, tell/2]).



%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% start_link/1
%% ========== 
start_link(Username) ->
    % Query database for the backing conversation
    gen_fsm:start_link(?MODULE, #conversation{id=Username, username=Username}, []).

stop(BotPid) ->
    gen_fsm:stop(BotPid).

tell(BotPid, TextMessage) ->
    gen_fsm:send_event(BotPid, #message{who=user, text=TextMessage}).

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
    io:format("Michelle has waken up. Will greet in 30s...~n"),
    {ok, greeting, Conversation, 30000}.


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

    Conversation2 = pick_up_message(Message, Conversation),
    io:format("Conversation2 is now ~p...~n", [Conversation2]),
    {next_state, listening, Conversation2, 20000}.


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
    io:format("Michelle: Sorry but are you still there...~n"),
    {next_state, listening, Conversation, 5000};
listening(Message, Conversation) when is_record(Message, message)->
    io:format("Michelle is listening to your request...~n"),
    io:format("listening: ~p ~p ~n", [Message, Conversation#conversation.messages]),
    % If conversation has one reference to "bus", then begin to guide

    case Message of
        #message{who=user, text="Bus how long"} ->
          Conversation2 = pick_up_message(Message, Conversation),
          Conversation3 = Conversation2#conversation{topic = bus},
          {next_state, guiding, Conversation3};
        _ -> {next_state, listening, Conversation#conversation{topic=undefined, messages = []}, 5000}
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
    io:format("Thank you for your information...~p~n", [Conversation2#conversation.messages]),
    {next_state, listening, Conversation2#conversation{topic=undefined, messages = []}, 30000}.

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
    {next_state, StateName, StateData}.


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