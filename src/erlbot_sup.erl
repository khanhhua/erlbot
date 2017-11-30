%%%-------------------------------------------------------------------
%% @doc erlbot top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erlbot_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([create_bot/1, get_bot/1, remove_bot/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({global, ?SERVER}, ?MODULE, []).

create_bot(Username) ->
  %% Bots are "singletons"
  Result = case get_bot(Username) of
    {error, Error} when (Error =:= not_found) or (Error =:= deadbot) ->
      if
        Error =:= deadbot -> io:format("Bot ~p was a deadbot~n", [Username]);
        true -> true
      end,

      MFA = {erlbot_bot,start_link,[Username]},
      supervisor:start_child({global, ?SERVER},
        #{id => Username,
          start => MFA,
          restart => transient,
          type => worker
        });

    {ok, BotPid} -> {ok, BotPid}
  end,

  io:format("Result: ~p~n", [Result]),

  Result.

%% --------------------
%% @doc
%% If a bot exists under the Username, returns {ok, BotPid}.
%% If a bot exists but has become stale, the childspec is to be removed, returns {error, deadbot}
%% If a bot has never exists under Username, returns {error, not_found}
%% @end
%% --------------------
-spec get_bot(Username) -> {ok, BotPid} | {error, deadbot} | {error, not_found} when
  Username :: string(),
  BotPid :: pid().
get_bot(Username) ->
  Children = supervisor:which_children({global, ?SERVER}),
  %% {Name, Pid, ChildType, Mods}
  case lists:keyfind(Username, 1, Children) of
    {Name, BotPid, _ChildType, _Mods} ->
      if
        BotPid =:= undefined ->
          supervisor:delete_child({global, ?SERVER}, Name),
          {error, deadbot};
        true -> {ok, BotPid}
      end;
    false -> {error, not_found}
  end.

remove_bot(Username) ->
  BotPid = get_bot(Username),
  io:format("Delete bot: ~p~n", [BotPid]),
  Result = supervisor:delete_child({global, ?SERVER}, Username),

  io:format("Result: ~p~n", [Result]),

  ok.

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  {ok, { {one_for_one, 0, 1}, [
    #{
      id => erlbot_bus,
      start => {erlbot_bus, start_link, []},
      restart => permanent,
      shutdown => brutal_kill,
      type => worker,
      modules => []
    }
  ]} }.

%%====================================================================
%% Internal functions
%%====================================================================
