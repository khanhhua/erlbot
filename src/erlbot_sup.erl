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
  MFA = {erlbot_bot,start_link,[Username]},
  supervisor:start_child({global, ?SERVER},
    #{id => Username,
      start => MFA,
      restart => transient,
      type => worker
    }).

get_bot(Username) ->
  BotPid = global:whereis_name(Username),
  {ok, BotPid}.

remove_bot(Username) ->
  BotPid = global:whereis_name(Username),
  global:unregister_name(Username),
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
