%%%-------------------------------------------------------------------
%% @doc erlbot top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erlbot_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([create_bot/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

create_bot(Username) ->
  MFA = {erlbot_bot,start_link,[Username]},
  supervisor:start_child(?SERVER,
    #{id => Username,
      start => MFA,
      restart => transient,
      type => worker
    }).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  {ok, { {one_for_one, 0, 1}, []} }.

%%====================================================================
%% Internal functions
%%====================================================================
