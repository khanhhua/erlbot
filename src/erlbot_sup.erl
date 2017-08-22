%%%-------------------------------------------------------------------
%% @doc erlbot top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erlbot_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

create_bot(ConversationID) ->
  ok.

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  {ok, { {one_for_one, 0, 1}, []} }.

%%====================================================================
%% Internal functions
%%====================================================================
