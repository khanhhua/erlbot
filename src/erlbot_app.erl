%%%-------------------------------------------------------------------
%% @doc erlbot public API
%% @end
%%%-------------------------------------------------------------------

-module(erlbot_app).

-behaviour(application).
-import(cowboy_router, [compile/1]).
-import(cowboy, [start_http/4]).
%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
      {'_', [
        {"/", erlbot_http, []},
        {"/api/:action", erlbot_http, []},
        {"/sse", erlbot_sse, []},
        {"/[...]", cowboy_static, {priv_dir, erlbot, "static"}}
      ]}
    ]),
    cowboy:start_http(my_http_listener, 100, [{port, 8080}],
        [{env, [{dispatch, Dispatch}]}]
    ),
    erlbot_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
