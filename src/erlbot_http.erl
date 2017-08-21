-module(erlbot_http).
-export([
  init/3,
  handle/2
]).

init(_Type, Req, _Opts) ->
    {ok, Req, no_state}.

handle(Req, _) -> % State=#state{}
    {ok, Req2} = cowboy_req:reply(200,
        [{<<"content-type">>, <<"text/plain">>}],
        <<"Hello Erlang!">>,
        Req),
    {ok, Req2, {}}.