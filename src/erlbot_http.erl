-module(erlbot_http).
-behavior(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

init(_Transport, Req, []) ->
  {ok, Req, undefined}.

handle(Req0, State) ->
  {Method, Req0} = cowboy_req:method(Req0),
  {Path, Req0} = cowboy_req:path(Req0),

  {ok, Req} = case Path of
    <<"/api/chat">> -> case Method of
      <<"POST">> ->
        create_message(Req0, State);
      _ ->
        invalid_action(Req0, State)
    end;
    _ ->
      index_page(Req0, State)
  end,

  {ok, Req, State}.

create_message(Req, State) ->
  case cowboy_req:body_qs(Req) of
    {ok, [{<<"message">>, Message}], Req2} ->
      io:format("~p~n", [Message]),

      {ok, BotPid} = erlbot_sup:get_bot("Joe"),
      erlbot_bot:tell(BotPid, binary_to_list(Message)),

      cowboy_req:reply(200, [
        {<<"content-type">>, <<"application/json; charset=utf-8">>}
      ], "OK", Req2);

    _ -> invalid_action(Req, State)
  end.

index_page(Req0, State) ->
  {ok, Binary} = file:read_file(filename:join(code:priv_dir(erlbot), "static/index.html")),
  {Username, Req1} = cowboy_req:qs_val(<<"u">>, Req0),

  Req = cowboy_req:set_resp_cookie("username", binary_to_list(Username), [], Req1),
  cowboy_req:reply(200, [
    {<<"content-type">>, <<"text/html; charset=utf-8">>}
  ], Binary, Req).

invalid_action(Req, _State) ->
  cowboy_req:reply(400, [
    {<<"content-type">>, <<"application/json; charset=utf-8">>}
  ], "Error", Req).

terminate(_Reason, _Req, _State) ->
  ok.