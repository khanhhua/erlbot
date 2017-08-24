%%%-------------------------------------------------------------------
%%% @author khanhhua
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Aug 2017 10:14 PM
%%%-------------------------------------------------------------------
-module(erlbot_sse).
-author("khanhhua").

-import(cowboy_req, [path/1, binding/2, reply/4]).
-export([
  init/3,
  info/3,
  terminate/3
]).
-include("headers.hrl").

init(_Type, Req0, _Opts) ->
  {Username, Req1} = cowboy_req:cookie(<<"username">>, Req0),

  case erlbot_sup:get_bot(binary_to_list(Username)) of
    {ok, undefined} ->
      {shutdown, Req1, undefined};
    {ok, BotPid} ->
      io:format("SEE initializing with BotPid ~p", [BotPid]),
      erlbot_bot:subscribe(BotPid, self()),
      {ok, Req2} = cowboy_req:chunked_reply(200,
      [
      {<<"content-type">>, <<"text/event-stream">>}
      ], Req1),
      {loop, Req2, {bot, BotPid}}
  end.

terminate(_, _, undefined) ->
  io:format("SEE terminating without any BotPid"),
  ok;
terminate(_, _, {bot, BotPid}) ->
  io:format("SEE terminating with BotPid ~p", [BotPid]),

  erlbot_bot:unsubscribe(BotPid, self()),
  ok.

info({reply, Text}, Req, State) ->
  io:format("~p SSE: Sending a chunk ~p~n", [self(), Text]),
  ok = cowboy_req:chunk(list_to_binary(io_lib:format("data: ~s~n~n", [Text])), Req),
  {loop, Req, State};
info(_Msg, Req, State) ->
  {loop, Req, State, hibernate}.