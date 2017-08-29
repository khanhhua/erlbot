%%%-------------------------------------------------------------------
%%% @author khanhhua
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Aug 2017 23:49
%%%-------------------------------------------------------------------
-module(erlbot_ai).
-author("khanhhua").

-include("headers.hrl").
%% API
-export([query/1]).

-define(end_point, "https://api.api.ai/v1/query").

%% ?v=20150910
%% &query=Bus%20to%20Ang%20Mo%20Kio%20Avenue%204&lang=en&sessionId=1&timezone=Asia/Singapore'
%% -H 'Authorization:Bearer ab8bc71108284909a45b9e86d4534e31'

query(Text) ->
  QueryParams = [
    {"v", "20150910"},
    {"lang", "en"},
    {"query", http_uri:encode(Text)},
    {"sessionId", "1"},
    {"timezone", "Asia/Singapore"}
  ],
  Headers = [
    {"authorization", "Bearer ab8bc71108284909a45b9e86d4534e31"}
  ],

  Url = lists:flatten(lists:foldl(
    fun ({Key, Value}, Acc0) ->
      string:join([Acc0, io_lib:format("~s=~s&", [Key, Value])], "")
    end,
    io_lib:format("~s?", [?end_point]), QueryParams)),

  io:format("Requesting ~p~n", [Url]),

  {ok, {{_Version, 200, _ReasonPhrase}, _NewHeaders, Body}} =
    httpc:request(get, {Url, Headers}, [], []),
  %% io:format("Body: ~p ~n", [Body]),

  Data = jsx:decode(list_to_binary(Body)),
  Intent = generate_intent(Data),

  {ok, Intent}.

generate_intent(Data) ->
  Result = proplists:get_value(<<"result">>, Data),
  Action = proplists:get_value(<<"action">>, Result),
  Parameters = proplists:get_value(<<"parameters">>, Result),

  #intent{action = Action, parameters = Parameters}.