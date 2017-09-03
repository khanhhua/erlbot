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
-export([query/2]).

-define(end_point, "https://api.api.ai/v1/query").

%% ?v=20150910
%% &query=Bus%20to%20Ang%20Mo%20Kio%20Avenue%204&lang=en&sessionId=1&timezone=Asia/Singapore'
%% -H 'Authorization:Bearer ab8bc71108284909a45b9e86d4534e31'

query(Text, [{sessionId, SessionId}]) ->
  query(Text, [{sessionId, SessionId}, {context, undefined}]);
query(Text, [{sessionId, SessionId}, {context, Context}]) ->
  QueryParams = [
    {"v", "20150910"},
    {"lang", "en"},
    {"query", http_uri:encode(Text)},
    {"sessionId", SessionId},
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

  Data = jsx:decode(list_to_binary(Body)),
  Intent = generate_intent(Data, Context),

  io:format("Intent: ~p ~n", [Intent]),
  {ok, Intent}.

generate_intent(Data, Context) ->
  Result = proplists:get_value(<<"result">>, Data),
  Action = proplists:get_value(<<"action">>, Result),
  Parameters = proplists:get_value(<<"parameters">>, Result, []),
  Contexts = proplists:get_value(<<"contexts">>, Result, []),

  io:format("generate_intent~n- Action: ~p ~n- Parameters: ~p~n", [Action, Parameters]),
  io:format("Context: ~p~n", [Contexts]),

  Intent = #intent{
    action =
    case Action of
      undefined -> unknown;
      _ ->  binary_to_list(Action)
    end,
    parameters = lists:foldl(
      fun
        ({_Key, <<>>}, Acc0) -> Acc0;
        ({Key, Value}, Acc0) -> [{binary_to_list(Key), binary_to_list(Value)},Acc0]
      end,
      [], Parameters)
  },

  ContextDefinedParameters = case lists:filter(
    fun (Item) ->
      Name = proplists:get_value(<<"name">>, Item),
      binary_to_list(Name) =:= Context
    end, Contexts) of
    [ContextDefined0] -> proplists:get_value(<<"parameters">>, ContextDefined0);
    [] -> undefined
  end,

  if
    (length(Contexts) =:= 0) or (ContextDefinedParameters =:= undefined) -> Intent;
    true ->
      ContextualParameters = lists:foldl(
        fun ({Key, Value}, Acc0) ->
          case proplists:get_value(Key, ContextDefinedParameters) of
            undefined -> [{Key, Value} | Acc0];
            ContextDefinedValue -> [{Key, ContextDefinedValue} | Acc0]
          end
        end, [], Intent#intent.parameters),
      Intent#intent{parameters = ContextualParameters}
  end.
