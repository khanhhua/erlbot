%%%-------------------------------------------------------------------
%%% @author khanhhua
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Aug 2017 12:48 PM
%%%-------------------------------------------------------------------
-module(erlbot_flow).
-author("khanhhua").

-include("headers.hrl").
%% API
-export([get_flow/1, get_current_flow_item/1, update_flow/2]).

-define(flow_from_yaml(FlowName), yamerl_constr:file(filename:join([code:priv_dir(erlbot), 'flows', FlowName ++ ".yaml"]))).

get_flow(FlowName) ->
  [FlowDefinition] = ?flow_from_yaml(FlowName),

  FlowsList = proplists:get_value("flow", FlowDefinition),

  Flows = lists:map(fun (PropList) ->
              case proplists:is_defined("optional", PropList) of
                false ->
                  Question = proplists:get_value("question", PropList),
                  Action = proplists:get_value("action", PropList),
                  Entity = proplists:get_value("entity", PropList),
                  #flow_item{question = Question, action = Action, entity = Entity};

                true ->
                  OptionalPropList = proplists:get_value("optional", PropList),
                  Question = proplists:get_value("question", OptionalPropList, undefined),
                  Action = proplists:get_value("action", OptionalPropList),
                  Entity = proplists:get_value("entity", OptionalPropList),
                  TriggerPropList = proplists:get_value("trigger", OptionalPropList),
                  Method = proplists:get_value("method", TriggerPropList),
                  Op = proplists:get_value("op", TriggerPropList),
                  #optional_flow_item{
                    question = Question,
                    action = Action,
                    entity = Entity,
                    trigger = #flow_trigger{method = Method, op = Op}
                  }
              end
            end, FlowsList),

  {ok, #flow{items = Flows, current_item = 0, entities = #{}}}.

get_current_flow_item(#flow{items=Items, current_item=CurrentItem}) ->
  lists:nth(CurrentItem, Items).

update_flow(Flow, _NewData) when is_record(Flow, flow) ->
  Flow#flow{current_item = Flow#flow.current_item + 1}.
