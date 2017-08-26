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
-export([get_flow/1, get_current_flow_item/1, update_flow/2, get_current_answer/1]).

-export([findBus/2, estimate/3]).

-define(flow_from_yaml(FlowName), yamerl_constr:file(filename:join([code:priv_dir(erlbot), 'flows', FlowName ++ ".yaml"]))).

get_flow(FlowName) ->
  [FlowDefinition] = ?flow_from_yaml(FlowName),

  FlowsList = proplists:get_value("flow", FlowDefinition),

  Flows = lists:map(fun (PropList) ->
              case proplists:is_defined("optional", PropList) of
                false ->
                  Question = proplists:get_value("question", PropList, undefined),
                  Action = proplists:get_value("action", PropList, undefined),
                  Entity = proplists:get_value("entity", PropList),
                  Answer = proplists:get_value("answer", PropList),

                  if
                    Question =/= undefined -> #flow_item_interactive{
                      question = Question,
                      entity = Entity
                    };
                    Action =/= undefined -> #flow_item_auto{
                      action = Action,
                      entity = Entity,
                      answer = Answer
                    };
                    true -> error(flow_format_error)
                  end;
                true ->
                  OptionalPropList = proplists:get_value("optional", PropList),

                  Question = proplists:get_value("question", OptionalPropList, undefined),
                  Action = proplists:get_value("action", OptionalPropList, undefined),
                  Entity = proplists:get_value("entity", OptionalPropList),
                  Answer = proplists:get_value("answer", OptionalPropList),

                  TriggerPropList = proplists:get_value("trigger", OptionalPropList),
                  Method = proplists:get_value("method", TriggerPropList),
                  Op = proplists:get_value("op", TriggerPropList),
                  Trigger = #flow_trigger{method = Method, op = Op},

                  if
                    Question =/= undefined -> #flow_item_interactive{
                      question = Question,
                      entity = Entity,
                      trigger = Trigger
                    };
                    Action =/= undefined -> #flow_item_auto{
                      action = Action,
                      entity = Entity,
                      trigger = Trigger
                    };
                    true -> error(flow_format_error)
                  end
              end
            end, FlowsList),

  {ok, #flow{items = Flows, current_item = 1, entities = #{}, current_answer = undefined}}.

- spec get_current_flow_item(Flow :: term()) -> FlowItem when
  FlowItem :: term().
get_current_flow_item(#flow{items=Items, current_item=CurrentItem}) ->
  if
    length(Items) < CurrentItem -> undefined;
    true -> lists:nth(CurrentItem, Items)
  end.

get_current_answer(#flow{current_answer = CurrentAnswer}) ->
  CurrentAnswer.

%% update_flow/2
%% Flow structure
%%  - question: 'Where are you now?'
%%    entity: pointA
%%  - question: Heading to?
%%    entity: pointB
%%  - action: findBus(pointA,pointB)
%%    entity: buses
%%  - optional:
%%      trigger:
%%        method: count(buses)
%%        op: PLURAL_CHECK
%%      question: There are {{count(buses)}} buses. Which one?
%%      choices: buses
%%      entity: bus
%%  - action: estimate(pointA,pointB,bus)
%%    entity: estimatedTime
%%    answer: Your bus will come in {{estimatedTime}}!
%% ====================================================================
-spec update_flow(Flow :: term(), Entity :: term()) -> Result when
  Result :: {ok, Flow2}
  | {error, Reason},
  Flow2 :: term(),
  Reason :: normal | term().
%% ====================================================================
update_flow(Flow, Entity) when is_record(Flow, flow) ->
%%  io:format("~p~n", [Flow#flow.items]),
  FlowItem = get_current_flow_item(Flow),

  Flow2 = execute_flow_item(Flow, FlowItem, Entity),
  FlowOut = execute_loop(Flow2),

  {ok, FlowOut}.

execute_loop(Flow) ->
  case get_current_flow_item(Flow) of
    undefined -> Flow;
    FlowItem -> execute_loop(Flow, FlowItem)
  end.

execute_loop(Flow, FlowItem) when is_record(FlowItem, flow_item_interactive) ->
  if
    FlowItem#flow_item_interactive.trigger =/= undefined -> % FIXME Skip interactive trigger
      FlowOut = execute_flow_item(Flow, FlowItem),
      execute_loop(FlowOut);
    true ->
      Flow
  end;
execute_loop(Flow, FlowItem) when is_record(FlowItem, flow_item_auto) ->
  FlowOut = execute_action_flow_item(Flow, FlowItem),

  execute_loop(FlowOut).

execute_flow_item(Flow, FlowItem, Entity) when is_record(FlowItem, flow_item_interactive) ->
  EntityName = Entity#entity.name,
  TriggerResult = case FlowItem#flow_item_interactive.trigger of
    #flow_trigger{method = _TriggerMethod, op = _TriggerOp} ->
      false; % TODO Execute trigger logic
    undefined ->
      true
  end,

  if
    TriggerResult =:= false ->
      Flow#flow{current_item = Flow#flow.current_item + 1};
    true -> case {FlowItem#flow_item_interactive.entity, Entity#entity.value} of
      {EntityName, EntityValue} ->
        Flow#flow{
          entities = maps:put(EntityName, EntityValue, Flow#flow.entities),
          current_item = Flow#flow.current_item + 1
        };
      _ ->
        Flow
    end
  end.

execute_flow_item(Flow, _FlowItem) ->
  Flow#flow{current_item = Flow#flow.current_item + 1}.

execute_action_flow_item(Flow, FlowItem) ->
  TriggerResult = case FlowItem#flow_item_auto.trigger of
    #flow_trigger{method = _TriggerMethod, op = _TriggerOp} ->
      false; % TODO Execute trigger logic
    undefined ->
      true
  end,

  Flow2 = if
    TriggerResult =:= false ->
      Flow#flow{current_item = Flow#flow.current_item + 1};
    true -> case FlowItem#flow_item_auto.action of
      Action when Action =:= "findBus(pointA,pointB)" -> % TODO: Extract findBus, pointA, pointB
        ActionMethodName = list_to_atom("findBus"),
        ActionEntityName = "buses",
        PointA = maps:get("pointA", Flow#flow.entities),
        PointB = maps:get("pointB", Flow#flow.entities),
        {ok, ActionResult} = ?MODULE:ActionMethodName(PointA, PointB),

        Flow#flow{
          entities = maps:put(ActionEntityName, ActionResult, Flow#flow.entities),
          current_item = Flow#flow.current_item + 1
        };
      Action when Action =:= "estimate(pointA,pointB,bus)" -> % TODO: Extract estimate Bus, pointA, pointB
        ActionMethodName = list_to_atom("estimate"),
        ActionEntityName = "bus",
        PointA = maps:get("pointA", Flow#flow.entities),
        PointB = maps:get("pointB", Flow#flow.entities),
        Bus = "Bus 269",% maps:get("bus", Flow#flow.entities),
        {ok, ActionResult} = ?MODULE:ActionMethodName(PointA, PointB,Bus),
        Flow#flow{
          entities = maps:put(ActionEntityName, ActionResult, Flow#flow.entities),
          current_item = Flow#flow.current_item + 1
        }
    end
  end,
  if
    FlowItem#flow_item_auto.answer =/= undefined ->
      io:format("FlowItem.answer: ~p ~n", [FlowItem#flow_item_auto.answer]),
      Flow2#flow{
        current_answer = FlowItem#flow_item_auto.answer
      };
    true ->
      Flow2
  end.

findBus(PointA,PointB) ->
  io:format("Finding buses from ~p to ~p~n", [PointA,PointB]),
  {ok, [
    "Bus 269",
    "Bus 130"
  ]}.

estimate(PointA,PointB,Bus) ->
  io:format("Estimating for Bus ~p from ~p to ~p~n", [Bus,PointA,PointB]),
  {ok, {10, minutes}}.