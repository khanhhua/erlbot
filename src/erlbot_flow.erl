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
-export([get_flow/1, get_entity_names/1, get_current_flow_item/1, update_flow/2, get_current_answer/1, reset_flow/1, render/2]).

-export([findBus/2, estimate/3, count/1, select_bus/1]).

-define(flow_from_yaml(FlowName), yamerl_constr:file(filename:join([code:priv_dir(erlbot), 'flows', FlowName ++ ".yaml"]))).

get_flow(FlowName) ->
  [FlowDefinition] = ?flow_from_yaml(FlowName),

  FlowsList = proplists:get_value("flow", FlowDefinition),

  FlowItems = lists:map(fun (PropList) ->
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

  {ok, #flow{items = FlowItems, current_item = 1, entities = #{}, current_answer = undefined}}.

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
update_flow(Flow, Entity) when is_record(Flow, flow), is_record(Entity, entity) ->
  io:format("erbot_flow:update_flow Updating flow ~p~n", [Flow]),
  FlowItem = get_current_flow_item(Flow),

  Flow2 = execute_flow_item(Flow, FlowItem, Entity),
  FlowOut = execute_loop(Flow2),

  {ok, FlowOut};

%% - Remove interactive questions from the flow,
update_flow(Flow, [Entity|Entities]) ->
  io:format("erbot_flow:update_flow Entities: ~p~n", [Entities]),
  FlowOut = Flow#flow{
    entities = maps:put(Entity#entity.name, Entity#entity.value, Flow#flow.entities),
    items = lists:filter(
      fun
        (#flow_item_interactive{entity = EntityName}) -> EntityName =/= Entity#entity.name;
        (_) -> true
      end,
      Flow#flow.items)
    },

  update_flow(FlowOut, Entities);
update_flow(Flow, []) ->
  %% FlowItem = get_current_flow_item(Flow),
  FlowOut = execute_loop(Flow),

  FlowOut.

reset_flow(Flow) ->
  io:format("reset_flow: ~p~n", [Flow]),
  Flow#flow{current_item = 1, entities = #{}, current_answer = undefined}.

get_entity_names(#flow{items = FlowItems}) ->
  io:format("FlowItems: ~p~n", [FlowItems]),
  lists:foldl(
    fun
      (#flow_item_interactive{entity = EntityName}, Acc0) -> [EntityName|Acc0];
      (_, Acc0) -> Acc0
    end,
    [], FlowItems).

execute_loop(Flow) ->
  case get_current_flow_item(Flow) of
    undefined -> Flow;
    FlowItem -> execute_loop(Flow, FlowItem)
  end.

execute_loop(Flow, FlowItem) when is_record(FlowItem, flow_item_interactive) ->
  if
    FlowItem#flow_item_interactive.trigger =/= undefined ->
      execute_flow_item(Flow, FlowItem);
    true ->
      Flow
  end;
execute_loop(Flow, FlowItem) when is_record(FlowItem, flow_item_auto) ->
  FlowOut = if
    FlowItem#flow_item_auto.trigger =/= undefined ->
      execute_action_flow_item(Flow, FlowItem);
    true ->
      execute_action_flow_item(Flow, FlowItem)
  end,

  execute_loop(FlowOut).

execute_flow_item(Flow, FlowItem, Entity) when is_record(FlowItem, flow_item_interactive) ->
  io:format("execute_flow_item... ~n"),

  EntityName = Entity#entity.name,
  TriggerResult = case FlowItem#flow_item_interactive.trigger of
    #flow_trigger{method = TriggerMethodExpression, op = TriggerOp} ->
      {Method, ArgNames} = parse_method_expression(TriggerMethodExpression),
      %% Execute the TriggerExpressionMethod
      MethodResult = apply_flow_method(Flow, Method, ArgNames),
      io:format("- MethodResult ~p... ~n", [MethodResult]),
      %% Evaluate TriggerOp to a boolean
      case TriggerOp of
        "PLURAL_CHECK" -> if
                            MethodResult > 1 -> true;
                            true -> false
                          end;
        "SINGULAR_CHECK" -> if
                              MethodResult =:= 1 -> true;
                              true -> false
                            end;
        _ -> false
      end;
    undefined ->
      true
  end,

  io:format("erbot_flow:execute_flow_item Executing flow item...~n"),
  io:format("erbot_flow:execute_flow_item FlowItem: ~p~n", [FlowItem]),
  io:format("erbot_flow:execute_flow_item Entity: ~p~n", [Entity]),

  FlowOut = if
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
  end,

  FlowOut.

execute_flow_item(Flow, FlowItem) when is_record(FlowItem, flow_item_interactive) ->
  io:format("execute_flow_item: ~n"),

  TriggerResult = case FlowItem#flow_item_interactive.trigger of
    #flow_trigger{method = TriggerMethodExpression, op = TriggerOp} ->
      {Method, ArgNames} = parse_method_expression(TriggerMethodExpression),
      %% Execute the TriggerExpressionMethod
      MethodResult = apply_flow_method(Flow, Method, ArgNames),
      io:format("- MethodResult ~p...~n", [MethodResult]),
      %% Evaluate TriggerOp to a boolean
      io:format("- TriggerOp ~p...~n", [TriggerOp]),

      case TriggerOp of
        "PLURAL_CHECK" -> if
                            MethodResult > 1 -> true;
                            true -> false
                          end;
        _ -> false
      end;
    undefined ->
      true
  end,

  if
    TriggerResult -> Flow;
    true -> Flow#flow{current_item = Flow#flow.current_item + 1}
  end.

execute_action_flow_item(Flow, FlowItem) ->
  io:format("execute_flow_item: ~n"),
  io:format("- Auto flow: ~n"),

  TriggerResult = case FlowItem#flow_item_auto.trigger of
    #flow_trigger{method = TriggerMethodExpression, op = TriggerOp} ->
      {Method, ArgNames} = parse_method_expression(TriggerMethodExpression),
      %% Execute the TriggerExpressionMethod
      MethodResult = apply_flow_method(Flow, Method, ArgNames),
      io:format("- MethodResult ~p...~n", [MethodResult]),
      %% Evaluate TriggerOp to a boolean
      io:format("- TriggerOp ~p...~n", [TriggerOp]),

      case TriggerOp of
        "SINGULAR_CHECK" -> if
                              MethodResult =:= 1 -> true;
                              true -> false
                            end;
        _ -> false
      end;
    undefined ->
      true
  end,

  Flow2 = if
    TriggerResult =:= false ->
      Flow#flow{current_item = Flow#flow.current_item + 1};
    true -> case FlowItem#flow_item_auto.action of
      Action when Action =:= "findBus(current_location,destination)" -> % TODO: Extract findBus, pointA, pointB
        ActionMethodName = list_to_atom("findBus"),
        ActionEntityName = "buses",
        PointA = maps:get("current_location", Flow#flow.entities),
        PointB = maps:get("destination", Flow#flow.entities),
        {ok, ActionResult} = ?MODULE:ActionMethodName(PointA, PointB),

        Flow#flow{
          entities = maps:put(ActionEntityName, ActionResult, Flow#flow.entities),
          current_item = Flow#flow.current_item + 1
        };

      Action when Action =:= "select_bus(buses)" -> % TODO: Extract
        ActionMethodName = list_to_atom("select_bus"),
        ActionEntityName = "bus",
        Buses = maps:get("buses", Flow#flow.entities),
        {ok, ActionResult} = ?MODULE:ActionMethodName(Buses),

        Flow#flow{
          entities = maps:put(ActionEntityName, ActionResult, Flow#flow.entities),
          current_item = Flow#flow.current_item + 1
        };

      Action when Action =:= "estimate(current_location,destination,bus)" -> % TODO: Extract estimate Bus, pointA, pointB
        ActionMethodName = list_to_atom("estimate"),
        ActionEntityName = "estimatedTime",
        PointA = maps:get("current_location", Flow#flow.entities),
        PointB = maps:get("destination", Flow#flow.entities),
        Bus = maps:get("bus", Flow#flow.entities),
        {ok, ActionResult} = ?MODULE:ActionMethodName(PointA, PointB, Bus),

        Flow#flow{
          entities = maps:put(ActionEntityName, ActionResult, Flow#flow.entities),
          current_item = Flow#flow.current_item + 1
        }
    end
  end,
  if
    FlowItem#flow_item_auto.answer =/= undefined ->
      ParameterizedAnswer = render(FlowItem#flow_item_auto.answer, Flow2#flow.entities),

      io:format("FlowItem.answer: ~p ~n", [ParameterizedAnswer]),
      Flow2#flow{
        current_answer = ParameterizedAnswer
      };
    true ->
      execute_loop(Flow2)
  end.

render(Template, EntitiesMap) ->
  io:format("render: ~n- Template:~p~n- Entities ~p~n", [Template, EntitiesMap]),

  lists:foldl(
    fun (Key, Acc0) ->
      Value = maps:get(Key, EntitiesMap),
      case Value of
        {TimeValue, minutes} -> re:replace(Acc0, "{{"++Key++"}}", lists:flatten(io_lib:format("~w minutes", [TimeValue])), [{return,list}]);
        List when is_list(List) -> re:replace(Acc0, "{{"++Key++"}}", lists:join(", ", List));
        _ -> Acc0
      end
    end
  , Template, maps:keys(EntitiesMap)).

parse_method_expression(Expression) ->
  if
    Expression =:= "count(buses)" -> {count, ["buses"]};
    true -> {error, invalid_expression}
  end.

apply_flow_method(#flow{entities = Entities}, Method, ArgNames) ->
  Args = lists:map(
    fun (ArgName) ->
      maps:get(ArgName, Entities)
    end, ArgNames),

  io:format("apply_flow_method: fun ~p (~p)~n", [Method, Args]),
  erlang:apply(?MODULE, Method, Args).

%% Exported
findBus(PointA,PointB) ->
  io:format("Finding buses from ~p to ~p~n", [PointA,PointB]),
  {ok, Buses} = erlbot_bus:find_buses(PointA,PointB),
  {ok, Buses}.

%% Exported
estimate(PointA,PointB,Bus) ->
  io:format("Estimating for Bus ~p from ~p to ~p~n", [Bus,PointA,PointB]),
  {ok, Estimation} = erlbot_bus:estimate(PointA,PointB,Bus),
  {ok, Estimation}.

%% Exported
count(Buses) -> length(Buses).

%% Exported
select_bus(Buses) ->
  [Bus] = Buses,

  io:format("select_bus: ~p~n", [Bus]),
  {ok, Bus}.