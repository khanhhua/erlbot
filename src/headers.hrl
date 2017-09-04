%% Record conversation
%% -id: Conversation identifier
%% -topic: guide, query and so on
%% -messages: list of message exchanged - each message is tuple {Who, What}
%% -Who: term() :: user, bot
%% -What: binary

-record(conversation, {id, username, context, flow, session, messages=[], subscribers=[]}).
-record(message, {who, text}).

-record(flow, {items, current_item, entities=#{}, current_answer}).
-record(flow_item_interactive, {question, entity, trigger}).
-record(flow_item_auto, {action, entity, answer, trigger}).
-record(flow_trigger, {method, op}).
-record(entity, {name :: iolist(), value :: term()}).

-record(intent, {action::term(), parameters :: list()}).