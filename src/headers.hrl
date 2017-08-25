%% Record conversation
%% -id: Conversation identifier
%% -topic: guide, query and so on
%% -messages: list of message exchanged - each message is tuple {Who, What}
%% -Who: term() :: user, bot
%% -What: binary

-record(conversation, {id, username, topic, flow, messages=[], subscribers=[]}).
-record(message, {who, text}).

-record(flow, {items, current_item, entities=#{}}).
-record(flow_item, {question, action, entity}).
-record(optional_flow_item, {trigger, question, action, entity}).
-record(flow_trigger, {method, op}).
