%% Record conversation
%% -id: Conversation identifier
%% -topic: guide, query and so on
%% -messages: list of message exchanged - each message is tuple {Who, What}
%% -Who: term() :: user, bot
%% -What: binary

-record(conversation, {id, topic, messages=[]}).
