-module(file_handler).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, terminate/2]).

init(_) ->
    io:fwrite("Initialised file_handler"),
    {ok, self()}.

% Event: {method, msg}
handle_event(Event, State) ->
    io:fwrite("Rcv event in file_handler \n"),
    case Event of 
        {new, {Topic, Message}} -> save_message(Topic, Message);
        {replay, _} -> ok
    end,
    {ok, State}.

handle_call(Event, State) -> 
    case Event of 
        {replay, Topic} ->
            Messages = retrieve_messages(Topic),
            {ok, Messages, State};
        {create_save, Topic} -> 
            case create_message_save(Topic) of 
                {ok} -> {ok, ok, State};
                {error, Msg} -> {ok, Msg, State}
            end;
        {topics} ->
            Topics = retrieve_topics(),
            {ok, Topics, State};
        {new_topic, Topic} ->
            append_topic(Topic),
            {ok, ok, State}

    end.

terminate(_Args, Fd) ->
    file:close(Fd).

create_message_save(Topic) ->
    FileName = io_lib:format("~s_msg_save.txt", [Topic]),
    case file:open(FileName, [write]) of
        {ok, Fh} ->
            file:write(Fh, io_lib:format("All messages sent on topic: ~s\n", [Topic])),
            {ok};
        {error, _} -> {error, "Error while creating message save."}
    end.

save_message(Topic, Message) ->
    FileName = Topic ++ "_msg_save.txt",
    case file:open(FileName, [append]) of 
        {ok, Fh} -> file:write(Fh, "** " ++ Message ++ "\n");
        {error, _} -> io:fwrite("Error while saving message")
    end.

retrieve_messages(Topic) ->
    FileName = Topic ++ "_msg_save.txt",
    {ok, Binary} = file:read_file(FileName),
    binary_to_list(Binary).

retrieve_topics() ->
    FileName = "topics.txt",
    {ok, Binary} = file:read_file(FileName),
    string:tokens(binary_to_list(Binary), "\n").

append_topic(Topic) ->
    FileName = "topics.txt",
    % Check if the topic is indicated as private, otherwise prepend a '+'
    NewTopic = Topic ++ "\n",
    % Write new Topic to topics file
    case file:open(FileName, [append]) of 
        {ok, Fh} -> 
            file:write(Fh, NewTopic),
            {ok};
        {error, _} -> {error, "Error while saving new topic."}
    end.