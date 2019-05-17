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
            io:fwrite("Rcv call in file_handler \n"),
            Messages = retrieve_messages(Topic),
            {ok, Messages, State};
        {create_save, {Topic}} -> 
            case create_message_save(Topic) of 
                {ok} -> {ok, ok, State};
                {error, Msg} -> {ok, Msg, State}
            end
    end.

terminate(_Args, Fd) ->
    file:close(Fd).

create_message_save(Topic) ->
    FileName = io:format("~s", [Topic]),
    case file:open(FileName, [write]) of
        {ok, Fh} ->
            file:write(Fh, io:format("All messages sent on topic: ~s\n", [Topic])),
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