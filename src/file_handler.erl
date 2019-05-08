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

handle_call({replay, Topic}, State) -> 
    io:fwrite("Rcv call in file_handler \n"),
    Messages = retrieve_messages(Topic),
    {ok, State, Messages}.

terminate(_Args, Fd) ->
    file:close(Fd).

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