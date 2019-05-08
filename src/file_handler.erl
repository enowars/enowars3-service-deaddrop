-module(file_handler).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, terminate/2]).

init(File) ->
    {ok, _} = file:open(File, read),
    {ok, self()}.

handle_event(ErrorMsg, Fd) ->
    io:format(Fd, "***Error*** ~p~n", [ErrorMsg]),
    {ok, Fd}.

handle_call(_, State) -> {ok, State}.

terminate(_Args, Fd) ->
    file:close(Fd).