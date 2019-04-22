-module(main).

-export([hello_world/0, read_file/1]).

read_file(Path) ->
    Result = file:open(Path, [read]),
    case Result of 
        {ok, Fh} -> 
            io:fwrite("Yueah.\nContent: ~p.\n", [file:read_line(Fh)]);
        {error, Reason} -> io:fwrite("Fail: ~p.\n", [Reason])
    end.

hello_world() ->
    io:fwrite("Hello World!").