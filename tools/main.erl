-module(main).
-compile(export_all).


hello_world() ->
    io:fwrite("Hello World!").

create_socket() ->
    case get_tcp:listen(0, []) of 
        {ok, ListenSocket} -> ListenSocket;
        {error, Reason} -> io:fwrite("Error occured while creating TCP Socket: ~p \n", [Reason])
    end.