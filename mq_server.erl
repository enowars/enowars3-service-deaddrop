-module(mq_server).

-import(erlang, [decode_packet/3]).
-export([start/0, stop/1, loop/1]).

start() ->
	socket_server:start(?MODULE, 7000, {?MODULE, loop}).

stop(Pid) ->
    gen_server:stop(Pid).

% This handler is started within its own process. It should handle everything that one client may do within one session with the MQ.
loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            io:fwrite("Received: ~p \n", [Data]),
            {_, Binary, _} = decode_packet(line, Data, []),
            io:fwrite("Decoded: ~p \n", [Binary]),
            gen_tcp:send(Socket, Data),
            loop(Socket);
        {error, closed} ->
            ok
    end.

handle_publish() ->
    "handle_publish".

handle_subscribe() ->
    "handle_subscribe".

handle_topics() ->
    "handle_topics".

handle_new_topic() ->
    "handle_new_topic".