-module(socket_server).
-behavior(gen_server).

-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([accept_loop/1]).
-export([start/3]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

-record(server_state, {
		port,
		loop,
		ip=any,
		lsocket=null}).


start(Name, Port, Loop) ->
	State = #server_state{port = Port, loop = Loop},
	case gen_server:start_link({local, Name}, ?MODULE, State, []) of 
		{ok, Pid} -> Pid;
		{error, {already_started, Pid}} -> Pid;
		% gen_server:init failed
		{error, Reason} -> Reason;
		ignore -> ignore
	end.


init(State = #server_state{port=Port}) ->
	case gen_tcp:listen(Port, ?TCP_OPTIONS) of
   		{ok, LSocket} ->
   			NewState = State#server_state{lsocket = LSocket},
   			{ok, spawn_accept_loop(NewState)};
   		{error, Reason} ->
   			{stop, Reason}
	end.

handle_cast({accepted, _Pid}, State=#server_state{}) ->
	{noreply, spawn_accept_loop(State)}.

% Loop waiting for new connection requests. 
% Upon request asynchronously spawns new accept_loop process and handles incoming connection.
accept_loop({Server, LSocket, {M, Handler}}) ->
	{ok, Socket} = gen_tcp:accept(LSocket),
	% Let the server spawn a new process and replace this loop
	% with the echo loop, to avoid blocking 
	gen_server:cast(Server, {accepted, self()}),
	M:Handler(Socket).
	
% To be more robust we should be using spawn_link and trapping exits
% Creates new accept_loop process.
spawn_accept_loop(State = #server_state{lsocket=LSocket, loop = Loop}) ->
	proc_lib:spawn(?MODULE, accept_loop, [{self(), LSocket, Loop}]),
	State.

% These are just here to suppress warnings.
handle_call(_Msg, _Caller, State) -> {noreply, State}.
handle_info(_Msg, Library) -> {noreply, Library}.
terminate(_Reason, _Library) -> ok.
code_change(_OldVersion, Library, _Extra) -> {ok, Library}.