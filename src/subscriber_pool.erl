%%% counter_server.erl
% simple counter implemented as a gen_server
-module(subscriber_pool).
-behavior(gen_server).

% API
-export([new/0, click/1]).

% required by gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%% API methods
new() ->
  gen_server:start(?MODULE, [], []).

click(Pid) ->
  gen_server:call(Pid, click).

%%% gen_server callbacks
%%%   these are required to implement the gen_server behavior
%%%   we're really only using init and handle_call
init([]) ->
  % the second value is the initial counter state
  {ok, 0}.

handle_call(Request, _From, State) ->
  io:fwrite("subscriber_pool received msg from call: ~p \n", [Request]),
  {reply, "Processed PUBLISH.", State}.

handle_cast(Request, State) -> 
  io:fwrite("subscriber_pool received msg from cast: ~p \n", [Request]),
  {noreply, State}.

% basically, we ignore these, but keep the same counter state
handle_info(_Msg, N) -> {noreply, N}.
code_change(_OldVsn, N, _Other) -> {ok, N}.
terminate(_Reason, _N) -> ok.