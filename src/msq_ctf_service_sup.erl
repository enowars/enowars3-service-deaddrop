-module(msq_ctf_service_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	SubPool = #{id => subscriber_pool, start => {gen_server, start_link, [{global, subscriber_pool}, subscriber_pool, [], []]}},
	FileHandler = #{id => file_handler, start => {gen_event, start_link, [{global, file_handler}]}},
	% {ok, _} = gen_server:start_link({global, subscriber_pool}, subscriber_pool, [], []),
	{ok, {{one_for_one, 1, 5}, [SubPool, FileHandler]}}.
