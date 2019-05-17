-module(msq_ctf_service_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	SubPool = #{id => subscriber_pool, start => {gen_server, start_link, [{global, subscriber_pool}, subscriber_pool, [], []]}},
	% Using this would be prefered but we don't know where to register a callback with this process within app startup.
	% FileHandler = #{id => file_handler, start => {gen_event, start_link, [{global, file_handler}]}},
	{ok, Pid} = gen_event:start_link({global, file_handler}),

	FileName = "topics.txt",
	PrivPath = get_priv_path(),
	Path = filename:join([PrivPath, FileName]),
	io:fwrite("Got path in startup: ~p \n", [Path]),
	_ = case filelib:is_regular(Path) of
        true -> 
			io:fwrite("true"),
			null;
        false -> 
			case filelib:is_dir(PrivPath) of 
				false -> file:make_dir(PrivPath);
				true -> null
			end,
			case file:open(Path, [raw]) of
				{ok, _} -> 
						io:fwrite("Writing to: ~p \n", [Path]),
						null;
				{error, eacces} -> io:fwrite("Error while creating topics.txt");
				{error, Error} -> io:fwrite("Got error while creating topics.txt: ~p \n", [Error])
			end
    end,

	gen_event:add_sup_handler(Pid, file_handler, []),

	% {ok, {{one_for_one, 1, 5}, [SubPool, FileHandler]}}.
	{ok, {{one_for_one, 1, 5}, [SubPool]}}.


get_priv_path() ->
    case code:priv_dir(msq_ctf_service) of
        {error, bad_name} ->
            "priv";
        PrivDir ->
            PrivDir
    end.