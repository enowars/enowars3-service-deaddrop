-module(topics_handler).
-behavior(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Topics = case file:read_file("topics.txt") of 
        {ok, Content} -> Content;
        {error, enoent} -> "No topics created yet."
    end,

	Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain">>},
        [Topics],
        Req0),
    {ok, Req, State}.