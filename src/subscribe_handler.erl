-module(subscribe_handler).
-behavior(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
	Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain">>},
        <<"SUBSCRIBE!">>,
        Req0),
    {ok, Req, State}.