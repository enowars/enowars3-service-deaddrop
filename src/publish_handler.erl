-module(publish_handler).
-behavior(cowboy_handler).

-export([init/2]).

init(Req0=#{method := <<"POST">>}, State) ->
    case cowboy_req:read_urlencoded_body(Req0) of 
        {ok, Data, _} -> io:fwrite("Got: ~p \n", [Data]);
        {_} -> io:fwrite("Got an error while parsing request body.")
    end,
	Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain">>},
        <<"PUBLISH!">>,
        Req0),
    {ok, Req, State}.