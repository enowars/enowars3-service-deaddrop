-module(publish_handler).
-behavior(cowboy_handler).

-export([init/2]).

init(Req0=#{method := <<"POST">>}, State) ->
    Reply = case cowboy_req:read_urlencoded_body(Req0) of 
        {ok, Data, _} -> 
            % Introduce error handling for unfinished messages
            {Message, true} = hd(Data),
            io:fwrite("Got: ~p \n", [Message]),
            gen_server:call({global, subscriber_pool}, {"New MSG", Message});
        {_} -> 
            io:fwrite("Got an error while parsing request body."),
            "Parsing Error."
    end,
	Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain">>},
        list_to_binary(Reply),
        Req0),
    {ok, Req, State};

init(Req0, State) ->
    Req = cowboy_req:reply(405, #{
        <<"allow">> => <<"POST">>
    }, Req0),
    {ok, Req, State}.