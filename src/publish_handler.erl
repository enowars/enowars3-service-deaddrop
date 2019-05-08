-module(publish_handler).
-behavior(cowboy_handler).

-export([init/2]).

init(Req0=#{method := <<"POST">>}, State) ->
    Reply = case cowboy_req:read_body(Req0) of 
        {ok, Data, _} -> 
            % TODO: Introduce error handling for unfinished messages
            Message = string:tokens(string:trim(binary_to_list(Data)), ":"),
            io:fwrite("Got: ~p \n", [Data]),
            Topic = hd(Message),
            Content = hd(tl(Message)),
            % Trigger saving
            gen_event:notify({global, file_handler}, {new, {Topic, Content}}),
            % Trigger publish
            gen_server:call({global, subscriber_pool}, {"New MSG", Topic, Content});
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