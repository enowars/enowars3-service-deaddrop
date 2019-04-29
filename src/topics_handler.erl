-module(topics_handler).
-behavior(cowboy_handler).

-export([init/2]).

init(Req0=#{method := <<"GET">>}, State) ->
    Topics = case file:read_file("topics.txt") of 
        {ok, Content} -> 
            Binary = binary_to_list(Content),
            List = string:tokens(Binary, "\n"),
            lists:map(fun remove_private_topics/1, List);
        {error, enoent} -> "No topics created yet."
    end,

	Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain">>},
        [Topics],
        Req0),
    {ok, Req, State};

init(Req0, State) ->
    Req = cowboy_req:reply(405, #{
        <<"allow">> => <<"GET">>
    }, Req0),
    {ok, Req, State}.

remove_private_topics(String) ->
    case string:prefix(String, "- ") of 
        nomatch -> String ++ "\n";
        _ -> "\n"
    end.