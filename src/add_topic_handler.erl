-module(add_topic_handler).
-behavior(cowboy_handler).

-export([init/2]).
-export([code_change/3, handle_call/3, handle_info/2, terminate/3]).

init(Req0=#{method := <<"PATCH">>}, State) ->
    % Decode http body
    NewTopic = case cowboy_req:read_urlencoded_body(Req0) of
        {ok, Data, _} -> 
            {Message, true} = hd(Data),
            binary_to_list(Message);
        {error, _} -> io:fwrite("Error while parsing sent topic")
    end,

    % Respond depending on maybe arisen errors
    Req = case append_topic(NewTopic) of 
        {ok} -> cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>},<<"">>,Req0);
        {error, ErrorMsg} -> cowboy_req:reply(400, #{<<"content-type">> => <<"text/plain">>},[ErrorMsg],Req0)
    end,
    {ok, Req, State};


init(Req0, State) ->
    Req = cowboy_req:reply(405, #{
        <<"allow">> => <<"PATCH">>
    }, Req0),
    {ok, Req, State}.

% Format topic string and write it to topics file.
append_topic(Topic) ->
    FileName = "topics.txt",
    % Check if the is indicated as private, otherwise prepend a '+'
    NewTopic = case string:prefix(Topic, "- ") of 
        nomatch -> "+ " ++ Topic ++ "\n";
        _ -> Topic ++ "\n"
    end,
    % Write new Topic to topics file
    case file:open(FileName, [append]) of 
        {ok, Fh} -> 
            file:write(Fh, NewTopic),
            {ok};
        {error, enoent} -> {error, "No topics created yet."}
    end.

% XXX: Silence compiler warnings.
handle_call(_Msg, _Caller, State) -> {noreply, State}.
handle_info(_Msg, Library) -> {noreply, Library}.
terminate(_Reason, _Library, _State) -> ok.
code_change(_OldVersion, Library, _Extra) -> {ok, Library}.