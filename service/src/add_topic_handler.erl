-module(add_topic_handler).
-behavior(cowboy_handler).

-export([init/2]).
-export([code_change/3, handle_call/3, handle_info/2, terminate/3]).

init(Req0=#{method := <<"PATCH">>}, State) ->
    % Decode http body
    NewTopic = case cowboy_req:read_body(Req0) of
        {ok, Data, _} -> 
            string:trim(binary_to_list(Data));
        {error, _} -> io:fwrite("Error while parsing sent topic")
    end,

    Req = case string:find(NewTopic, "\n") of 
        nomatch -> 
            case check_duplicate_topic(NewTopic) of 
                {ok, false} -> 
                    create_message_save(NewTopic),
                    gen_event:call({global, file_handler}, file_handler, {new_topic, NewTopic}),
                    cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>},<<"">>,Req0);
                % {ok, true} -> cowboy_req:reply(400, #{<<"content-type">> => <<"text/plain">>},["Topic already exists."],Req0);
                {ok, true} -> cowboy_req:reply(400, #{<<"content-type">> => <<"text/plain">>},["Bad Request."],Req0);
                {error, _} -> cowboy_req:reply(400, #{<<"content-type">> => <<"text/plain">>},["Bad Request."],Req0)
                % {error, ErrMsg} -> cowboy_req:reply(400, #{<<"content-type">> => <<"text/plain">>},[io:format("Error: ~p \n", [ErrMsg])],Req0)
            end;
        _ -> cowboy_req:reply(400, #{<<"content-type">> => <<"text/plain">>},["Bad Request."],Req0)
    end,
    {ok, Req, State};


init(Req0, State) ->
    Req = cowboy_req:reply(405, #{
        <<"allow">> => <<"PATCH">>
    }, Req0),
    {ok, Req, State}.

% Create file to save messages published to this topic
create_message_save(Topic) ->
    CleanTopic = string:trim(Topic, leading, "+- "),
    gen_event:call({global, file_handler}, file_handler, {create_save, CleanTopic}).

check_duplicate_topic(Topic) ->
    % Check for duplicates
    Topics = gen_event:call({global, file_handler}, file_handler, {topics}),
    case lists:member(Topic, Topics) of 
        true -> {ok, true};
        false -> {ok, false}
    end.

% XXX: Silence compiler warnings.
handle_call(_Msg, _Caller, State) -> {noreply, State}.
handle_info(_Msg, Library) -> {noreply, Library}.
terminate(_Reason, _Library, _State) -> ok.
code_change(_OldVersion, Library, _Extra) -> {ok, Library}.