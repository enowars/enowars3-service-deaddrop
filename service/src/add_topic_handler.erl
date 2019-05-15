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

    % Respond depending on maybe arisen errors
    FileName = "topics.txt",
    Req = case check_duplicate_topic(FileName, NewTopic) of 
        {ok, false} -> 
            create_message_save(NewTopic),
            append_topic(FileName, NewTopic),
            cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>},<<"">>,Req0);
        {ok, true} -> cowboy_req:reply(400, #{<<"content-type">> => <<"text/plain">>},["Topic already exists."],Req0);
        {error, ErrMsg} -> cowboy_req:reply(400, #{<<"content-type">> => <<"text/plain">>},[io:format("Error: ~p \n", [ErrMsg])],Req0)
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
    FileName = CleanTopic ++ "_msg_save.txt",
    case file:open(FileName, [write]) of
        {ok, Fh} ->
            file:write(Fh, "All messages sent on topic: " ++ CleanTopic ++ "\n"),
            {ok};
        {error, _} -> {error, "Error while creating message save."}
    end.

% Check for duplicates, format and write topic string to topics file.
append_topic(FileName, Topic) ->
    % Check if the topic is indicated as private, otherwise prepend a '+'
    NewTopic = Topic ++ "\n",
    % Write new Topic to topics file
    case file:open(FileName, [append]) of 
        {ok, Fh} -> 
            file:write(Fh, NewTopic),
            {ok};
        {error, _} -> {error, "Error while saving new topic."}
    end.

% XXX: Silence compiler warnings.
handle_call(_Msg, _Caller, State) -> {noreply, State}.
handle_info(_Msg, Library) -> {noreply, Library}.
terminate(_Reason, _Library, _State) -> ok.
code_change(_OldVersion, Library, _Extra) -> {ok, Library}.


check_duplicate_topic(FileName, Topic) ->
    % Check for duplicates
    Tmp = case filelib:is_regular(FileName) of
        true -> {ok};
        false -> 
            case file:open(FileName, [write]) of
                {ok, _} -> {ok};
                {error, eacces} -> {error, "Error while creating topics.txt"};
                {error, _} -> {error, "Unknown error while creating topics.txt"}
            end
    end,
    case Tmp of 
        {ok} -> 
            case file:read_file(FileName) of 
            {ok, Binary} -> 
                String = binary_to_list(Binary),
                Topics = string:tokens(String, "\n"),
                case lists:member(Topic, Topics) of 
                    true -> {ok, true};
                    false -> {ok, false}
                end;
            {error, enoent} -> {error, "No topics.txt"}
            end;
        {error, ErrMsg} -> {error, ErrMsg}
    end.