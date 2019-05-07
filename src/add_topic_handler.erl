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

% Check for duplicates, format and write topic string to topics file.
append_topic(Topic) ->
    FileName = "topics.txt",
    case check_duplicate_topic(FileName, Topic) of 
        true -> {error, "Topic already exists."};
        false ->
            % Check if the topic is indicated as private, otherwise prepend a '+'
            NewTopic = Topic ++ "\n",
            % Write new Topic to topics file
            case file:open(FileName, [append]) of 
                {ok, Fh} -> 
                    file:write(Fh, NewTopic),
                    {ok};
                {error, enoent} -> {error, "No topics created yet."}
            end
    end.

% XXX: Silence compiler warnings.
handle_call(_Msg, _Caller, State) -> {noreply, State}.
handle_info(_Msg, Library) -> {noreply, Library}.
terminate(_Reason, _Library, _State) -> ok.
code_change(_OldVersion, Library, _Extra) -> {ok, Library}.


check_duplicate_topic(FileName, Topic) ->
    % Check for duplicates
    case file:read_file(FileName) of 
        {ok, Binary} -> 
            String = binary_to_list(Binary),
            Topics = string:tokens(String, "\n"),
            case lists:member(Topic, Topics) of 
                true -> true;
                false -> false
            end;
        {error, enoent} -> {error, "No topics created yet."}
    end.