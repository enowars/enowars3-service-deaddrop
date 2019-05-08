-module(subscribe_handler).
-behaviour(cowboy_handler).

-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2]).

init(Req0, State) ->
    % gen_server:cast({global, subscriber_pool}, {"New WS", self()}),
    {cowboy_websocket, Req0, State, #{idle_timeout => 120000}}.

websocket_init(State) ->
    io:fwrite("WS State: ~p \n", [State]),
    {reply, {text, "Heyhey from WS Handler.."}, State}.


websocket_handle(Frame = {text, MessageBin}, State) ->
    io:fwrite("ws handler received frame: ~p \n", [Frame]),
    List = string:tokens(binary_to_list(MessageBin), ":"),
    Method = hd(List),
    Content = hd(tl(List)),
    % Remove whitespaces and newlines
    Topic = string:trim(Content),
    Reply = case check_topic(Topic) of
        true -> 
            case Method of 
                "SUBSCRIBE" -> 
                    subscribe(Topic),
                    "Subscribed.";
                "REPLAY" ->
                    Result = replay(Topic),
                    Result ++ "\n\n Finished replay.";
                _ -> "Invalid Method."
            end;
        false -> "Unknown Topic."
    end,
    {reply, {text, list_to_binary(Reply)}, State};

websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info({publish, Text}, State) ->
    {reply, {text, Text}, State};

websocket_info(_Info, State) ->
    {ok, State}.

subscribe(Topic) ->
    gen_server:cast({global, subscriber_pool}, {"New SUB", self(), Topic}).

replay(Topic) -> 
    Result = gen_event:call({global, file_handler}, file_handler, {replay, Topic}),
    % io:fwrite("rplay got: ~p \n", [Result]).
    Result.

check_topic(Topic) ->
    case file:read_file("topics.txt") of 
        {ok, Binary} -> 
            % Transform read binary into list of strings, one item per line.
            String = binary_to_list(Binary),
            List = string:tokens(String, "\n"),
            search_topic(List, Topic);
        {error, _} -> 
            io:fwrite("Error while reading topics file."),
            false
    end.

search_topic(List, Topic) ->
    % Remove preceeding special character from file line.
    case catch string:slice(hd(List), 2) of 
        Topic -> true;
        {'EXIT', {badarg, _}} -> false;
        _ -> 
            search_topic(tl(List), Topic)
    end.