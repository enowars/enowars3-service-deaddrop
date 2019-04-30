-module(subscribe_handler).
-behaviour(cowboy_handler).

-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2]).

init(Req0, State) ->
    % gen_server:cast({global, subscriber_pool}, {"New WS", self()}),
    {cowboy_websocket, Req0, State}.

websocket_init(State) ->
    io:fwrite("WS State: ~p \n", [State]),
    {reply, {text, "Heyhey from WS Handler.."}, State}.


websocket_handle(Frame = {text, MessageBin}, State) ->
    io:fwrite("ws handler received frame: ~p \n", [Frame]),
    Message = binary_to_list(MessageBin),
    Reply = case string:prefix(Message, "SUBSCRIBE: ") of 
        nomatch -> "Invalid Method.";
        Topic -> 
            % Remove whitespaces and newlines
            NewTopic = string:trim(Topic),
            case check_topic(NewTopic) of
                true -> 
                    subscribe(NewTopic),
                    "Subscribed.";
                false -> "Unknown Topic."
            end
    end,
    {reply, {text, list_to_binary(Reply)}, State};

websocket_handle(_Frame, State) ->
    {ok, State}.


websocket_info({log, Text}, State) ->
    {reply, {text, Text}, State};
websocket_info(_Info, State) ->
    {ok, State}.

subscribe(Topic) ->
    gen_server:cast({global, subscriber_pool}, {"New SUB", self(), Topic}).


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