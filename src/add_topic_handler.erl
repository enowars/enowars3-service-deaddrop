-module(add_topic_handler).
-behavior(cowboy_handler).

-export([init/2]).
-export([code_change/3, handle_call/3, handle_info/2, terminate/3]).

init(Req0, State) ->
	Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain">>},
        <<"ADD TOPIC!">>,
        Req0),
    {ok, Req, State}.

% XXX: Silence compiler warnings.
handle_call(_Msg, _Caller, State) -> {noreply, State}.
handle_info(_Msg, Library) -> {noreply, Library}.
terminate(_Reason, _Library, _State) -> ok.
code_change(_OldVersion, Library, _Extra) -> {ok, Library}.
