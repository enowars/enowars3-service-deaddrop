#!/usr/bin/env escript

main(_) ->
    case io:fread(standard_io, "", "~s") of
        {ok, Terms} ->    
            io:fwrite("~p", Terms);
        {error, _} ->
            io:fwrite("error occured")
    end.

% main(_) ->
%     io:fwrite("Got more than one argument").

handle_input(Input) ->
    case is_binary(Input) of
        true -> io:fwrite("~p \n", [binary_to_term(Input)]);
        false -> io:fwrite("~p \n", [term_to_binary(Input)])
        % true -> binary_to_term(Input);
        % false -> term_to_binary(Input)
    end.