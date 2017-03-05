-module(index).
-export([index_file/1]).


-spec index_file(nonempty_string()) -> [{nonempty_string(),[{integer(),integer()}]}].
index_file(Name) ->
    {Index,_} = build_index(get_file_contents(Name)),
    lists:sort(maps:to_list(Index)).

% Not filtering out 'common words' atm
build_index(Lines) ->
    lists:foldl(fun(L,{Map,LineNum}) -> {process_line(L,LineNum,Map),LineNum+1} end,{#{},1},Lines).

process_line(Line,LineNum,Map) ->
    word_occurrences(longer_than(3,letters_only(tokens(to_lower(Line)))),LineNum,Map).

letters_only(Xss) ->
    lists:foldr(fun(Xs,Acc) -> [lists:filter(fun letter/1, Xs)|Acc] end,[[]],Xss).

letter(X) ->
    (X >= $A andalso X =< $Z) or (X >= $a andalso X =< $z).

longer_than(N,Xs) ->
    lists:filter(fun(X) -> length(X) > N end, Xs).

-spec to_lower(string()) -> string().
to_lower(Xs) ->
    to_lower(Xs,[]).
to_lower([],Acc) ->
    Acc;
to_lower([X|Xs],Acc) when X >= $A andalso X < $Z ->
    XtoLowerCase = X + 32,
    to_lower(Xs,Acc ++ [XtoLowerCase]);
to_lower([X|Xs],Acc) ->
    to_lower(Xs,Acc ++ [X]).

-spec tokens(nonempty_string()) -> [nonempty_string()].
tokens(Line) ->
    lists:filter(fun(Xs) -> Xs =/= [] end,lists:foldr(fun break_on_space/2,[[]],Line)).
break_on_space(X,Xss) when X == 32 -> % ASCII code for space
    [[]]++Xss;
break_on_space(X,Xss) ->
    [[X]++hd(Xss)]++tl(Xss).

word_occurrences([],_,Map) ->
    Map;
word_occurrences([W|Ws],LineNum,Map) ->
    word_occurrences(Ws,LineNum,put_occurrence(W,LineNum,Map)).

put_occurrence(W,LineNum,Map) ->
    case maps:find(W,Map) of
        error ->
            maps:put(W,[{LineNum,LineNum}],Map);
        {_,Xs} ->
            Occs = case lists:member({LineNum,LineNum},Xs) of
                       false ->
                           normalise_occurrences(Xs++[{LineNum,LineNum}]);
                       true ->
                           Xs
                   end,
            maps:put(W,Occs,Map)
    end.

-spec normalise_occurrences([{integer(),integer()}]) -> [{integer(),integer()}].
normalise_occurrences(Occs) ->
    lists:foldr(fun normalise_occurrence/2, [], Occs).

-spec normalise_occurrence({integer(),integer()}, [{integer(),integer()}]) -> [{integer(),integer()}].
normalise_occurrence(Occ,[]) ->
    [Occ];
normalise_occurrence({S,E},[{XStart,XEnd}|Xs]) when E + 1 == XStart ->
    [{S,XEnd}|Xs];
normalise_occurrence(Occ,Occs) ->
    [Occ|Occs].

% Used to read a file into a list of lines.
% Example files available in:
%   gettysburg-address.txt (short)
%   dickens-christmas.txt  (long)


% Get the contents of a text file into a list of lines.
% Each line has its trailing newline removed.
-spec get_file_contents(nonempty_string()) -> [string()].
get_file_contents(Name) ->
    {ok,File} = file:open(Name,[read]),
    Rev = get_all_lines(File,[]),
    lists:reverse(Rev).

% Auxiliary function for get_file_contents.
% Not exported.
%-spec get_all_lines(nonempty_string(), []).
get_all_lines(File,Partial) ->
    case io:get_line(File,"") of
        eof -> file:close(File),
               Partial;
        Line -> {Strip,_} = lists:split(length(Line)-1,Line),
                get_all_lines(File,[Strip|Partial])
    end.

% Show the contents of a list of strings.
% Can be used to check the results of calling get_file_contents.
show_file_contents([L|Ls]) ->
    io:format("~s~n",[L]),
    show_file_contents(Ls);
 show_file_contents([]) ->
    ok.
