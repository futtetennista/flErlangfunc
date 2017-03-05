-module(index).
-export([index/1]).


-spec index(nonempty_string()) -> [{nonempty_string(),[{integer(),integer()}]}].
index(Name) ->
    {Res,_} = build_index(get_file_contents(Name)),
    lists:sort(Res).

-spec build_index([nonempty_string()]) -> {[{nonempty_string(),[{integer(),integer()}]}],integer()}.
build_index(Lines) ->
    lists:foldl(fun(L,{Xs,LineNum}) -> {word_occurrences(nub(longer_than(3,nopunct(words(to_lower(L))))),LineNum,Xs),LineNum+1} end,{[],1},Lines).

nopunct(Xss) ->
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

-spec nub([T]) -> [T].
nub(Xs) ->
    lists:foldr(fun duplicate/2, [], Xs).
duplicate(X,Xs) ->
    case lists:member(X,Xs) of
        true ->
            Xs;
        false ->
            [X|Xs]
    end.

-spec words(nonempty_string()) -> [nonempty_string()].
words(Line) ->
    lists:filter(fun(Xs) -> Xs =/= [] end,lists:foldr(fun break_on_space/2,[[]],Line)).
break_on_space(X,Xss) when X == 32 -> % ASCII code for space
    [[]]++Xss;
break_on_space(X,Xss) ->
    [[X]++hd(Xss)]++tl(Xss).

-spec word_occurrences(string(),integer(),[{nonempty_string(),[{integer(),integer()}]}]) -> [{nonempty_string(),[{integer(),integer()}]}].
word_occurrences([],_,Acc) ->
    Acc;
word_occurrences([W|Ws],LineNum,Acc) ->
    word_occurrences(Ws,LineNum,save_occurrence(W,LineNum,Acc)).

-spec save_occurrence(string(),integer(),[{nonempty_string(),[{integer(),integer()}]}]) -> [{nonempty_string(),[{integer(),integer()}]}].
save_occurrence(W,LineNum,[]) ->
    [{W,[{LineNum,LineNum}]}];
save_occurrence(W,LineNum,[{W,Occs}|Xs]) ->
    [{W,normalise_occurrences(Occs++[{LineNum,LineNum}])}|save_occurrence(W,LineNum,Xs)];
save_occurrence(W,LineNum,[X|Xs]) ->
    [X|save_occurrence(W,LineNum,Xs)].

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
