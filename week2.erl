-module(week2).
-export([product_r/1,product_tr/1,max_r/1,max_tr/1,median/1,modes/1,take/2,nub/1,bun/1,palindrome/1]).
-include_lib("eunit/include/eunit.hrl").


product_test() ->
    producttest(fun product_r/1),
    producttest(fun product_tr/1).
producttest(F) ->
    {"Product",
     [
      {"Non-empty list", ?assertEqual(24, F([1,2,3,4]))},
      {"Empty list", ?assertEqual(1, F([]))}
     ]
    }.

max_test() ->
    [
     ?assertEqual(3, max_r([1,2,3,-4])),
     ?assertEqual(3, max_tr([1,2,3,-4])),
     ?assertEqual(1, max_r([1])),
     ?assertEqual(1, max_tr([1])),
     ?assertEqual(33, max_fold([1,11,6,33])),
     ?assertEqual(1, max_fold([1]))
    ].

median_test() ->
    [
     ?assertEqual(6, median([9,8,6,5,3])),
     ?assertEqual(6, median([9,8,6,5,3,11])),
     ?assertEqual(9, median([9]))
    ].

modes_test() ->
    [
     ?assertEqual([9], modes([8,  11,  9,  14,  9,  15,  18,  6,  9,  10])),
     ?assertEqual([18,24], modes([15,  18,  18,  18,  20,  22,  24,  24,  24,  26,  26]))
    ].

palindrome_test() ->
    [
     {"'Madam I\'m Adam' is a palindrome", ?assertEqual(true,palindrome("Madam I\'m Adam"))},
      {"'Hello World' is a not palindrome", ?assertEqual(false,palindrome("Madam I\'m Adamo"))}
    ].


product_r([]) ->
    1;
product_r([X|Xs]) ->
    X*product_r(Xs).

product_tr([], Acc) ->
    Acc;
product_tr([X|Xs], Acc) ->
    product_tr(Xs, X*Acc).
product_tr(X) ->
    product_tr(X, 1).

max_r([X]) ->
    X;
max_r([X,Y]) ->
    max(X,Y);
max_r([X|Xs]) ->
    max(X,max_r(Xs)).

max_tr([], Max) ->
    Max;
max_tr([X|Xs], Max) ->
    max_tr(Xs, max(X,Max)).

max_tr([X|Xs]) ->
    max_tr(Xs, X).

max_fold([X|Xs]) ->
    lists:foldl(fun(Max,Y) -> max(Max,Y) end, X, Xs).

%% Define an Erlang function double/1 to double the elements of a list of numbers.
double([],Acc) ->
    Acc;
double([X|Xs],Acc) ->
    double(Xs,Acc++[2*X]).
double(Xs) ->
    double(Xs,[]).

%% Define a function evens/1 that extracts the even numbers from a list of integers.
even([],Acc) ->
    Acc;
even([X|Xs],Acc) when X rem 2 == 0 ->
    even(Xs,Acc++[X]);
even([_|Xs],Acc) ->
    even(Xs,Acc).
even(Xs) ->
    even(Xs,[]).

even1(Xs) ->
    filter(fun(X) -> X rem 2 == 0 end, Xs).
filter(_,[]) ->
    [];
filter(F,[X|Xs]) ->
    case F(X) of
        true -> [X|filter(F,Xs)];
        false -> filter(F,Xs)
    end.

%% the median of a list of numbers: this is the middle element when the list is ordered (if the list is of even length you should average the middle two)
median(Xs) ->
    Ys = isort(Xs),
    find_median(Ys,Ys,0,hd(Ys)).

isort(Xs) ->
    foldr(fun insert/2,[],Xs).
foldr(_,Acc,[]) ->
    Acc;
foldr(F,Acc,[X|Xs]) ->
    F(X,foldr(F,Acc,Xs)).
insert(X,Xs) ->
    foldr(fun swap/2,[X],Xs).
swap(Y,[X|_Xs]=Acc) when X > Y ->
    [Y|Acc];
swap(Y,[X|Xs]) ->
    [X|[Y|Xs]].

find_median([],_,_,Median) ->
    Median;
find_median([_|Xs],[Y|Ys],Idx,_) when Idx rem 2 == 0 ->
    find_median(Xs,Ys,Idx+1,Y);
find_median([_|Xs],Ys,Idx,Median) ->
    find_median(Xs,Ys,Idx+1,Median).

%% the modes of a list of numbers: this is a list consisting of the numbers that occur most frequently in the list; if there is is just one, this will be a list with one element only
modes(Xs) ->
    {Modes,_N}=modes(Xs,[]),
    Modes.
modes([],Occs) ->
    extract_modes(Occs,{[],0});
modes([X|Xs],Occs) ->
    modes(Xs,count_occurrence(X,[],Occs)).

count_occurrence(X,Init,[{X,Occ}|Tail]) ->
    Init ++ [{X,Occ+1}] ++ Tail;
count_occurrence(X,Init, [Y|Tail]) ->
    count_occurrence(X,Init ++ [Y],Tail);
count_occurrence(X,Init,[]) ->
    Init ++ [{X,1}].

extract_modes([],Acc) ->
    Acc;
extract_modes([{X,Occ}|Xs],{Ys,Occ}) ->
    extract_modes(Xs,{Ys++[X],Occ});
extract_modes([{X,OccX}|Xs],{_,OccY}) when OccX > OccY ->
    extract_modes(Xs,{[X],OccX});
extract_modes([{_,OccX}|Xs],{_,OccY}=Acc) when OccY > OccX ->
    extract_modes(Xs,Acc).

-spec take(integer(),[T]) -> [T].
take(N,Xs) ->
    take(N,Xs,[]).
take(_,[],Acc) ->
    Acc;
take(0,_,Acc) ->
    Acc;
take(N,[X|Xs],Acc) when N > 0 ->
    take(N-1,Xs,Acc++[X]).

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

-spec bun([T]) -> [T].
bun([]) ->
    [];
bun([X|Xs]) ->
    case lists:member(X,Xs) of
        true ->
            bun(Xs);
        false ->
            [X|bun(Xs)]
    end.

-spec bun1([T]) -> [T].
bun1(Xs) ->
    lists:foldl(fun duplicate1/2, [], Xs).
duplicate1(X,Xs) ->
    case lists:member(X,Xs) of
        true ->
            Xs;
        false ->
            Xs ++ [X]
    end.

-spec palindrome([integer()]) -> boolean().
palindrome(Xs) ->
    Ys = to_lower(nopunct(Xs,[]),[]),
    Ys == lists:reverse(Ys,[]).

to_lower([],Acc) ->
    Acc;
to_lower([X|Xs],Acc) when X >= $A andalso X < $Z ->
    XtoLowerCase = X + 32,
    to_lower(Xs,Acc ++ [XtoLowerCase]);
to_lower([X|Xs],Acc) ->
    to_lower(Xs,Acc ++ [X]).

% skip character if is not a letter
nopunct([],Acc) ->
    Acc;
nopunct([X|Xs],Acc) ->
    case is_letter(X) of
        true ->
            nopunct(Xs,Acc++[X]);
        false ->
            nopunct(Xs,Acc)
    end.

is_letter(X) when (X >= $A andalso X =< $Z) orelse (X >= $a andalso X =< $z) ->
    true;
is_letter(_X) ->
    false.
