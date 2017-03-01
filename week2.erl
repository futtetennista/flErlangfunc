-module(week2).
-export([]).

test() ->
    assert_equals("productR", 24, productR([1,2,3,4])),
    assert_equals("productTR", 24, productTR([1,2,3,4])),
    assert_equals("maxR", 3, maxR([1,2,3,-4])),
    assert_equals("maxTR", 3, maxTR([1,2,3,-4])),
    assert_equals("maxR (singleton list)", 1, maxR([1])),
    assert_equals("maxTR (singleton list)", 1, maxTR([1])),
    assert_equals("maxFold (singleton list)", 33, maxFold([1,11,6,33])),
    assert_equals("maxFold (singleton list)", 1, maxFold([1])),
    assert_equals("median of [9,8,6,5,3]", 6, median([9,8,6,5,3])),
    assert_equals("modes of [8,  11,  9,  14,  9,  15,  18,  6,  9,  10]",
                 [9], modes([8,  11,  9,  14,  9,  15,  18,  6,  9,  10])),
    assert_equals("modes of [15,  18,  18,  18,  20,  22,  24,  24,  24,  26,  26]",
                 [18,24], modes([15,  18,  18,  18,  20,  22,  24,  24,  24,  26,  26])).


assert_equals(Name,Expected,Actual) ->
    case Expected == Actual of
        true ->
            io:format("~s test passed~n", [Name]);
        false ->
            io:format("~s test failed: '~p' not equal to '~p'~n", [Name,Actual,Expected])
    end.

productR([]) ->
    1;
productR([X|Xs]) ->
    X*productR(Xs).

productTR([], Acc) ->
    Acc;
productTR([X|Xs], Acc) ->
    productTR(Xs, X*Acc).
productTR(X) ->
    productTR(X, 1).

maxR([X]) ->
    X;
maxR([X,Y]) ->
    max(X,Y);
maxR([X|Xs]) ->
    max(X,maxR(Xs)).

maxTR([], Max) ->
    Max;
maxTR([X|Xs], Max) ->
    maxTR(Xs, max(X,Max)).

maxTR([X|Xs]) ->
    maxTR(Xs, X).

maxFold([X|Xs]) ->
    lists:foldl(fun(Max,Y) -> max(Max,Y) end, X, Xs).

double([],Acc) ->
    Acc;
double([X|Xs],Acc) ->
    double(Xs,Acc++[2*X]).
double(Xs) ->
    double(Xs,[]).

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


median([]) ->
    [];
median(Xs) ->
    Ys = isort(Xs),
    find_median(Ys,Ys,0,hd(Ys)).

foldr(_,Acc,[]) ->
    Acc;
foldr(F,Acc,[X|Xs]) ->
    F(X,foldr(F,Acc,Xs)).
isort(Xs) ->
    foldr(fun insert/2,[],Xs).
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


modes(Xs) ->
    {Modes,_Occs}=modes(Xs,[]),
    Modes.
modes([],Occs) ->
    extract_modes(Occs,{[],0});
modes([X|Xs],Occs) ->
    modes(Xs,count_occurrence(X,Occs)).

count_occurrence(X,[{X,Occ}|Xs]) ->
    [{X,Occ+1}|Xs];
count_occurrence(X,[Y|Xs]) ->
    [Y] ++ count_occurrence(X,Xs);
count_occurrence(X,[]) ->
    [{X,1}].

extract_modes([],Acc) ->
    Acc;
extract_modes([{X,Occ}|Xs],{Ys,Occ}) ->
    extract_modes(Xs,{Ys++[X],Occ});
extract_modes([{X,OccX}|Xs],{_,OccY}) when OccX > OccY ->
    extract_modes(Xs,{[X],OccX});
extract_modes([{_,OccX}|Xs],{_,OccY}=Acc) when OccY > OccX ->
    extract_modes(Xs,Acc).
