-module(week1).
-export([testXors/0, xOr1/2, xOr2/2, xOr3/2, xOr4/2, xOr5/2, xOr6/2, maxThree/3, howManyEqual/3, fib/1, maxPieces/1, fib1/1, fib/3, perfect/1, perfect1/1]).


testXor(F) ->
    not(F(true,true)) and not(F(false,false)) and F(false,true) and F(true,false).

testXord([F|[]]) ->
    testXor(F);
testXord([F|FS]) ->
    testXor(F) and testXord(FS).

testXors() ->
    %% This doesn't work: how can it be done?
    Xors = [xOr1],
    testXord(Xors).

xOr1(true,false) ->
    true;
xOr1(false,true) ->
    true;
xOr1(_,_) ->
    false.

xOr2(X,X) ->
    false;
xOr2(_,_) ->
    true.

xOr3(X,Y) ->
    X =/= Y.

xOr4(X,Y) ->
    not(X == Y).

xOr5(X,Y) ->
    not(X and Y) and (X or Y).

xOr6(true,X) ->
    not(X);
xOr6(false,X) ->
    not(not(X)).

maxThree(X,Y,Z) ->
    max(max(X,Y),Z).

howManyEqual(X,X,X) ->
    3;
howManyEqual(X,X,_) ->
    2;
howManyEqual(X,_,X) ->
    2;
howManyEqual(_,X,X) ->
    2;
howManyEqual(_,_,_) ->
    0.

fib(0) ->
    0;
fib(1) ->
    1;
fib(X) when X>0 ->
    fib(X - 2) + fib(X - 1).

maxPieces(0) ->
    1;
maxPieces(X) when X>0 ->
    X + maxPieces(X-1).

fib(0,Prev,_Curr) ->
    Prev;
fib(N,Prev,Curr) ->
    fib(N-1,Curr,Prev+Curr).

fib1(X) ->
    fib(X, 0, 1).

fibd(0) ->
    {0,1};
fibd(N) ->
    {Prev,Curr} = fibd(N-1),
    {Curr,Prev+Curr}.

fib2(N) ->
    {Prev,_} = fibd(N),
    Prev.

divisors(X) ->
    lists:filter(fun(Y) -> X rem Y == 0 end, lists:seq(1, X div 2)).

perfect1(X) ->
    lists:sum(divisors(X)) == X.

perfect(X) ->
    perfect(X, X div 2, 0).

%% Optimisation
perfect(X,_,Acc) when Acc > X ->
    false;
perfect(X,1,Acc) ->
    1 + Acc == X;
perfect(X,Y,Acc) when X rem Y == 0 ->
    perfect(X, Y - 1, Acc + Y);
perfect(X,Y,Acc) ->
    perfect(X, Y - 1, Acc).

perfect1(X) ->
    perfect1(X, 0, X div 2).
perfect1(X, X, 0) ->
    true;
perfect1(X,_,0) ->
    false;
perfect1(X, Acc, Div) when X rem Div == 0 ->
    perfect(X,Acc+Div,Div-1);
perfect1(X,Acc,Div) ->
    perfect(X,Acc,Div-1).
