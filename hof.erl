-module(hof).
-export([add/1,times/1,compose/2,id/1,iterate/1,composef/1]).

add(X) ->
    fun(Y) -> X+Y end.

times(X) ->
    fun(Y) -> X*Y end.

compose(F,G) ->
    fun(X) -> G(F(X)) end.

id(X) ->
    X.

%% Define a function iterate that takes a number N and returns a function that takes a function and returns that function iterated N times. When N is zero, it should return the identity function (that is, the function that returns its argument unchanged).
iterate(0) ->
    fun(_) -> fun id/1 end;
iterate(N) ->
    fun(F) -> compose(F, (iterate(N-1))(F))  end.

composef([]) ->
    fun id/1;
composef(Fs) ->
    fun(X) -> lists:foldr(fun(F,Acc) -> F(Acc) end,X,Fs) end.

twice(F,X) ->
    (composef([F,F]))(X).


foldr(_,Zero,[]) ->
    Zero;
foldr(F,Zero,[X|Xs]) ->
    F(X,foldr(F,Zero,Xs)).

%% Why are foldl different ?!?!
foldlhs(_,Acc,[]) ->
    Zero;
foldlhs(F,Acc,[X|Xs]) ->
    foldlhs(F,F(Acc,X),Xs).
foldlerl(_,Acc,[]) ->
    Zero;
foldlerl(F,Acc,[X|Xs]) ->
    foldlerl(F,F(X,Acc),Xs).
