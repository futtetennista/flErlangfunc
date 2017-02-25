-module(first).
-export([double/1, mult/2, area/3, treble/1, sqrt/1]).

mult(X, Y) ->
     X * Y.

double(X) ->
     mult(2, X).

area(A, B, C) ->
    S = (A + B + C) / 2,
    math:sqrt(S*(S-A)*(S-B)*(S-C)).

treble(X) ->
    mult(X, 3).

sqrt(X) ->
    math:sqrt(X).
