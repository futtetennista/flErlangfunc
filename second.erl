-module(second).
-export([hypothenuse/2, trianglePerimeter/2, triangleArea/2]).

hypothenuse(A, B) ->
    first:sqrt(square(A) + square(B)).

square(X) ->
    first:mult(X, X).

trianglePerimeter(A, B) ->
    C = hypothenuse(A, B),
    A + B + C.

triangleArea(A, B) ->
    first:mult(A, B) / 2.
