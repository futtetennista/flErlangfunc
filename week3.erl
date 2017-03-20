-module(week3).
-export([]).
-include_lib("eunit/include/eunit.hrl").


doubleAll(Xs) ->
    lists:map(fun(X) -> X*2 end,Xs).

evens(Xs) ->
    lists:filter(fun(X) -> X rem 2==0 end, Xs).

product(Xs) ->
    lists:foldr(fun(X, Acc) -> X*Acc end,1,Xs).

zip_test() ->
    [
     ?assertEqual([{1,2},{3,4}],zip([1,3,5,7], [2,4])),
     ?assertEqual([{1,2},{3,4}],zip([1,3], [2,4,6,8,10]))
    ].

zip([],_,Acc) ->
    Acc;
zip(_,[],Acc) ->
    Acc;
zip([X|Xs],[Y|Ys],Acc) ->
    zip(Xs,Ys,Acc ++ [{X,Y}]).

zip(Xs,Ys) ->
    zip(Xs,Ys,[]).

zip_with_test() ->
    [
     ?assertEqual([3,7],zip_with(fun(X,Y) -> X+Y end, [1,3,5,7], [2,4]))
    ].

zip_with(F,Xs,Ys) ->
    lists:map(fun({X,Y}) -> F(X,Y) end,zip(Xs,Ys)).


beat(rock) ->
    paper;
beat(paper) ->
    scissors;
beat(scissors) ->
    rock.

lose(rock) ->
    scissors;
lose(paper) ->
    rock;
lose(scissors) ->
    paper.

result_test() ->
    [
     ?assertEqual([scissors],result(paper,scissors)),
     ?assertEqual([scissors],result(scissors,paper)),
     ?assertEqual([paper],result(paper,rock)),
     ?assertEqual([paper],result(rock,paper)),
     ?assertEqual([rock],result(scissors,rock)),
     ?assertEqual([rock],result(rock,scissors)),
     ?assertEqual([],result(rock,rock)),
     ?assertEqual([],result(scissors,scissors)),
     ?assertEqual([],result(paper,paper))
 ].

result(X,Y) ->
    case {beat(X),lose(X)} of
        {Y,_} ->
            [Y];
        {_,Y} ->
            [X];
        _ ->
            []
    end.

tournament_test() ->
    [
     ?assertEqual(-1,week3:tournament([rock,rock,paper,paper],[rock,paper,scissors,rock])),
     ?assertEqual(+3,week3:tournament([rock,rock,paper,paper],[rock,scissors,rock,rock])),
     ?assertEqual(0,week3:tournament([rock,rock],[rock,rock]))
    ].

tournament(Xs,Ys) ->
    %% Results=lists:zipwith(fun outcome/2,Xs,Ys),
    %% lists:foldr(fun erlang:'+'/2,0,Results).
    lists:sum(lists:zipwith(fun outcome/2,Xs,Ys)).

outcome(X,Y) ->
    case result(X,Y) of
        [X] ->
            1;
        [Y] ->
            -1;
        [] ->
            0
    end.
