-module(rps).
-export([play/1,echo/1,play_two/3,rock/1,no_repeat/1,const/1,enum/1,cycle/1,rand/1,val/1,tournament/2]).


%
% play one strategy against another, for N moves.
%

play_two(StrategyL,StrategyR,N) ->
    play_two(StrategyL,StrategyR,[],[],N).

% tail recursive loop for play_two/5
% 0 case computes the result of the tournament
play_two(_,_,PlaysL,PlaysR,0) ->
    FinalResult=tournament(PlaysL,PlaysR),
    io:format("Final Result: ~p~n",[FinalResult]);
play_two(StrategyL,StrategyR,PlaysL,PlaysR,N) ->
    PlayL=StrategyL(PlaysR),
    PlayR=StrategyR(PlaysL),
    PlayResult=result(PlayL,PlayR),
    io:format("Play Result: ~p~n",[outcome(PlayResult)]),
    play_two(StrategyL,StrategyR,[PlayL|PlaysL],[PlayR|PlaysR],N-1).

%
% interactively play against a strategy, provided as argument.
%

play(Strategy) ->
    io:format("Rock - paper - scissors~n"),
    io:format("Play one of rock, paper, scissors, ...~n"),
    io:format("... r, p, s, stop, followed by '.'~n"),
    play(Strategy,[]).

% tail recursive loop for play/1

play(Strategy,Moves) ->
    {ok,P} = io:read("Play: "),
    Play = expand(P),
    case Play of
	stop ->
	    io:format("Stopped~n");
	_    ->
	    Result = result(Play,Strategy(Moves)),
	    io:format("Result: ~p~n",[Result]),
	    play(Strategy,[Play|Moves])
    end.

%
% auxiliary functions
%

% transform shorthand atoms to expanded form

expand(r) -> rock;
expand(p) -> paper;
expand(s) -> scissors;
expand(X) -> X.

% result of one set of plays

result(rock,rock) -> draw;
result(rock,paper) -> lose;
result(rock,scissors) -> win;
result(paper,rock) -> win;
result(paper,paper) -> draw;
result(paper,scissors) -> lose;
result(scissors,rock) -> lose;
result(scissors,paper) -> win;
result(scissors,scissors) -> draw.

% result of a tournament

tournament(PlaysL,PlaysR) ->
    lists:sum(
      lists:map(fun outcome/1,
		lists:zipwith(fun result/2,PlaysL,PlaysR))).

outcome(win)  ->  1;
outcome(lose) -> -1;
outcome(draw) ->  0.

% transform 0, 1, 2 to rock, paper, scissors and vice versa.

enum(0) ->
    rock;
enum(1) ->
    paper;
enum(2) ->
    scissors.

val(rock) ->
    0;
val(paper) ->
    1;
val(scissors) ->
    2.

% give the play which the argument beats.

beats(rock) ->
    scissors;
beats(paper) ->
    rock;
beats(scissors) ->
    paper.

beat(rock) ->
    paper;
beat(paper) ->
    scissors;
beat(scissors) ->
    rock.


%
% strategies.
%
echo([]) ->
     paper;
echo([Last|_]) ->
    Last.

rock(_) ->
    rock.

no_repeat([]) ->
    rock;
no_repeat([X|_]) ->
    case beats(X) of
        scissors ->
            paper;
        rock ->
            scissors;
        paper ->
            rock
    end.

const(_Plays) ->
    paper.

cycle(Plays) ->
    enum(length(Plays) rem 3).

rand(_) ->
    enum(random:uniform(3) - 1).

least_freq(Plays) ->
    least_freq(Plays,[{r,0},{p,0},{s,0}]).
least_freq([],[{r,X},{p,Y},{s,Z}]) ->
    case lists:min([X,Y,Z]) of
        X ->
            rock;
        Y ->
            paper;
        Z ->
            scissors
    end;
least_freq([rock|Plays],[{r,X},{p,Y},{s,Z}]) ->
    least_freq(Plays,[{r,X+1},{p,Y},{s,Z}]);
least_freq([paper|Plays],[{r,X},{p,Y},{s,Z}]) ->
    least_freq(Plays,[{r,X},{p,Y+1},{s,Z}]);
least_freq([scissors|Plays],[{r,X},{p,Y},{s,Z}]) ->
    least_freq(Plays,[{r,X},{p,Y},{s,Z+1}]).

most_freq(Plays) ->
    mode(Plays,maps:new()).
mode([],Dict) ->
    {Play,_}=maps:fold(fun(K,V,{_,Occ}=Acc) ->
                               case V > Occ of
                                   true ->
                                       {K,V};
                                   false ->
                                       Acc
                               end
                       end,{rock,0},Dict),
    Play;
mode([X|Xs],Dict) ->
    F=fun(Occs) -> Occs+1 end,
    case maps:is_key(X,Dict) of
        true ->
            mode(Xs,maps:update_with(X,F,Dict));
        false ->
            mode(Xs,maps:put(X,1,Dict))
    end.

all_strategies() ->
    [fun echo/1
    ,fun rock/1
    ,fun no_repeat/1
    ,fun const/1
    ,fun cycle/1
    ,fun rand/1
    ,fun least_freq/1
    ,fun most_freq/1].

rand_among(Strategies) ->
    RandomStrategy=lists:nth(random:uniform(length(Strategies)),Strategies),
    fun(Plays) -> RandomStrategy(Plays) end.

best_among(Strategies) ->
    fun(Plays) ->
            BestStrategy=pick_best_strategy(Strategies,Plays),
            BestStrategy(Plays)
    end.

pick_best_strategy(Strategies,PlaysL) ->
    Init={rand_among(Strategies),0},
    {BestStrategy,_}=lists:foldr(fun(Strategy,{_BestStrategy,MaxWins}=Acc) ->
                                         PlaysR=replay_strategy(Strategy,PlaysL),
                                         %% -N means right wins
                                         Wins=tournament(PlaysL,PlaysR),
                                         %% wins is a negative for right, so pick the smaller value
                                         case Wins > MaxWins of
                                             true ->
                                                 Acc;
                                             false ->
                                                 {Strategy,Wins}
                                         end
                                 end,Init,Strategies),
    BestStrategy.

%% Build plays from opponent's ones.
replay_strategy(Strategy,Plays) ->
    {_,PlaysR}=lists:foldr(fun(PL,{PLs,PRs}) ->
                                   PlaysLeft=[PL|PLs],
                                   PlaysRight=[Strategy(PlaysLeft)|PRs],
                                   {PlaysLeft,PlaysRight}
                           end,{[],[]},Plays),
    PlaysR.
