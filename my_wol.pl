% keep imports from other file
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(random)).

% test_strategy/3

test_strategy(N, Strat1, Strat2) :-
  test_strategy(N, 0, Strat1, Strat2, 0, 0, 0, 0, 0, 0, 100).

% helper : test_strategy/11

test_strategy(NTot, NTot, _, _, TotTime, TotMoves,          %if we've finished
              BWins, RWins, NumDraws, MostMoves, LeastMoves) :-
  AvgMoves is TotMoves/NTot,
  AvgTime is TotTime/NTot,
  format('number of draws = ~d~n', [NumDraws]), 
  format('player 1 (b) wins = ~d~n', [BWins]), 
  format('player 2 (r) wins = ~d~n', [RWins]), 
  format('longest game = ~d~n', [MostMoves]), 
  format('shortest game = ~d~n', [LeastMoves]), 
  format('average moves = ~d~n', [AvgMoves]), 
  format('average time = ~3d seconds~n', [AvgTime]). 

test_strategy(NTot, N, Strat1, Strat2, TotTime, TotMoves, 
              BWins, RWins, NumDraws, MostMoves, LeastMoves) :-
  statistics(runtime, [T0|_]),
  play(quiet,Strat1,Strat2,Moves,Winner),
  statistics(runtime, [T1|_]),        %time game
  TotTime2 is TotTime + T1 - T0,      %increase total time
  TotMoves2 is TotMoves + Moves,      %increase total moves
  (
    Winner == r,
    NumDraws2 is NumDraws,
    BWins2 is BWins,
    RWins2 is RWins + 1 %if red wins
    ;
    Winner == b,
    NumDraws2 is NumDraws,
    BWins2 is BWins + 1,
    RWins2 is RWins     %if blue wins
    ;
    NumDraws2 is NumDraws + 1,
    BWins2 is BWins,
    RWins2 is RWins     %if game is a draw
  ),
  (
    Moves > MostMoves,
    Moves =\= 250,
    MostMoves2 is Moves,
    LeastMoves2 is LeastMoves %if new longest game
    ;
    Moves < LeastMoves,
    MostMoves2 is MostMoves,
    LeastMoves2 is Moves      %if new shortest game
    ;
    MostMoves2 is MostMoves,
    LeastMoves2 is LeastMoves %otherwise
  ),
  N2 is N + 1,   %increment counter
  test_strategy(NTot,N2,Strat1,Strat2,TotTime2,TotMoves2,
                BWins2,RWins2,NumDraws2,MostMoves2,LeastMoves2). %tail recurse
