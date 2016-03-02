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


% bloodlust/4
bloodlust(Colour,CurrentBoard,NewBoard,Move) :-
  colour_to_string(Colour,ColourStr),
  findall([R,C,NewR,NewC],
      (
        cell(R,C),
        cell(NewR,NewC),
        what_in_cell(CurrentBoard,R,C,ColourStr),
        what_in_cell(CurrentBoard,NewR,NewC,' ')
      ), Moves),
  bloodlust(Colour, CurrentBoard, Moves, [(100,[])], NewBoard, Move).


%helper : bloodlust/8
bloodlust(_, _, [], [(_,Move,NewBoard)|_], NewBoard, Move).

bloodlust(Colour, CurrentBoard, [M|Ms], BestMoves, NewBoard, Move) :-
  count_after_move(M,Colour,CurrentBoard,NewBoard2,Blues,Reds),
  [(BestNo,_)|_] = BestMoves,
  (
    Colour = b,
    Reds < BestNo,
    Best2 is Reds,
    BestMove2 = M
    ;
    Colour = r,
    Blues < BestNo,
    Best2 is Blues,
    BestMove2 = M
  ),
  bloodlust(b, CurrentBoard, Ms, [(Best2,BestMove2,NewBoard2)|BestMoves],NewBoard,Move).

bloodlust(Colour, CurrentBoard, [_|Ms], BestMoves, NewBoard, Move) :-
  bloodlust(Colour, CurrentBoard, Ms, BestMoves, NewBoard, Move).

%colour_to_string/2
colour_to_string(b,'b').
colour_to_string(r,'r').


% count_after_move/4
% gives the number of each colour of tile left on board
% after move given as [R,C,NewR,NewC]

count_after_move(Move, Colour, [Blues, Reds],
                 NewBoard, NumberOfBlue, NumberOfRed) :-
  (
    Colour = b,
    alter_board(Move,Blues,NewBlues),
    next_generation([NewBlues,Reds],[FinalBlues,FinalReds])
    ;
    Colour = r,
    alter_board(Move,Reds,NewReds),
    next_generation([Blues,NewReds],[FinalBlues,FinalReds])
  ),
  NewBoard = [FinalBlues,FinalReds],
  length(FinalBlues,NumberOfBlue),
  length(FinalReds,NumberOfRed).



