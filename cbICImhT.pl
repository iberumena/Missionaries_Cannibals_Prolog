% Missionaries and Cannibals.

% we define the postions in which we want the missionaries and cannibals 
% to start and finish.
start([3,3,left,0,0]).
goal([0,0,right,3,3]).

% We create a number of restrictions so there will never be more 
% cannibals than missionaries on any given side of the river, the 
% variables CL, ML, CR and MR represent the number of missionaries 
% and cannibals on the left or right side of the river.
restrictions(CL,ML,CR,MR) :-
	ML>=0, CL>=0, MR>=0, CR>=0,
	(ML>=CL ; ML=0),
	(MR>=CR ; MR=0).

% Next we create all the possible moves that the boat is allowed to make
% making it so that the boat never fits more than 2 people or Less than 1. 
% One missionary crosses left to right.
move([CL,ML,left,CR,MR],[CL,ML2,right,CR,MR2]):-
	% Two missionaries cross left to right.
	MR2 is MR+2,
	ML2 is ML-2,
	restrictions(CL,ML2,CR,MR2).

move([CL,ML,left,CR,MR],[CL2,ML,right,CR2,MR]):-
	% Two cannibals cross left to right.
	CR2 is CR+2,
	CL2 is CL-2,
	restrictions(CL2,ML,CR2,MR).

move([CL,ML,left,CR,MR],[CL2,ML2,right,CR2,MR2]):-
	%  One missionary and one cannibal cross left to right.
	CR2 is CR+1,
	CL2 is CL-1,
	MR2 is MR+1,
	ML2 is ML-1,
	restrictions(CL2,ML2,CR2,MR2).

move([CL,ML,left,CR,MR],[CL,ML2,right,CR,MR2]):-
	% One missionary crosses left to right.
	MR2 is MR+1,
	ML2 is ML-1,
	restrictions(CL,ML2,CR,MR2).

move([CL,ML,left,CR,MR],[CL2,ML,right,CR2,MR]):-
	% One cannibal crosses left to right.
	CR2 is CR+1,
	CL2 is CL-1,
	restrictions(CL2,ML,CR2,MR).

move([CL,ML,right,CR,MR],[CL,ML2,left,CR,MR2]):-
	% Two missionaries cross right to left.
	MR2 is MR-2,
	ML2 is ML+2,
	restrictions(CL,ML2,CR,MR2).

move([CL,ML,right,CR,MR],[CL2,ML,left,CR2,MR]):-
	% Two cannibals cross right to left.
	CR2 is CR-2,
	CL2 is CL+2,
	restrictions(CL2,ML,CR2,MR).

move([CL,ML,right,CR,MR],[CL2,ML2,left,CR2,MR2]):-
	%  One missionary and one cannibal cross right to left.
	CR2 is CR-1,
	CL2 is CL+1,
	MR2 is MR-1,
	ML2 is ML+1,
	restrictions(CL2,ML2,CR2,MR2).

move([CL,ML,right,CR,MR],[CL,ML2,left,CR,MR2]):-
	% One missionary crosses right to left.
	MR2 is MR-1,
	ML2 is ML+1,
	restrictions(CL,ML2,CR,MR2).

move([CL,ML,right,CR,MR],[CL2,ML,left,CR2,MR]):-
	% One cannibal crosses right to left.
	CR2 is CR-1,
	CL2 is CL+1,
	restrictions(CL2,ML,CR2,MR).

% The next function is given our initial state, goal and the possible moves
% the boat can make, we also ask it to keep record of all the moves used as
% well as the previous moves, this ones are stored in Explored and MovesList
boat_moves([CL1,ML1,B1,CR1,MR1],[CL2,ML2,B2,CR2,MR2],Explored,MovesList) :- 
   move([CL1,ML1,B1,CR1,MR1],[CL3,ML3,B3,CR3,MR3]), 
   not(member([CL3,ML3,B3,CR3,MR3],Explored)),
   boat_moves([CL3,ML3,B3,CR3,MR3],[CL2,ML2,B2,CR2,MR2],[[CL3,ML3,B3,CR3,MR3]|Explored],[ [[CL3,ML3,B3,CR3,MR3],[CL1,ML1,B1,CR1,MR1]] | MovesList ]).


% We then call the function again so that it returns the Moves List that contains
% all the moves it did in order to reach the goal
boat_moves([CL, ML, B, CR, MR], [CL, ML, B, CR, MR], _, MovesList) :-
	output(MovesList).

% We print the results
output([]) :- nl. 
output([[A,B]|MovesList]) :- 
	output(MovesList), 
   	write(B), write(' -> '), write(A), nl.

% We initialize a procedure to start the function boat_moves, if it is
% able to solve it will return the list of moves done and TRUE
find :- 
   boat_moves([3,3,left,0,0],[0,0,right,3,3],[[3,3,left,0,0]],_).