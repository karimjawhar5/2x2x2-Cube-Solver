/* ===========================================================================
    author : Karim Jaouhar
     first : Mon Oct 31 2022
    latest : Fri Nov 4 2022

    Solves the 2x2x2 Rubik's mini-cube ...
=========================================================================== */

:- use_module(r2x2x2).

:- dynamic sphere/4, sphere/3.

%----------------------------------------------------------------------------%
% twosy
%     State: state of user cube.
%     Path:  shortest path to solving the cube.

twosy(State, Path) :-
    retractall(sphere(_,_,_,_)),
    build_sphere(scrambled,State,[],0),
    state_zero(Solved),
    build_sphere(solved,Solved,[],0),
    build_tree(solved,0),
    build_tree(scrambled,0),
    find_path(Path),
    apply_path(State,Path).

% build_sphere
%   Origin: originated from 'scrambled' or 'solved' State
%   State: state of the cube
%   Move: list of moves leading upto State from original scrambled or solved state.
%   Level: the level that this sphere belongs to.
% build_sphere/4 Asserts a sphere/4 with Origin, State, Move, Level.

build_sphere(Origin, State, _, Level):-
   sphere(Origin, State, _, Level),
   !.

build_sphere(Origin, State, Move, Level):-
    assertz(sphere(Origin, State, Move, Level)).

% build_tree
%   Origin: 'scrambled' or 'solved' (Tree's Origin State)
%   Level: Level to build from.
% build_tree/2 recursivly builds the children of the tree originating from Origin,
% until either the Level is 7, or atleast two spheres that share a common state 
% and opposite origins are found.

build_tree(_, Level):-
    Level is 7,
    !.

build_tree(_,_):-
    sphere(scrambled,State,_,_),
    sphere(solved,State,_,_),
    !.

build_tree(Origin, Level):-
    sphere(Origin,State,PM,Level),
    move(M),
    move_transform(State,M,ChildState),
    Level2 is Level+1,
    concat([M], PM, NewMoves),
    build_sphere(Origin,ChildState,NewMoves,Level2),
    fail.

build_tree(Origin, Level):-
    Level2 is Level+1,
    build_tree(Origin, Level2).

% find_path:
%   Path: an ordered list of moves leading the given scrambled state to the solved state.
% find_path/1, assigns Path to an ordered list of moves leading the given scrambled state to the solved state.
% it find a Path by: finding two spheres that share a common state but opposite origins,
% flipping the list of moves from the sphere with origin 'scrambled', and
% reversing all the moves in the list of moves from the sphere with origin 'solved',
% the concatenation of both those list is Path.


find_path(Path):-
    sphere(scrambled,State,Ms,_),
    sphere(solved,State,RMs,_),
    reverse_all_moves(RMs,VMoves),
    reverse(Ms,RevMoves),
    concat(RevMoves,VMoves,Path).

%reverse_all_moves:
% List_of_moves: a list of valid moves.
% List_of_moves_reversed: list of the revese of the moves in List_of_moves.
% reverse_all_moves/2 recursivly makes List_of_moves_reversed containing the reversal of each move in List_of_moves

reverse_all_moves([],RMoves):- RMoves =[].

reverse_all_moves([M|MRest], [RM|RMREst]):-
    reversal(M,RM),
    reverse_all_moves(MRest,RMREst).

%concat:
%   List1: List that should appear in the first half of List3.
%   List2: List that should appear in the second half of List3.
%   List3: the concatenation of List1 + List2
% concat/3 recursivly makes List3 by: taking the head of List1 to until the List1 is empty,
% then making list 2 the rest of List3.

concat([],L,L).
concat([X|L1],L2,[X|L3]):-
    concat(L1,L2,L3).
