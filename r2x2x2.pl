/* ===========================================================================
    author : parke godfrey
     first : 2022-09-24
    latest : 2022-10-11

    Utilities for representing the 2x2x2 Rubik's mini-cube, moves,
    and for applying moves.
=========================================================================== */

:- module(r2x2x2, [colouring/1,
                   state_zero/1,
                   state_w_soln/3,
                   apply_path/2,
                   display_path/2,
                   display_move_by_move/2,
                   shuffle/2,
                   move/1,
                   reversal/2,
                   move_transform/3]).
:- license(gplv2).

:- use_module(r2x2x2pce).

% ==============================================================================
% MINI-CUBE ENCODING

% The cube has six faces: 0 .. 5.
% By our convention, face (5 - F) is opposite face F.
% Let one corner be 012, reading the faces it shares clockwise,
% starting with face 0.
%
% Hold the cube with that corner 012 closest you and face 0 on top.
% Three faces are hidden then: 5 (5 - 0), 4 (5 - 1), and 3 (5 - 2).
% Nickname these three "bottom" (5), "left" (4), and "right" (3).

% if you have a physical mini-cube and want to match here the way
% it is coloured, change the numbers for the faces with letters instead
% that denote your colours. E.g.,
% 0. k: blacK
% 1. b: Blue
% 2. r: Red
% 3. o: Orange
% 4. g: Green
% 5. y: Yellow

% ------------------------------------------------------------------------------
% CROSS PICTURE

% The solved cube in "cross picture" then looks like:
%   kk       00
%   kk       00
% ggrrbb   442211
% ggrrbb   442211
%   yy       55
%   yy       55
%   oo       33
%   oo       33

% So, for a colouring, list the faces of the solved cube in the order
% as [0, 4, 2, 1, 5, 3], or as [k, g, r, b, y, o], given the colouring
% above.
% ------------------------------------------------------------------------------

:- dynamic colouring/1.
colouring([black, green, red, blue, yellow, orange]).

% ------------------------------------------------------------------------------
% STATE

% Each of the eight corner pieces of the mini-cube participate in three
% adjacent faces of the cube's six faces. We can "name" a corner piece by
% its three face colours. Given the faces 0 .. 5 in the pattern as defined
% above, the eight corners are 012, 024, 043, 031, 521, 513, 534, and 542.
% For corner 521, we can write it as 152, 215, or 521, depending on how
% it is oriented with respect to the cube over all. And likewise with the
% others.

% We can choose one corner to NEVER move, without loss of generality: 012.
% Therefore, we could encode any state of the mini cube by stating where,
% and in what orientation, the seven other corners are in. And as we have
% fixed corner 012 to never move, where, and in what orientation, the
% other seven are will be with respect to corner 012.

% Thus, we represent the state of the cube by the sequence of seven
% corner states.  Hold the cube so corner 012 is facing you, and face
% 0 is up (so face 1 is to your right and face 2 to your left).  Look
% down on face 0.  The first corner clockwise from 012 is position
% 1; the next corner going clockwise is position 2; and the final
% corner going clockwise is position 3.  The order of the faces in
% listing the corner is to start with the top face (0), and walk the
% corner clockwise.  So position 1 is 024, for example, in the solved
% state.

% For positions 4 to 7, turn the cube over.  So now face 5 is up.
% The corner adjacent to 012 is position 4.  Positions 5 to 7 are the
% corners walking clockwise around face 5 from position 3.  Each
% corner is named by listing its colors from top (on face 5) and
% walking clockwise.

% Any cube state can be encoded this way.
% ------------------------------------------------------------------------------

% the solved state ("state zero")
state_zero('024 043 031 521 513 534 542').

% ------------------------------------------------------------------------------
% MOVES

% Since we keep corner 012 fixed, we have six possible 90 degree moves
% from any state:
% Looking down on face 5, the BOTTOM (b) face, we can rotate it 90
% degrees clockwise (c) or anti-clockwise (a). Let us name those two
% moves "bc" and "ba", respectively.
% Looking down on face 4, call this LEFT (l), we have two moves twisting
% it either clockwise, "lc", or anti-clockwise, "la".
% Looking down on face 3, call this RIGHT (r), we likewise have two moves,
% "rc" and "ra".
% ------------------------------------------------------------------------------

% far: a state as scrambled as you can get, 14 moves from solved
state_w_soln(far,
             '351 304 215 453 240 542 310',
             [bc, bc, lc, ba, la, ra, bc, la, rc, rc, ba, rc, ba, rc]).
% meson: adjacent corners rotated, 12 moves from solved
state_w_soln(meson,
             '024 043 031 152 135 534 542',
             [bc, la, bc, lc, ba, rc, ba, lc, ra, la, bc, ra]).
% scrambled: no color twice on any face, 11 moves from solved
state_w_soln(scrambled,
             '240 430 310 135 345 425 521',
             [bc, bc, rc, bc, la, ba, rc, lc, ba, lc, bc]).

% ------------------------------------------------------------------------------
% PATH

% A path is a list of moves. A path applied to a START state applies the
% first move in the path list to START to get a NEXT state, recursively
% applying subsequent moves until there are no more moves, resulting in a
% FINAL state. Let us say if the path applied to START results in the
% SOLVED state, that the path "solves" START.
% ------------------------------------------------------------------------------
% apply_path
%     +Start : the start state
%     +Moves : the moves to apply
% This has the side-effect of writing out the moves and the
% intermediate states. 
% The call succeeds if the final state reached is the solved state,
% State Zero. Otherwise, it fails.

apply_path(Start, Moves) :-
    write("          "), write(Start),
    apply_path_(Start, Moves, 1).

apply_path_(Start, [Mv|Mvs], Step) :-
    write("\n"),
    move_transform(Start, Mv, Next),
    writef("%3r. [", [Step]), write(Mv), write("] "), write(Next),
    Step2 is Step + 1,
    !,
    apply_path_(Next, Mvs, Step2).
apply_path_(State, [], _) :-
    state_zero(State),
    !,
    write("    SOLVED!\n").
apply_path_(_, [], _) :-
    write("\n"),
    fail.

% ------------------------------------------------------------------------------
% display_path
%     +Start : the start state
%     +Moves : the moves to apply
% This has the side-effect of raising a GUI window to show the state of
% the cube after each move.  It pauses a second between subsequent
% moves. The host must support XPCE for this to work.
% The call succeeds if the final state reached is the solved state,
% State Zero. Otherwise, it fails.

display_path(Start, Moves) :-
    map_state_cube(Start, Cube),
    r2x2x2(Cube, Frame),
    !,
    display_path_(Start, Moves, 1, Frame).

display_path(Start, Moves, Title) :-
    map_state_cube(Start, Cube),
    r2x2x2(Cube, Frame, Title),
    !,
    display_path_(Start, Moves, 1, Frame).

display_path_(Start, [Mv|Mvs], Step, Frame) :-
    move_transform(Start, Mv, Next),
    map_state_cube(Next, Cube),
    sleep(1.0),
    u2x2x2(Cube, Frame),
    Step2 is Step + 1,
    !,
    display_path_(Next, Mvs, Step2, Frame).
display_path_(State, [], _, _) :-
    state_zero(State),
    !.
display_path_(_, [], _, _) :-
    fail.

% ------------------------------------------------------------------------------
% display_move_by_move
%     ++Start : the start state
%     ++Moves : the moves to apply
% This has the side-effect of raising a GUI window per move (the titles
% of the each window indicates the move). The host must support XPCE for
% this to work.

display_move_by_move(Start, Moves) :-
    map_state_cube(Start, Cube),
    r2x2x2(Cube, _, '#0.'),
    display_move_by_move_(Start, Moves, 1).

display_move_by_move_(State, [Mv|Mvs], Step) :-
    move_transform(State, Mv, Next),
    map_state_cube(Next, Cube),
    atomic_list_concat(['#', Step, '. (', Mv, ')'], Title),
    r2x2x2(Cube, _, Title),
    Step_inc is Step + 1,
    display_move_by_move_(Next, Mvs, Step_inc).
display_move_by_move_(_, [], _).

map_state_cube(State, Cube) :-
    map_state_cube(State,
                   Cube,
                   [black, green, red, blue, yellow, orange]).

% ------------------------------------------------------------------------------
% map_state_cube
%     +-State: the cube's state
%     -+Cube: encoding of the cube in cross format for display
%     ++colours: list of the six colours to use for the faces of the
%                cube in the display

map_state_cube(State, Cube, Colours) :-
    map_state_listed(State, Listed),
    append([['0', '1', '2']], Listed, Corners),
    Template = [[A0, B0, C0],
                [A1, C1, E1],
                [A2, E2, D2],
                [A3, D3, B3],
                [F4, C4, B4],
                [F5, B5, D5],
                [F6, D6, E6],
                [F7, E7, C7]],
    Cube =  cube(A2,A3,
                 A1,A0,
           E2,E1,C1,C0,B0,B3,
           E6,E7,C7,C4,B4,B5,
                 F7,F4,
                 F6,F5,
                 D6,D5,
                 D2,D3),
    map_constants(Corners, Coloured, ['0', '4', '2', '1', '5', '3'], Colours),
    Template = Coloured.

% ------------------------------------------------------------------------------
% map_constants
%     +-A: term "in"
%     -+B: term "out"
%     ++Ins:  constants for in the in-term
%     ++Outs: constants for in the out-term
% Replaces the constants listed in Ins in A with the constants listed in
% Outs to preduce B.

map_constants(A, Z, Ins, Outs) :-
    ground(Ins),
    length(Ins,  Len),
    length(Outs, Len),
    map_constants_(Ins, Outs, A, Z).

map_constants_(Ins, Outs, A, Z) :-
    \+(var(A)),
    A =.. [F, A1|As],
    !,
    maplist(map_constants_(Ins, Outs), [A1|As], Zs),
    Z =.. [F|Zs].
map_constants_(Ins, Outs, A, Z) :-
    \+(var(A)),
    nth0(I, Ins,  A),
    !,
    nth0(I, Outs, Z),
    !.
map_constants_(_, _, A, A).

% ------------------------------------------------------------------------------
% shuffle
%     -Rand: a random state of the cube obtained by applying a random
%            path to state zero.

shuffle(Rand) :-
    state_zero(Zero),
    shuffle(Zero, Rand).

shuffle(Start, Rand) :-
    shuffle(Start, 30, Rand).

shuffle(Start, Steps, Rand) :-
    setof(Move, move(Move), Moves),
    shuffle(Start, Steps, Rand, Moves).

shuffle(State, Steps, Rand, Moves) :-
    Steps > 0,
    random_member(Move, Moves),
    move_transform(State, Move, Next),
    Steps_dec is Steps - 1,
    !,
    shuffle(Next, Steps_dec, Rand, Moves).
shuffle(State, 0, State, _).

% ------------------------------------------------------------------------------
% move
%     move: a legal move for the cube
% We define six 90-degree moves as described above.

move(bc).
move(ba).
move(lc).
move(la).
move(rc).
move(ra).

% ------------------------------------------------------------------------------
% reversal
%     move:    a move
%     reverse: the move that undoes the first move
% a bidrectional map of the moves and reversals

reversal(bc, ba).
reversal(ba, bc).
reversal(lc, la).
reversal(la, lc).
reversal(rc, ra).
reversal(ra, rc).

% ------------------------------------------------------------------------------
% move_transform
%     +-In_state:  the state in
%       Move:      the move to make
%     -+Out_state: the resulting state
% how each of the six permitted moves transform a state
% into an "adjacent" state.

move_transform(In_state, bc, Out_state) :-
    map_state_listed(In_state, As),
    As = [[A1,C1,E1], [A2,E2,D2], [A3,D3,B3],
          [F4,C4,B4], [F5,B5,D5], [F6,D6,E6], [F7,E7,C7]],
    Zs = [[A1,C1,E1], [A2,E2,D2], [A3,D3,B3],
          [F7,E7,C7], [F4,C4,B4], [F5,B5,D5], [F6,D6,E6]],
    map_state_listed(Out_state, Zs).
move_transform(In_state, ba, Out_state) :-
    map_state_listed(In_state, As),
    As = [[A1,C1,E1], [A2,E2,D2], [A3,D3,B3],
          [F4,C4,B4], [F5,B5,D5], [F6,D6,E6], [F7,E7,C7]],
    Zs = [[A1,C1,E1], [A2,E2,D2], [A3,D3,B3],
          [F5,B5,D5], [F6,D6,E6], [F7,E7,C7], [F4,C4,B4]],
    map_state_listed(Out_state, Zs).
move_transform(In_state, lc, Out_state) :-
    map_state_listed(In_state, As),
    As = [[A1,C1,E1], [A2,E2,D2], [A3,D3,B3],
          [F4,C4,B4], [F5,B5,D5], [F6,D6,E6], [F7,E7,C7]],
    Zs = [[D2,A2,E2], [D6,E6,F6], [A3,D3,B3],
          [F4,C4,B4], [F5,B5,D5], [C7,F7,E7], [C1,E1,A1]],
    map_state_listed(Out_state, Zs).
move_transform(In_state, la, Out_state) :-
    map_state_listed(In_state, As),
    As = [[A1,C1,E1], [A2,E2,D2], [A3,D3,B3],
          [F4,C4,B4], [F5,B5,D5], [F6,D6,E6], [F7,E7,C7]],
    Zs = [[C7,F7,E7], [C1,E1,A1], [A3,D3,B3],
          [F4,C4,B4], [F5,B5,D5], [D2,A2,E2], [D6,E6,F6]],
    map_state_listed(Out_state, Zs).
move_transform(In_state, rc, Out_state) :-
    map_state_listed(In_state, As),
    As = [[A1,C1,E1], [A2,E2,D2], [A3,D3,B3],
          [F4,C4,B4], [F5,B5,D5], [F6,D6,E6], [F7,E7,C7]],
    Zs = [[A1,C1,E1], [B3,A3,D3], [B5,D5,F5],
          [F4,C4,B4], [E6,F6,D6], [E2,D2,A2], [F7,E7,C7]],
    map_state_listed(Out_state, Zs).
move_transform(In_state, ra, Out_state) :-
    map_state_listed(In_state, As),
    As = [[A1,C1,E1], [A2,E2,D2], [A3,D3,B3],
          [F4,C4,B4], [F5,B5,D5], [F6,D6,E6], [F7,E7,C7]],
    Zs = [[A1,C1,E1], [E6,F6,D6], [E2,D2,A2],
          [F4,C4,B4], [B3,A3,D3], [B5,D5,F5], [F7,E7,C7]],
    map_state_listed(Out_state, Zs).

% ------------------------------------------------------------------------------
% map_state_listed
%     +-Atom:   a cube state
%     -+Listed: a nested list representation of the state

map_state_listed(Atom, List) :-
    ground(List),
    var(Atom),
    maplist(atomic_list_concat, List, As),
    atomic_list_concat(As, ' ', Atom),
    !.
map_state_listed(Atom, List) :-
    var(List),
    ground(Atom),
    atomic_list_concat(As, ' ', Atom),
    maplist(atom_chars, As, List).

end_of_file.