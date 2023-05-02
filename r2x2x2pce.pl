/* ===========================================================================
    author : parke godfrey
     first : 2022-10-02

    For the 2x2x2 Rubik's "mini" cube just for XPCE display.
    Modified from the XPCE Rubik's code distributed with SWI Prolog
    and attributed as follows:
------------------------------------------------------------------------------ 
   Part of XPCE --- The SWI-Prolog GUI toolkit
    Author:        Christian Schlichtherle
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1995, Christian Schlichtherle
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
=========================================================================== */

:- module(r2x2x2pce, [r2x2x2/0,
                      r2x2x2/1,
                      r2x2x2/2,
                      r2x2x2/3,
                      u2x2x2/2]).
:- use_module(library(pce)).
:- require([send_list/3
           ]).
:- license(gplv2).

% First read the Prolog predicates for solving Rubik's Cube. These predicates
% do the real work (i.e. order Rubik's Cube) and are defined so that they
% can be used alone without XPCE. This gives you the ability to use these
% predicates with any ordinary Prolog system (hopefully).

% This neat little trick gives us the ability to store a cube as an object
% level attribute of a frame.
:-pce_begin_class(cube,vector,
                  "Instances of this class represent Rubik's Cube.").
:-pce_end_class.

r2x2x2():-
    ordered(black, green, red, blue, yellow, orange, Cube),
    r2x2x2(Cube).

r2x2x2(Cube):-
    r2x2x2(Cube, _).

r2x2x2(Cube, Frame):-
    r2x2x2(Cube, Frame, 'Rubik''s 2x2x2 Cube').

r2x2x2(Cube, Frame, Title):-
    new(Frame, frame(Title)),
    mk_pathlist(Frame, FrontWindow, _),
    mk_boxlist(Frame, CrossWindow),
    send(FrontWindow, left, CrossWindow),
    send(Frame, append, new(Dialog, dialog)),
    send(CrossWindow, above, Dialog),
    send(Dialog,
         append,
         button(quit, message(@prolog, quit, @receiver?frame))),
    send_cube(Frame, Cube),
    refresh(Frame),
    send(FrontWindow, open).

u2x2x2(Cube, Frame):-
    send_cube(Frame, Cube),
    refresh(Frame).

disorder(Frame):-
    random_draws(DrawList),
    load_sequence(Frame, DrawList),
    play(Frame).

quit(Frame):-
    discard_sequence(Frame),
    retractall(steplist(Frame,_)),
    retractall(mousepos(Frame,_)),
    retractall(do_pause(Frame)),
    free(Frame).

load_sequence(Frame, Draws):-
    retractall(drawlist(Frame,_)),
    assert(drawlist(Frame,Draws)).

discard_sequence(Frame):-
    retractall(drawlist(Frame,_)).

% mk_pathlist(+Frame, -FrontWindow, -BackWindow).
mk_pathlist(Frame, FrontWindow, BackWindow):-
    new(Size, size(6*20, 6*20)),
    send(Frame,
         append,
         new(FrontWindow, window(cube_frontside, Size))),
    send(Frame,
         append,
         new(BackWindow, window(cube_backside, Size))),
    send(FrontWindow,
         above,
         BackWindow),
    send(FrontWindow,
         display,
         new(FrontDevice, device)),
    send(BackWindow,
         display,
         new(BackDevice,device)),
    send(FrontDevice,
         position,
         point(3.5*20, 2.5*20)),
    send(BackDevice,
         position,
         point(3.5*20, 3.5*20)),
    new(Vector, vector),
    send(Frame,
         attribute,
         attribute(pathvector, Vector)),
    path_position(FrontDevice,
                  BackDevice,
                  PosList),
    box_position(BoxPosList),
    mk_paths(PosList, BoxPosList, Vector, 1).

% This predicate is used to compute the graphical position of each path.
% in the list.
path_position(DF, DB, Ls):-
    path_position(DF, DB, u, l, f, r, d, b, Ls).
path_position(DF, DB, U, L, F, R, D, B,
                     [(DF,1,1,U),(DF,0,1,U),
                      (DF,1,0,U),(DF,0,0,U),
(DB,1,0,L),(DB,1,1,L),(DF,0,1,F),(DF,0,0,F),(DF,0,0,R),(DF,1,0,R),
(DB,0,0,L),(DB,0,1,L),(DF,1,1,F),(DF,1,0,F),(DF,0,1,R),(DF,1,1,R),
                      (DB,1,0,D),(DB,1,1,D),
                      (DB,0,0,D),(DB,0,1,D),
                      (DB,0,0,B),(DB,1,0,B),
                      (DB,0,1,B),(DB,1,1,B)]).

mk_paths([], [], _, _).
mk_paths([(Device, X, Y, Side)|PosList],
         [(Row, Col)|BoxPosList],
         Vector,
         I):-
    compute_pathpoints(X, Y, Side, Point1, Point2, Point3, Point4),
    send(Device,
         display,
         new(Path, path)),
    send(Path,
         closed,
         @on),
    send_list(Path,
              append,
              [Point1, Point2, Point3, Point4]),
    send(Path,
         fill_pattern,
         @black_image),
    send(Path,
         attribute,
         attribute(row, Row)),
    send(Path,
         attribute,
         attribute(col, Col)),
    send(Vector,
         element,
         I,
         Path),
    II is I + 1,
    mk_paths(PosList, BoxPosList, Vector, II).

compute_pathpoints(X, Y, Side,
                   point(OX1,OY1), point(OX2,OY2),
                   point(OX3,OY3), point(OX4,OY4)):-
    path_shape(Side,(IX1,IY1),(IX2,IY2),(IX3,IY3),(IX4,IY4)),
      sgn(IX2, IX2SGN),
      sgn(IX4, IX4SGN),
      sgn(IY4, IY4SGN),
      sgn(IY2, IY2SGN),
      PX is Y * (IX2 + IX2SGN) + X * (IX4 + IX4SGN),
      PY is X * (IY4 + IY4SGN) + Y * (IY2 + IY2SGN),
    OX1 is PX+IX1,OY1 is PY+IY1,
    OX2 is PX+IX2,OY2 is PY+IY2,
    OX3 is PX+IX3,OY3 is PY+IY3,
    OX4 is PX+IX4,OY4 is PY+IY4.

% path_shape(?Side,?Point1,?Point2,?Point3,?Point4) defines the shape of the
% paths. The points are listed counterclockwise. The first point is always
% (0,0).
path_shape(u,(0,0),( 11,-11),( -9,-11),(-20,  0)).
path_shape(l,(0,0),( 11, 11),( 11, -9),(  0,-20)).
path_shape(f,(0,0),(-20,  0),(-20, 20),(  0, 20)).
path_shape(r,(0,0),(  0, 20),( 11,  9),( 11,-11)).
path_shape(d,(0,0),(-20,  0),( -9, 11),( 11, 11)).
path_shape(b,(0,0),(  0,-20),(-20,-20),(-20,  0)).

sgn(0,0).
sgn(X,Y):-
    (X >= 0 -> Y is 1; Y is -1).

mk_boxlist(Frame, Window):-
    send(Frame,
         append,
         new(Window, window(cube_cross, size(10*20, 13*20)))),
    send(Window,
         display,
         new(Device, device)),
    send(Device,
         move,
         point(10,10)),
    new(Vector, vector),
    send(Frame,
         attribute,
         attribute(boxvector, Vector)),
    box_position(PosList),
    % This does not yet work because window<-area does not return the
    % visible area of the window.
    %new(_,constraint(Window,Device,spatial(xref=x+w/2,yref=y+h/2,xref=x+w/2,yref=y+h/2))),
    make_boxes(Device, PosList, Vector, 1).

% This predicate is used to compute the graphical position of each box
% in the list.
box_position(
             [( 0,2),( 0,3),
              ( 1,2),( 1,3),
( 2,0),( 2,1),( 2,2),( 2,3),( 2,4),( 2,5),
( 3,0),( 3,1),( 3,2),( 3,3),( 3,4),( 3,5),
              ( 4,2),( 4,3),
              ( 5,2),( 5,3),
              ( 6,2),( 6,3),
              ( 7,2),( 7,3)]).

make_boxes(_,[],_,_).
make_boxes(Device,[(Y,X)|PosList],Vector,I):-
    send(Device,display,new(Box,box(20,20)),point(X*20,Y*20)),
    send(Box,radius,3),
    send(Box,fill_pattern,@black_image),
    send_list(Box,attribute,[attribute(row,Y),attribute(col,X)]),
    send(Vector,element,I,Box),
    II is I+1,
    make_boxes(Device,PosList,Vector,II).

% refresh(+Frame) will redisplay the cube associated with Frame on the
% display.
%
% To alter the cube associated with the frame and refresh the display use
%
%   send_cube(Frame,Cube),refresh(Frame).
refresh(Frame):-
    get(Frame,attribute,cube,CubeVector),
    send(?(Frame,attribute,pathvector),for_all,
         message(@arg1,colour,?(CubeVector,element,@arg2))),
    send(?(Frame,attribute,boxvector),for_all,
         message(@arg1,colour,?(CubeVector,element,@arg2))).

% display_cube(+Frame,+CubeTerm) will display CubeTerm on Frame. However, it
% will not alter the cube associated with Frame. To do this, use send_cube/2.
%
% To alter the cube associated with the frame and refresh the display use
%
%   send_cube(Frame,Cube),refresh(Frame).
display_cube(Frame,CubeTerm):-
    new(CubeVector,CubeTerm),
    send(?(Frame,attribute,pathvector),for_all,
         message(@arg1,colour,?(CubeVector,element,@arg2))),
    send(?(Frame,attribute,boxvector),for_all,
         message(@arg1,colour,?(CubeVector,element,@arg2))).

% These "methods" should be really implemented as such one day:

% send_cube(+Frame,+Cube) associates cube with the frame. However, it does not
% display the cube.
%
% To alter the cube associated with the frame and refresh the display use
%
%   send_cube(Frame,Cube),refresh(Frame).
send_cube(Frame,Cube):-
    send(Frame,attribute,attribute(cube,Cube)).

get_cube(Frame,Cube):-
    Cube=cube(_,_,
              _,_,
          _,_,_,_,_,_,
          _,_,_,_,_,_,
              _,_,
              _,_,
              _,_,
              _,_),
    get(Frame,attribute,cube,Cube).

end_of_file.