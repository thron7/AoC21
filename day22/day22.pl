:- use_module(library(readutil)).
:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(apply)).

:- dynamic(cuboid).
:- retractall(cuboid).

% cuboid(on,x=10..12,y=10..12,z=10..12).
% cuboid(on, x=11..13,y=11..13,z=11..13).
% cuboid(off, x=9..11,y=9..11,z=9..11).
% cuboid(on, x=10..10,y=10..10,z=10..10).

check_cube_state([X,Y,Z],S):-
    cuboid(S,x=X1,y=Y1,z=Z1),
    X in X1,
    Y in Y1,
    Z in Z1.

find_cube_state(C,S):-
    findall(S1, check_cube_state(C,S1), States),
    (   States = [] 
    ->  S = off
    ;   reverse(States, [S|_])
    ).

count_on_cubes(N):-
    X in -50..50,
    Y in -50..50,
    Z in -50..50,
    labeling([],[X,Y,Z]),
    findall(_, find_cube_state([X,Y,Z],on), States),
    length(States,N).
    
main(Solution1, Solution2):-
    main('input.txt',Solution1, Solution2).
main(File, Solution1, Solution2):-
    read_data(File,Lines),
    lines_cuboids(Lines),
    solve_1(Calls,Boards,Solution1),
    solve_2(Calls,Boards,Solution2).

solve_1(Calls,Boards,R).

solve_2(Calls,Boards,R).
    
lines_cuboids([]).
lines_cuboids([L|Ls]):-
    split_string(L, " ", " ", [State,Ranges]),
    atom_string(S,State),
    split_string(Ranges, ",", " ",[X1,Y1,Z1]),
    atom_string(X,X1),
    atom_string(Y,Y1),
    atom_string(Z,Z1),
    assertz(cuboid(S,X,Y,Z)),
    lines_cuboids(Ls).

read_data(File,Lines):-
    open(File, read, Stream),
    read_file(Stream, Lines), !,
    close(Stream).

read_file(Stream, []):-
    at_end_of_stream(Stream).
read_file(Stream, [X|L]):-
    \+ at_end_of_stream(Stream),
    read_line_to_codes(Stream, X),
    read_file(Stream, L).
