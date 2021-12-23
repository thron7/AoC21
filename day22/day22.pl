:- use_module(library(readutil)).
:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- op(100,fy,-).

:- dynamic cuboid/4.
:- retractall(cuboid(S,X,Y,Z)).

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
    findall(C, (list_relevant_cubes(C),find_cube_state(C,on)), OnCubes),
    length(OnCubes,N).

list_relevant_cubes([X,Y,Z]):-
    X in -50..50,
    Y in -50..50,
    Z in -50..50,
    labeling([],[X,Y,Z]).
    
main(Solution1, Solution2):-
    main('input.txt',Solution1, Solution2).
main(File, Solution1, Solution2):-
    read_data(File,Lines),
    lines_cuboids(Lines),
    solve_1(Solution1),
    solve_2(Calls,Boards,Solution2).

solve_1(S):-
    count_on_cubes(S).

solve_2(Calls,Boards,R).
    
lines_cuboids([]).
lines_cuboids([L|Ls]):-
    split_string(L, " ", " ", [State,Ranges]),
    atom_string(S,State),
    split_string(Ranges, ",", " ",[X1,Y1,Z1]),
    term_range(X,X1),
    term_range(Y,Y1),
    term_range(Z,Z1),
    assertz(cuboid(S,X,Y,Z)),
    lines_cuboids(Ls).

term_range(T,R):-
    split_string(R,"=","",[H,R1]),
    split_string(R1,".",".",[RA,RE]),
    term_string(H1, H),
    term_string(R2, RA),
    term_string(R3, RE),
    T = (H1=R2..R3).

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
