:- use_module(library(readutil)).
:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- dynamic cuboid/4.

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
    
lines_cuboids([]).
lines_cuboids([L|Ls]):-
    split_string(L, " ", " ", [State,Ranges]),
    atom_string(S,State),
    split_string(Ranges, ",", " ",[X1,Y1,Z1]),
    (   term_range(X,X1),
        term_range(Y,Y1),
        term_range(Z,Z1)
    ->  assertz(cuboid(S,X,Y,Z))
    ;   true),
    lines_cuboids(Ls).

term_range(T,R):-
    split_string(R,"=","",[H,R1]),
    split_string(R1,".",".",[RA,RE]),
    term_string(H1, H),
    term_string(R2, RA),
    term_string(R3, RE),
    % uncomment to restrict to initialization region
    % R2 #=< 50,
    % R3 #>= -50,
    T = (H1=R2..R3).

count_on_in_cuboid():-
    cuboid(S,x=X,y=Y,z=Z),
    states_in_cuboid(X,Y,Z,on,C).

states_in_cuboid(x=XRange,y=YRange,z=ZRange,State,[X,Y,Z]):-
    X in XRange,
    Y in YRange,
    Z in ZRange,
    labeling([],[X,Y,Z]),
    find_cube_state([X,Y,Z],S),
    S = State.

overlap(C1,C2):-
    C1 = [x=X1,y=Y1,z=Z1],
    C2 = [x=X2,y=Y2,z=Z2],
    overlap(X1,X2),
    overlap(Y1,Y2),
    overlap(Z1,Z2).
overlap(XA..XE,YA..YE):-
    YA #=< XE,
    YE #>= XA.

test_overlaps():-
    cuboid(S,X,Y,Z),
    cuboid(S,X1,Y1,Z1),
    (   overlap([X,Y,Z],[X1,Y1,Z1])
    ->  format('operlapping: ~w - ~w~n',[[X,Y,Z],[X1,Y1,Z1]])
    ;   true).

combine_cuboids(C1,C2,C):-
    C1 = [x=X1A..X1E,y=Y1A..Y1E,z=Z1A..Z1E],
    C2 = [x=X2A..X2E,y=Y2A..Y2E,z=Z2A..Z2E],
    XA is min(X1A,X2A), XE is max(X1E,X2E),
    YA is min(Y1A,Y2A), YE is max(Y1E,Y2E),
    ZA is min(Z1A,Z2A), ZE is max(Z1E,Z2E),
    C = [x=XA..XE,y=YA..YE,z=ZA..ZE].

cubes_in_cuboid(C,N):-
    C = [x=XA..XE,y=YA..YE,z=ZA..ZE],
    X is XE - XA,
    Y is YE - YA,
    Z is ZE - ZA,
    N is X * Y * Z.

cuboid_intersection(C1,C2,C):-
    \+ overlap(C1,C2),
    C = [x=0..0,y=0..0,z=0..0].
cuboid_intersection(C1,C2,C):-
    overlap(C1,C2),
    C1 = [x=X1,y=Y1,z=Z1],
    C2 = [x=X2,y=Y2,z=Z2],
    common_range(X1,X2,X),
    common_range(Y1,Y2,Y),
    common_range(Z1,Z2,Z),
    C = [x=X,y=Y,z=Z].

common_range(R1,R2,R):-
    \+ overlap(R1,R2),
    R = 0..0.
common_range(R1,R2,R):-
    overlap(R1,R2),
    R1 = X1A..X1E,
    R2 = X2A..X2E,
    XA is max(X1A,X2A),
    XE is min(X1E,X2E),
    R = XA..XE. 

is_empty_cuboid(C):-
    C = [x=XA..XE,y=YA..YE,z=ZA..ZE],
    (   XA #= XE 
    ->  true
    ;   YA #= YE
    ->  true
    ;   ZA #= ZE
    ->  true).

    

main(Solution1, Solution2):-
    main('input.txt',Solution1, Solution2).
main(File, Solution1, Solution2):-
    read_cuboids(File),
    solve_1(Solution1),
    solve_2(Calls,Boards,Solution2).

solve_1(S):-
    count_on_cubes(S).

solve_2(Calls,Boards,R).

read_cuboids(File):-
    retractall(cuboid(_,_,_,_)),
    read_data(File,Lines),
    lines_cuboids(Lines).
    
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
