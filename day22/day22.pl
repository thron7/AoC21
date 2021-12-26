:- use_module(library(readutil)).
:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- dynamic cuboid_state/4.

% backtracks over all cuboid states and returns corresp. state of given cube
check_cube_state([X,Y,Z],S):-
    cuboid_state(S,x=X1,y=Y1,z=Z1),
    X in X1,
    Y in Y1,
    Z in Z1.

% finds the latest state of a cube - is det
find_cube_state(C,S):-
    findall(S1, check_cube_state(C,S1), States),
    % format('find_cube_state for: ~w~n',[C]),
    (   States = [] 
    ->  S = off
    ;   reverse(States, [S|_])
    ).

% counts number of cubes in cuboid
cubes_in_cuboid(C,N):-
    C = [x=XA..XE,y=YA..YE,z=ZA..ZE],
    X is XE - XA,
    Y is YE - YA,
    Z is ZE - ZA,
    format('~:d',[N]),
    N is X * Y * Z.

% count the on cubes from the enumerator - is det.
count_on_cubes(Enumerator, N):-
    findall(_, (
        call(Enumerator,C),
        % format('backtracking from fcs~n',[]),
        find_cube_state(C,on)
        ), OnCubes),
    length(OnCubes,N).

% helper
cubes_in_hull(N):-
    hull_cuboid(H),
    count_on_cubes(enumerate_cuboid(H),N),
    format('~:d~n',[N]).

% generate the list of cubes in this cuboid
enumerate_cuboid(C, [X,Y,Z]):-
    C = [x=X1,y=Y1,z=Z1],
    X in X1,
    Y in Y1,
    Z in Z1,
    labeling([],[X,Y,Z]).

count_cuboid_states(N):-
    findall(_, cuboid_state(_,_,_,_), L),
    length(L, N).
    

initialization_cuboid(C):- C = [=(x,..(-50,50)),=(y,..(-50,50)),=(z,..(-50,50))].
    
lines_cuboids([]).
lines_cuboids([L|Ls]):-
    split_string(L, " ", " ", [State,Ranges]),
    atom_string(S,State),
    split_string(Ranges, ",", " ",[X1,Y1,Z1]),
    (   term_range(X,X1),
        term_range(Y,Y1),
        term_range(Z,Z1)
    ->  assertz(cuboid_state(S,X,Y,Z))
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
    cuboid_state(S,X,Y,Z),
    cuboid_state(S,X1,Y1,Z1),
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

hull_cuboid(C):-
    extrema(on, xa_lense, min_list, XA),
    extrema(on, xe_lense, max_list, XE),
    extrema(on, ya_lense, min_list, YA),
    extrema(on, ye_lense, max_list, YE),
    extrema(on, za_lense, min_list, ZA),
    extrema(on, ze_lense, max_list, ZE),
    C = [x=XA..XE,y=YA..YE,z=ZA..ZE].    

xa_lense([x=XA.._,_,_],XA).
xe_lense([x=_..XE,_,_],XE).
ya_lense([_,y=YA.._,_],YA).
ye_lense([_,y=_..YE,_],YE).
za_lense([_,_,z=ZA.._],ZA).
ze_lense([_,_,z=_..ZE],ZE).

extrema(State, Lense, MinOrMax, Extremum):-
    findall([X,Y,Z], cuboid_state(State,X,Y,Z), L),
    map(Lense,L,L1),
    call(MinOrMax,L1,Extremum).

main(Solution1, Solution2):-
    main('input.txt',Solution1, Solution2).
main(File, Solution1, Solution2):-
    read_states(File),
    solve_1(Solution1),
    solve_2(Solution2).

solve_1(S):-
    initialization_cuboid(C),
    count_on_cubes(enumerate_cuboid(C), S).

solve_2(S):-
    hull_cuboid(H),
    count_on_cubes(enumerate_cuboid(H), S).

read_states(File):-
    retractall(cuboid_state(_,_,_,_)),
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

map(_,[],[]).
map(Callable,[L|Ls],[R|Rs]):-
    call(Callable, L, R),
    map(Callable, Ls, Rs), !.

