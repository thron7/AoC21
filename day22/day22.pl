:- use_module(library(readutil)).
:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(aggregate)).
:- set_prolog_stack(global, limit(5 000 000 000)).
:- set_prolog_stack(local, limit(2 000 000 000)).
:- dynamic cuboid_state/4.

%
% Enumeration approach (mostly)
%

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

% count the on cubes from the enumerator - is det.
count_on_cubes(Enumerator, N):-
    findall(_, (
        call(Enumerator,C),
        % format('backtracking from fcs~n',[]),
        find_cube_state(C,on)
        ), OnCubes),
    length(OnCubes,N).

% generate the list of cubes in this cuboid
enumerate_cuboid(C, [X,Y,Z]):-
    C = [x=X1,y=Y1,z=Z1],
    X in X1,
    Y in Y1,
    Z in Z1,
    labeling([],[X,Y,Z]).

initialization_cuboid(C):- 
    C = [=(x,..(-50,50)),=(y,..(-50,50)),=(z,..(-50,50))].
    
%
% Tracking approach
%
overlap(C1,C2):-
    C1 = [x=X1,y=Y1,z=Z1],
    C2 = [x=X2,y=Y2,z=Z2],
    overlap(X1,X2),
    overlap(Y1,Y2),
    overlap(Z1,Z2).
overlap(XA..XE,YA..YE):-
    YA #=< XE,
    YE #>= XA.

cuboid_intersection(C1,C2,C):-
    overlap(C1,C2),
    C1 = [x=X1,y=Y1,z=Z1],
    C2 = [x=X2,y=Y2,z=Z2],
    common_range(X1,X2,X),
    common_range(Y1,Y2,Y),
    common_range(Z1,Z2,Z),
    C = [x=X,y=Y,z=Z].

split_from_intersection(C,I,[]):-
    \+ overlap(C,I), !.
split_from_intersection(C,I,[]):-
    C = I, !.
split_from_intersection(C,I,[C1|Ls]):-
    overlap(C,I),
    cuboid_intersection(C,I,I),  % intersecting reproduces intersection
    C = [x=XA..XE,y=YA..YE,z=ZA..ZE],
    I = [x=X1A..X1E,y=Y1A..Y1E,z=Z1A..Z1E],
    split_range(XA..XE,X1A..X1E,Lx),
    split_range(YA..YE,Y1A..Y1E,Ly),
    split_range(ZA..ZE,Z1A..Z1E,Lz),
    (   Lx \= []
    ->  Lx = [Split,Rest],
        C1 = [x=Split,y=YA..YE,z=ZA..ZE],
        R = [x=Rest,y=YA..YE,z=ZA..ZE]
    ;   Ly \= []
    ->  Ly = [Split,Rest],
        C1 = [x=XA..XE,y=Split,z=ZA..ZE],
        R = [x=XA..XE,y=Rest,z=ZA..ZE]
    ;   Lz = [Split,Rest],
        C1 = [x=XA..XE,y=YA..YE,z=Split],
        R = [x=XA..XE,y=YA..YE,z=Rest]
    ),
    split_from_intersection(R,I,Ls).

process_states(OnCuboids):-
    findall([State,X,Y,Z], cuboid_state(State,X,Y,Z), States),
    process_states(States,0,[],OnCuboids).
process_states([],_,Cs,Cs).
process_states([S|Ss],I,Ons,OnCuboids):-
    length(Ons, LOns),
    format('processing: (~w) ~w~nlength Ons: ~w~n', [I, S,LOns]),
    I1 is I + 1,
    time(process_state(S,Ons,NewOns)),
    process_states(Ss,I1,NewOns,OnCuboids).

process_state([on|C], Ons, NewOns):-
    add_cuboid(Ons,[C], Ons, NewOns).
process_state([off|C], Ons, NewOns):-
    remove_cuboid(Ons, C, NewOns).

% add_cuboid(+RestOldOnCuboids, +StateSplits, +OldOnCuboids, -NewOnCuboids).
add_cuboid([],Ss,OldOnCuboids,NewOnCuboids):-
    append([Ss,OldOnCuboids],NewOnCuboids).
add_cuboid([R|RestOnCuboids], Splits, OldOns, NewOns):-
    add_cuboid(R, Splits, NewSplits),
    add_cuboid(RestOnCuboids, NewSplits, OldOns, NewOns).
% add_cuboid(+OldOnCuboid, +StateSplits, -NewStateSplits)
add_cuboid(_,[],[]).
add_cuboid(R,[S|Splits],NewSplits):-
    overlap(R,S),
    cuboid_intersection(R,S,I),
    split_from_intersection(S,I,Ss),
    add_cuboid(R,Splits,Ns),
    append([Ss,Ns],NewSplits).
add_cuboid(R,[S|Splits],NewSplits):-
    \+ overlap(R,S),
    add_cuboid(R,Splits,Ns),
    append([[S],Ns],NewSplits).

remove_cuboid([],_,[]).
remove_cuboid([O|Ons], R, NewOns):-
    overlap(O,R),
    cuboid_intersection(O,R,I),
    split_from_intersection(O,I,Ss),
    remove_cuboid(Ons,R,Ns),
    append([Ss,Ns],NewOns).
remove_cuboid([O|Ons], R, NewOns):-
    \+ overlap(O,R),
    remove_cuboid(Ons, R, Ns),
    append([[O],Ns],NewOns).

% returns at most a single split and the rest of the range
split_range(XA..XE,X1A..X1E,L):-
    common_range(XA..XE,X1A..X1E,XCA..XCE),
    (   XA #< XCA
    ->  Xu is XCA - 1,
        S = XA..Xu,
        R = XCA..XE,
        L = [S,R]
    ;   XCE #< XE
    ->  Xl is XCE + 1,
        S = Xl..XE,
        R = XA..XCE,
        L = [S,R]
    ;   L = []).
    
common_range(R1,R2,R):-
    overlap(R1,R2),
    R1 = X1A..X1E,
    R2 = X2A..X2E,
    XA is max(X1A,X2A),
    XE is min(X1E,X2E),
    R = XA..XE. 

%
% Main
%
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
    process_states(Os), 
    map(cubes_in_cuboid, Os, Ns), !,
    sum_list(Ns,S).

% Data i/o

read_states(File):-
    retractall(cuboid_state(_,_,_,_)),
    read_data(File,Lines),
    lines_cuboids(Lines).
    
read_data(File,Lines):-
    open(File, read, Stream),
    read_file(Stream, Lines), !,
    close(Stream).

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

read_file(Stream, []):-
    at_end_of_stream(Stream).
read_file(Stream, [X|L]):-
    \+ at_end_of_stream(Stream),
    read_line_to_codes(Stream, X),
    read_file(Stream, L).

% Helpers

cubes_in_states(State, N):-
    aggregate_all(
        sum(N0), 
        (   cuboid_state(State,X,Y,Z),
            cubes_in_cuboid([X,Y,Z],N0)
        ), N).

% counts number of cubes in cuboid
cubes_in_cuboid(C,N):-
    C = [x=XA..XE,y=YA..YE,z=ZA..ZE],
    X is XE - XA + 1,
    Y is YE - YA + 1,
    Z is ZE - ZA + 1,
    N is X * Y * Z.

fnumber(I):- format('~:d', [I]).

% counts the db clauses
count_state_clauses(N):-
    findall(_, cuboid_state(_,_,_,_), L),
    length(L, N).

test_overlaps():-
    cuboid_state(S,X,Y,Z),
    cuboid_state(S,X1,Y1,Z1),
    (   overlap([X,Y,Z],[X1,Y1,Z1])
    ->  format('operlapping: ~w - ~w~n',[[X,Y,Z],[X1,Y1,Z1]])
    ;   true).

map(_,[],[]).
map(Callable,[L|Ls],[R|Rs]):-
    call(Callable, L, R),
    map(Callable, Ls, Rs), !.

