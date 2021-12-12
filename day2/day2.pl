:- use_module(library(readutil)).

main(Product, Product2):-
    main('input.txt',Product, Product2).
main(File, Product, Product2):-
    open(File, read, Stream),
    read_file(Stream, Lines),
    close(Stream), !,
    prepare(Lines, AtomLines),
    solve_1(AtomLines, Product),
    solve_2(AtomLines, Product2).

read_file(Stream, []):-
    at_end_of_stream(Stream).
read_file(Stream, [X|L]):-
    \+ at_end_of_stream(Stream),
    read_line_to_codes(Stream, X),
    read_file(Stream, L).

prepare([], []).
prepare([L|Lines], [X|Xs]):-
    split_string(L, [32], [], [Verb,Num]),
    atom_codes(X1,Verb),
    number_codes(X2,Num),
    X = [X1,X2],
    prepare(Lines, Xs).

% DCGs
line(V,C) --> move(V), count(C).
move(up) --> [up].
move(down) --> [down].
move(forward) --> [forward].
count(C) --> [C].

solve_1([],0).
solve_1(L, Result):-
    reduce(L,Forward,Down),
    Result is Forward * Down.

solve_2([],0).
solve_2(L,Result):-
    reduce(L, Forward, Down, [0, 0, 0]),
    Result is Forward * Down.

reduce([], 0, 0).
reduce([L|Ls],Forward,Down):-
    line(Verb,Amount,L,[]),
    reduce(Ls,F,D),
    calc(Verb, Amount, Forward, Down, F, D).

calc(forward, Amount, Forward, Down, F, D):-
    Forward is F + Amount,
    Down is D.
calc(up, Amount, Forward, Down, F, D):-
    Forward is F,
    Down is D - Amount.
calc(down, Amount, Forward, Down, F, D):-
    Forward is F,
    Down is D + Amount.

reduce([], F, D, [F, _, D]).
reduce([L|Ls], Forward, Down, [Pos, Aim, Depth]):-
    line(Verb,Amount,L,[]),
    calc(Verb, Amount, P, A, D, Pos, Aim, Depth),
    reduce(Ls, Forward, Down, [P,A,D]).

calc(forward, Amount, P, A, D, Pos, Aim, Depth):-
    P is Pos + Amount,
    A is Aim,
    D is Depth + Aim * Amount.
calc(up, Amount, P, A, D, Pos, Aim, Depth):-
    P is Pos,
    D is Depth,
    A is Aim - Amount.
calc(down, Amount, P, A, D, Pos, Aim, Depth):-
    P is Pos,
    D is Depth,
    A is Aim + Amount.