:- use_module(library(readutil)).

main(Product, Product2):-
    main('input.txt',Product, Product2).
main(File, Product, Product2):-
    open(File, read, Stream),
    read_file(Stream, Lines),
    close(Stream), !,
    prepare(Lines, AtomLines),
    solve_1(AtomLines, Product),
    solve_2(AtomLines, Product2),
    true.

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

reduce([], 0, 0).
reduce([L|Ls],Forward,Down):-
    line(Verb,Amount,L,[]),
    reduce(Ls,F,D),
    (   Verb = forward ->
        Forward is F + Amount,
        Down is D
    ;   Forward is F,
        (Verb = up ->
            Down is D - Amount
        ;   Down is D + Amount)).

solve_2([],0).
solve_2(L,Result):-
    reduce2(L,Forward,_,Down),
    Result is Forward * Down.

reduce2([], 0, 0, 0).
reduce2([L|Ls], Forward, Aim, Down):-
    line(Verb,Amount,L,[]),
    reduce2(Ls,F,A,D),
    (   Verb = forward ->
        Forward is F + Amount,
        Down is D + A * Amount,
        Aim is A
    ;   Forward is F,
        (Verb = up ->
            Aim is A - Amount
        ;   Aim is A + Amount)).