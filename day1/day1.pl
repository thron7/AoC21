:- use_module(library(readutil)).

day1(Count, SlidingCount):-
    open('input.txt', read, Stream),
    read_file(Stream, Lines),
    close(Stream), !,
    prepare(Lines, Nums),
    solve_1(Nums, Count),
    solve_2(Nums, SlidingCount).

read_file(Stream, []):-
    at_end_of_stream(Stream).
read_file(Stream, [X|L]):-
    \+ at_end_of_stream(Stream),
    read_line_to_codes(Stream, X),
    read_file(Stream, L).

prepare([], []).
prepare([L|Lines], [X|Xs]):-
    atom_codes(A, L),
    read_term_from_atom(A, X, []),
    prepare(Lines, Xs).

solve_1([],0).
solve_1([_],0).
solve_1([Num1, Num2|Nums], C):-
    (   Num1 < Num2
    ->  solve_1([Num2|Nums], X),
        C is X + 1;
    solve_1([Num2|Nums], C)).

solve_2([], 0).
solve_2([_], 0).
solve_2([_,_], 0).
solve_2([_,_,_], 0).
solve_2([Num1, Num2, Num3, Num4| Nums], C):-
    CurrSum is Num1 + Num2 + Num3,
    NextSum is Num2 + Num3 + Num4,
    (   CurrSum < NextSum
    ->  solve_2([Num2, Num3, Num4|Nums], X),
        C is X + 1;
    solve_2([Num2, Num3, Num4|Nums], C)).
