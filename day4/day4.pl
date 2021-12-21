:- use_module(library(readutil)).
:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(apply)).

main(Solution1, Solution2):-
    main('input.txt',Solution1, Solution2).
main(File, Solution1, Solution2):-
    read_data(File,[L1|Lines]), !,
    number_calls(L1,Calls),
    boards(Lines,Boards),
    prepare(Lines, BitLines),
    solve_1(BitLines,Solution1),
    solve_2(BitLines,Solution2).

string_number(S,N):- number_string(N,S).

number_calls(Line, Calls):-
    split_string(Line, ",", " ", SNumbers),
    map(string_number, SNumbers, Calls).

board_line(Line,Nums):-
    split_string(Line, " ", " ", SNumbs),
    map(string_number, SNumbs, Nums).

boards([],[]). % :- format('multiple_boards: base case~n').
boards([L|Lines],Boards):-
    board([L|Lines],Board,Rest),
    (   Board = [] -> Boards = Bs;
        Boards = [Board|Bs]),
    boards(Rest, Bs).

board([],[],[]). % :- format('single_board: base case~n').
board([[]|Ls],[],Ls). % :- format('single_board: rest case~n').
board([L|Ls],[B|Bs],Rest):-
    board_line(L, B),
    % format('line: ~w~n', [B]),
    board(Ls,Bs,Rest).

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

prepareLine([],[]).
prepareLine([X|Xs],[Y|Ys]):-
    number_codes(Y,[X]),
    prepareLine(Xs,Ys).

prepare([], []).
prepare([L|Ls], [X|Xs]):-
    prepareLine(L,X),
    prepare(Ls, Xs).

map(_,[],[]).
map(Callable,[L|Ls],[R|Rs]):-
    call(Callable, L, R),
    map(Callable, Ls, Rs).

reduce(_,[],Accu,Accu).
reduce(Reducer,[L|Ls],Accu,R):-
    call(Reducer,Accu,L,Accu1),
    reduce(Reducer,Ls,Accu1,R).

solve_1(B,R).
solve_2(B,R).