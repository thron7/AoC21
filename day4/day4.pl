:- use_module(library(readutil)).
:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- op(100,fy,~).

main(Solution1, Solution2):-
    main('input.txt',Solution1, Solution2).
main(File, Solution1, Solution2):-
    read_data(File,[L1|Lines]),
    number_calls(L1,Calls),
    boards(Lines,Boards),
    solve_1(Calls,Boards,Solution1),
    solve_2(Calls,Boards,Solution2).

solve_1(Calls,Boards,R):-
    bingo(Calls,Boards,BingoCall,BingoBoards),
    BingoBoards = [BingoBoard|_],
    sum_unmarked(BingoBoard,Sum),
    R is BingoCall * Sum.

solve_2(Calls,Boards,R):-
    bingo1(Calls,Boards,LastCall,LastBoard),
    sum_unmarked(LastBoard,Sum),
    R is LastCall * Sum.

bingo([],_,_,[]).
bingo([C|Calls], Boards, Call, BingoBoards):-
    update_boards(Boards, C, NewBoards),
    check_boards(NewBoards,BBs),
    (   BBs \= [] 
        ->  BingoBoards = BBs,
            Call = C
        ;   bingo(Calls,NewBoards,Call,BingoBoards)).

bingo1([],_,_,_).
bingo1([C|Calls], Boards, LastCall, LastBoard):-
    bingo_round(C,Boards,LC,LB,NewBoards),
    bingo1(Calls, NewBoards, LC1, LB1),
    (   bound(LC1) 
        ->  LastCall = LC1,
            LastBoard = LB1
        ;   LastCall = LC,
            LastBoard = LB).

bingo_round(_,[],_,_,[]).
bingo_round(C, [Board|Boards], LastCall, LastBoard, NewBoards):-
    update_board(C,Board,NewBoard),
    check_board(NewBoard),
    bingo_round(C, Boards,LC,LB,NewBoards),
    (   bound(LC) 
        ->  LastCall = LC,
            LastBoard = LB
        ;   LastCall = C,
            LastBoard = NewBoard).
bingo_round(C, [Board|Boards], LastCall, LastBoard, [NewBoard|NewBoards]):-
    update_board(C,Board,NewBoard),
    \+ check_board(NewBoard),
    bingo_round(C, Boards, LastCall, LastBoard, NewBoards).

sum_unmarked([],0).
sum_unmarked([B|Bs],Sum):-
    sum_unmarked_line(B,S),
    sum_unmarked(Bs,S1),
    Sum is S + S1.

sum_unmarked_line([],0).
sum_unmarked_line([~_|Es],Sum):-
    sum_unmarked_line(Es,Sum).
sum_unmarked_line([E|Es],Sum):-
    \+ marked(E),
    sum_unmarked_line(Es,S1),
    Sum is S1 + E.

string_number(S,N):- number_string(N,S).

number_calls(Line, Calls):-
    split_string(Line, ",", " ", SNumbers),
    map(string_number, SNumbers, Calls).

board_line(Line,Nums):-
    split_string(Line, " ", " ", SNumbs),
    map(string_number, SNumbs, Nums).

boards([],[]).
boards([L|Lines],Boards):-
    board([L|Lines],Board,Rest),
    (   Board = [] -> Boards = Bs;
        Boards = [Board|Bs]),
    boards(Rest, Bs).

board([],[],[]).
board([[]|Ls],[],Ls).
board([L|Ls],[B|Bs],Rest):-
    board_line(L, B),
    % format('line: ~w~n', [B]),
    board(Ls,Bs,Rest).

update_boards(Boards,Val,NewBoards):- map(update_board(Val),Boards,NewBoards).

update_board(_,[],[]).
update_board(Val,[B|Bs],[N|Ns]):-
    mark(B,Val,N),
    update_board(Val,Bs,Ns).

mark([],_,[]).
mark([L|Ls],L,[~L|Ns]):- mark(Ls,L,Ns).
mark([L|Ls],Val,[L|Ns]):- L \= Val, mark(Ls,Val,Ns).

check_boards(Boards,BingoBoards):- include(check_board,Boards,BingoBoards).

check_board(B):-
    any(check_line,B).
check_board(B):-
    transpose(B,B1),
    any(check_line,B1).

check_line(B):- all(marked,B).

marked(~_).

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
    map(Callable, Ls, Rs).

reduce(_,[],Accu,Accu).
reduce(Reducer,[L|Ls],Accu,R):-
    call(Reducer,Accu,L,Accu1),
    reduce(Reducer,Ls,Accu1,R).

all(P,L):- maplist(P,L).
any(P,L):- include(P,L,L1), L1 \= [].

bound(V):- nonvar(V).