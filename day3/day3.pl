% s. https://stackoverflow.com/questions/29380105/prolog-binary-addition
:- use_module(library(readutil)).
:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(apply)).

main(Product, Product2):-
    main('input.txt',Product, Product2).
main(File, Product, Product2):-
    read_data(File,Lines), !,
    prepare(Lines, BitLines),
    solve_1(BitLines, Product),
    solve_2(BitLines, Product2).

read_data(File,Lines):-
    open(File, read, Stream),
    read_file(Stream, Lines),
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

solve_1(BitLines, Product):-
    BitLines = [L|_],
    init(L,A),
    length(BitLines,Len),
    accumulateLines(BitLines,A,A1),
    gammaRate(A1,Len,G),
    binary_number(G,GammaRate),
    invert(G,E),
    binary_number(E,EpsilonRate),
    Product #= GammaRate * EpsilonRate.

solve_2(BitLines, Product2):-
    BitLines = [L|_],
    init(L,A),
    accumulateLines(BitLines,A,A1),
    A = [Bit1|_],
    filterByBitInPos(BitLines,Bit1,1,R).

oxyRate(BitLines,R):-
    filterBitLines(BitLines,1,R).

filterBitLines([B],_,B).
filterBitLines(BitLines,Pos,R):-
    initAccu(BitLines,A,Len),
    Pos < Len,
    accumulateLines(BitLines,A,Accu),
    gammaRate(Accu,Len,SigBits),
    nth1(Pos,SigBits,SigBit,_),
    filterByBitInPos(BitLines,SigBit,Pos,NewSet),
    P #= Pos + 1,
    filterBitLines(NewLines,P,R).

initAccu(BitLines,A,L):-
    length(BitLines,L),
    BitLines = [L1|_],
    init(L1,A).

co2Rate().

% init(Line, Accu).
init([],[]).
init([_|Xs],[0|Ys]):-
    init(Xs,Ys).

% lineToAccu(Line, Accu, NewAccu).
lineToAccu([],[],[]).
lineToAccu([0|Xs],[Y|Ys],[Y|Rs]):-
    lineToAccu(Xs,Ys,Rs).
lineToAccu([1|Xs],[Y|Ys],[R|Rs]):-
    R is Y+1,
    lineToAccu(Xs,Ys,Rs).

% accumulateLines(Ls,A,A1).
accumulateLines([],A,A).
accumulateLines([L|Ls],A,A1):-
    lineToAccu(L,A,A2),
    accumulateLines(Ls,A2,A1).

gammaRate(A,L,R):-
    gammaRate(A,L,R,0).
gammaRate([],_,[],_).
gammaRate([A|As],Len,[R|Rs],Slant):-
    L is Len / 2,
    A1 #= A + Slant,
    ( A1 > L -> R is 1 ; R is 0 ),
    gammaRate(As,Len,Rs).

binary_number(Bs0, N) :-
    reverse(Bs0, Bs),
    binary_number(Bs, 0, 0, N).

binary_number([], _, N, N).
binary_number([B|Bs], I0, N0, N) :-
    B in 0..1,
    N1 #= N0 + (2^I0)*B,
    I1 #= I0 + 1,
    binary_number(Bs, I1, N1, N).

invert([],[]).
invert([B|Bs],[I|Is]):-
    B in 0..1,
    ( B #= 1 -> I #= 0 ; I #= 1 ),
    invert(Bs,Is).

% filterByBitInPos(ListOfBytes,SearchBit,BitPos,Result).
filterByBitInPos([],_,_,[]).
filterByBitInPos([L|Ls],Bit,Pos,[L|Rs]):-
    nth1(Pos,L,Bit),
    filterByBitInPos(Ls,Bit,Pos,Rs).
filterByBitInPos([L|Ls],Bit,Pos,Rs):-
    \+ nth1(Pos,L,Bit),
    filterByBitInPos(Ls,Bit,Pos,Rs).

isOne(X):- X #= 1.
isZero(X):- X #= 0.

ones(L,N):-
    partition(isOne, L, Included, Excluded),
    length(Included, N).

zeros(L,N):-
    partition(isZero, L, I, _),
    length(I, N).

onesDominate(Col,R):-
    ones(Col,Os),
    zeros(Col,Zs),
    (   Os #> Zs -> R = 1; R = 0).

map([],_,[]).
map([L|Ls],Callable,[R|Rs]):-
    call(Callable, L, R),
    map(Ls, Callable, Rs).


day3_1(Input,Result):-
    transpose(Input,T),
    map(T,onesDominate, Lst),
    invert(Lst, Eps),
    binary_number(Lst, Gamma),
    binary_number(Eps, Epsilon),
    Result is Gamma * Epsilon.
    