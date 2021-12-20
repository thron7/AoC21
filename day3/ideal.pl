% s. https://stackoverflow.com/questions/29380105/prolog-binary-addition
:- use_module(library(readutil)).
:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(apply)).

main(Solution1, Solution2):-
    main('input.txt',Solution1, Solution2).
main(File, Solution1, Solution2):-
    read_data(File,Lines), !,
    prepare(Lines, BitLines),
    solve_1(BitLines,Solution1),
    solve_2(BitLines,Solution2).

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

solve_1(BitLines,Result):-
    gammaRate(BitLines,GRate),
    epsilonRate(BitLines,ERate),
    binary_number(GRate,GammaRate),
    binary_number(ERate,EpsilonRate),
    Result is GammaRate * EpsilonRate.

gammaRate(BitLines,GammaRate):-
    transpose(BitLines, TransposedLines),
    dominantBits1(TransposedLines,GammaRate).

epsilonRate(BitLines,EpsilonRate):-
    gammaRate(BitLines,ERate),
    map(invert,ERate,EpsilonRate).
    
solve_2(BitLines,Result):-
    findGas(BitLines,oxy,Oxy),
    findGas(BitLines,co2,C),
    binary_number(Oxy, Oxygen),
    binary_number(C,Co2),
    Result is Oxygen * Co2.

findGas(BitLines,Gas,R):-
    findGas(BitLines,1,Gas,R).
findGas([B],_,_,B):- !.
findGas(BitLines,Pos,Gas,R):-
    filterBitLines(BitLines,Pos,Gas,NewLines),
    P is Pos + 1,
    findGas(NewLines,P,Gas,R).

filterBitLines([],_,_,[]).
filterBitLines(BitLines,Pos,Gas,NewLines):-
    dominantBitInPos(BitLines,Pos,DomBit),
    (   Gas = oxy -> SearchBit = DomBit; invert(DomBit,SearchBit)),
    selectMatching(BitLines,Pos,SearchBit,NewLines).

dominantBitInPos(BitLines, Pos, DomBit):-
    transpose(BitLines,Transposed),
    nth1(Pos,Transposed,Col),
    dominantBit(Col,DomBit).

% direct version
dominantBits([],[]).
dominantBits([Bline|BitLines],[DBit|DomBits]):-
    dominantBit(Bline,DBit),
    dominantBits(BitLines,DomBits).
% version using reduce/4
dominantBits1(BitLines,DomBits):-
    reduce(dominantBitsReducer, BitLines, [], DBits),
    reverse(DBits,DomBits).
% reducer
dominantBitsReducer(Accu,L,[R|Accu]):-
    dominantBit(L,R).

selectMatching([],_,_,[]).
selectMatching([B|BitLines],Pos,DomBit,[B|NewLines]):-
    nth1(Pos,B,PosBit),
    PosBit = DomBit,
    selectMatching(BitLines,Pos,DomBit,NewLines).
selectMatching([B|BitLines],Pos,DomBit,NewLines):-
    nth1(Pos,B,PosBit),
    \+ PosBit = DomBit,
    selectMatching(BitLines,Pos,DomBit,NewLines).

binary_number(Bs0, N) :-
    reverse(Bs0, Bs),
    binary_number(Bs, 0, 0, N).
binary_number([], _, N, N).
binary_number([B|Bs], I0, N0, N) :-
    B in 0..1,
    N1 #= N0 + (2^I0)*B,
    I1 #= I0 + 1,
    binary_number(Bs, I1, N1, N).

invert(B,I):-
    B in 0..1,
    ( B #= 1 -> I #= 0 ; I #= 1 ).

isOne(X):- X #= 1.
isZero(X):- X #= 0.

ones(L,N):-
    partition(isOne, L, Included, Excluded),
    length(Included, N).

zeros(L,N):-
    partition(isZero, L, I, _),
    length(I, N).

dominantBit(L,R):-
    ones(L,Os),
    zeros(L,Zs),
    (   Os #>= Zs -> R = 1; R = 0).

map(_,[],[]).
map(Callable,[L|Ls],[R|Rs]):-
    call(Callable, L, R),
    map(Callable, Ls, Rs).

reduce(_,[],Accu,Accu).
reduce(Reducer,[L|Ls],Accu,R):-
    call(Reducer,Accu,L,Accu1),
    reduce(Reducer,Ls,Accu1,R).