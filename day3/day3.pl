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
    % solve_1(BitLines, Solution1),
    % day3_1(BitLines, Solution1),
    % solve_2(BitLines, Solution2).
    % day3_2(BitLines, Solution2).
    sol_1(BitLines,Solution1),
    sol_2(BitLines,Solution2).

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
    oxyRate(BitLines, OxyRate),
    co2Rate(BitLines, CO2Rate),
    binary_number(OxyRate,ORate),
    binary_number(CO2Rate, CRate),
    Product2 is ORate * CRate.

oxyRate(BitLines,R):-
    filterBitLines1(BitLines,1,R,true).

co2Rate(BitLines,R):-
    filterBitLines1(BitLines,1,R,false).

filterBitLines1([B],_,B,_):- !.
filterBitLines1(BitLines,Pos,R,ForOxy):-
    initAccu(BitLines,A,Len),
    accumulateLines(BitLines,A,Accu),
    gammaRate(Accu,Len,SigBits),
    (   ForOxy -> SigBits1 = SigBits; invert(SigBits,SigBits1) ),
    nth1(Pos,SigBits1,SigBit,_), % fails if Pos > length(SigBits)
    filterByBitInPos(BitLines,SigBit,Pos,NewLines),
    P #= Pos + 1,
    filterBitLines1(NewLines,P,R,ForOxy).

initAccu(BitLines,A,L):-
    length(BitLines,L),
    BitLines = [L1|_],
    init(L1,A).

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

gammaRate([],_,[]).
gammaRate([A|As],Len,[R|Rs]):-
    L is Len / 2,
    ( A >= L -> R is 1 ; R is 0 ),
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

%
% Haskell-esque Loesung
%

isOne(X):- X #= 1.
isZero(X):- X #= 0.

ones(L,N):-
    partition(isOne, L, Included, Excluded),
    length(Included, N).

zeros(L,N):-
    partition(isZero, L, I, _),
    length(I, N).

dominantBit(Col,R):-
    ones(Col,Os),
    zeros(Col,Zs),
    (   Os #>= Zs -> R = 1; R = 0). % day3_1 braucht hier Os #> Zs

map([],_,[]).
map([L|Ls],Callable,[R|Rs]):-
    call(Callable, L, R),
    map(Ls, Callable, Rs).

day3_1(Input,Result):-
    transpose(Input,T),
    map(T,dominantBit, Lst),
    invert(Lst, Eps),
    binary_number(Lst, Gamma),
    binary_number(Eps, Epsilon),
    Result is Gamma * Epsilon.

heads([],[]).
heads([[H|_]|Ls],[H|Hs]):- heads(Ls,Hs).

xssPrime([],_,[]).
xssPrime([L|Ls],Bit,[R|Rs]):-
    L = [Bit|R],
    xssPrime(Ls,Bit,Rs).
xssPrime([L|Ls],Bit,Rs):-
    \+ L = [Bit|_],
    xssPrime(Ls,Bit,Rs).

calc(_,Bits,[Xs],R):-
    !,
    reverse(Bits, Bits1),
    append(Bits1,Xs,R).
calc(Comp,Bits,Xss,R):-
    heads(Xss,Heads),
    ones(Heads,Ones),
    zeros(Heads,Zeros),
    (   call(Comp,Ones,Zeros)
        -> Bit is 1; Bit is 0),
    xssPrime(Xss,Bit,Xss1),
    calc(Comp,[Bit|Bits],Xss1,R).
    
toInt(Comp,Input,R):- 
    calc(Comp,[],Input,R1),
    binary_number(R1,R).

day3_2(Input,Result):-
    toInt(>=, Input, Oxygen),
    toInt(<, Input, Co2),
    Result is Oxygen * Co2.

%
% Declarative Solution
%

sol_1(BitLines,Result):-
    gammaRate(BitLines,GRate),
    epsilonRate(BitLines,ERate),
    binary_number(GRate,GammaRate),
    binary_number(ERate,EpsilonRate),
    Result is GammaRate * EpsilonRate.

epsilonRate(BitLines,EpsilonRate):-
    gammaRate(BitLines,ERate),
    invert(ERate,EpsilonRate).
    
gammaRate(BitLines,GammaRate):-
    transpose(BitLines, TransposedLines),
    dominantBits(TransposedLines,GammaRate).

dominantBits([],[]).
dominantBits([Bline|BitLines],[DBit|DomBits]):-
    dominantBit(Bline,DBit),
    dominantBits(BitLines,DomBits).

sol_2(BitLines,Result):-
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
    (   Gas = oxy -> SearchBit = DomBit; invert([DomBit],[SearchBit])),
    selectMatching(BitLines,Pos,SearchBit,NewLines).

dominantBitInPos(BitLines, Pos, DomBit):-
    transpose(BitLines,Transposed),
    nth1(Pos,Transposed,Col),
    dominantBit(Col,DomBit).

selectMatching([],_,_,[]).
selectMatching([B|BitLines],Pos,DomBit,[B|NewLines]):-
    nth1(Pos,B,PosBit),
    PosBit = DomBit,
    selectMatching(BitLines,Pos,DomBit,NewLines).
selectMatching([B|BitLines],Pos,DomBit,NewLines):-
    nth1(Pos,B,PosBit),
    \+ PosBit = DomBit,
    selectMatching(BitLines,Pos,DomBit,NewLines).