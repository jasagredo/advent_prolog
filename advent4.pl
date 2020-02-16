:- use_module(library(clpfd)).

hasAdjacentDuplicates(X) :-
    number_string(X, S),
    sub_string(S, _, 2, _, SS),
    string_chars(SS, [N,N]), !.

hasAdjacentDuplicatesIsolated(X) :-
    number_string(X, S),
    sub_string(S, Be, 2, Af, SS), %has duplicates
    string_chars(SS, [N,N]),
    (Af = 0 ;
     sub_string(S, Be, 3, _, SSA), %not repeated after
     string_chars(SSA, [_, _, A]),
     A \= N),
    (Be = 0;
     sub_string(S, _, 3, Af, SSB), %not repeated before
     string_chars(SSB, [B|_]),
     B \= N), !.

increasing([_]).
increasing([A,B|T]) :-
    A #=< B,
    increasing([B|T]).

alwaysIncrease(X) :-
    number_string(X, S),
    string_chars(S, Chars),
    maplist(string_chars, Chars, Strings),
    maplist(number_string, Digits, Strings),
    increasing(Digits).

advent4a(Min, Max, Sol) :-
    findall(X,
            (between(Min, Max, X),
             hasAdjacentDuplicates(X),
             alwaysIncrease(X)),
            Valid),
    length(Valid, Sol).

advent4b(Min, Max, Sol) :-
    findall(X,
            (between(Min, Max, X),
             hasAdjacentDuplicatesIsolated(X),
             alwaysIncrease(X)),
            Valid),
    length(Valid, Sol).

run(SolA, SolB) :-
    MinVal = 109165,
    MaxVal = 576723,
    advent4a(MinVal, MaxVal, SolA),
    advent4b(MinVal, MaxVal, SolB).
