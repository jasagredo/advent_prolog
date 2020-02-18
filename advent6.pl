:- use_module(library(clpfd)).
:- use_module(library(lambda)).

input(Input) :-
    read_file_to_string("advent6.txt", String, []),
    split_string(String, "\n", "", Lines),
    select("", Lines, InputLines),
    maplist([In, Out]>>(split_string(In, ")", "", [A, B]),
                        Out = (A, B)), InputLines, Input).

assertInput(Input, TuplesA, TuplesB) :-
    maplist(\X^Y^Z^ (Z = (X, Y)), TuplesA, TuplesB, Input),
    maplist(\X^Y^ (assert(directlyOrbit(Y, X))), TuplesA, TuplesB).

%% directlyOrbit(X, Y). % X -> Y

orbit(X, Y) :- % X [-> ...]* -> Y
    directlyOrbit(X, Y);
    indirectlyOrbits(X, Y).

indirectlyOrbits(X, Y) :-    %X -> Z [-> ...]* -> Y
    directlyOrbit(X, Z),
    orbit(Z, Y).

removeDuplicates(Input, Output) :-
    sort(0, @<, Input, Output).

advent6a(TuplesA, TuplesB, Sol) :-
    append(TuplesA, TuplesB, AllPlanetsWithDup),
    removeDuplicates(AllPlanetsWithDup, Planets),
    maplist(\X^Y^ (findall(Z, (orbit(X, Z)), PlanetsItOrbits),
                   length(PlanetsItOrbits, Y)), Planets, Solution),
    sumlist(Solution, Sol).

advent6b(Sol) :-
    findall(W,
            orbit("YOU", W),
            YouOrbits),
    findall(W,
            orbit("SAN", W),
            SanOrbits),
    subtract(YouOrbits, SanOrbits, OnlyYou),
    subtract(SanOrbits, YouOrbits, OnlySan),
    length(OnlyYou, A),
    length(OnlySan, B),
    Sol #= A+B.

run(SolA, SolB) :-
    input(Input),
    assertInput(Input, TuplesA, TuplesB),
    advent6a(TuplesA, TuplesB, SolA),
    advent6b(SolB),
    % This just cleans up the current kb
    retractall(directlyOrbit(X,Y)).
