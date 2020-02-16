:- use_module(library(clpfd)).

input(Input) :-
    read_file_to_string("advent1.txt", String, []),
    split_string(String, "\n", "", SplitRaw),
    select("", SplitRaw, Split),
    maplist([In, Out]>>number_string(Out,In), Split, Input).

fuel(X,Y) :- Y #= (X//3)-2.

recFuel(In, F) :-
    fuel(In, StepFuel),
    sign(StepFuel, 1),
    recFuel(StepFuel, NextFuel),
    F #= StepFuel + NextFuel;
    F #= 0.

advent1a(Input, Solution) :-
    maplist(fuel, Input, Fuel),
    sumlist(Fuel, Solution).

advent1b(Input, Solution) :-
    maplist(recFuel, Input, Fuel),
    sumlist(Fuel, Solution).

run(Sol1, Sol2) :-
    input(Input),
    advent1a(Input, Sol1),
    advent1b(Input, Sol2).
