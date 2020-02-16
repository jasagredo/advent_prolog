:- use_module(library(clpfd)).

input(Input) :- csv_read_file("advent2.txt", [Row], []),
            Row =.. [_|Input].

step(PC, Program, Solution) :-
    length(Init, PC),
    append(Init, [OpCode, A, B, R|_], Program),
    nth0(A, Program, Va),
    nth0(B, Program, Vb),
    (
        OpCode=1,
        Val #= Va+Vb,
        setValue(Program, Val, R, NewProgram),
        NewPC #= PC+4,
        step(NewPC, NewProgram, Solution);

        OpCode=2,
        Val #= Va*Vb,
        setValue(Program, Val, R, NewProgram),
        NewPC #= PC+4,
        step(NewPC, NewProgram, Solution);

        OpCode=99,
        [Solution|_] = Program
    ).

setValue(In, Val, Addr, Out) :-
    length(Init, Addr),
    append(Init, [_|Tail], In),
    append(Init, [Val|Tail], Out).

advent2a([X,_,_|T], O) :-
    step(0, [X,12,2|T], O).

advent2b([X,_,_|T], Res, S) :-
    [N,V] ins 0..99,
    step(0, [X,N,V|T], Res),
    S #= (N*100)+V.

run(Sol1, Sol2) :-
    input(Input),
    advent2a(Input, Sol),
    advent2b(Input, 19690720, Sol).
