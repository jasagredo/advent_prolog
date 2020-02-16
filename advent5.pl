:- use_module(library(clpfd)).

input(Input) :- csv_read_file("advent5.txt", [Row], []),
            Row =.. [_|Input].

getValue(Mode, V, Program, Value) :-
    Mode = 0,
    nth0(V, Program, Value);
    Value #= V.

setValue(In, Val, Addr, Out) :-
    length(Init, Addr),
    append(Init, [_|Tail], In),
    append(Init, [Val|Tail], Out).

step(PC, Program, Input, Outputs, Solution) :-
    length(Init, PC),
    ((append(Init, [Instruction, A, B, R|_], Program),
      OpCode is Instruction mod 100,
      ModeA is (Instruction//100) mod 10,
      ModeB is (Instruction//1000) mod 10,
      (OpCode=1,                          % P(C) = A + B
       getValue(ModeA, A, Program, Va),
       getValue(ModeB, B, Program, Vb),
       Val #= Va+Vb,
       setValue(Program, Val, R, NewProgram),
       NewPC #= PC+4,
       step(NewPC, NewProgram, Input, Outputs, Solution), !;

       OpCode=2,                          % P(C) = A * B
       getValue(ModeA, A, Program, Va),
       getValue(ModeB, B, Program, Vb),
       Val #= Va*Vb,
       setValue(Program, Val, R, NewProgram),
       NewPC #= PC+4,
       step(NewPC, NewProgram, Input, Outputs, Solution), !;

       OpCode=3,                          % P(A) = Input
       Val #= Input,
       setValue(Program, Val, A, NewProgram),
       NewPC #= PC+2,
       step(NewPC, NewProgram, Input, Outputs, Solution), !;

       OpCode=4,                          % append([P(A)], Outputs)
       getValue(ModeA, A, Program, Va),
       NewPC #= PC+2,
       step(NewPC, Program, Input, [Va|Outputs], Solution), !;

       OpCode=5,                          % if P(A) \= 0 { PC = P(B) } { PC = PC + 3 }
       getValue(ModeA, A, Program, Va),
       (Va #\= 0,
        getValue(ModeB, B, Program, Vb),
        NewPC #= Vb;
        NewPC #= PC+3),
       step(NewPC, Program, Input, Outputs, Solution), !;

       OpCode=6,                          % if P(A) == 0 { PC = P(B) } { PC = PC + 3 }
       getValue(ModeA, A, Program, Va),
       (Va #= 0,
        getValue(ModeB, B, Program, Vb),
        NewPC #= Vb;
        NewPC #= PC+3),
       step(NewPC, Program, Input, Outputs, Solution), !;

       OpCode=7,                          % if P(A) < P(B) { P(C) = 1 } { P(C) = 0 }
       getValue(ModeA, A, Program, Va),
       getValue(ModeB, B, Program, Vb),
       (Va #< Vb,
        setValue(Program, 1, R, NewProgram);
        setValue(Program, 0, R, NewProgram)),
       NewPC #= PC+4,
       step(NewPC, NewProgram, Input, Outputs, Solution), !;

       OpCode=8,                          % if P(A) == P(B) { P(C) = 1 } { P(C) = 0 }
       getValue(ModeA, A, Program, Va),
       getValue(ModeB, B, Program, Vb),
       (Va #= Vb,
        setValue(Program, 1, R, NewProgram);
        setValue(Program, 0, R, NewProgram)),
       NewPC #= PC+4,
       step(NewPC, NewProgram, Input, Outputs, Solution), !;

       OpCode=99, [RC|_] = Program,
       Solution = (RC, Outputs), !));
     % If we reach the end of the program, we might not have 4 memory positions,
     % although we should consume the input depending on the opcode, we can assume
     % that this will be a 99 instruction. We could also have this case:
     %  append(_, [4, 0, 99], Program)
     % That would also not unify with any of the branches, but we won't consider it
     % for now.
     append(Init, [99|_], Program),[RC|_] = Program,
     Solution = (RC, Outputs)).

advent5a(Program, Solution) :-
    step(0, Program, 1, [], (_, [Solution|_])).

advent5b(Program, Solution) :-
    step(0, Program, 5, [], (_, [Solution|_])).

run(SolA, SolB) :-
    input(Program),
    advent5a(Program, SolA),
    advent5b(Program, SolB).
