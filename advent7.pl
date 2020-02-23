:- use_module(library(clpfd)).
:- use_module(library(lambda)).

input(Input) :- csv_read_file("advent7.txt", [Row], []),
            Row =.. [_|Input].

getValue(Mode, V, Program, Value) :-    % if Mode = 0 then Value = M[V] else Value = V
    Mode = 0,
    nth0(V, Program, Value);
    Value #= V.

setValue(In, Val, Addr, Out) :-         % M[Addr] = Val
    length(Init, Addr),
    append(Init, [_|Tail], In),
    append(Init, [Val|Tail], Out).

% Mem[C] = Mem*[A][ModeA] + Mem*[B][ModeB]
opcode(1, ModeA, ModeB, A, B, R, PC, Program, Input, Output, Solution) :-
        getValue(ModeA, A, Program, Va),
        getValue(ModeB, B, Program, Vb),
        Val #= Va+Vb,
        setValue(Program, Val, R, NewProgram),
        NewPC #= PC+4,
        step(NewPC, NewProgram, Input, Output, Solution).

% Mem[C] = Mem*[A][ModeA] * Mem*[B][ModeB]
opcode(2, ModeA, ModeB, A, B, R, PC, Program, Input, Output, Solution) :-
        getValue(ModeA, A, Program, Va),
        getValue(ModeB, B, Program, Vb),
        Val #= Va*Vb,
        setValue(Program, Val, R, NewProgram),
        NewPC #= PC+4,
        step(NewPC, NewProgram, Input, Output, Solution).

% Input is [I|_]; Mem[A] = I
% Input is []; Solution = {0, Output, PC, Mem}
opcode(3, _, _, A, _, _, PC, Program, Input, Output, Solution) :-
        Input = [Val|RestInputs],
        setValue(Program, Val, A, NewProgram),
        NewPC #= PC+2,
        step(NewPC, NewProgram, RestInputs, Output, Solution);
        Solution = [0, Output, PC, Program].

% Output = Mem*[A][ModeA]
opcode(4, ModeA, _, A, _, _, PC, Program, Input, _, Solution) :-
        getValue(ModeA, A, Program, Va),
        NewPC #= PC+2,
        step(NewPC, Program, Input, Va, Solution).

% if Mem*[A][ModeA] \= 0 { PC = Mem*[B][ModeB] } { PC = PC + 3 }
opcode(5, ModeA, ModeB, A, B, _, PC, Program, Input, Output, Solution) :-
        getValue(ModeA, A, Program, Va),
        (Va #\= 0,
         getValue(ModeB, B, Program, Vb),
         NewPC #= Vb;
         NewPC #= PC+3),
        step(NewPC, Program, Input, Output, Solution).

% if Mem*[A][ModeA] == 0 { PC = Mem*[B][ModeB] } { PC = PC + 3 }
opcode(6, ModeA, ModeB, A, B, _, PC, Program, Input, Output, Solution) :-
        getValue(ModeA, A, Program, Va),
        (Va #= 0,
         getValue(ModeB, B, Program, Vb),
         NewPC #= Vb;
         NewPC #= PC+3),
        step(NewPC, Program, Input, Output, Solution).

% if Mem*[A][ModeA] < Mem*[B][ModeB] { Mem[C] = 1 } { Mem[C] = 0 }
opcode(7, ModeA, ModeB, A, B, R, PC, Program, Input, Output, Solution) :-
        getValue(ModeA, A, Program, Va),
        getValue(ModeB, B, Program, Vb),
        (Va #< Vb,
         setValue(Program, 1, R, NewProgram);
         setValue(Program, 0, R, NewProgram)),
        NewPC #= PC+4,
        step(NewPC, NewProgram, Input, Output, Solution).

% if Mem*[A][ModeA] == Mem*[B][ModeB] { Mem[C] = 1 } { Mem[C] = 0 }
opcode(8, ModeA, ModeB, A, B, R, PC, Program, Input, Output, Solution) :-
        getValue(ModeA, A, Program, Va),
        getValue(ModeB, B, Program, Vb),
        (Va #= Vb,
         setValue(Program, 1, R, NewProgram);
         setValue(Program, 0, R, NewProgram)),
        NewPC #= PC+4,
        step(NewPC, NewProgram, Input, Output, Solution).

step(PC, Program, Input, Output, Solution) :-
    length(Init, PC),
    % 4 position instructions
    (append(Init, [Instruction, A, B, R|_], Program),
     OpCode is Instruction mod 100,
     ModeA is (Instruction//100) mod 10,
     ModeB is (Instruction//1000) mod 10,
     opcode(OpCode, ModeA, ModeB, A, B, R, PC, Program, Input, Output, Solution), !;

     % 3 position instructions
     append(Init, [Instruction, A, B|_], Program),
     OpCode is Instruction mod 100,
     ModeA is (Instruction//100) mod 10,
     ModeB is (Instruction//100) mod 10,
     opcode(OpCode, ModeA, ModeB, A, B, _, PC, Program, Input, Output, Solution), !;

     % 2 position instructions
     append(Init, [Instruction, A|_], Program),
     OpCode is Instruction mod 100,
     ModeA is (Instruction//100) mod 10,
     opcode(OpCode, ModeA, _, A, _, _, PC, Program, Input, Output, Solution), !;

     % End of program
     append(Init, [99|_], Program),
     Solution = [1, Output], !).

amplifier(Input, Program, Output) :-
    step(0, Program, Input, [], Output).

amplifier(PC, Input, Program, Output) :-
    step(PC, Program, Input, [], Output).

theAmplifiers(Input, [PCA, PCB, PCC, PCD, PCE], [PA, PB, PC, PD, PE], Output) :-
    Input = [A,B,C,D,E], !,
    amplifier(PCA, [A,0], PA, SolA),
    (SolA = [0, OA, NPCA, NPA], !,
     amplifier(PCA, [B, OA], PB, [0, OB, NPCB, NPB]),
     amplifier(PCB, [C, OB], PC, [0, OC, NPCC, NPC]),
     amplifier(PCC, [D, OC], PD, [0, OD, NPCD, NPD]),
     amplifier(PCD, [E, OD], PE, [0, OE, NPCE, NPE]),
     theAmplifiers(OE, [NPCA, NPCB, NPCC, NPCD, NPCE], [NPA, NPB, NPC, NPD, NPE], Output);
     SolA = [1, OA],
     amplifier(PCB, [B, OA], PB, [1, OB]),
     amplifier(PCC, [C, OB], PC, [1, OC]),
     amplifier(PCD, [D, OC], PD, [1, OD]),
     amplifier(PCE, [E, OD], PE, [1, Output]));
    amplifier(PCA, [Input], PA, SolA),
    (SolA = [0, OA, NPCA, NPA], !,
     amplifier(PCB, [OA], PB, [0, OB, NPCB, NPB]),
     amplifier(PCC, [OB], PB, [0, OC, NPCC, NPC]),
     amplifier(PCD, [OC], PC, [0, OD, NPCD, NPD]),
     amplifier(PCE, [OD], PD, [0, OE, NPCE, NPE]),
     theAmplifiers(OE, [NPCA, NPCB, NPCC, NPCD, NPCE], [NPA, NPB, NPC, NPD, NPE], Output);
     SolA = [1, OA],
     amplifier(PCB, [OA], PB, [1, OB]),
     amplifier(PCC, [OB], PC, [1, OC]),
     amplifier(PCD, [OC], PD, [1, OD]),
     amplifier(PCE, [OD], PE, [1, Output])).

naiveGeneration(Input, Combinations) :-
    findall(Total,
            (select(A, Input, LA),
             select(B, LA, LB),
             select(C, LB, LC),
             select(D, LC, LD),
             select(E, LD, []),
             Total = [A,B,C,D,E]),
            Combinations).

executeAmplifiers(Program, Combinations, Result):-
    maplist({Program}/[X, Y]>>theAmplifiers(X, [0,0,0,0,0], [Program, Program, Program, Program, Program], Y), Combinations, Outputs),
    max_list(Outputs, Result).

advent7a(Program, Result) :-
    naiveGeneration([0,1,2,3,4], Combinations),
    executeAmplifiers(Program, Combinations, Result).

advent7b(Program, Result) :-
    naiveGeneration([5,6,7,8,9], Combinations),
    executeAmplifiers(Program, Combinations, Result).

run(SolA, SolB) :-
        input(Program),
        advent7a(Program, SolA),
        advent7b(Program, SolB).