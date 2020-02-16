:- use_module(library(clpfd)).

input(CableA, CableB) :-
    csv_read_file("advent3.txt", [Row1, Row2], []),
    Row1 =.. [_|RawA],
    maplist(string_to_tuple, RawA, CableA),
    Row2 =.. [_|RawB],
    maplist(string_to_tuple, RawB, CableB).

string_to_tuple(Input, (D, Mag)) :-
    string_chars(Input, [D|N]),
    string_chars(Num, N),
    number_string(Mag, Num).

generateSegments([], _, []).
generateSegments([(D,Num)|Steps], (PosX, PosY, Dist), [V|NextSegments]) :-
    (D = 'R', NX #= PosX + Num, NY #= PosY, Dir = h;
     D = 'U', NX #= PosX, NY #= PosY + Num, Dir = v;
     D = 'L', NX #= PosX - Num, NY #= PosY, Dir = h;
     D = 'D', NX #= PosX, NY #= PosY - Num, Dir = v),
    ND #= Dist + Num,
    generateSegments(Steps, (NX, NY, ND), NextSegments),
    V = segment{ dir:Dir, start:(PosX, PosY), end:(NX,NY), acc_steps:Dist }.

manhattan(VA, VB, Sol) :-
    (VA.dir = h,
     VA.start = (_, Y),
     VB.start = (X, _);
     VA.start = (X, _),
     VB.start = (_, Y)),
    Sol #= abs(X) + abs(Y), !.

steps(VA, VB, Sol) :-
    (VA.dir = h,
     VA.start = (Ax, Y),
     VB.start = (X, By),
     Steps #= abs(Ax - X) + abs(By - Y);
     VA.start = (X, Ay),
     VB.start = (Bx, Y),
     Steps #= abs(Bx - X) + abs(Ay - Y)),
    Sol #= VA.acc_steps + VB.acc_steps + Steps.

intersect(V1, V2, F, X) :-
    V1.dir \= V2.dir,
    V1.start = (X1s, Y1s),
    V1.end = (X1e, Y1e),
    V2.start = (X2s, Y2s),
    V2.end = (X2e, Y2e),
    (V1.dir = h,
     (between(X1s, X1e, X2s) ; between(X1e, X1s, X2s)), X1s \= X2s, X1e \= X2s,
     (between(Y2s, Y2e, Y1s) ; between(Y2e, Y2s, Y1s)), Y2s \= Y1s, Y2e \= Y1s;
     (between(X2s, X2e, X1s) ; between(X2e, X2s, X1s)), X2s \= X1s, X2e \= X1s,
     (between(Y1s, Y1e, Y2s) ; between(Y1e, Y1s, Y2s)), Y1s \= Y2s, Y1e \= Y2s),
    call(F, V1, V2, X).

find_intersects(W1, W2, F, Sol) :-
    findall(X,
            (select(V1, W1, _),
             select(V2, W2, _),
             call(intersect, V1, V2, F, X)
            ),
            Distances),
    min_list(Distances, Sol).

run(SolA, SolB) :-
    input(CableA, CableB),
    generateSegments(CableA, (0,0,0), VectorsA),
    generateSegments(CableB, (0,0,0), VectorsB),
    find_intersects(VectorsA, VectorsB, manhattan, SolA),
    find_intersects(VectorsA, VectorsB, steps, SolB).
