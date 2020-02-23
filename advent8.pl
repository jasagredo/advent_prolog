:- use_module(library(clpfd)).
:- use_module(library(aggregate)).
:- use_module(library(lambda)).

input(Input) :-
        read_file_to_string("advent8.txt", RawInput, []),
        split_string(RawInput, "\n", "", [String|_]),
        string_to_list(String, List),
        maplist([X, Y]>>(Y is X - 48), List, Input).

width(25).
height(6).

chop_list(_, [], []).
chop_list(H, In, Out) :-
        append(L, Next, In),
        length(L, H),
        chop_list(H, Next, NextOut),
        append([L], NextOut, Out).

occurences(Str, Char, N) :-
  aggregate_all(count, select(Char, Str, _), N).

countZerosInLayer(L, (Zeros, L)) :-
        maplist([L, Z]>>occurences(L, 0, Z), L, ZL),
        sumlist(ZL, Zeros).

getMinZerosLine(Lines, MinLine) :-
        width(W),
        NW #= W + 1,
        getMinZerosLineAux(Lines, MinLine, (NW, "")).

getMinZerosLineAux([], MinLine, (_, MinLine)).
getMinZerosLineAux([(LC, LS)|Next], MinLine, (CurrentMin, L)) :-
        LC #< CurrentMin,
        getMinZerosLineAux(Next, MinLine, (LC, LS)), !;
        getMinZerosLineAux(Next, MinLine, (CurrentMin, L)).

advent8a(In, Sol) :-
        width(W),
        chop_list(W, In, Lines),
        height(H),
        chop_list(H, Lines, Layers),
        maplist(countZerosInLayer, Layers, Counted),
        getMinZerosLine(Counted, Layer),
        maplist([L, O]>>occurences(L, 1, O), Layer, Ones),
        sumlist(Ones, One),
        maplist([L, T]>>occurences(L, 2, T), Layer, Twos),
        sumlist(Twos, Two),
        Sol #= One * Two.

traverse(X, Y, Layers, AccumLayer, Solution) :-
         height(Y),
         Solution = AccumLayer, !;
         width(X),
         NewY #= Y + 1,
         traverse(0, NewY, Layers, AccumLayer, Solution), !;
         maplist({X,Y}/[Layer, Pix]>>
                (append(PreviousLines, [ThisLine|_], Layer),
                 length(PreviousLines, Y),
                 append(PreviousPixels, [Pix|_], ThisLine),
                 length(PreviousPixels, X)),
                 Layers, Pixel),
         NewX #= X + 1,
         append(AccumLayer, [Pixel], NextAccum),
         traverse(NewX, Y, Layers, NextAccum, Solution).

resolvePixel([2|Next], Sol) :-
        resolvePixel(Next, Sol), !.
resolvePixel([X|_], X) :- !.

advent8b(In) :-
        width(W),
        chop_list(W, In, Lines),
        height(H),
        chop_list(H, Lines, Layers),
        traverse(0, 0, Layers, [], Pixels),
        maplist(resolvePixel, Pixels, Photo),
        maplist([P, A]>>(P = 1,
                         A = '\u2588', !;
                         A = ' '), Photo, NewPhoto),
        chop_list(W, NewPhoto, Res),
        maplist([X]>>(maplist([Y]>>format("~w", [Y]), X), format("~n", [])), Res).

run(SolA) :-
        input(I),
        advent8a(I, SolA),
        advent8b(I).
