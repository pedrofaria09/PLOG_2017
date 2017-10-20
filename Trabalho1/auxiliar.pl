:- use_module(library(lists)).

% Clear screen
cls :- write('\e[2J').

% get input from user
getNewLine:- get_code(T), (T == 10 -> ! ; getNewLine).
getDigit(D):- get_code(Dt), D is Dt - 48, (Dt == 10 -> ! ; getNewLine).
getChar(C) :- get_char(C), char_code(C, Co), (Co == 10 -> ! ; getNewLine).

% Retorna elemento de uma posicao X,Y
getElement(Board, Row, Col, Element):-
  nth1(Row, Board, MatrixRow),
  nth1(Col, MatrixRow, Element).

% Faz update a Board
updateTo(_,[],[],_,_).
updateTo(ElemToChange,[[_|Xs]|Ys],[[ElemToChange|Xs1]|Ys1],1,1) :-
                    !,updateTo(ElemToChange,[Xs|Ys],[Xs1|Ys1],0,0).

updateTo(ElemToChange,[[X]|Xs],[[X]|Xs1],0,0) :-
                    updateTo(ElemToChange,Xs,Xs1,0,0),!.

updateTo(ElemToChange,[[X|Xs]|Ys],[[X|Xs1]|Ys1],0,0) :-
                    updateTo(ElemToChange,[Xs|Ys],[Xs1|Ys1],0,0).

updateTo(ElemToChange,[[X|Xs]|Ys],[[X|Xs1]|Ys1],N,1) :-
                    N1 is N-1,
                    updateTo(ElemToChange,[Xs|Ys],[Xs1|Ys1],N1,1).

updateTo(ElemToChange,[Xs|Ys],[Xs|Ys1],N,M) :-
                    M1 is M-1,
                    updateTo(ElemToChange,Ys,Ys1,N,M1),!.

updateBoard(ElemToChange,Y,X,Board,NewBoard) :-
                    updateTo(ElemToChange,Board,NewBoard,X,Y).

% Conta o número de peças de um jogador
conta_pecas(_,[],0).
conta_pecas(Peca, [H|T], Numero) :-
  contaLista(Peca, H, T, Numero).
contaLista(Peca, [H|T], L2, Numero):-
  (Peca == H -> (contaLista(Peca, T, L2, Y), Numero is Y + 1); contaLista(Peca, T, L2, Numero)).
contaLista(Peca,[], L2, Numero) :- conta_pecas(Peca, L2, Numero).

% Verifica o jogador - Par ou Impar
par(Jogada):-
  Jogada mod 2 =:= 0.

impar(Jogada):-
  Jogada mod 2 =:= 1.