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

% Conta o número de peças de um jogador
conta_pecas(_,[],0).
conta_pecas(Peca, [H|T], Numero) :-
  contaLista(Peca, H, T, Numero).
contaLista(Peca, [H|T], L2, Numero):-
  (Peca == H -> (contaLista(Peca, T, L2, Y), Numero is Y + 1); contaLista(Peca, T, L2, Numero)).
contaLista(Peca,[], L2, Numero) :- conta_pecas(Peca, L2, Numero).