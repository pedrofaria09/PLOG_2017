:- use_module(library(lists)).
:- use_module(library(random)).

% Clear screen
cls :- write('\e[2J').

% get input from user
getNewLine:- get_code(T), (T == 10 -> ! ; getNewLine).
getDigit(D):- get_code(Dt), D is Dt - 48, (Dt == 10 -> ! ; getNewLine).
getChar(C):- get_char(C), char_code(C, Co), (Co == 10 -> ! ; getNewLine).

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

% Verifica input se está entre 1 a 8
verify_line(Linha):-
  (Linha >= 1, Linha =< 8);
  nl,write('!!AVISO!! Introduza a linha correta: 1-8'),nl,nl,false.

% Verifica input se está entre A a H e converte 1 a 9
verify_column(Coluna_Letra,Coluna):-
  (
    ((Coluna_Letra == (a);Coluna_Letra == ('A')), Coluna is 1);
    ((Coluna_Letra == (b);Coluna_Letra == ('B')), Coluna is 2);
    ((Coluna_Letra == (c);Coluna_Letra == ('C')), Coluna is 3);
    ((Coluna_Letra == (d);Coluna_Letra == ('D')), Coluna is 4);
    ((Coluna_Letra == (e);Coluna_Letra == ('E')), Coluna is 5);
    ((Coluna_Letra == (f);Coluna_Letra == ('F')), Coluna is 6);
    ((Coluna_Letra == (g);Coluna_Letra == ('G')), Coluna is 7);
    ((Coluna_Letra == (h);Coluna_Letra == ('H')), Coluna is 8)
  );
  nl,write('!!AVISO!! Introduza a coluna correta: A-H'),nl,nl,false.

conta_total_pecas(Jogador, Board, Valor):-
  (Jogador == 1, conta_pecas(b,Board,Nr_brancas), conta_pecas(rb,Board,Nr_rei_brancas), Valor is Nr_brancas+Nr_rei_brancas);
  (Jogador == 2, conta_pecas(p,Board,Nr_pretas), conta_pecas(rp,Board,Nr_rei_pretas), Valor is Nr_pretas+Nr_rei_pretas).
