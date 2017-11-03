start :- middle(Board),
  Jogada is 1,
  ciclo_jogada(Board, Jogada).

ciclo_jogada(Board,Jogada):-
  display_game_area(Board, Jogada),
  joga(Jogada, Board, BoardNova),
  NovaJogada is Jogada + 1,
  ciclo_jogada(BoardNova, NovaJogada).

joga(Jogada, BoardAtual, BoardNova):-
  repeat,
    ask_for_play(Jogada,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,NovaBoard),
  !,
  move(Jogada,NovaBoard,Linha,Coluna,NovaLinha,NovaColuna,BoardNova).

ask_for_play(Jogada,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,NovaBoard):-
  ((par(Jogada), write('Jogam as pretas'));
  impar(Jogada), write('Jogam as brancas')),nl,
  ask_for_type_of_move(TipoDeMov),
  repeat,
    repeat,
      write('Introduza a coluna da dama a mover:'),
      getChar(Coluna_Letra),
      verify_column(Coluna_Letra,Coluna),
    !,
    repeat,
      write('Introduza a linha da dama a mover:'),
      getDigit(Linha),
      verify_line(Linha),
    !,
    verify_piece_player(Jogada,Linha,Coluna,BoardAtual),
    (getElement(BoardAtual,Linha,Coluna,Peca), ((Peca == rb; Peca == rp), FlagKing = 1); FlagKing = 0),
  !,
  write(FlagKing),
  repeat,
    repeat,
      write('Introduza a nova coluna:'),
      getChar(NovaColuna_Letra),
      verify_column(NovaColuna_Letra,NovaColuna),
    !,
    repeat,
      write('Introduza a nova linha:'),
      getDigit(NovaLinha),
      verify_line(NovaLinha),
    !,
    verifiy_new_piece_player(NovaLinha,NovaColuna,BoardAtual),
  !,
  verify_movement(Jogada,Linha,Coluna,NovaLinha,NovaColuna,TipoDeMov,BoardAtual,NovaBoard).

move(Jogada,BoardAtual,Linha,Coluna,NovaLinha,NovaColuna,BoardNova2):-
  (
    NovaLinha \= 1, NovaLinha \= 8, getElement(BoardAtual,Linha,Coluna,PecaAntiga);
    (
      (par(Jogada), ((NovaLinha == 8, PecaAntiga = rp);(PecaAntiga = p)));
      (impar(Jogada), NovaLinha == 1, (PecaAntiga = rb);(PecaAntiga = b))
    )
  ),
  getElement(BoardAtual,NovaLinha,NovaColuna,PecaNova),
  updateBoard(PecaAntiga,NovaLinha,NovaColuna,BoardAtual,BoardNova),
  updateBoard(PecaNova,Linha,Coluna,BoardNova,BoardNova2).


ask_for_type_of_move(TipoDeMov):-
  repeat,
    write('Por favor escolha o tipo de movimento:'),nl,
    write('1- Normal'),nl,
    write('2- Linear'),nl,
    getDigit(TipoDeMov),
    ((TipoDeMov == 1; TipoDeMov == 2);nl,write('!!AVISO!! Escolha entre 1 a 2'),nl,nl,false),
  !.

verify_piece_player(Jogada,Linha,Coluna,BoardAtual):-
  getElement(BoardAtual,Linha,Coluna,Peca),
  % Verifica se a casa escolhida e branca
  ((Peca == none, nl,write('!!AVISO!! Nao pode escolher uma casa vazia'),nl,nl,!,false);
  % Verifica o tipo de jogador, nao deixando escolher as pecas do outro jogador
  (((par(Jogada),(Peca == p; Peca == rp));
  (impar(Jogada),(Peca == b; Peca == rb)));
  nl,write('!!AVISO!! Nao pode escolher uma dama do adversario'),nl,nl,!,false)).

verifiy_new_piece_player(NovaLinha,NovaColuna,BoardAtual):-
  getElement(BoardAtual,NovaLinha,NovaColuna,Peca),
  % Verifica se a nova peca e vazia, impossivel escolher uma do adversario ou sua
  ((Peca == none);nl,write('!!AVISO!! A sua nova posicao tem de ser vazia'),nl,nl,false).

verify_movement(Jogada,Linha,Coluna,NovaLinha,NovaColuna,TipoDeMov,BoardAtual,NovaBoard):-
  (TipoDeMov == 1,
  /* Se Aux 1, movimento simples para nao comer, se Aux1 = 0 ou 2, vai comer, movimento com dif. de duas casas */
  (
    (
      (impar(Jogada),Aux1 is (Linha - NovaLinha));
      (par(Jogada),Aux1 is (NovaLinha - Linha))
    ), AuxColuna is (Coluna - NovaColuna), AuxColuna2 is abs(AuxColuna),
    (
      ((Aux1 == 0; Aux1 == 2; Aux1 == -2), AuxColuna2 \= 1, eat_piece_simple(Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,NovaBoard));
      (Aux1 == 1, (Aux2 is (Coluna - NovaColuna), Aux22 is abs(Aux2), Aux22 >= 0, Aux22 =< 1),NovaBoard=BoardAtual)
    )
  )
  );
  nl,write('!!AVISO!! Efetuou um moviento invalido'),nl,nl,false.

eat_piece_simple(Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,NovaBoard):-
  (
  AuxLinha is (Linha - NovaLinha),
  AuxLinha2 is abs(AuxLinha),
  AuxColuna is (Coluna - NovaColuna),
  AuxColuna2 is abs(AuxColuna),
  (
    (
      /* Comer simples vertical */
      (AuxLinha2 == 2, AuxColuna2 == 0, X is Coluna, (NovaLinha > Linha, Y is Linha + 1; Y is Linha - 1), getElement(BoardAtual,Y,X,Peca), Peca\=none, updateBoard(none,Y,X,BoardAtual,NovaBoard));
      /* Comer simples horizontal */
      (AuxLinha2 == 0, AuxColuna2 == 2, Y is Linha, (NovaColuna > Coluna, X is Coluna + 1; X is Coluna - 1), getElement(BoardAtual,Y,X,Peca), Peca\=none, updateBoard(none,Y,X,BoardAtual,NovaBoard))
    )
  )
  );false.