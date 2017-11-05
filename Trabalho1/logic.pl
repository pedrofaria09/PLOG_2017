start :- end2(Board),
  Jogada is 1,
  ciclo_jogada(Board, Jogada).

ciclo_jogada(Board,-1):-display_game_area(Board, 1),write('JOGADOR 1 PERDEU').
ciclo_jogada(Board,-2):-display_game_area(Board, 2),write('JOGADOR 2 PERDEU').
ciclo_jogada(Board,Jogada):-
  display_game_area(Board, Jogada),
  joga(Jogada, Board, BoardNova),
  verify_end_game(BoardNova, Jogada, NovaJogada),
  ciclo_jogada(BoardNova, NovaJogada).

verify_end_game(Board, Jogada, NovaJogada):-
  conta_pecas(b,Board,Nr_brancas),
  conta_pecas(p,Board,Nr_pretas),
  conta_pecas(rb,Board,Nr_rei_brancas),
  conta_pecas(rp,Board,Nr_rei_pretas),
  (
    ((Nr_brancas == 0, Nr_rei_brancas == 0),NovaJogada is -1);
    ((Nr_pretas == 0, Nr_rei_pretas == 0),NovaJogada is -2);
    (NovaJogada is Jogada + 1)
  ).

joga(Jogada, BoardAtual, BoardNova):-
  ((par(Jogada), Jogador = 2);(impar(Jogada), Jogador = 1)),
  repeat,
    ask_for_play(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,NovaBoard),
  !,
  move(Jogador,NovaBoard,Linha,Coluna,NovaLinha,NovaColuna,BoardNova).

ask_for_play(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,NovaBoard):-
  ((Jogador == 2, write('Jogam as pretas'));
  Jogador == 1, write('Jogam as brancas')),nl,
  ask_for_type_of_move(TipoDeMov),
  repeat,
    ask_for_initial_piece(Linha,Coluna),
    verify_initial_piece_player(Jogador,Linha,Coluna,BoardAtual),
  !,
  verify_if_king_and_not_linear(BoardAtual,Linha,Coluna,FlagKing,TipoDeMov), /* Nao pode escolher rei e movimento linear */
  repeat,
    ask_for_new_piece(NovaLinha,NovaColuna),
    verifiy_new_piece_player(NovaLinha,NovaColuna,BoardAtual),
  !,
  (
    (FlagKing == 0, verify_movement(Jogador,Linha,Coluna,NovaLinha,NovaColuna,TipoDeMov,BoardAtual,NovaBoard));
    (FlagKing == 1, verify_king_movement(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,NovaBoard))
  ).

move(Jogador,BoardAtual,Linha,Coluna,NovaLinha,NovaColuna,BoardNova2):-
  verify_piece_to_king(Jogador,BoardAtual,Linha,Coluna,NovaLinha,PecaAntiga),
  getElement(BoardAtual,NovaLinha,NovaColuna,PecaNova),
  updateBoard(PecaAntiga,NovaLinha,NovaColuna,BoardAtual,BoardNova),
  updateBoard(PecaNova,Linha,Coluna,BoardNova,BoardNova2).

ask_for_initial_piece(Linha,Coluna):-
  repeat,
    write('Introduza a coluna da dama a mover:'),
    getChar(Coluna_Letra),
    verify_column(Coluna_Letra,Coluna),
  !,
  repeat,
    write('Introduza a linha da dama a mover:'),
    getDigit(Linha),
    verify_line(Linha),
  !.

ask_for_new_piece(NovaLinha,NovaColuna):-
  repeat,
    write('Introduza a nova coluna:'),
    getChar(NovaColuna_Letra),
    verify_column(NovaColuna_Letra,NovaColuna),
  !,
  repeat,
    write('Introduza a nova linha:'),
    getDigit(NovaLinha),
    verify_line(NovaLinha),
  !.

ask_for_type_of_move(TipoDeMov):-
  repeat,
    write('Por favor escolha o tipo de movimento:'),nl,
    write('1- Normal'),nl,
    write('2- Linear'),nl,
    getDigit(TipoDeMov),
    ((TipoDeMov == 1; TipoDeMov == 2);nl,write('!!AVISO!! Escolha entre 1 a 2'),nl,nl,false),
  !.

verify_if_king_and_not_linear(BoardAtual,Linha,Coluna,FlagKing,TipoDeMov):-
  (getElement(BoardAtual,Linha,Coluna,Peca), ((Peca == rb; Peca == rp), FlagKing = 1); FlagKing = 0), /* Verifica se escolheu um rei */
  ((FlagKing == 1, TipoDeMov == 2, nl,write('!!AVISO!! Nao pode escolher o rei para o movimento linear.'),nl,nl,!,false);true).

verify_initial_piece_player(Jogador,Linha,Coluna,BoardAtual):-
  getElement(BoardAtual,Linha,Coluna,Peca),
  % Verifica se a casa escolhida e branca
  ((Peca == none, nl,write('!!AVISO!! Nao pode escolher uma casa vazia'),nl,nl,!,false);
  % Verifica o tipo de jogador, nao deixando escolher as pecas do outro jogador
  (((Jogador == 2,(Peca == p; Peca == rp));
  (Jogador == 1,(Peca == b; Peca == rb)));
  nl,write('!!AVISO!! Nao pode escolher uma dama do adversario'),nl,nl,!,false)).

verifiy_new_piece_player(NovaLinha,NovaColuna,BoardAtual):-
  getElement(BoardAtual,NovaLinha,NovaColuna,Peca),
  % Verifica se a nova peca e vazia, impossivel escolher uma do adversario ou sua
  ((Peca == none);nl,write('!!AVISO!! A sua nova posicao tem de ser vazia'),nl,nl,false).

/* Verifica se chegou a casa do adversario e muda para rei */
verify_piece_to_king(Jogador,BoardAtual,Linha,Coluna,NovaLinha,PecaAntiga):-
  (
    NovaLinha \= 1, NovaLinha \= 8, getElement(BoardAtual,Linha,Coluna,PecaAntiga);
    ( /* Verifica se chegou a casa do adversario e atualiza para rei, verifica se volta para tras ao comer e nao altera o estado caso nao seja rei */
      (Jogador == 2, ((NovaLinha == 8, PecaAntiga = rp);(getElement(BoardAtual,Linha,Coluna,PecaAntiga), NovaLinha == 1, PecaAntiga == rp, true);(PecaAntiga = p)));
      (Jogador == 1, ((NovaLinha == 1, PecaAntiga = rb);(getElement(BoardAtual,Linha,Coluna,PecaAntiga), NovaLinha == 8, PecaAntiga == rb, true);(PecaAntiga = b)))
    )
  ).

verify_movement(Jogador,Linha,Coluna,NovaLinha,NovaColuna,TipoDeMov,BoardAtual,NovaBoard2):-
  (TipoDeMov == 1,
  /* Se Aux 1, movimento simples para nao comer, se Aux1 = 0 ou 2, vai comer, movimento com dif. de duas casas */
  (
    (
      (Jogador == 1, Aux1 is (Linha - NovaLinha));
      (Jogador == 2, Aux1 is (NovaLinha - Linha))
    ), AuxColuna is (Coluna - NovaColuna), AuxColuna2 is abs(AuxColuna),
    (
      ((Aux1 == 0; Aux1 == 2; Aux1 == -2), AuxColuna2 \= 1, eat_piece_simple(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,NovaBoard), check_if_can_eat_more(Jogador,Linha,Coluna,NovaLinha,NovaColuna,NovaBoard,NovaBoard2),NovaBoard2=NovaBoard);
      (Aux1 == 1, (Aux2 is (Coluna - NovaColuna), Aux22 is abs(Aux2), Aux22 >= 0, Aux22 =< 1),NovaBoard2=BoardAtual)
    )
  )
  );
  (TipoDeMov == 2,
    (
      (Jogador == 1, Linha > NovaLinha, DeltaLinha is (Linha - NovaLinha));
      (Jogador == 2, NovaLinha > Linha, DeltaLinha is (NovaLinha - Linha))
    ), AuxColuna is (Coluna - NovaColuna), DeltaColuna is abs(AuxColuna),
    (
      ((Aux is abs(DeltaLinha), Aux < 2; DeltaColuna == 1), nl,write('!!AVISO!! Movimento linear errado'),nl,nl,!,false);
      (AuxDeltaLinha is DeltaLinha - 1, (DeltaColuna == 0, AuxDeltaColuna is 0; AuxDeltaColuna is DeltaColuna -1), check_if_is_pieces(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,AuxDeltaLinha, AuxDeltaColuna),NovaBoard2 = BoardAtual)
    )
  );
  nl,write('!!AVISO!! Efetuou um moviento invalido'),nl,nl,false.

/* Have a bug on last movement */
check_if_can_eat_more(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,NovaBoard):-
  (
    (Aux1 is (NovaColuna-1), getElement(BoardAtual,NovaLinha,Aux1,Peca), (Jogador == 1, (Peca == p; Peca == rp);Jogador == 2, (Peca == b; Peca == rb)),
      Aux2 is (NovaColuna-2), getElement(BoardAtual,NovaLinha,Aux2,Peca2), Peca2 == none, move(Jogador,BoardAtual,Linha,Coluna,NovaLinha,NovaColuna,BoardNova2),
      display_game_area(BoardNova2, Jogador), write('Pode comer mais uma peca'), nl, ask_for_new_piece(NovaLinha2,NovaColuna2),
      check_if_can_eat_more(Jogador,NovaLinha,NovaColuna,NovaLinha2,NovaColuna2,BoardNova2,BoardNova2));
    (Aux1 is (NovaColuna+1), getElement(BoardAtual,NovaLinha,Aux1,Peca), (Jogador == 1, (Peca == p; Peca == rp);Jogador == 2, (Peca == b; Peca == rb)),
      Aux2 is (NovaColuna+2), getElement(BoardAtual,NovaLinha,Aux2,Peca2), Peca2 == none, move(Jogador,BoardAtual,Linha,Coluna,NovaLinha,NovaColuna,BoardNova2),
      display_game_area(BoardNova2, Jogador), write('Pode comer mais uma peca'), nl, ask_for_new_piece(NovaLinha2,NovaColuna2),
      check_if_can_eat_more(Jogador,NovaLinha,NovaColuna,NovaLinha2,NovaColuna2,BoardNova2,BoardNova2));
    (Aux1 is (NovaLinha-1), getElement(BoardAtual,Aux1,NovaColuna,Peca), (Jogador == 1, (Peca == p; Peca == rp);Jogador == 2, (Peca == b; Peca == rb)),
      Aux2 is (NovaLinha-2), getElement(BoardAtual,Aux2,NovaColuna,Peca2), Peca2 == none, move(Jogador,BoardAtual,Linha,Coluna,NovaLinha,NovaColuna,BoardNova2),
      display_game_area(BoardNova2, Jogador), write('Pode comer mais uma peca'), nl, ask_for_new_piece(NovaLinha2,NovaColuna2),
      check_if_can_eat_more(Jogador,NovaLinha,NovaColuna,NovaLinha2,NovaColuna2,BoardNova2,BoardNova2));
    (Aux1 is (NovaLinha+1), getElement(BoardAtual,Aux1,NovaColuna,Peca), (Jogador == 1, (Peca == p; Peca == rp);Jogador == 2, (Peca == b; Peca == rb)),
      Aux2 is (NovaLinha+2), getElement(BoardAtual,Aux2,NovaColuna,Peca2), Peca2 == none, move(Jogador,BoardAtual,Linha,Coluna,NovaLinha,NovaColuna,BoardNova2),
      display_game_area(BoardNova2, Jogador), write('Pode comer mais uma peca'), nl, ask_for_new_piece(NovaLinha2,NovaColuna2),
      check_if_can_eat_more(Jogador,NovaLinha,NovaColuna,NovaLinha2,NovaColuna2,BoardNova2,BoardNova2))
  );NovaBoard=BoardAtual,true.

check_if_is_pieces(_,_,_,_,_,_,0,0).
check_if_is_pieces(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,DeltaLinha,DeltaColuna):-
  (Jogador == 1,
    (DeltaColuna == 0, DeltaColuna \= DeltaLinha, (Y is Linha - 1, X is Coluna, getElement(BoardAtual,Y,X,Peca), Peca == b, AuxDeltaLinha is DeltaLinha -1, !,check_if_is_pieces(Jogador,Y,X,NovaLinha,NovaColuna,BoardAtual,AuxDeltaLinha, DeltaColuna));
    (DeltaColuna == DeltaLinha, (Y is Linha - 1, (NovaColuna > Coluna, X is Coluna + 1; X is Coluna - 1), getElement(BoardAtual,Y,X,Peca), Peca == b, AuxDeltaLinha is DeltaLinha - 1, AuxDeltaColuna is DeltaColuna - 1, !,check_if_is_pieces(Jogador,Y,X,NovaLinha,NovaColuna,BoardAtual,AuxDeltaLinha,AuxDeltaColuna))))
  );
  (Jogador == 2,
    (DeltaColuna == 0, DeltaColuna \= DeltaLinha,  (Y is Linha + 1, X is Coluna, getElement(BoardAtual,Y,X,Peca), Peca == p, AuxDeltaLinha is DeltaLinha -1, !,check_if_is_pieces(Jogador,Y,X,NovaLinha,NovaColuna,BoardAtual,AuxDeltaLinha,DeltaColuna));
    (DeltaColuna == DeltaLinha, (Y is Linha + 1, (NovaColuna > Coluna, X is Coluna + 1; X is Coluna - 1), getElement(BoardAtual,Y,X,Peca), Peca == p, AuxDeltaLinha is DeltaLinha - 1, AuxDeltaColuna is DeltaColuna - 1, !,check_if_is_pieces(Jogador,Y,X,NovaLinha,NovaColuna,BoardAtual,AuxDeltaLinha,AuxDeltaColuna))))
  );
  !,false.

eat_piece_simple(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,NovaBoard):-
  (
  AuxLinha is (Linha - NovaLinha),
  AuxLinha2 is abs(AuxLinha),
  AuxColuna is (Coluna - NovaColuna),
  AuxColuna2 is abs(AuxColuna),
  (
    (
      /* Comer simples vertical */
      (AuxLinha2 == 2, AuxColuna2 == 0, X is Coluna,
        (NovaLinha > Linha, Y is Linha + 1; Y is Linha - 1),
        getElement(BoardAtual,Y,X,Peca),
        Peca\=none, ((Jogador == 1, (Peca == p; Peca == rp));((Jogador == 2, (Peca == b; Peca == rb)))),
        updateBoard(none,Y,X,BoardAtual,NovaBoard)
      );
      /* Comer simples horizontal */
      (AuxLinha2 == 0, AuxColuna2 == 2, Y is Linha,
        (NovaColuna > Coluna, X is Coluna + 1; X is Coluna - 1),
        getElement(BoardAtual,Y,X,Peca),
        Peca\=none, ((Jogador == 1, (Peca == p; Peca == rp));((Jogador == 2, (Peca == b; Peca == rb)))),
        updateBoard(none,Y,X,BoardAtual,NovaBoard)
      )
    )
  )
  );false.

verify_king_movement(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,NovaBoard):-
  (
  AuxLinha is (Linha - NovaLinha),
  AuxLinhaABS is abs(AuxLinha),
  AuxColuna is (Coluna - NovaColuna),
  AuxColunaABS is abs(AuxColuna),
    (
      /* Vertical - TypeMove = 1 */
      ((AuxLinhaABS > 0, AuxColunaABS == 0), Delta is AuxLinhaABS, check_king_eating(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,Delta,1,0), king_eat(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,NovaBoard,Delta,1));
      /* Horizontal - TypeMove = 2 */
      ((AuxLinhaABS == 0, AuxColunaABS > 0), Delta is AuxColunaABS, check_king_eating(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,Delta,2,0), king_eat(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,NovaBoard,Delta,2));
      /* Diagonal - TypeMove = 3 */
      ((AuxLinhaABS > 0, AuxColunaABS > 0, AuxLinhaABS == AuxColunaABS), Delta is AuxColunaABS,check_king_eating(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,Delta,3,0), king_eat(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,NovaBoard,Delta,3))
    )
  );
  (nl,write('!!AVISO!! Movimento invalido do rei'),nl,nl,false).

check_king_eating(_,_,_,_,_,_,0,_,0):-true.
check_king_eating(_,_,_,_,_,_,0,_,1):-true.
check_king_eating(_,_,_,_,_,_,_,_,2):-!,false.
check_king_eating(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,Delta,TypeMove,Conta):-
  (
    (TypeMove == 1, X is Coluna, (NovaLinha > Linha, Y is Linha + 1; Y is Linha - 1), getElement(BoardAtual,Y,X,Peca), AuxDelta is Delta -1,
      (
        (Peca == none, !,check_king_eating(Jogador,Y,X,NovaLinha,NovaColuna,BoardAtual,AuxDelta,1,Conta));
        (Jogador == 1, (((Peca == b; Peca == rb), !,false);(Peca == p; Peca == rp), AuxConta is Conta + 1, !,check_king_eating(1,Y,X,NovaLinha,NovaColuna,BoardAtual,AuxDelta,1,AuxConta)));
        (Jogador == 2, (((Peca == p; Peca == rp), !,false);(Peca == b; Peca == rb), AuxConta is Conta + 1, !,check_king_eating(2,Y,X,NovaLinha,NovaColuna,BoardAtual,AuxDelta,1,AuxConta)))
      )
    );
    (TypeMove == 2, Y is Linha, (NovaColuna > Coluna, X is Coluna + 1; X is Coluna - 1), getElement(BoardAtual,Y,X,Peca), AuxDelta is Delta -1,
      (
        (Peca == none, !,check_king_eating(Jogador,Y,X,NovaLinha,NovaColuna,BoardAtual,AuxDelta,2,Conta));
        (Jogador == 1, (((Peca == b; Peca == rb), !,false);(Peca == p; Peca == rp), AuxConta is Conta + 1, !,check_king_eating(1,Y,X,NovaLinha,NovaColuna,BoardAtual,AuxDelta,2,AuxConta)));
        (Jogador == 2, (((Peca == p; Peca == rp), !,false);(Peca == b; Peca == rb), AuxConta is Conta + 1, !,check_king_eating(2,Y,X,NovaLinha,NovaColuna,BoardAtual,AuxDelta,2,AuxConta)))
      )
    );
    (TypeMove == 3, (NovaLinha > Linha, Y is Linha + 1; Y is Linha - 1), (NovaColuna > Coluna, X is Coluna + 1; X is Coluna - 1), getElement(BoardAtual,Y,X,Peca), AuxDelta is Delta -1,
      (
        (Peca == none, !,check_king_eating(Jogador,Y,X,NovaLinha,NovaColuna,BoardAtual,AuxDelta,3,Conta));
        (Jogador == 1, (((Peca == b; Peca == rb), !,false);(Peca == p; Peca == rp), AuxConta is Conta + 1, !,check_king_eating(1,Y,X,NovaLinha,NovaColuna,BoardAtual,AuxDelta,3,AuxConta)));
        (Jogador == 2, (((Peca == p; Peca == rp), !,false);(Peca == b; Peca == rb), AuxConta is Conta + 1, !,check_king_eating(2,Y,X,NovaLinha,NovaColuna,BoardAtual,AuxDelta,3,AuxConta)))
      )
    )
  ).

king_eat(_,_,_,_,_,BoardAtual,NovaBoard,0,_):-NovaBoard = BoardAtual, true.
king_eat(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,NovaBoard,Delta,TypeMove):-
  (
    (TypeMove == 1, X is Coluna, (NovaLinha > Linha, Y is Linha + 1; Y is Linha - 1), getElement(BoardAtual,Y,X,Peca), AuxDelta is Delta -1,
      (
        (Peca == none, !,king_eat(Jogador,Y,X,NovaLinha,NovaColuna,BoardAtual,NovaBoard,AuxDelta,1));
        (Jogador == 1, (Peca == p; Peca == rp), updateBoard(none,Y,X,BoardAtual,NovaBoard), !,king_eat(1,Y,X,NovaLinha,NovaColuna,NovaBoard,_,AuxDelta,1));
        (Jogador == 2, (Peca == b; Peca == rb), updateBoard(none,Y,X,BoardAtual,NovaBoard), !,king_eat(2,Y,X,NovaLinha,NovaColuna,NovaBoard,_,AuxDelta,1))
      )
    );
    (TypeMove == 2, Y is Linha, (NovaColuna > Coluna, X is Coluna + 1; X is Coluna - 1), getElement(BoardAtual,Y,X,Peca), AuxDelta is Delta -1,
      (
        (Peca == none, !,king_eat(Jogador,Y,X,NovaLinha,NovaColuna,BoardAtual,NovaBoard,AuxDelta,2));
        (Jogador == 1, (Peca == p; Peca == rp), updateBoard(none,Y,X,BoardAtual,NovaBoard), !,king_eat(1,Y,X,NovaLinha,NovaColuna,NovaBoard,_,AuxDelta,2));
        (Jogador == 2, (Peca == b; Peca == rb), updateBoard(none,Y,X,BoardAtual,NovaBoard), !,king_eat(2,Y,X,NovaLinha,NovaColuna,NovaBoard,_,AuxDelta,2))
      )
    );
    (TypeMove == 3, (NovaLinha > Linha, Y is Linha + 1; Y is Linha - 1), (NovaColuna > Coluna, X is Coluna + 1; X is Coluna - 1), getElement(BoardAtual,Y,X,Peca), AuxDelta is Delta -1,
      (
        (Peca == none, !,king_eat(Jogador,Y,X,NovaLinha,NovaColuna,BoardAtual,NovaBoard,AuxDelta,3));
        (Jogador == 1, (Peca == p; Peca == rp), updateBoard(none,Y,X,BoardAtual,NovaBoard), !,king_eat(1,Y,X,NovaLinha,NovaColuna,NovaBoard,_,AuxDelta,3));
        (Jogador == 2, (Peca == b; Peca == rb), updateBoard(none,Y,X,BoardAtual,NovaBoard), !,king_eat(2,Y,X,NovaLinha,NovaColuna,NovaBoard,_,AuxDelta,3))
      )
    )
  ).