:-dynamic flagEated/1.
:-dynamic flagCheckEated/1.
:-dynamic flagKingEating/1.
:-dynamic jogada/1.
:-dynamic backBoard/1.

% Predicado inicial que chama o ciclo de jogo
% Tipo 1 - Player vs Player
% Tipo 2 - Player vs PC
% Tipo 3 - PC vs PC
start(Tipo) :- initial(Board),
  Jogada is 1,
  ciclo_jogada(Board, Jogada, Tipo).

% Predicado do ciclo do jogo, no qual recebe uma board, uma jogada e o tipo de jogo
ciclo_jogada(Board,-1,_):-jogada(Jogada),display_game_area(Board, Jogada),write('PRETAS GANHOU!!').
ciclo_jogada(Board,-2,_):-jogada(Jogada),display_game_area(Board, Jogada),write('BRANCAS GANHOU!!').
ciclo_jogada(Board,Jogada,Tipo):-
  (Tipo \= 3, display_game_area(Board, Jogada);true),
  %display_game_area(Board, Jogada),
  joga(Jogada, Board, BoardNova, Tipo),
  verify_end_game(BoardNova, Jogada, NovaJogada),
  ciclo_jogada(BoardNova, NovaJogada, Tipo).

% Predicado que verifica se o jogo chegou ao final quando um jogador tem 0 damas e 0 reis.
verify_end_game(Board, Jogada, NovaJogada):-
  conta_pecas(b,Board,Nr_brancas),
  conta_pecas(p,Board,Nr_pretas),
  conta_pecas(rb,Board,Nr_rei_brancas),
  conta_pecas(rp,Board,Nr_rei_pretas),
  (
    ((Nr_brancas == 0, Nr_rei_brancas == 0),NovaJogada is -1, asserta(jogada(Jogada)));
    ((Nr_pretas == 0, Nr_rei_pretas == 0),NovaJogada is -2, asserta(jogada(Jogada)));
    (NovaJogada is Jogada + 1)
  ).

% Predicado no qual chama os devidos predicados de jogada dependendo do tipo de jogo escolhido
% Se o tipo for 1, Humano Vs Humano e chama o predicado ask_for_play
% Se o tipo for 2, Humano Vs PC e chama o predicado ask_for_play_plr_vs_pc
% Se o tipo for 3, Humano Vs PC e chama o predicado ask_for_play_pc_vs_pc
% Apos ser verificada todas as regras da jogada dentro dos ask_for_play respetivos, é feito o movimento
% No final verifica se o jogador pode comer mais peças no caso de ter comido uma peca durante o este movimento.
joga(Jogada, BoardAtual, NovaBoard3, Tipo):-
  ((par(Jogada), Jogador = 2);(impar(Jogada), Jogador = 1)),
  repeat,
    (
      (Tipo == 1, ask_for_play(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,NovaBoard));
      (Tipo == 2, ask_for_play_plr_vs_pc(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,NovaBoard),
        ((Jogador = 2, info_jogada_pc(Tipo,Jogador,Linha,Coluna,NovaLinha,NovaColuna));true)
      );
      (Tipo == 3, ask_for_play_pc_vs_pc(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,NovaBoard),
        info_jogada_pc(Tipo,Jogador,Linha,Coluna,NovaLinha,NovaColuna)
      )
    ),
  !,
  move(Jogador,NovaBoard,Linha,Coluna,NovaLinha,NovaColuna,NovaBoard2),
  check_can_eat_more(Jogador,NovaBoard2,NovaLinha,NovaColuna,Tipo,NovaBoard3).

/***************** Player vs Player *******************/
% Predicado importante na verificação da jogada no modo jogador humano vs jogador humano. Recebe um jogador e uma board atual
% Devolve a linha e coluna da peça escolhida, a nova linha e nova coluna escolhida e uma nova board caso haja movimentos intermédios realizados (numa jogada o jogador pode comer 2 ou mais peças)
% Começa por verificar qual o jogador a jogar e é identificado na consola, pergunta qual o tipo de movimento que quer realizar (Linear ou Simples) -> ask_for_type_of_move
% Pergunta ao jogador pela linha e coluna da peça que deseja mover -> ask_for_initial_piece
% Verifica se essa peça é sua -> verify_initial_piece_player
% Verifica se a peça escolhida é rei ou não, pois o rei não pode ter um movimento Linear -> verify_if_king_and_not_linear
% Pergunta ao jogador pela nova linha e nova coluna da peça a mover -> ask_for_new_piece
% Verifica se a nova peça não é sua e nem é do jogador -> verifiy_new_piece_player
% Dependendo da peça escolhida, dama normal ou rei, chama o predicado verify_movement e verify_king_movement, respetivamente e verifica todas as regras dessa jogada, caso alguma falhe, o jogador poderá repetir a jogada
% No final deste predicado é verificado se o jogador comeu alguma peça, caso não tenha comido verifica se poderia ter comido alguma peça, caso possa, o jogador terá de repetir a jogada.
ask_for_play(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,NovaBoard):-
  ((Jogador == 2, write('Jogam as pretas'));
  Jogador == 1, write('Jogam as brancas')),nl,
  FlagEat is 0, asserta(flagEated(FlagEat)),
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
    (FlagKing == 0, verify_movement(Jogador,Linha,Coluna,NovaLinha,NovaColuna,TipoDeMov,BoardAtual,NovaBoard,1));
    (FlagKing == 1, verify_king_movement(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,NovaBoard,1))
  ),
  flagEated(FlagEated), ((FlagEated == 0, loop_to_check_eaten(NovaBoard,Jogador,1,1,0),flagCheckEated(Reply),write('flag:'),write(Reply),nl,retract(flagCheckEated(_)),((Reply == 1,nl,write('!!AVISO!! E obrigado a comer'),nl,nl,!,false);true));true).

/***************** Player vs PC *******************/
% Predicado importante, semelhante ao ask_for_play do jogador humano vs jogador humano.
% Este predicado trata do jogo humano vs jogo computador
% O jogador 1 será sempre o jogador humano e jogador 2 o computador
ask_for_play_plr_vs_pc(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,NovaBoard):-
  ((Jogador == 1, write('Jogam as brancas'),nl);true),
  FlagEat is 0, asserta(flagEated(FlagEat)),
  ((Jogador == 1, ask_for_type_of_move(TipoDeMov));(Jogador == 2, ask_for_type_of_move_pc(TipoDeMov))),
  repeat,
    ((Jogador == 1, ask_for_initial_piece(Linha,Coluna));(Jogador == 2, ask_for_initial_piece_pc(Linha,Coluna))),
    ((Jogador == 1, verify_initial_piece_player(Jogador,Linha,Coluna,BoardAtual));(Jogador == 2, verify_initial_piece_player_pc(Jogador,Linha,Coluna,BoardAtual))),
  !,
  ((Jogador == 1, verify_if_king_and_not_linear(BoardAtual,Linha,Coluna,FlagKing,TipoDeMov));(Jogador == 2, verify_if_king_and_not_linear_pc(BoardAtual,Linha,Coluna,FlagKing,TipoDeMov))),
  repeat,
    ((Jogador == 1, ask_for_new_piece(NovaLinha,NovaColuna));(Jogador == 2, ask_for_new_piece_pc(NovaLinha,NovaColuna))),
    ((Jogador == 1, verifiy_new_piece_player(NovaLinha,NovaColuna,BoardAtual));(Jogador == 2, verifiy_new_piece_player_pc(NovaLinha,NovaColuna,BoardAtual))),
  !,
  (
    (FlagKing == 0, ((Jogador == 1, verify_movement(Jogador,Linha,Coluna,NovaLinha,NovaColuna,TipoDeMov,BoardAtual,NovaBoard,1));(Jogador == 2, verify_movement(Jogador,Linha,Coluna,NovaLinha,NovaColuna,TipoDeMov,BoardAtual,NovaBoard,0))));
    (FlagKing == 1, ((Jogador == 1, verify_king_movement(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,NovaBoard,1));(Jogador == 2, verify_king_movement(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,NovaBoard,0))))
  ),
  flagEated(FlagEated), ((FlagEated == 0, loop_to_check_eaten(NovaBoard,Jogador,1,1,0),flagCheckEated(Reply),((Reply == 1, Jogador == 1,nl,write('!!AVISO!! E obrigado a comer'),nl,nl,!,false);(Reply == 1, Jogador == 2,!,false);true));true).

/***************** PC vs PC *******************/
% Predicado importante, semelhante ao ask_for_play do jogador humano vs jogador humano.
% Este predicado trata do jogo computador vs jogo computador
ask_for_play_pc_vs_pc(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,NovaBoard):-
  FlagEat is 0, asserta(flagEated(FlagEat)),
  ask_for_type_of_move_pc(TipoDeMov),
  repeat,
    ask_for_initial_piece_pc(Linha,Coluna),
    verify_initial_piece_player_pc(Jogador,Linha,Coluna,BoardAtual),
  !,
  verify_if_king_and_not_linear_pc(BoardAtual,Linha,Coluna,FlagKing,TipoDeMov),
  repeat,
    ask_for_new_piece_pc(NovaLinha,NovaColuna),
    verifiy_new_piece_player_pc(NovaLinha,NovaColuna,BoardAtual),
  !,
  (
    (FlagKing == 0, verify_movement(Jogador,Linha,Coluna,NovaLinha,NovaColuna,TipoDeMov,BoardAtual,NovaBoard,0));
    (FlagKing == 1, verify_king_movement(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,NovaBoard,0))
  ),
  %format('Linha=~d,Coluna=~d,NovaLinha=~d,NovaColuna=~d',[Linha,Coluna,NovaLinha,NovaColuna]),nl,
  flagEated(FlagEated), ((FlagEated == 0, loop_to_check_eaten(NovaBoard,Jogador,1,1,0),flagCheckEated(Reply),((Reply == 1, !,false);true));true).

% Devolve a linha e coluna de uma posicao random no modo jogador computador
ask_for_initial_piece_pc(Linha,Coluna):-
  repeat,
    random(1,9,Coluna),
    verify_line(Coluna),
  !,
  repeat,
    random(1,9,Linha),
    %format('ask_for_initial_piece_pc - Linha=~d,Coluna=~d',[Linha,Coluna]),nl,
    verify_line(Linha),
  !.

% Devolve a nova linha e nova coluna de uma posicao random no modo jogador computador
ask_for_new_piece_pc(NovaLinha,NovaColuna):-
  repeat,
    random(1,9,NovaColuna),
    verify_line(NovaColuna),
  !,
  repeat,
    random(1,9,NovaLinha),
    %format('ask_for_new_piece_pc - NovaLinha=~d,NovaColuna=~d',[NovaLinha,NovaColuna]),nl,
    verify_line(NovaLinha),
  !.

% Devolve o tipo de movimento no modo jogador computador, se 1 movimento simples, se 2 movimento linear
ask_for_type_of_move_pc(TipoDeMov):-
  random(1,3,TipoDeMov).

% Verifica se escolheu um movimento linear e uma dama normal não rei no modo jogador computador
% Recebe uma Board, linha e coluna, verifica essa peça se é rei, se for rei o jogador não poderá ter escolhido o TipoDeMov == 2 (Movimento linear)
% Se falhar o jogador irá repetir a jogada
verify_if_king_and_not_linear_pc(BoardAtual,Linha,Coluna,FlagKing,TipoDeMov):-
  (getElement(BoardAtual,Linha,Coluna,Peca), ((Peca == rb; Peca == rp), FlagKing = 1); FlagKing = 0), /* Verifica se escolheu um rei */
  ((FlagKing == 1, TipoDeMov == 2,!,false);true).

% Verifica se a peça inicial escolhida é sua no modo jogador computador
% Recebe um jogador as posições da peça e a board atual
% Se falhar o jogador irá repetir a jogada
verify_initial_piece_player_pc(Jogador,Linha,Coluna,BoardAtual):-
  getElement(BoardAtual,Linha,Coluna,Peca),
  ((Peca == none,!,false);
  (((Jogador == 2,(Peca == p; Peca == rp));
  (Jogador == 1,(Peca == b; Peca == rb)));
  !,false)).

% Verifica se a nova peça escolhida é sua vazia modo jogador computador
% Recebe um jogador as posições da peça e a board atual
% Se falhar o jogador irá repetir a jogada
verifiy_new_piece_player_pc(NovaLinha,NovaColuna,BoardAtual):-
  getElement(BoardAtual,NovaLinha,NovaColuna,Peca),
  ((Peca == none);false).

/* ================================ END PC OLD AND NEW PIECES RULES ================================ */

% Predicado importante que altera a board no final da jogada.
% Depois de todas as regras de jogo terem sido verificados, este predicado irá mover as peças.
% Recebe um jogador, uma board, as posicoes iniciais e as posicoes finais
% Devolve uma nova board atualizada
move(Jogador,BoardAtual,Linha,Coluna,NovaLinha,NovaColuna,BoardNova2):-
  verify_piece_to_king(Jogador,BoardAtual,Linha,Coluna,NovaLinha,PecaAntiga),
  getElement(BoardAtual,NovaLinha,NovaColuna,PecaNova),
  updateBoard(PecaAntiga,NovaLinha,NovaColuna,BoardAtual,BoardNova),
  updateBoard(PecaNova,Linha,Coluna,BoardNova,BoardNova2).


% Predicado importante que é chamado no final do joga depois do movimento
% Este predicado trata de verificar se o jogador acabou de comer uma peca, caso tenha irá verificar se pode comer mais dependendo se é dama ou rei
% Chamará os devidos predicados para verificar todas as regras dos movimentos
% Recebe um jogador, uma board, as posicoes finais do movimento anterior
% Devolve uma nova board atualizada
check_can_eat_more(Jogador,NovaBoard2,Linha,Coluna,Tipo,NovaBoard3):-
  flagEated(FlagEated),
  (
    (FlagEated == 1, asserta(backBoard(NovaBoard2)),
      (
        (getElement(NovaBoard2,Linha,Coluna,Peca), (Peca == p; Peca == b),
          (
            ((Tipo == 1; (Tipo == 2, Jogador == 1)), check_if_can_eat_more_simple(Jogador,Linha,Coluna));
            (check_if_can_eat_more_simple_pc(Jogador,Linha,Coluna))
          )
        );
        (getElement(NovaBoard2,Linha,Coluna,Peca), (Peca == rp; Peca == rb),write('vou ver rei'),nl,nl,
          (
            ((Tipo == 1; (Tipo == 2, Jogador == 1)), write('vou ver rei JOGADOR HUMANO'),nl,nl, check_if_can_eat_more_king(Jogador,Linha,Coluna));
            (write('vou ver rei JOGADOR COMPUTADOR'),nl,nl,check_if_can_eat_more_king_pc(Jogador,Linha,Coluna))
          )
        )
      ),
      backBoard(NovaBoard3),retract(backBoard(_)),retract(flagEated(_)));
    (NovaBoard3=NovaBoard2)
  ).

% Pergunta ao utilizador pela posição da peça que deseja mover
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

% Pergunta ao utilizador pela posição da nova posição para onde deseja mover a peça
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

% Pergunta ao utilizador pelo tipo de movimento
% Retorna 1 se o movimento foi Normal, 2 se o movimento for Linear
% Repete até escolher um tipo de movimento correto
ask_for_type_of_move(TipoDeMov):-
  repeat,
    write('Por favor escolha o tipo de movimento:'),nl,
    write('1- Normal'),nl,
    write('2- Linear'),nl,
    getDigit(TipoDeMov),
    ((TipoDeMov == 1; TipoDeMov == 2);nl,write('!!AVISO!! Escolha entre 1 a 2'),nl,nl,false),
  !.

% Verifica se escolheu um movimento linear e uma dama normal não rei no modo jogador humano
% Recebe uma Board, linha e coluna, verifica essa peça se é rei, se for rei o jogador não poderá ter escolhido o TipoDeMov == 2 (Movimento linear)
% Se falhar o jogador irá repetir a jogada
verify_if_king_and_not_linear(BoardAtual,Linha,Coluna,FlagKing,TipoDeMov):-
  (getElement(BoardAtual,Linha,Coluna,Peca), ((Peca == rb; Peca == rp), FlagKing = 1); FlagKing = 0), /* Verifica se escolheu um rei */
  ((FlagKing == 1, TipoDeMov == 2, nl,write('!!AVISO!! Nao pode escolher o rei para o movimento linear.'),nl,nl,!,false);true).

% Verifica se a peça inicial escolhida é sua no modo jogador humano
% Recebe um jogador as posições da peça e a board atual
% Se falhar o jogador é notificado do erro e irá repetir a jogada
verify_initial_piece_player(Jogador,Linha,Coluna,BoardAtual):-
  getElement(BoardAtual,Linha,Coluna,Peca),
  % Verifica se a casa escolhida e branca
  ((Peca == none, nl,write('!!AVISO!! Nao pode escolher uma casa vazia'),nl,nl,!,false);
  % Verifica o tipo de jogador, nao deixando escolher as pecas do outro jogador
  (((Jogador == 2,(Peca == p; Peca == rp));
  (Jogador == 1,(Peca == b; Peca == rb)));
  nl,write('!!AVISO!! Nao pode escolher uma dama do adversario'),nl,nl,!,false)).

% Verifica se a nova peça escolhida é sua vazia modo jogador humano
% Recebe um jogador as posições da peça e a board atual
% Se falhar o jogador é notificado do erro e irá repetir a jogada
verifiy_new_piece_player(NovaLinha,NovaColuna,BoardAtual):-
  getElement(BoardAtual,NovaLinha,NovaColuna,Peca),
  % Verifica se a nova peca e vazia, impossivel escolher uma do adversario ou sua
  ((Peca == none);nl,write('!!AVISO!! A sua nova posicao tem de ser vazia'),nl,nl,false).

% Predicado auxiliar do move no qual verifica se chegou a casa do adversario e muda para rei
verify_piece_to_king(Jogador,BoardAtual,Linha,Coluna,NovaLinha,PecaAntiga):-
  (
    NovaLinha \= 1, NovaLinha \= 8, getElement(BoardAtual,Linha,Coluna,PecaAntiga);
    ( /* Verifica se chegou a casa do adversario e atualiza para rei, verifica se volta para tras ao comer e nao altera o estado caso nao seja rei */
      (Jogador == 2, ((NovaLinha == 8, PecaAntiga = rp);(getElement(BoardAtual,Linha,Coluna,PecaAntiga), NovaLinha == 1, PecaAntiga == rp, true);(PecaAntiga = p)));
      (Jogador == 1, ((NovaLinha == 1, PecaAntiga = rb);(getElement(BoardAtual,Linha,Coluna,PecaAntiga), NovaLinha == 8, PecaAntiga == rb, true);(PecaAntiga = b)))
    )
  ).

% Predicado importante na jogada. Depois do jogador ter escolhido as posições das peças este predicado tratará de verificar se as posições escolhidas são válidas para o tipo de mvimento escolhido.
% Verifica se o jogador escolher o movimento simples, poderá apenas andar uma posição para a frente na vertical ou diagonal.
% Verifica se o jogador escolher o movimento simples, vai comer uma peça na vertical ou horizontal.
% Verifica se o jogador escolher o movimento simples, pode comer mais que uma peça na mesma jogada.
% Verifica se o jogador escolher o movimento linear, não pode comer e apenas pode deslocar uma posicao na diagonal ou horizontal.
% Se warnings == 0, não será mostrado na consola os erros da jogada (Evita sobrecarga no modo jogador computador), se warnings == 1, mostra os erros na jogada do jogador humano.
% Caso falhe o jogador pode repetir a jogada
% Recebe um jogador, as posições das peças, o tipo de movimento, a board atual e uma flag warnings.
% Devolve uma nova board para o caso de serem comidas peças nesse movimento.
verify_movement(Jogador,Linha,Coluna,NovaLinha,NovaColuna,TipoDeMov,BoardAtual,NovaBoard,Warnings):-
  (TipoDeMov == 1,
  /* Se Aux 1, movimento simples para nao comer, se Aux1 = 0 ou 2, vai comer, movimento com dif. de duas casas */
  (
    (
      (Jogador == 1, Aux1 is (Linha - NovaLinha));
      (Jogador == 2, Aux1 is (NovaLinha - Linha))
    ), AuxColuna is (Coluna - NovaColuna), AuxColuna2 is abs(AuxColuna),
    (
      ((Aux1 == 0; Aux1 == 2; Aux1 == -2), AuxColuna2 \= 1, eat_piece_simple(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,NovaBoard));
      (Aux1 == 1, (Aux2 is (Coluna - NovaColuna), Aux22 is abs(Aux2), Aux22 >= 0, Aux22 =< 1),NovaBoard=BoardAtual)
    )
  )
  );
  (TipoDeMov == 2,
    (
      (Jogador == 1, Linha > NovaLinha, DeltaLinha is (Linha - NovaLinha));
      (Jogador == 2, NovaLinha > Linha, DeltaLinha is (NovaLinha - Linha))
    ), AuxColuna is (Coluna - NovaColuna), DeltaColuna is abs(AuxColuna),
    (
      ((Aux is abs(DeltaLinha), Aux < 2; DeltaColuna == 1), ((Warnings == 1, nl,write('!!AVISO!! Movimento linear errado'),nl,nl,!,false);!,false));
      (AuxDeltaLinha is DeltaLinha - 1, (DeltaColuna == 0, AuxDeltaColuna is 0; AuxDeltaColuna is DeltaColuna -1), check_if_is_pieces(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,AuxDeltaLinha, AuxDeltaColuna),NovaBoard = BoardAtual)
    )
  );
  ((Warnings == 1, nl,write('!!AVISO!! Efetuou um moviento invalido'),nl,nl,false);false).

% Predicado auxiliar importante do verify_movement no caso do movimento for linear. Verifica se este movimento é valido ou nao
% Verifica se as pecas entre as posicoes iniciais e finais são do jogador em questão e não reis, Verifica se o movimento é realizado na diagonal ou horizontal.
% Recebe um jogador, as posições das peças, a board atual e os deltas que indicam a diferença de casas das pecas escolhidas
% Caso falhe o jogador repetirá a jogada
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

% Predicado auxiliar importante do verify_movement. Tratará de verificar se o jogador pode ou não comer a peça do adversário (Rei ou dama)
% Recebe um jogador, as posições das peças, a board atual e retorna uma nova board com as peças atualizadas
% Atualiza na base de dados interna do prolog o predicado flagEated, importante para depois no final da jogada verificar se comeu alguma peça ou não nesta jogada.
% Caso falhe o jogador repetirá a jogada
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
        ((NovaLinha > Linha, Y is Linha + 1); (NovaLinha < Linha, Y is Linha - 1)),
        getElement(BoardAtual,Y,X,Peca),
        (Peca\=none, ((Jogador == 1, (Peca == p; Peca == rp));((Jogador == 2, (Peca == b; Peca == rb))))),
        updateBoard(none,Y,X,BoardAtual,NovaBoard),retract(flagEated(_)), FlagEat is 1, asserta(flagEated(FlagEat))
      );
      /* Comer simples horizontal */
      (AuxLinha2 == 0, AuxColuna2 == 2, Y is Linha,
        ((NovaColuna > Coluna, X is Coluna + 1); (NovaColuna < Coluna,X is Coluna - 1)),
        getElement(BoardAtual,Y,X,Peca),
        (Peca\=none, ((Jogador == 1, (Peca == p; Peca == rp));((Jogador == 2, (Peca == b; Peca == rb))))),
        updateBoard(none,Y,X,BoardAtual,NovaBoard),retract(flagEated(_)), FlagEat is 1, asserta(flagEated(FlagEat))
      )
    )
  )
  );false.

% Predicado importante, chamado caso o jogador escolha uma peça rei. Trata de verificar se o movimento da peça rei foi realizado com sucesso e se irá comer peças.
% Recebe um jogador, as posições das peças, a board atual e uma flag warnings. Retorna uma nova board com as peças atualizadas
% Se warnings == 0, não será mostrado na consola os erros da jogada (Evita sobrecarga no modo jogador computador), se warnings == 1, mostra os erros na jogada do jogador humano.
% Caso falhe o jogador repetirá a jogada
verify_king_movement(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,NovaBoard,Warnings):-
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
      /* Diagonal - TypeMove = 3 PLAYER CANT EAT HERE!!!*/
      ((AuxLinhaABS > 0, AuxColunaABS > 0, AuxLinhaABS == AuxColunaABS), Delta is AuxColunaABS,check_king_eating(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,Delta,3,0), king_eat(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,NovaBoard,Delta,3))
    )
  );
  ((Warnings == 1, nl,write('!!AVISO!! Movimento invalido do rei'),nl,nl,false); false).

% Predicado chamado dentro do verify_king_movement, que apenas verifica se o jogador irá comer alguma peça pelo caminho.
% Verifica se entre as posicoes escolhidas não tem nenhuma peça sua nem mais que uma peça do adversário, pois so pode comer uma de cada vez.
% Verifica se o jogador pode comer na vertical e horizontal, nunca na diagonal.
% Verifica se econtra uma peça do jogador adversário entre as posicoes escolhidas, e caso encontre, a peça a seguir ao do adversário terá de ser vazia.
% Recebe um jogador, as posições das peças, a board atual, uma flag Delta(diferença de peças movidas), uma flag TypeMove(1 movimento horizontal, 2 movimento vertical, 3 movimento diagonal)
% Recebe tambem um contador que é inicializado a 0 que conta o numero de peças do adversario encontradas nesse movimento, se duas ou mais falha.
% Caso falhe o jogador repetirá a jogada
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
        (Peca == none, !,check_king_eating(Jogador,Y,X,NovaLinha,NovaColuna,BoardAtual,AuxDelta,3,Conta))
      )
    )
  ).

% Predicado chamado dentro do verify_king_movement, semelhante ao check_king_eating, que tratará de atualizar a board caso o predicado check_king_eating suceda.
% Recebe um jogador, as posições das peças, a board atual, uma flag Delta(diferença de peças movidas), uma flag TypeMove(1 movimento horizontal, 2 movimento vertical, 3 movimento diagonal)
king_eat(_,_,_,_,_,BoardAtual,NovaBoard,0,_):-NovaBoard = BoardAtual, true.
king_eat(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,NovaBoard,Delta,TypeMove):-
  (
    (TypeMove == 1, X is Coluna, (NovaLinha > Linha, Y is Linha + 1; Y is Linha - 1), getElement(BoardAtual,Y,X,Peca), AuxDelta is Delta -1,
      (
        (Peca == none, !,king_eat(Jogador,Y,X,NovaLinha,NovaColuna,BoardAtual,NovaBoard,AuxDelta,1));
        (Jogador == 1, (Peca == p; Peca == rp), updateBoard(none,Y,X,BoardAtual,NovaBoard),retract(flagEated(_)), FlagEat is 1, asserta(flagEated(FlagEat)), !,king_eat(1,Y,X,NovaLinha,NovaColuna,NovaBoard,_,AuxDelta,1));
        (Jogador == 2, (Peca == b; Peca == rb), updateBoard(none,Y,X,BoardAtual,NovaBoard),retract(flagEated(_)), FlagEat is 1, asserta(flagEated(FlagEat)), !,king_eat(2,Y,X,NovaLinha,NovaColuna,NovaBoard,_,AuxDelta,1))
      )
    );
    (TypeMove == 2, Y is Linha, (NovaColuna > Coluna, X is Coluna + 1; X is Coluna - 1), getElement(BoardAtual,Y,X,Peca), AuxDelta is Delta -1,
      (
        (Peca == none, !,king_eat(Jogador,Y,X,NovaLinha,NovaColuna,BoardAtual,NovaBoard,AuxDelta,2));
        (Jogador == 1, (Peca == p; Peca == rp), updateBoard(none,Y,X,BoardAtual,NovaBoard),retract(flagEated(_)), FlagEat is 1, asserta(flagEated(FlagEat)), !,king_eat(1,Y,X,NovaLinha,NovaColuna,NovaBoard,_,AuxDelta,2));
        (Jogador == 2, (Peca == b; Peca == rb), updateBoard(none,Y,X,BoardAtual,NovaBoard),retract(flagEated(_)), FlagEat is 1, asserta(flagEated(FlagEat)), !,king_eat(2,Y,X,NovaLinha,NovaColuna,NovaBoard,_,AuxDelta,2))
      )
    );
    (TypeMove == 3, (NovaLinha > Linha, Y is Linha + 1; Y is Linha - 1), (NovaColuna > Coluna, X is Coluna + 1; X is Coluna - 1), getElement(BoardAtual,Y,X,Peca), AuxDelta is Delta -1,
      (
        (Peca == none, !,king_eat(Jogador,Y,X,NovaLinha,NovaColuna,BoardAtual,NovaBoard,AuxDelta,3))
      )
    )
  ).

% Predicado importante chamado no final do movimento caso o jogador não tenha comido nenhuma peça.
% Precorrerá o tabuleiro a procura das peças do jogador em questão se for dama normal chamara o predicado check_hor_ver_normal_neighbours, se for rei chamara check_hor_ver_king_neighbours
% Essas chamadas verificarão se existem peças dos adversários que podem ser comidas ou não.
% Recebe a board atual, o jogador, X e Y inicializados a 1 para a varredura.
% Retorna a resposta em Reply
% O loop acaba quando encontrar uma peça do adversário que seja possível comer ou caso varra as posições todas e não encontre nenhuma
loop_to_check_eaten(_,_,_,_,1):-true.
loop_to_check_eaten(_,_,8,8,0):-true.
loop_to_check_eaten(Board,Jogador,X,Y,Reply):-
  (
    (getElement(Board,Y,X,Peca), ((Jogador == 1, (Peca == b;Peca == rb));(Jogador == 2, (Peca == p;Peca == rp))),
      Flag is 0, asserta(flagCheckEated(Flag)),
      (
        (Peca == p; Peca == b),check_hor_ver_normal_neighbours(Board,Jogador,X,Y);
        (Peca == rp; Peca == rb),check_hor_ver_king_neighbours(Board,Jogador,X,Y)
      ),flagCheckEated(Reply2),(X < 8, AuxX is X + 1, AuxY is Y;Y < 8, AuxY is Y + 1, AuxX is 1), !,loop_to_check_eaten(Board,Jogador,AuxX,AuxY,Reply2));
    ((X < 8, AuxX is X + 1, AuxY is Y;Y < 8, AuxY is Y + 1, AuxX is 1),!,loop_to_check_eaten(Board,Jogador,AuxX,AuxY,Reply))
  ).

% Predicado auxiliar de loop_to_check_eaten, que verificará se na adjacencia de X e Y existe uma peça do adversário que seja possível come-la e alterará a flag presente do predicado flagCheckEated.
check_hor_ver_normal_neighbours(Board,Jogador,X,Y):-
  (
    (AuxY is Y - 1, getElement(Board,AuxY,X,Peca), ((Jogador == 1,(Peca == p; Peca == rp));(Jogador == 2,(Peca == b; Peca == rb))), AuxY2 is Y - 2, getElement(Board,AuxY2,X,Peca2), Peca2 == none, retract(flagCheckEated(_)), FlagEat is 1, asserta(flagCheckEated(FlagEat)),true);
    (AuxX is X + 1, getElement(Board,Y,AuxX,Peca), ((Jogador == 1,(Peca == p; Peca == rp));(Jogador == 2,(Peca == b; Peca == rb))), AuxX2 is X + 2, getElement(Board,Y,AuxX2,Peca2), Peca2 == none, retract(flagCheckEated(_)), FlagEat is 1, asserta(flagCheckEated(FlagEat)),true);
    (AuxY is Y + 1, getElement(Board,AuxY,X,Peca), ((Jogador == 1,(Peca == p; Peca == rp));(Jogador == 2,(Peca == b; Peca == rb))), AuxY2 is Y + 2, getElement(Board,AuxY2,X,Peca2), Peca2 == none, retract(flagCheckEated(_)), FlagEat is 1, asserta(flagCheckEated(FlagEat)),true);
    (AuxX is X - 1, getElement(Board,Y,AuxX,Peca), ((Jogador == 1,(Peca == p; Peca == rp));(Jogador == 2,(Peca == b; Peca == rb))), AuxX2 is X - 2, getElement(Board,Y,AuxX2,Peca2), Peca2 == none, retract(flagCheckEated(_)), FlagEat is 1, asserta(flagCheckEated(FlagEat)),true)
  ).

% Predicado auxiliar de loop_to_check_eaten, que verificará se nas posicções verticais ou horizontais de X e Y recorrendo o predicado loop_aux_king, existe uma peça do adversário que seja possível come-la e alterará a flag presente do predicado flagCheckEated.
check_hor_ver_king_neighbours(Board,Jogador,X,Y):-
  (
    (loop_aux_king(Board,Jogador,X,Y,0,1), flagKingEating(AuxY2), AuxY is AuxY2 - 1, retract(flagKingEating(_)), getElement(Board,AuxY,X,Peca2), Peca2 == none, retract(flagCheckEated(_)), FlagEat is 1, asserta(flagCheckEated(FlagEat)),true);
    (loop_aux_king(Board,Jogador,X,Y,0,2), flagKingEating(AuxX2), AuxX is AuxX2 + 1, retract(flagKingEating(_)), getElement(Board,Y,AuxX,Peca2), Peca2 == none, retract(flagCheckEated(_)), FlagEat is 1, asserta(flagCheckEated(FlagEat)),true);
    (loop_aux_king(Board,Jogador,X,Y,0,3), flagKingEating(AuxY2), AuxY is AuxY2 + 1, retract(flagKingEating(_)), getElement(Board,AuxY,X,Peca2), Peca2 == none, retract(flagCheckEated(_)), FlagEat is 1, asserta(flagCheckEated(FlagEat)),true);
    (loop_aux_king(Board,Jogador,X,Y,0,4), flagKingEating(AuxX2), AuxX is AuxX2 - 1, retract(flagKingEating(_)), getElement(Board,Y,AuxX,Peca2), Peca2 == none, retract(flagCheckEated(_)), FlagEat is 1, asserta(flagCheckEated(FlagEat)),true)
  ).

% Predicado auxiliar de check_hor_ver_king_neighbours que irá varrer a posicao X e Y na vertical ou horizontal a procura de uma peça do adversário, retornando a posicao coluna ou linha desta peça no predicado flagKingEating dependendo do tipo.
loop_aux_king(_,_,_,_,0,_):-false.
loop_aux_king(_,_,_,_,1,_):-true.
loop_aux_king(Board,Jogador,X,Y,_,Tipo):-
  (
    (
      (Tipo == 1, (Y > 0, AuxY is Y - 1, AuxX is X,asserta(flagKingEating(AuxY))));
      (Tipo == 2, (X < 8, AuxY is Y, AuxX is X + 1,asserta(flagKingEating(AuxX))));
      (Tipo == 3, (Y < 8, AuxY is Y + 1, AuxX is X,asserta(flagKingEating(AuxY))));
      (Tipo == 4, (X > 0, AuxY is Y, AuxX is X - 1,asserta(flagKingEating(AuxX))))
    ), getElement(Board,AuxY,AuxX,Peca), (((Jogador == 1,(Peca == p; Peca == rp));(Jogador == 2,(Peca == b; Peca == rb))),loop_aux_king(Board,Jogador,AuxX,AuxY,1,Tipo);loop_aux_king(Board,Jogador,AuxX,AuxY,0,Tipo))
  ).

check_if_can_eat_more_king(Jogador,Linha,Coluna):-
  (/*Para cima*/
    backBoard(BoardAtual), !,loop_aux_king(BoardAtual,Jogador,Coluna,Linha,0,1),
    flagKingEating(AuxY2), AuxLinha is AuxY2 - 1, retract(flagKingEating(_)), getElement(BoardAtual,AuxLinha,Coluna,Peca), Peca == none, display_board(BoardAtual), write('CIMA - Tera de comer a peca do advesario'),nl,
    repeat,
      ask_for_new_piece(NovaLinha,NovaColuna),
      verifiy_new_piece_player(NovaLinha,NovaColuna,BoardAtual),
      verify_king_movement_for_eat_more(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,NovaBoard2,1),
    !,
    move(Jogador,NovaBoard2,Linha,Coluna,NovaLinha,NovaColuna,NovaBoard), retract(backBoard(_)), asserta(backBoard(NovaBoard)),!,check_if_can_eat_more_king(Jogador,NovaLinha,NovaColuna)
  );
  (/*Para Baixo*/
    backBoard(BoardAtual), !,loop_aux_king(BoardAtual,Jogador,Coluna,Linha,0,3),
    flagKingEating(AuxY2), AuxLinha is AuxY2 + 1, retract(flagKingEating(_)), getElement(BoardAtual,AuxLinha,Coluna,Peca), Peca == none, display_board(BoardAtual), write('BAIXO - Tera de comer a peca do advesario'),nl,
    repeat,
      ask_for_new_piece(NovaLinha,NovaColuna),
      verifiy_new_piece_player(NovaLinha,NovaColuna,BoardAtual),
      verify_king_movement_for_eat_more(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,NovaBoard2,1),
    !,
    move(Jogador,NovaBoard2,Linha,Coluna,NovaLinha,NovaColuna,NovaBoard), retract(backBoard(_)), asserta(backBoard(NovaBoard)),!,check_if_can_eat_more_king(Jogador,NovaLinha,NovaColuna)
  );
  (/*Para Direita*/
    backBoard(BoardAtual), !,loop_aux_king(BoardAtual,Jogador,Coluna,Linha,0,2),
    flagKingEating(AuxX2), AuxColuna is AuxX2 + 1, retract(flagKingEating(_)), getElement(BoardAtual,Linha,AuxColuna,Peca), format('Linha=~d, AuxColuna=~d',[Linha,AuxColuna]),nl,Peca == none, display_board(BoardAtual), write('DIREITA - Tera de comer a peca do advesario'),nl,
    repeat,
      ask_for_new_piece(NovaLinha,NovaColuna),
      verifiy_new_piece_player(NovaLinha,NovaColuna,BoardAtual),
      verify_king_movement_for_eat_more(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,NovaBoard2,1),
    !,
    move(Jogador,NovaBoard2,Linha,Coluna,NovaLinha,NovaColuna,NovaBoard), retract(backBoard(_)), asserta(backBoard(NovaBoard)),!,check_if_can_eat_more_king(Jogador,NovaLinha,NovaColuna)
  );
  (/*Para Esquerda*/
    backBoard(BoardAtual), !,loop_aux_king(BoardAtual,Jogador,Coluna,Linha,0,4),
    flagKingEating(AuxX2), AuxColuna is AuxX2 - 1, retract(flagKingEating(_)), getElement(BoardAtual,Linha,AuxColuna,Peca), Peca == none, display_board(BoardAtual), write('ESQUERDA - Tera de comer a peca do advesario'),nl,
    repeat,
      ask_for_new_piece(NovaLinha,NovaColuna),
      verifiy_new_piece_player(NovaLinha,NovaColuna,BoardAtual),
      verify_king_movement_for_eat_more(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,NovaBoard2,1),
    !,
    move(Jogador,NovaBoard2,Linha,Coluna,NovaLinha,NovaColuna,NovaBoard), retract(backBoard(_)), asserta(backBoard(NovaBoard)),!,check_if_can_eat_more_king(Jogador,NovaLinha,NovaColuna)
  );true.

check_if_can_eat_more_king_pc(Jogador,Linha,Coluna):-
  format('check_if_can_eat_more_king_pc Jogador=~d,Linha=~d,Coluna=~d',[Jogador,Linha,Coluna]),nl,
  (/*Para cima*/
    backBoard(BoardAtual), write('ANTES DO loop_aux_king'),nl, loop_aux_king(BoardAtual,Jogador,Coluna,Linha,0,1), write('DEPOIS DO loop_aux_king'),nl,
    flagKingEating(AuxY2), AuxLinha is AuxY2 - 1, retract(flagKingEating(_)), getElement(BoardAtual,AuxLinha,Coluna,Peca), Peca == none,
    repeat,
      ask_for_new_piece_pc(NovaLinha,NovaColuna),
      verifiy_new_piece_player_pc(NovaLinha,NovaColuna,BoardAtual),
      format('PARA CIMA - Jogador=~d,Linha=~d,Coluna=~d,NovaLinha=~d,NovaColuna=~d',[Jogador,Linha,Coluna,NovaLinha,NovaColuna]),nl,
      verify_king_movement_for_eat_more(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,NovaBoard2,0),
    !,
    info_jogada_pc(1,Jogador,Linha,Coluna,NovaLinha,NovaColuna),
    move(Jogador,NovaBoard2,Linha,Coluna,NovaLinha,NovaColuna,NovaBoard), retract(backBoard(_)), asserta(backBoard(NovaBoard)),!,check_if_can_eat_more_king_pc(Jogador,NovaLinha,NovaColuna)
  );
  (/*Para Baixo*/
    backBoard(BoardAtual), write('ANTES DO loop_aux_king'),nl, loop_aux_king(BoardAtual,Jogador,Coluna,Linha,0,3), write('DEPOIS DO loop_aux_king'),nl,
    flagKingEating(AuxY2), AuxLinha is AuxY2 + 1, retract(flagKingEating(_)), getElement(BoardAtual,AuxLinha,Coluna,Peca), Peca == none,
    repeat,
      ask_for_new_piece_pc(NovaLinha,NovaColuna),
      verifiy_new_piece_player_pc(NovaLinha,NovaColuna,BoardAtual),
      format('PARA BAIXO - Jogador=~d,Linha=~d,Coluna=~d,NovaLinha=~d,NovaColuna=~d',[Jogador,Linha,Coluna,NovaLinha,NovaColuna]),nl,
      verify_king_movement_for_eat_more(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,NovaBoard2,0),
    !,
    info_jogada_pc(1,Jogador,Linha,Coluna,NovaLinha,NovaColuna),
    move(Jogador,NovaBoard2,Linha,Coluna,NovaLinha,NovaColuna,NovaBoard), retract(backBoard(_)), asserta(backBoard(NovaBoard)),!,check_if_can_eat_more_king_pc(Jogador,NovaLinha,NovaColuna)
  );
  (/*Para Direita*/
    backBoard(BoardAtual), write('ANTES DO loop_aux_king'),nl, loop_aux_king(BoardAtual,Jogador,Coluna,Linha,0,2), write('DEPOIS DO loop_aux_king'),nl,
    flagKingEating(AuxX2), AuxColuna is AuxX2 + 1, retract(flagKingEating(_)), getElement(BoardAtual,Linha,AuxColuna,Peca), Peca == none,
    repeat,
      ask_for_new_piece_pc(NovaLinha,NovaColuna),
      verifiy_new_piece_player_pc(NovaLinha,NovaColuna,BoardAtual),
      format('PARA DIREITA - Jogador=~d,Linha=~d,Coluna=~d,NovaLinha=~d,NovaColuna=~d',[Jogador,Linha,Coluna,NovaLinha,NovaColuna]),nl,
      verify_king_movement_for_eat_more(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,NovaBoard2,0),
    !,
    info_jogada_pc(1,Jogador,Linha,Coluna,NovaLinha,NovaColuna),
    move(Jogador,NovaBoard2,Linha,Coluna,NovaLinha,NovaColuna,NovaBoard), retract(backBoard(_)), asserta(backBoard(NovaBoard)),!,check_if_can_eat_more_king_pc(Jogador,NovaLinha,NovaColuna)
  );
  (/*Para Esquerda*/
    backBoard(BoardAtual), write('ANTES DO loop_aux_king'),nl, loop_aux_king(BoardAtual,Jogador,Coluna,Linha,0,4), write('DEPOIS DO loop_aux_king'),nl,
    flagKingEating(AuxX2), AuxColuna is AuxX2 - 1, retract(flagKingEating(_)), getElement(BoardAtual,Linha,AuxColuna,Peca), Peca == none,
    repeat,
      ask_for_new_piece_pc(NovaLinha,NovaColuna),
      verifiy_new_piece_player_pc(NovaLinha,NovaColuna,BoardAtual),
      format('PARA ESQUERDA - Jogador=~d,Linha=~d,Coluna=~d,NovaLinha=~d,NovaColuna=~d',[Jogador,Linha,Coluna,NovaLinha,NovaColuna]),nl,
      verify_king_movement_for_eat_more(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,NovaBoard2,0),
    !,
    info_jogada_pc(1,Jogador,Linha,Coluna,NovaLinha,NovaColuna),
    move(Jogador,NovaBoard2,Linha,Coluna,NovaLinha,NovaColuna,NovaBoard), retract(backBoard(_)), asserta(backBoard(NovaBoard)),!,check_if_can_eat_more_king_pc(Jogador,NovaLinha,NovaColuna)
  );true.

verify_king_movement_for_eat_more(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,NovaBoard,Warnings):-
  (
  AuxLinha is (Linha - NovaLinha),
  AuxLinhaABS is abs(AuxLinha),
  AuxColuna is (Coluna - NovaColuna),
  AuxColunaABS is abs(AuxColuna),
    (
      /* Horizontal - TypeMove = 1 */
      ((AuxLinhaABS > 0, AuxColunaABS == 0), Delta is AuxLinhaABS, check_king_eating_for_eat_more(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,Delta,1,0), king_eat(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,NovaBoard,Delta,1));
      /* Vertical - TypeMove = 2 */
      ((AuxLinhaABS == 0, AuxColunaABS > 0), Delta is AuxColunaABS, check_king_eating_for_eat_more(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,Delta,2,0), king_eat(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,NovaBoard,Delta,2))
    )
  );
  ((Warnings == 1, nl,write('!!AVISO!! Tera de comer a peca do adversario'),nl,nl,!,false); !,false).


check_king_eating_for_eat_more(_,_,_,_,_,_,0,_,1):-true.
check_king_eating_for_eat_more(_,_,_,_,_,_,0,_,0):-!,false.
check_king_eating_for_eat_more(_,_,_,_,_,_,_,_,2):-!,false.
check_king_eating_for_eat_more(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,Delta,TypeMove,Conta):-
  (
    (TypeMove == 1, X is Coluna, ((NovaLinha > Linha, Y is Linha + 1); (NovaLinha < Linha, Y is Linha - 1)), getElement(BoardAtual,Y,X,Peca), AuxDelta is Delta -1,
      (
        (Peca == none, !,check_king_eating_for_eat_more(Jogador,Y,X,NovaLinha,NovaColuna,BoardAtual,AuxDelta,1,Conta));
        (Jogador == 1, (((Peca == b; Peca == rb), !,false);(Peca == p; Peca == rp), AuxConta is Conta + 1, !,check_king_eating_for_eat_more(1,Y,X,NovaLinha,NovaColuna,BoardAtual,AuxDelta,1,AuxConta)));
        (Jogador == 2, (((Peca == p; Peca == rp), !,false);(Peca == b; Peca == rb), AuxConta is Conta + 1, !,check_king_eating_for_eat_more(2,Y,X,NovaLinha,NovaColuna,BoardAtual,AuxDelta,1,AuxConta)))
      )
    );
    (TypeMove == 2, Y is Linha, ((NovaColuna > Coluna, X is Coluna + 1); (NovaColuna < Coluna,X is Coluna - 1)), getElement(BoardAtual,Y,X,Peca), AuxDelta is Delta -1,
      (
        (Peca == none, !,check_king_eating_for_eat_more(Jogador,Y,X,NovaLinha,NovaColuna,BoardAtual,AuxDelta,2,Conta));
        (Jogador == 1, (((Peca == b; Peca == rb), !,false);(Peca == p; Peca == rp), AuxConta is Conta + 1, !,check_king_eating_for_eat_more(1,Y,X,NovaLinha,NovaColuna,BoardAtual,AuxDelta,2,AuxConta)));
        (Jogador == 2, (((Peca == p; Peca == rp), !,false);(Peca == b; Peca == rb), AuxConta is Conta + 1, !,check_king_eating_for_eat_more(2,Y,X,NovaLinha,NovaColuna,BoardAtual,AuxDelta,2,AuxConta)))
      )
    )
  ).

% Predicado importante do joga. No final do predicado joga, caso o jogador tenha comido, vai verificar se pode comer mais peças.
% Apos o jogador ter escolhido a nova posicao, este predicado vai verificar nas suas adjacências (Vertical e horizontal) se a peça é do adversario e caso seja do adversario verifica se a peça a seguir a essa é vazia, podendo assim comer essa peça do adversario
check_if_can_eat_more_simple_pc(Jogador,Linha,Coluna):-
  (/*Para cima*/
    backBoard(BoardAtual),
    AuxLinha is (Linha-1), getElement(BoardAtual,AuxLinha,Coluna,Peca), ((Jogador == 1, (Peca == p; Peca == rp));(Jogador == 2, (Peca == b; Peca == rb))),
    AuxLinha2 is (Linha-2), getElement(BoardAtual,AuxLinha2,Coluna,Peca2), Peca2 == none,
    repeat,
      ask_for_new_piece_pc(NovaLinha,NovaColuna),
      verifiy_new_piece_player_pc(NovaLinha,NovaColuna,BoardAtual),
      verify_movement_for_eat_more(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,NovaBoard2,0),
    !,
    info_jogada_pc(1,Jogador,Linha,Coluna,NovaLinha,NovaColuna),
    move(Jogador,NovaBoard2,Linha,Coluna,NovaLinha,NovaColuna,NovaBoard), retract(backBoard(_)), asserta(backBoard(NovaBoard)),!,check_if_can_eat_more_simple_pc(Jogador,NovaLinha,NovaColuna)
  );
  (/*Para Baixo*/
    backBoard(BoardAtual),
    AuxLinha is (Linha+1), getElement(BoardAtual,AuxLinha,Coluna,Peca), ((Jogador == 1, (Peca == p; Peca == rp));(Jogador == 2, (Peca == b; Peca == rb))),
    AuxLinha2 is (Linha+2), getElement(BoardAtual,AuxLinha2,Coluna,Peca2), Peca2 == none,
    repeat,
      ask_for_new_piece_pc(NovaLinha,NovaColuna),
      verifiy_new_piece_player_pc(NovaLinha,NovaColuna,BoardAtual),
      verify_movement_for_eat_more(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,NovaBoard2,0),
    !,
    info_jogada_pc(1,Jogador,Linha,Coluna,NovaLinha,NovaColuna),
    move(Jogador,NovaBoard2,Linha,Coluna,NovaLinha,NovaColuna,NovaBoard), retract(backBoard(_)), asserta(backBoard(NovaBoard)),!,check_if_can_eat_more_simple_pc(Jogador,NovaLinha,NovaColuna)
  );
  (/*Para Direita*/
    backBoard(BoardAtual),
    AuxColuna is (Coluna+1), getElement(BoardAtual,Linha,AuxColuna,Peca), ((Jogador == 1, (Peca == p; Peca == rp));(Jogador == 2, (Peca == b; Peca == rb))),
    AuxColuna2 is (Coluna+2), getElement(BoardAtual,Linha,AuxColuna2,Peca2), Peca2 == none,
    repeat,
      ask_for_new_piece_pc(NovaLinha,NovaColuna),
      verifiy_new_piece_player_pc(NovaLinha,NovaColuna,BoardAtual),
      verify_movement_for_eat_more(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,NovaBoard2,0),
    !,
    info_jogada_pc(1,Jogador,Linha,Coluna,NovaLinha,NovaColuna),
    move(Jogador,NovaBoard2,Linha,Coluna,NovaLinha,NovaColuna,NovaBoard), retract(backBoard(_)), asserta(backBoard(NovaBoard)),!,check_if_can_eat_more_simple_pc(Jogador,NovaLinha,NovaColuna)
  );
  (/*Para Esquerda*/
    backBoard(BoardAtual),
    AuxColuna is (Coluna-1), getElement(BoardAtual,Linha,AuxColuna,Peca), ((Jogador == 1, (Peca == p; Peca == rp));(Jogador == 2, (Peca == b; Peca == rb))),
    AuxColuna2 is (Coluna-2), getElement(BoardAtual,Linha,AuxColuna2,Peca2), Peca2 == none,
    repeat,
      ask_for_new_piece_pc(NovaLinha,NovaColuna),
      verifiy_new_piece_player_pc(NovaLinha,NovaColuna,BoardAtual),
      verify_movement_for_eat_more(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,NovaBoard2,0),
    !,
    info_jogada_pc(1,Jogador,Linha,Coluna,NovaLinha,NovaColuna),
    move(Jogador,NovaBoard2,Linha,Coluna,NovaLinha,NovaColuna,NovaBoard), retract(backBoard(_)), asserta(backBoard(NovaBoard)),!,check_if_can_eat_more_simple_pc(Jogador,NovaLinha,NovaColuna)
  );true.

% Predicado importante do joga. No final do predicado joga, caso o jogador tenha comido, vai verificar se pode comer mais peças.
% Apos o jogador ter escolhido a nova posicao, este predicado vai verificar nas suas adjacências (Vertical e horizontal) se a peça é do adversario e caso seja do adversario verifica se a peça a seguir a essa é vazia, podendo assim comer essa peça do adversario
check_if_can_eat_more_simple(Jogador,Linha,Coluna):-
  (/*Para cima*/
    backBoard(BoardAtual),
    AuxLinha is (Linha-1), getElement(BoardAtual,AuxLinha,Coluna,Peca), ((Jogador == 1, (Peca == p; Peca == rp));(Jogador == 2, (Peca == b; Peca == rb))),
    AuxLinha2 is (Linha-2), getElement(BoardAtual,AuxLinha2,Coluna,Peca2), Peca2 == none, display_board(BoardAtual), write('Tera de comer a peca do advesario'),nl,
    repeat,
      ask_for_new_piece(NovaLinha,NovaColuna),
      verifiy_new_piece_player(NovaLinha,NovaColuna,BoardAtual),
      verify_movement_for_eat_more(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,NovaBoard2,1),
    !,
    move(Jogador,NovaBoard2,Linha,Coluna,NovaLinha,NovaColuna,NovaBoard), retract(backBoard(_)), asserta(backBoard(NovaBoard)),!,check_if_can_eat_more_simple(Jogador,NovaLinha,NovaColuna)
  );
  (/*Para Baixo*/
    backBoard(BoardAtual),
    AuxLinha is (Linha+1), getElement(BoardAtual,AuxLinha,Coluna,Peca), ((Jogador == 1, (Peca == p; Peca == rp));(Jogador == 2, (Peca == b; Peca == rb))),
    AuxLinha2 is (Linha+2), getElement(BoardAtual,AuxLinha2,Coluna,Peca2), Peca2 == none, display_board(BoardAtual), write('Tera de comer a peca do advesario'),nl,
    repeat,
      ask_for_new_piece(NovaLinha,NovaColuna),
      verifiy_new_piece_player(NovaLinha,NovaColuna,BoardAtual),
      verify_movement_for_eat_more(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,NovaBoard2,1),
    !,
    move(Jogador,NovaBoard2,Linha,Coluna,NovaLinha,NovaColuna,NovaBoard), retract(backBoard(_)), asserta(backBoard(NovaBoard)),!,check_if_can_eat_more_simple(Jogador,NovaLinha,NovaColuna)
  );
  (/*Para Direita*/
    backBoard(BoardAtual),
    AuxColuna is (Coluna+1), getElement(BoardAtual,Linha,AuxColuna,Peca), ((Jogador == 1, (Peca == p; Peca == rp));(Jogador == 2, (Peca == b; Peca == rb))),
    AuxColuna2 is (Coluna+2), getElement(BoardAtual,Linha,AuxColuna2,Peca2), Peca2 == none, display_board(BoardAtual), write('Tera de comer a peca do advesario'),nl,
    repeat,
      ask_for_new_piece(NovaLinha,NovaColuna),
      verifiy_new_piece_player(NovaLinha,NovaColuna,BoardAtual),
      verify_movement_for_eat_more(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,NovaBoard2,1),
    !,
    move(Jogador,NovaBoard2,Linha,Coluna,NovaLinha,NovaColuna,NovaBoard), retract(backBoard(_)), asserta(backBoard(NovaBoard)),!,check_if_can_eat_more_simple(Jogador,NovaLinha,NovaColuna)
  );
  (/*Para Esquerda*/
    backBoard(BoardAtual),
    AuxColuna is (Coluna-1), getElement(BoardAtual,Linha,AuxColuna,Peca), ((Jogador == 1, (Peca == p; Peca == rp));(Jogador == 2, (Peca == b; Peca == rb))),
    AuxColuna2 is (Coluna-2), getElement(BoardAtual,Linha,AuxColuna2,Peca2), Peca2 == none, display_board(BoardAtual), write('Tera de comer a peca do advesario'),nl,
    repeat,
      ask_for_new_piece(NovaLinha,NovaColuna),
      verifiy_new_piece_player(NovaLinha,NovaColuna,BoardAtual),
      verify_movement_for_eat_more(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,NovaBoard2,1),
    !,
    move(Jogador,NovaBoard2,Linha,Coluna,NovaLinha,NovaColuna,NovaBoard), retract(backBoard(_)), asserta(backBoard(NovaBoard)),!,check_if_can_eat_more_simple(Jogador,NovaLinha,NovaColuna)
  );true.

verify_movement_for_eat_more(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,NovaBoard,Warnings):-
  (
    (
      (Jogador == 1, Aux1 is (Linha - NovaLinha));
      (Jogador == 2, Aux1 is (NovaLinha - Linha))
    ), AuxColuna is (Coluna - NovaColuna), AuxColuna2 is abs(AuxColuna),
    ((Aux1 == 0; Aux1 == 2; Aux1 == -2), AuxColuna2 \= 1, eat_piece_simple(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual,NovaBoard))
  );
  ((Warnings == 1, nl,write('!!AVISO!! Tera de comer a peca do adversario'),nl,nl,!,false);(!,false)).
