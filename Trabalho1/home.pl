:- consult(auxiliar).

% =========== Menus ===========
menu :- cls,
  write('==============================='),nl,
  write('============ Dameo ============'),nl,
  write('=                             ='),nl,
  write('=          1: Jogar           ='),nl,
  write('=          2: Modo            ='),nl,
  write('=          3: Sair            ='),nl,
  write('=                             ='),nl,
  write('==============================='),nl,
  menu_opt(1).

% TypeMenu = 1 -> Main Menu ±± TypeMenu = 2 -> Mode Menu
menu_opt(TypeMenu) :- write('Por favor introduza a sua escolha'), nl,
  getDigit(In),
  ((TypeMenu == 1, check_in_main_menu(In));
  (TypeMenu == 2, check_in_mode_menu(In))).

check_in_main_menu(In):- (In < 0 ; In > 3), nl, nl, write('!!AVISO!! Por favor escolha uma opcao entre 1 a 3'),nl ,nl, menu_opt(1).
check_in_main_menu(1):- start.
check_in_main_menu(2):- menu_mode.
check_in_main_menu(3):- halt.

menu_mode :- cls,
  write('==============================='),nl,
  write('============ Dameo ============'),nl,
  write('=                             ='),nl,
  write('=  1: Jogador - Jogador       ='),nl,
  write('=  2: Jogador - Computador    ='),nl,
  write('=  3: Computador - Computador ='),nl,
  write('=                             ='),nl,
  write('==============================='),nl,
  menu_opt(2).

check_in_mode_menu(In):- (In < 0 ; In > 3), nl, nl, write('!!AVISO!! Por favor escolha uma opcao entre 1 a 3'),nl ,nl, menu_opt(2).
check_in_mode_menu(1):- write('Ply Vs Ply').
check_in_mode_menu(2):- write('Ply Vs PC').
check_in_mode_menu(3):- write('PC Vs PC').

start :- middle(Board),
  Jogada is 1,
  ciclo_jogada(Board, Jogada).

ciclo_jogada(Board,Jogada):-
  display_game_area(Board, Jogada),
  joga(Jogada, Board, BoardNova),
  NovaJogada is Jogada + 1,
  display_game_area(BoardNova, NovaJogada),
  format('SAIIII ~d',[NovaJogada]).
  %ciclo_jogada(BoardNova, NovaJogada).

display_game_area(Board,Jogada):-
  %cls,
  display_first_line("A","H",Board), nl,
  board_display(1,Board),
  info_display(Jogada,Board).

info_display(Jogada,Board):-
  conta_pecas(b,Board,Nr_brancas),
  conta_pecas(p,Board,Nr_pretas),
  format('Jogada numero: ~d', [Jogada]), nl,
  format('Numero de damas brancas: ~w', [Nr_brancas]), nl,
  format('Numero de damas pretas: ~w', [Nr_pretas]), nl, nl, nl.

% =========== Play ===========
verify_piece_player(Jogada,Linha,Coluna,BoardAtual):-
  getElement(BoardAtual,Linha,Coluna,Peca),
  % Verifica se a casa escolhida e branca
  ((Peca == none, write('!!AVISO!! Nao pode escolher uma casa vazia'),nl,false);
  % Verifica o tipo de jogador, nao deixando escolher as pecas do outro jogador
  (((par(Jogada),(Peca == p; Peca == rp));
  (impar(Jogada),(Peca == b; Peca == rb)));
  write('!!AVISO!! Nao pode escolher uma dama do adversario'),nl,false)).

verifiy_new_piece_player(NovaLinha,NovaColuna,BoardAtual):-
  getElement(BoardAtual,NovaLinha,NovaColuna,Peca),
  % Verifica se a nova peca e vazia, impossivel escolher uma do adversario ou sua
  ((Peca == none);write('!!AVISO!! A sua nova posicao tem de ser vazia'),nl,false).


ask_for_type_of_move(TipoDeMov):-
  repeat,
    write('Por favor escolha o tipo de movimento:'),nl,
    write('1- Normal'),nl,
    write('2- Linear'),nl,
    getDigit(TipoDeMov),
    ((TipoDeMov == 1; TipoDeMov == 2);write('!!AVISO!! Escolha entre 1 a 2'),nl,false),
  !.

eat_piece_simple(Jogador,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual):-
  (Jogador == 1,
  AuxLinha is (Linha - NovaLinha),
  AuxColuna is (Coluna - NovaColuna),
  AuxColuna2 is abs(AuxColuna),
  format('AuxLinha= ~d -- AuxColuna= ~d', [AuxLinha,AuxColuna]),nl,
  (
    ((AuxLinha == 2, AuxColuna2 == 0),write('Sim este sim'),nl, Y is Linha + 1, X is Coluna, format('Y= ~d -- X= ~d', [Y,X]),nl);
    ((AuxLinha == 0, AuxColuna2 == 2),write('Este nao!!!'),nl, Y is Linha, (NovaColuna > Coluna, X is Coluna + 1; X is Coluna - 1), format('Y= ~d -- X= ~d', [Y,X]),nl)
  ), true
  );write('NOOO'),false.

verify_movement(Jogada,Linha,Coluna,NovaLinha,NovaColuna,TipoDeMov,BoardAtual):-
  (TipoDeMov == 1,
  /* Se Aux 1, movimento simples para nao comer, se Aux1 = 0 ou 2, vai comer, movimento com dif. de duas casas */
  /* Jogador Brancas*/
  (impar(Jogada),Aux1 is (Linha - NovaLinha), (Aux1 == 0; Aux1 == 2), eat_piece_simple(1,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual)),

  /* COMUM AOS JOGADORES no movimento simples para nao comer */
  (Aux1 == 1, Aux2 is (Coluna - NovaColuna), Aux22 is abs(Aux2), format('Aux2= ~d -- Aux22= ~d', [Aux2,Aux22]),nl, Aux22 >= 0, Aux22 =< 1);true);
  write('!!AVISO!! Efetuou um moviento invalido'),nl,false.

ask_for_play(Jogada,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual):-
  ((par(Jogada), write('Jogam as pretas'));
  impar(Jogada), write('Jogam as brancas')),nl,
  ask_for_type_of_move(TipoDeMov),
  repeat,
    write('Introduza a linha da dama a mover:'),
    getDigit(Linha),
    write('Introduza a coluna da dama a mover:'),
    getDigit(Coluna),
    verify_piece_player(Jogada,Linha,Coluna,BoardAtual),
  !,
  repeat,
    write('Introduza a nova linha:'),
    getDigit(NovaLinha),
    write('Introduza a nova coluna:'),
    getDigit(NovaColuna),
    verifiy_new_piece_player(NovaLinha,NovaColuna,BoardAtual),
  !,
  verify_movement(Jogada,Linha,Coluna,NovaLinha,NovaColuna,TipoDeMov,BoardAtual).

move(BoardAtual,Linha,Coluna,NovaLinha,NovaColuna,BoardNova2):-
  getElement(BoardAtual,Linha,Coluna,PecaAntiga),
  getElement(BoardAtual,NovaLinha,NovaColuna,PecaNova),
  updateBoard(PecaAntiga,NovaLinha,NovaColuna,BoardAtual,BoardNova),
  updateBoard(PecaNova,Linha,Coluna,BoardNova,BoardNova2).

joga(Jogada, BoardAtual, BoardNova):-
  repeat,
    ask_for_play(Jogada,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual),
  !,
  move(BoardAtual,Linha,Coluna,NovaLinha,NovaColuna,BoardNova).

% =========== BOARDS ===========
initial([[p, p, p, p, p, p, p, p],
      [none, p, p, p, p, p, p, none],
      [none, none, p, p, p, p, none, none],
      [none, none, none, none, none, none, none, none],
      [none, none, none, none, none, none, none, none],
      [none, none, b, b, b, b, none, none],
      [none, b, b, b, b, b, b, none],
      [b, b, b, b, b, b, b, b]]).

middle([[none, none, none, none, none, p, none, none],
      [p, p, p, none, none, none, none, none],
      [none, none, none, p, p, none, none, none],
      [none, b, none, b, none, p, none, none],
      [none, none, none, b, b, none, none, none],
      [none, none, none, b, b, none, p, none],
      [none, none, none, none, b, b, p, b],
      [none, none, none, none, none, none, none, none]]).

end([[rb, none, none, none, none, none, none, none],
      [none, none, none, none, none, none, none, none],
      [none, none, none, none, none, none, none, none],
      [none, none, none, none, none, none, none, none],
      [none, none, none, none, none, none, none, none],
      [none, none, none, none, none, none, none, none],
      [none, none, none, none, none, none, none, none],
      [none, none, none, none, none, none, none, none]]).


% =========== Display ===========
board_display(X,[L2|L2s]) :- write(X),
  Y is X+1,
  write('| '),
  display_line(L2), nl,
  write(' '),
  display_underline(L2), nl,
  board_display(Y,L2s).
board_display(_,[]) :- nl.

display_line([E|Es]) :- translate(E,V),
  write(V),
  write('  | '),
  display_line(Es).
display_line([]).

display_underline([_|Es]) :- write('------'), display_underline(Es).
display_underline([]) :- write(' ').

display_first_line(Inicio, Fim, Board):- write('    '),
  Y is Inicio + 0,
  Inicio =< Fim,
  char_code(Imprime, Y),
  write(Imprime),
  write(' '),
  X is Y +1,
  display_first_line(X, Fim, Board).
display_first_line(_, _, Board) :- nl, write(' '), display_underline(Board).

translate(none,'  ').
translate(p,' P').
translate(b,' B').
translate(rp,'rP').
translate(rb,'rB').