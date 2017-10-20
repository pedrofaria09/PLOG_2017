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

start :- initial(Board),
  Jogada is 1,
  ciclo_jogada(Board, Jogada).

ciclo_jogada(Board,Jogada):-
  display_game_area(Board, Jogada),
  joga(Jogada, Board, BoardNova),
  NovaJogada is Jogada + 1,
  ciclo_jogada(BoardNova, NovaJogada).

display_game_area(Board,Jogada):-
  cls,
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
  ((Peca == none, write('!!AVISO!! Nao pode escolher uma casa vazia'),nl,fail);
  (((par(Jogada),(Peca == p; Peca == rp));
  (impar(Jogada),(Peca == b; Peca == rb)));
  write('!!AVISO!! Nao pode escolher uma dama do adversario'),nl,fail)).

ask_for_play(Jogada,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual):-
  ((par(Jogada), write('Jogam as pretas'));
  impar(Jogada), write('Jogam as brancas')),nl,
  repeat,
  write('Introduza a linha da dama a mover:'),
  getDigit(Linha),
  write('Introduza a coluna da damas a mover:'),
  getDigit(Coluna),
  verify_piece_player(Jogada,Linha,Coluna,BoardAtual),
  write('Introduza a nova linha:'),
  getDigit(NovaLinha),
  write('Introduza a nova coluna:'),
  getDigit(NovaColuna).

move(BoardAtual,Linha,Coluna,NovaLinha,NovaColuna,BoardNova2):-
  getElement(BoardAtual,Linha,Coluna,PecaAntiga),
  getElement(BoardAtual,NovaLinha,NovaColuna,PecaNova),
  updateBoard(PecaAntiga,NovaLinha,NovaColuna,BoardAtual,BoardNova),
  updateBoard(PecaNova,Linha,Coluna,BoardNova,BoardNova2).

joga(Jogada, BoardAtual, BoardNova):-
  ask_for_play(Jogada,Linha,Coluna,NovaLinha,NovaColuna,BoardAtual),
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
      [p, p, p, p, none, none, none, none],
      [none, none, none, none, p, none, none, none],
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