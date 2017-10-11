:- consult(aux).

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
check_in_main_menu(3):- break.

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

start :- initial(B1),
  display_first_line("A","H",B1), nl,
  board_display(1,B1).


% =========== BOARDS ===========
initial([[v, v, v, v, v, v, v, v],
      [none, v, v, v, v, v, v, none],
      [none, none, v, v, v, v, none, none],
      [none, none, none, none, none, none, none, none],
      [none, none, none, none, none, none, none, none],
      [none, none, a, a, a, a, none, none],
      [none, a, a, a, a, a, a, none],
      [a, a, a, a, a, a, a, a]]).

middle([[none, none, none, none, none, v, none, none],
      [v, v, v, v, none, none, none, none],
      [none, none, none, none, v, none, none, none],
      [none, a, none, a, none, v, none, none],
      [none, none, none, a, a, none, none, none],
      [none, none, none, a, a, none, v, none],
      [none, none, none, none, a, a, v, a],
      [none, none, none, none, none, none, none, none]]).

end([[qa, none, none, none, none, none, none, none],
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

display_first_line(Inicio, Fim, B1):- write('    '),
  Y is Inicio + 0,
  Inicio =< Fim,
  char_code(Imprime, Y),
  write(Imprime),
  write(' '),
  X is Y +1,
  display_first_line(X, Fim, B1).
display_first_line(_, _, B1) :- nl, write(' '), display_underline(B1).

translate(none,'  ').
translate(v,' V').
translate(a,' A').
translate(qv,'qV').
translate(qa,'qA').