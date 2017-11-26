:- consult(auxiliar).
:- consult(logic).

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
check_in_main_menu(1):- start(1).
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
check_in_mode_menu(1):- start(1).
check_in_mode_menu(2):- start(2).
check_in_mode_menu(3):- start(3).

display_game_area(Board,Jogada):-
  cls,
  display_first_line("A","H",Board), nl,
  board_display(1,Board),
  info_display(Jogada,Board).

display_game_area_pc(Board,Jogada):-
  cls,
  display_first_line("A","H",Board), nl,
  board_display(1,Board),
  info_display(Jogada,Board),
  get_code(_).

display_board(Board):-
  display_first_line("A","H",Board), nl,
  board_display(1,Board).

info_jogada_pc(Tipo,Jogador,Linha,Coluna,NovaLinha,NovaColuna):-
  ((Coluna == 1, Letra_Coluna = 'A');
  (Coluna == 2, Letra_Coluna = 'B');
  (Coluna == 3, Letra_Coluna = 'C');
  (Coluna == 4, Letra_Coluna = 'D');
  (Coluna == 5, Letra_Coluna = 'E');
  (Coluna == 6, Letra_Coluna = 'F');
  (Coluna == 7, Letra_Coluna = 'G');
  (Coluna == 8, Letra_Coluna = 'H')),
  ((NovaColuna == 1, Letra_NovaColuna = 'A');
  (NovaColuna == 2, Letra_NovaColuna = 'B');
  (NovaColuna == 3, Letra_NovaColuna = 'C');
  (NovaColuna == 4, Letra_NovaColuna = 'D');
  (NovaColuna == 5, Letra_NovaColuna = 'E');
  (NovaColuna == 6, Letra_NovaColuna = 'F');
  (NovaColuna == 7, Letra_NovaColuna = 'G');
  (NovaColuna == 8, Letra_NovaColuna = 'H')),
  ((Jogador == 1, write('Jogaram as brancas'),nl);(Jogador == 2, write('Jogaram as pretas'),nl)),
  write('Jogada do PC:'),nl,format('Pos. Inicial [~w,~d]',[Letra_Coluna,Linha]),nl,format('Pos. Final [~w,~d]',[Letra_NovaColuna,NovaLinha]),nl,nl,(Tipo==2,write('Clique para continuar'),get_char(_);true).

info_display(Jogada,Board):-
  conta_pecas(b,Board,Nr_brancas),
  conta_pecas(p,Board,Nr_pretas),
  conta_pecas(rb,Board,Nr_rei_brancas),
  conta_pecas(rp,Board,Nr_rei_pretas),
  format('Jogada numero: ~d', [Jogada]), nl,
  format('Numero de damas brancas: ~w', [Nr_brancas]), nl,
  format('Numero de reis brancos: ~w', [Nr_rei_brancas]), nl,
  format('Numero de damas pretas: ~w', [Nr_pretas]), nl,
  format('Numero de reis pretos: ~w', [Nr_rei_pretas]), nl,nl,nl.


% =========== BOARDS ===========
initial([[p, p, p, p, p, p, p, p],
      [none, p, p, p, p, p, p, none],
      [none, none, p, p, p, p, none, none],
      [none, none, none, none, none, none, none, none],
      [none, none, none, none, none, none, none, none],
      [none, none, b, b, b, b, none, none],
      [none, b, b, b, b, b, b, none],
      [b, b, b, b, b, b, b, b]]).

middle([[rb, rp, none, rp, p, none, rp, rp],
        [none, b, rp, b, none, none, none, none],
        [none, none, p, rb, b, none, none, none],
        [p, none, none, none, none, p, b, none],
        [none, none, p, none, none, none, none, none],
        [none, none, none, p, none, none, b, none],
        [p, none, rb, none, none, none, none, none],
        [none, none, none, rp, none, none, none, none]]).

teste([[none, none, none, none, none, rb, none, rb],
      [p, p, p, none, b, none, none, none],
      [none, none, none, p, p, none, p, none],
      [none, b, none, b, none, p, none, none],
      [none, p, none, b, b, none, none, none],
      [none, none, none, b, b, p, b, none],
      [none, none, none, none, b, none, p, b],
      [none, none, none, none, none, none, none, none]]).

/*Para multi king a comer*/
teste2([[none, none, none, none, none, b, none, none],
      [none, none, p, none, rb, none, b, none],
      [none, rp, none, rp, none, none, none, none],
      [none, none, none, none, none, none, none, none],
      [none, none, none, p, none, none, none, none],
      [none, p, none, p, rb, none, none, none],
      [none, none, none, none, none, none, none, none],
      [none, rb, none, none, none, none, rp, none]]).

/*Para multi dama a comer*/
teste3([[none, none, none, none, none, none, none, none],
      [none, none, none, none, none, p, none, none],
      [none, p, none, none, none, none, none, none],
      [p, none, p, none, none, b, none, none],
      [none, none, none, none, none, none, none, none],
      [p, none, p, none, none, none, none, none],
      [none, none, b, none, none, none, none, none],
      [none, none, none, none, none, none, none, none]]).

end([[rb, none, none, none, none, none, none, none],
      [none, none, none, none, none, none, none, none],
      [none, none, none, none, none, none, none, none],
      [none, none, none, none, none, none, none, none],
      [none, none, none, none, none, none, none, none],
      [none, none, none, none, none, none, none, none],
      [none, none, none, none, none, none, none, none],
      [none, none, none, none, none, none, rp, rb]]).


% =========== Display ===========
board_display(X,[L2|L2s]) :-
  display_blank_line(L2),nl,
  write(X),
  Y is X+1,
  write('| '),
  display_line(L2), nl,
  display_blank_line(L2),nl,
  write(' '),
  display_underline(L2), nl,
  board_display(Y,L2s).
board_display(_,[]) :- nl.

display_line([E|Es]) :- translate(E,V),
  write(' '),
  write(V),
  write('  | '),
  display_line(Es).
display_line([]).

display_blank_line([_|Es]) :- write(' |     '), display_blank_line(Es).
display_blank_line([]) :- write(' |').

display_underline([_|Es]) :- write('-------'), display_underline(Es).
display_underline([]) :- write(' ').

display_first_line(Inicio, Fim, Board):- write('    '),
  Y is Inicio + 0,
  Inicio =< Fim,
  char_code(Imprime, Y),
  write(Imprime),
  write('  '),
  X is Y +1,
  display_first_line(X, Fim, Board).
display_first_line(_, _, Board) :- nl, write(' '), display_underline(Board).

translate(none,'  ').
translate(p,'P ').
translate(b,'B ').
translate(rp,'rP').
translate(rb,'rB').