start :- board(B1),
        display_first_line("A","H"), nl,
        board_display(1,B1).



% =========== BOARDS ===========
board([[v, v, v, v, v, v, v, v],
      [blank, v, v, v, v, v, v, blank],
      [blank, blank, v, v, v, v, blank, blank],
      [blank, blank, blank, blank, blank, blank, blank, blank],
      [blank, blank, blank, blank, blank, blank, blank, blank],
      [blank, blank, a, a, a, a, blank, blank],
      [blank, a, a, a, a, a, a, blank],
      [a, a, a, a, a, a, a, a]]).


board_display(X,[L2|L2s]) :- write(X), Y is X+1, write('- '), display_line(L2), nl, display_line__(L2), nl, board_display(Y,L2s).
board_display(_,[]) :- nl.

display_line([E|Es]) :- translate(E,V), write(V), write(' | '), display_line(Es).
display_line([]):- !.

display_line__([_|Es]) :- write(' - -'), display_line__(Es).
display_line__([]):- !.

display_first_line(Inicio, Fim):- write('   '), Y is Inicio + 0, Inicio =< Fim, char_code(Imprime, Y), write(Imprime), X is Y +1, display_first_line(X, Fim).
display_first_line(_, _).

translate(blank,' ').
translate(v,'V').
translate(a,'A').