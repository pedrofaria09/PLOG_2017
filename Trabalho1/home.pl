start :- middle(B1),
        display_first_line("A","H",B1), nl,
        board_display(1,B1).


% =========== BOARDS ===========
board([[v, v, v, v, v, v, v, v],
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

display_first_line(Inicio, Fim, _):- write('    '),
  Y is Inicio + 0,
  Inicio =< Fim,
  char_code(Imprime, Y),
  write(Imprime),
  write(' '),
  X is Y +1,
  display_first_line(X, Fim).
display_first_line(_, _, B1) :- write(' '), display_underline(B1), nl.

translate(none,'  ').
translate(v,' V').
translate(a,' A').
translate(qv,'qV').
translate(qa,'qA').