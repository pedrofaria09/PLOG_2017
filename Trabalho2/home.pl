:-use_module(library(clpfd)).
:-use_module(library(lists)).

magic(Vars):-
Linha1=[A1,A2,A3,A4,A5,A6],
Linha2=[B1,B2,B3,B4,B5,B6],
Linha3=[C1,C2,C3,C4,C5,C6],
Linha4=[D1,D2,D3,D4,D5,D6],
Linha5=[E1,E2,E3,E4,E5,E6],
Linha6=[F1,F2,F3,F4,F5,F6],
Coluna1=[A1,B1,C1,D1,E1,F1],
Coluna2=[A2,B2,C2,D2,E2,F2],
Coluna3=[A3,B3,C3,D3,E3,F3],
Coluna4=[A4,B4,C4,D4,E4,F4],
Coluna5=[A5,B5,C5,D5,E5,F5],
Coluna6=[A6,B6,C6,D6,E6,F6],

domain(Linha1,0,6),
domain(Linha2,0,6),
domain(Linha3,0,6),
domain(Linha4,0,6),
domain(Linha5,0,6),
domain(Linha6,0,6),
domain(Coluna1,0,6),
domain(Coluna2,0,6),
domain(Coluna3,0,6),
domain(Coluna4,0,6),
domain(Coluna5,0,6),
domain(Coluna6,0,6),

all_distinct(Linha1),
all_distinct(Linha2),
all_distinct(Linha3),
all_distinct(Linha4),
all_distinct(Linha5),
all_distinct(Linha6),
all_distinct(Coluna1),
all_distinct(Coluna2),
all_distinct(Coluna3),
all_distinct(Coluna4),
all_distinct(Coluna5),
all_distinct(Coluna6),

append([Linha1,Linha2,Linha3,Linha4,Linha5,Linha6],Vars),

%exactly_two_zeros(Vars),

labeling([],Vars), printValues(Vars).

test(Vars):-
  length(Vars,5), exactly_two_zeros(Vars), labeling([],Vars).
exactly_two_zeros(Vars):-
  automaton(Vars,
            [source(n1), sink(n4)],
            [ arc(n1,1,n1),
              arc(n1,0,n2),
              arc(n2,1,n3),
              arc(n3,1,n3),
              arc(n3,0,n4),
              arc(n4,1,n4)]).


printValues([]).
printValues([A1,A2,A3,A4,A5,A6|Resto]):-
  format('~d ~d ~d ~d ~d ~d',[A1,A2,A3,A4,A5,A6]),nl,printValues(Resto).