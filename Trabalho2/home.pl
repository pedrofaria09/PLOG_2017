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

domain(Linha1,1,6),
domain(Linha2,1,6),
domain(Linha3,1,6),
domain(Linha4,1,6),
domain(Linha5,1,6),
domain(Linha6,1,6),
domain(Coluna1,1,6),
domain(Coluna2,1,6),
domain(Coluna3,1,6),
domain(Coluna4,1,6),
domain(Coluna5,1,6),
domain(Coluna6,1,6),

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

A1+A2+A3+A4+A5+A6 #= Soma,
B1+B2+B3+B4+B5+B6 #= Soma,
C1+C2+C3+C4+C5+C6 #= Soma,
D1+D2+D3+D4+D5+D6 #= Soma,
E1+E2+E3+E4+E5+E6 #= Soma,
F1+F2+F3+F4+F5+F6 #= Soma,
A1+B1+C1+D1+E1+F1 #= Soma,
A2+B2+C2+D2+E2+F2 #= Soma,
A3+B3+C3+D3+E3+F3 #= Soma,
A4+B4+C4+D4+E4+F4 #= Soma,
A5+B5+C5+D5+E5+F5 #= Soma,
A6+B6+C6+D6+E6+F6 #= Soma,

labeling([],Vars), printValues(Vars).

printValues([]).
printValues([A1,A2,A3,A4,A5,A6|Resto]):-
  format('~d ~d ~d ~d ~d ~d',[A1,A2,A3,A4,A5,A6]),nl,printValues(Resto).