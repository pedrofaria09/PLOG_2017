:-use_module(library(clpfd)).
:-use_module(library(lists)).
:-use_module(library(random)).

%====== STATIC WAY ======
magic(Vars3):-

Valores=[L1,L2,L3,L4,L5,L6,R1,R2,R3,R4,R5,R6],
domain(Valores,1,10),

append([[L1],[L2],[L3],[L4],[L5],[L6]],Linhas),
append([[R1],[R2],[R3],[R4],[R5],[R6]],Colunas),

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

domain(Linha1,-1,4),
domain(Linha2,-1,4),
domain(Linha3,-1,4),
domain(Linha4,-1,4),
domain(Linha5,-1,4),
domain(Linha6,-1,4),

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

verifylist(Linha1,SubLinha1),
sum(SubLinha1,#=,L1),
verifylist(Linha2,SubLinha2),
sum(SubLinha2,#=,L2),
verifylist(Linha3,SubLinha3),
sum(SubLinha3,#=,L3),
verifylist(Linha4,SubLinha4),
sum(SubLinha4,#=,L4),
verifylist(Linha5,SubLinha5),
sum(SubLinha5,#=,L5),
verifylist(Linha6,SubLinha6),
sum(SubLinha6,#=,L6),
verifylist(Coluna1,SubColuna1),
sum(SubColuna1,#=,R1),
verifylist(Coluna2,SubColuna2),
sum(SubColuna2,#=,R2),
verifylist(Coluna3,SubColuna3),
sum(SubColuna3,#=,R3),
verifylist(Coluna4,SubColuna4),
sum(SubColuna4,#=,R4),
verifylist(Coluna5,SubColuna5),
sum(SubColuna5,#=,R5),
verifylist(Coluna6,SubColuna6),
sum(SubColuna6,#=,R6),

append([Linha1,Linha2,Linha3,Linha4,Linha5,Linha6],Vars),
append(Linhas,Colunas,Vars2),
append(Vars,Vars2,Vars3),

reset_timer,
labeling([],Vars3),
printValues(Vars,Linhas,Colunas),
print_time.

%====== AUXILIAR ======
verifylist([],[]).
verifylist([Primeiro|T],SubLista):-
  Primeiro #\= -1 #/\ Primeiro #\= 0,
  verifylist(T,SubLista).
verifylist([Primeiro|[H|T]],SubLista):-
  (((Primeiro #= -1 #/\ H #\= 0) #\/ (Primeiro #= 0 #/\ H #\= -1)),
  coletar([H|T],SubLista)).

coletar(Resto,SubLista):-
  coletar_aux(Resto,[],SubLista).

coletar_aux([],L,L).
coletar_aux([H|T],LI,LF):-
  ((H #\= -1) #/\ (H #\= 0)),
  append([H],LI,SubLista),
  coletar_aux(T,SubLista,LF).
coletar_aux([H|_],L,L):-((H #= -1) #\/ (H #= 0)).

%====== PRINTS ======
printValues(Vars,Linhas,Colunas):-
  nl,
  write('     '),
  print_primeira(Colunas),nl,
  write('   -------------------------'),nl,
  printValuesValores(Vars,Linhas).

print_primeira([]).
print_primeira([Coluna|Resto]):-
  write(Coluna),
  write('   '),
  print_primeira(Resto).

printValuesValores([],[]).
printValuesValores([A1,A2,A3,A4,A5,A6|Resto],[Linha|RestoLinhas]):-
  translate(A1,X1),
  translate(A2,X2),
  translate(A3,X3),
  translate(A4,X4),
  translate(A5,X5),
  translate(A6,X6),
  ite(Linha < 10,
    format(' ~d | ~w | ~w | ~w | ~w | ~w | ~w |~N',[Linha,X1,X2,X3,X4,X5,X6]),
    format('~d | ~w | ~w | ~w | ~w | ~w | ~w |~N',[Linha,X1,X2,X3,X4,X5,X6])),
  write('   -------------------------'),nl,
  printValuesValores(Resto,RestoLinhas).


reset_timer:- statistics(walltime,_).
print_time:-
	statistics(walltime,[_,T]),
	TS is ((T//10)*10)/1000,
	nl, write('Time: '), write(TS), write('s'), nl, nl.

translate(-1,'X').
translate(0,'X').
translate(1,'1').
translate(2,'2').
translate(3,'3').
translate(4,'4').

%Utils
ite(If,Then,_):-If,!,Then.
ite(_,_,Else):-Else.
