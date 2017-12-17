:-use_module(library(clpfd)).
:-use_module(library(lists)).

magic:-
L1=9,
L2=7,
L3=2,
L4=10,
L5=3,
L6=1,
R1=4,
R2=8,
R3=4,
R4=5,
R5=6,
R6=5,

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
domain(Coluna1,-1,4),
domain(Coluna2,-1,4),
domain(Coluna3,-1,4),
domain(Coluna4,-1,4),
domain(Coluna5,-1,4),
domain(Coluna6,-1,4),

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
/*verifylist(Coluna1,SubColuna1),
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
sum(SubColuna6,#=,R6),*/

append([Linha1,Linha2,Linha3,Linha4,Linha5,Linha6],Vars),

reset_timer,
labeling([],Vars),
printValues(Vars),
print_time.


verifylist([],[]).
verifylist([Primeiro|Resto],SubLista):-
  (((Primeiro #= -1) #\/ (Primeiro #= 0)),
  coletar(Resto,SubLista)) ; verifylist(Resto,SubLista).


coletar([],[]).
coletar([Primeiro|_],[]):-((Primeiro #= -1) #\/ (Primeiro #= 0)).
coletar([Primeiro|Resto],Lista):-
  coletar(Resto,Temp),
  ((Primeiro #\= -1) #\/ (Primeiro #\= 0)),Lista=[Primeiro|Temp].

/*coletar(Resto,SubLista):-
  coletar_aux(Resto,[],SubLista).

coletar_aux([],L,L).
coletar_aux([H|_],L,L):-((H #= -1) #\/ (H #= 0)).
coletar_aux([H|T],Temp,SubLista):-
  ((H #\= -1) #\/ (H #\= 0)),
  append(Temp,[H],SubLista),
  coletar_aux(T,SubLista,SubLista).*/

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

test2(Lista,Res):-
  verifylist2(Lista,Res1),
  sumlist(Res1,Res).

verifylist2([],[]).
verifylist2([Primeiro|Resto],SubLista):-
  (((Primeiro = -1) ; (Primeiro = 0)),
  coletar2(Resto,SubLista));verifylist2(Resto,SubLista).

coletar2([],[]).
coletar2([Primeiro|_],[]):-((Primeiro = -1) ; (Primeiro = 0)).
coletar2([Primeiro|Resto],Lista):-
  coletar2(Resto,Temp),
  ((Primeiro \= -1) ; (Primeiro \= 0)),Lista=[Primeiro|Temp].


printValues([]).
printValues([A1,A2,A3,A4,A5,A6|Resto]):-
  translate(A1,X1),
  translate(A2,X2),
  translate(A3,X3),
  translate(A4,X4),
  translate(A5,X5),
  translate(A6,X6),
  format('~N ~w  ~w  ~w  ~w  ~w  ~w ~N ~N',[X1,X2,X3,X4,X5,X6]),printValues(Resto).

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