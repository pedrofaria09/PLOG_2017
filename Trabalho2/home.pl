:-use_module(library(clpfd)).
:-use_module(library(lists)).

magic:-
L1=9,
L2=7,
L3=2,
L4=10,
L5=3,
L6=1,
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

append([Linha1,Linha2,Linha3,Linha4,Linha5,Linha6],Vars),

labeling([],Vars), printValues(Vars).


verifylist([],[]).
verifylist([Primeiro|Resto],SubLista):-
  ((Primeiro #= -1) #\/ (Primeiro #= 0)),
  coletar(Resto,SubLista).
verifylist([_|Resto],[],SubLista):-
  verifylist(Resto,SubLista).


coletar([],[]).
%coletar([Primeiro|_],_):-((Primeiro #= -1) #\/ (Primeiro #= 0)).
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


printValues([]).
printValues([A1,A2,A3,A4,A5,A6|Resto]):-
  format('~d ~d ~d ~d ~d ~d',[A1,A2,A3,A4,A5,A6]),nl,printValues(Resto).