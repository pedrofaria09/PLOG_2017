
coletar([],[]).
coletar([Primeiro|_],[]):-((Primeiro #= -1) #\/ (Primeiro #= 0)).
coletar([Primeiro|Resto],Lista):-
  coletar(Resto,Temp),
  ((Primeiro #\= -1) #/\ (Primeiro #\= 0)),Lista=[Primeiro|Temp].

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
verifylist2([Primeiro|T],SubLista):-
  Primeiro \= -1 , Primeiro \= 0,
  verifylist2(T,SubLista).
verifylist2([Primeiro|[H|T]],SubLista):-
  (((Primeiro = -1, H \= 0) ; (Primeiro = 0, H \= -1)),
  coletar3([H|T],SubLista)).

coletar2([],[]).
coletar2([Primeiro|_],[]):-((Primeiro = -1) ; (Primeiro = 0)).
coletar2([Primeiro|Resto],Lista):-
  coletar2(Resto,Temp),
  ((Primeiro \= -1) ; (Primeiro \= 0)),Lista=[Primeiro|Temp].


coletar3(Resto,SubLista):-
  coletar_aux(Resto,[],SubLista).

coletar_aux([],L,L).
coletar_aux([H|T],LI,LF):-
  ((H \= -1), (H \= 0)),
  append([H],LI,SubLista),
  coletar_aux(T,SubLista,LF).
coletar_aux([H|_],L,L):-((H = -1) ; (H = 0)).

