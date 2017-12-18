:-use_module(library(clpfd)).
:-use_module(library(lists)).

%====== STATIC WAY ======
exemplo_enunciado:-
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
  solve_puzzle(L1,L2,L3,L4,L5,L6,R1,R2,R3,R4,R5,R6,_).

todas_solucoes_66(Vars3):-
  Valores=[L1,L2,L3,L4,L5,L6,R1,R2,R3,R4,R5,R6],
  domain(Valores,1,10),
  solve_puzzle(L1,L2,L3,L4,L5,L6,R1,R2,R3,R4,R5,R6,Vars3).

solve_puzzle(L1,L2,L3,L4,L5,L6,R1,R2,R3,R4,R5,R6,Vars3):-

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


%====== DYNAMIC WAY ======
solve_dynamic(Tamanho,Vars):-
  T is Tamanho-2,
  max_number(T,R),
  NewR is round(R),
  length(Linhas,Tamanho),
  length(Colunas,Tamanho),
  append(Linhas,Colunas,Valores),
  domain(Valores,1,NewR),

  generate_matrix(Tamanho, Tamanho, Matrix),
  domainrowloop(Tamanho,T,Matrix),
  rowloop(Tamanho,Matrix),
  colloop(Tamanho,Matrix),
  verifyrowloop(Tamanho,Matrix,Linhas),
  verifycolloop(Tamanho,Matrix,Colunas),

  appendrowloop(Tamanho,Matrix,[],Vars),
  append(Linhas,Colunas,Vars2),
  append(Vars,Vars2,Vars3),

  reset_timer,
  labeling([],Vars3),
  printValuesDynamic(Vars,Linhas,Colunas,Tamanho),
  print_time.

domainrowloop(0,_,_).
domainrowloop(N,Tamanho,Matrix) :-
  N>0, nth1(N,Matrix,L), domain(L,-1,Tamanho), M is N-1, domainrowloop(M,Tamanho,Matrix).

verifyrowloop(0,_,_).
verifyrowloop(N,Matrix,Linhas) :-
  N>0, nth1(N,Matrix,L), verifylist(L,SubLinha), nth1(N,Linhas,Val), sum(SubLinha,#=,Val), M is N-1, verifyrowloop(M,Matrix,Linhas).

verifycolloop(0,_,_).
verifycolloop(N,Matrix,Colunas) :-
  N>0, columnN(Matrix,N,L), verifylist(L,SubColuna), nth1(N,Colunas,Val), sum(SubColuna,#=,Val), M is N-1, verifycolloop(M,Matrix,Colunas).

rowloop(0,_).
rowloop(N,Matrix) :-
  N>0, nth1(N,Matrix,L), all_distinct(L), M is N-1, rowloop(M,Matrix).

colloop(0,_).
colloop(N,Matrix) :-
  N>0, columnN(Matrix,N,L), all_distinct(L), M is N-1, colloop(M,Matrix).

appendrowloop(0,_,L,L).
appendrowloop(N,Matrix,LI,LR) :-
  N>0, nth1(N,Matrix,L), append(L,LI,LF), M is N-1, appendrowloop(M,Matrix,LF,LR).

rowN([H|_],1,H):-!.
rowN([_|T],I,X) :-
    I1 is I-1,
    rowN(T,I1,X).

columnN([],_,[]).
columnN([H|T], I, [R|X]):-
   rowN(H, I, R),
columnN(T,I,X).

max_number(Tamanho,R):-
  R is Tamanho * (Tamanho+1) / 2 .

length_list(N, List) :- length(List, N).

generate_matrix(Cols, Rows, Matrix) :-
  length_list(Rows, Matrix),
  maplist(length_list(Cols), Matrix).

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
  ite(Coluna < 10,write('   '),write('  ')),
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

printValuesDynamic(Vars,Linhas,Colunas,Tamanho):-
  nl,
  write('      '),
  print_primeira(Colunas),nl,
  write('    '), print_tracos(Tamanho),write('-'), nl,
  ciclo_imprime(Vars,Linhas,Tamanho,Tamanho,1).

print_tracos(0).
print_tracos(Tamanho):-
  Tamanho > 0,
  write('----'),
  N is Tamanho-1,
  print_tracos(N).

ciclo_imprime(_,_,0,_,_).
ciclo_imprime(Vars,Linhas,Tamanho,TamFixo,AtualLine):-
  Tamanho>0, nth1(AtualLine,Linhas,ValorLinha),
  it(ValorLinha < 10,write(' ')), write(ValorLinha), write(' |'), take(TamFixo,Vars,LinhaPrint), print_linha(LinhaPrint), nl,
  write('    '), print_tracos(TamFixo),write('-'), nl,
  M is Tamanho-1, Aux is AtualLine + 1, append(LinhaPrint,Res,Vars),ciclo_imprime(Res,Linhas,M,TamFixo,Aux).

take(N, _, Xs) :- N =< 0, !, N =:= 0, Xs = [].
take(_, [], []).
take(N, [X|Xs], [X|Ys]) :- M is N-1, take(M, Xs, Ys).

print_linha([]).
print_linha([Linha|Resto]):-
  ite(Linha =< 0,translate(Linha,Valor),Valor is Linha),
  format(' ~w |',[Valor]),
  print_linha(Resto).

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

%====== MENUS ======
menu(X):- cls,
  write('==============================='),nl,
  write('========= Doppelblock ========='),nl,
  write('=                             ='),nl,
  write('=     1: Exemplo Enunciado    ='),nl,
  write('=     2: Todas Sol. 6*6       ='),nl,
  write('=     3: Tabuleiro Dinamico   ='),nl,
  write('=     4: Sair                 ='),nl,
  write('=                             ='),nl,
  write('==============================='),nl,
  write('Por favor introduza a sua escolha'), nl,
  menu_opt(X).

menu_opt(X):-
    getDigit(In),
    ((In == 1,exemplo_enunciado);
    (In == 2,todas_solucoes_66(X));
    (In == 3,tabuleiro_dinamico(X));
    (In == 4,halt);
    (write('Escolha entre 1 a 4'),nl,menu_opt)).

tabuleiro_dinamico(X):-
  write('Por favor introduza o tamanho do tabuleiro: '),
  getDigit(In),
  solve_dynamic(In,X).

%====== UTILS ======
ite(If,Then,_):-If,!,Then.
ite(_,_,Else):-Else.

it(If,Then):-If,!,Then.
it(_,_).

% Clear screen
cls :- write('\e[2J').

% get input from user
getNewLine:- get_code(T), (T == 10 -> ! ; getNewLine).
getDigit(D):- get_code(Dt), D is Dt - 48, (Dt == 10 -> ! ; getNewLine).
getChar(C):- get_char(C), char_code(C, Co), (Co == 10 -> ! ; getNewLine).
