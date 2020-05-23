%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI

% Autor
% Pedro Machado, a83719


% ~/bin/sicstus -l main.pl


%:- [paragem_autocarros_oeiras_processado].


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

%:- use_module(library(codesio)).

%read_from_codes("lista_adjacencias_paragens.ods", Lista).

% Predicado paragem. Este predicado recebe como argumento o seu identificador (gid) e as suas características
paragem().

% Predicado caracteristicas
%   Este predicado recebe caracteriza todas as características 
caracteristicas().

percurso(CARREIRA, PARAGEM_INICIO, PARAGEM_FIM)

% FORMA USADA NA FICHA 9
%-----------------------------------------------------

g(grafo([a,b,c,d,e,f,g], [aresta(a,b),aresta(c,d),aresta(c,f),aresta(d,f),aresta(f,g)])).	

g1(grafo([a,b,c,d,e,f,g], [aresta(a,b),aresta(c,d),aresta(c,f),aresta(d,f),aresta(f,g), aresta(f,e), aresta(e,d)])).	


% Iremos usar a representação b)

% 1
adjacente(X,Y,grafo(_,Es)) :- member(aresta(X,Y),Es).
adjacente(X,Y,grafo(_,Es)) :- member(aresta(Y,X),Es).


% 2
caminho(G, A, B, P) :- caminho1(G, A, [B], P).

caminho1(G, A, [A|P1], [A|P1]).
caminho1(G, A, [Y|P1], P) :- 
    adjacente(X, Y, G),
    \+ member(X, [Y|P1]),  % \+ é igual a not
    caminho1(G, A, [X, Y|P1], P).


% 3
ciclo(G, A, P) :- 
    adjacente(B, A, G),
    caminho(G, A, B, P1), 
    length(P1, L),
    L > 2,
    append(P1, [A], P).


%---------------------------------  dados do problema ---------

% Problema de estado único

% estado inicial
inicial(jarros(0, 0)).

% estados finais
final(jarros(4, _)).
final(jarros(_, 4)).

% transições possíveis transicao: Einicial x Op x Efinal

% encher balde 1
transicao(jarros(V1, V2), encher(1), jarros(8, V2)) :-
    V1 < 8.
% encher balde 2
transicao(jarros(V1, V2), encher(2), jarros(V1, 5)) :-
    V2 < 5.
% encher balde 2 a partir do 1
transicao(jarros(V1, V2), encher(1, 2), jarros(NV1, NV2)) :-
    V1 > 0,
    NV1 is max(V1 - 5 + V2, 0),
    NV1 < V1,
    NV2 is V2 + V1 - NV1.

%----------------------------------------------------

% Pesquisa em profundidade
resolvedf(Solucao) :-
    inicial(InicialEstado),
    resolvedf(InicialEstado, [InicialEstado], Solucao).

resolvedf(Estado, _, []) :-
    final(Estado), !.

resolvedf(Estado, Historico, [Move|Solucao]) :-
    transicao(Estado, Move, Estado1),
    nao(membro(Estado1, Historico)),
    resolvedf(Estado1, [Estado1|Historico], Solucao).

% Par de uma solução com o seu custo (comprimento da lista)
todos(L) :-
    findall((S, C), (resolvedf(S), length(S, C)), L).

melhor(S, Custo) :- 
    findall((S,C), (resolvedf(S), length(S, C)), L),
    minimo(L, (S, Custo)).

minimo([(P, X)], (P, X)).
minimo([(Px, X)|L], (Py, Y)) :- 
    minimo(L, (Py, Y)),
    X > Y.
minimo([(Px, X)|L], (Px, X)) :- 
    minimo(L, (Py, Y)),
    X =< Y.


% Pesquisa em largura (acaba quando encontro na cabeça da lista o estado final esperado)
resolvebf(Solucao) :-
    inicial(InicialEstado),
    resolvebf([(InicialEstado, [])|Xs]-Xs, [], Solucao).

resolvebf([(Estado, Vs)|_]-_, _, Rs) :-
    final(Estado), !, inverso(Vs, Rs).

resolvebf([(Estado, _)|Xs]-Ys, Historico, Solucao) :-
    membro(Estado, Historico), !,
    resolvebf(Xs-Ys, Historico, Solucao).

resolvebf([(Estado, Vs)|Xs]-Ys, Historico, Solucao) :-
    setof((Move, Estado1), transicao(Estado, Move, Estado1), Ls),
    atualizar(Ls, Vs, [Estado|Historico], Yx-Zs),
    resolvebf(Xs-Zs, [Estado|Historico], Solucao).

atualizar([], _, _, X-X).
atualizar([(_, Estado)|Ls], Vs, Historico, Xs-Ys) :-
    membro(Estado, Historico), !,
    atualizar(Ls, Vs, Historico, Xs-Ys).

atualizar([(Move, Estado)|Ls], Vs, Historico, [(Estado, [Move|Vs])|Xs]-Ys) :-
    atualizar(Ls, Vs, Historico, Xs-Ys).

% Escrever as soluções todas
escrever([]).
escrever([X|L]) :- 
    write(X), write(' - '), write(Y), nl, escrever(L).

membro(X, [X|_]).
membro(X, [_|Xs]) :-
    membro(X,Xs).

membros([], _).
membros([X|Xs], Members) :-
    membro(X, Members),
    membros(Xs, Members).

inverso(Xs, Ys) :-
    inverso(Xs, [], Ys).
inverso([], Xs, Xs).
inverso([X|Xs], Ys, Zs) :-
    inverso(Xs, [X|Ys], Zs).

nao(Questao) :-
    Questao, !, fail.
nao(Questao).
