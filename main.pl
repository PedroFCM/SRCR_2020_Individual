%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI

% Autor
% Pedro Machado, a83719


% ~/bin/sicstus -l main.pl


:- [paragens].
:- [viagens].


% -----------------------------------------------------------------------------------------
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).



% -----------------------------------------------------------------------------------------
% Predicados usados

% paragem -> gid, latitude, longitude, estado de conservação, tipo de abrigo, abrigo com publicidade,
%            codigo de rua, nome da rua, freguesia

% viagem -> carreira, paragemInicio(gid), paragemFim(gid), operadora, tempo de viagem (em minutos)
% as viagens são entre paragens adjacentes


% PESQUISA EM PROFUNDIDADE
pesquisaProfundidade(Origem, Destino, Caminho, TempoViagem) :-
    paragem(Origem, _, _, _, _, _, Operadora, _, _, _),
	profundidade_func(Origem, Destino, [(Origem, Operadora, 'Inicio')], Caminho, TempoViagem).

profundidade_func(Destino, Destino, His, Caminho, T) :-
	inverso(His, Caminho), 
    T is 0.

profundidade_func(Origem, Destino, His, Caminho, TempoViagem) :-
    adjacente_func(Origem, Prox, Carreira, T1),
	\+ member(Prox, His),
    ((T1 == 'N/A') -> TLimpo is 0.0 ; TLimpo is T1),
    paragem(Origem, _, _, _, _, _, Operadora, _, _, _),
    profundidade_func(Prox, Destino, [(Prox, Operadora, Carreira)|His], Caminho, T),                 
    TempoViagem is T + TLimpo + 5.  % Adiciona 5 min entre paragens de autocarros

adjacente_func(Nodo, ProxNodo, Carreira, TempoViagem) :-
    viagem(Carreira, Nodo, ProxNodo, TempoViagem).

% pesquisaProfundidade(183, 791, Caminho, Tempo).
% pesquisaProfundidade(183, 595, Caminho, Tempo).

% -----------------------------------------------------------------------------
% PESQUISA EM LARGURA


pesquisaLargura(Origem, Destino, Caminho) :-
    pesquisaLargura_fun([[Origem]], Destino, C),
    inverso(C, Caminho).

pesquisaLargura_fun([[Destino|Path]|_], Destino, [Destino|Path]) :-
    TempoViagem is 0.

pesquisaLargura_fun([[Nodo|Path]|Paths], Destino, Caminho) :-
    bagof(
        [M,Nodo|Path],
        (viagem(_, Nodo, M, T), \+ member(M, [Nodo | Path])),
        NewPaths
        ),
    append(Paths, NewPaths, Pathsl), !,
    pesquisaLargura_fun(Pathsl, Destino, Caminho);
    pesquisaLargura_fun(Paths, Destino, Caminho).

% pesquisaLargura(183, 78, Caminho, Tempo).




% -----------------------------------------------------------------------------
% PESQUISA ESTRELA

tempoEstimado((LatX, LongX), (LatY, LongY), Estima) :- 
    ((LatX == 'N/A') -> LatLimpoX is 0.0; LatLimpoX is LatX),
    ((LongX == 'N/A') -> LongLimpoX is 0.0; LongLimpoX is LongX),
    ((LatY == 'N/A') -> LatLimpoY is 0.0; LatLimpoY is LatY),
    ((LongY == 'N/A') -> LongLimpoY is 0.0; LongLimpoY is LongY),
    DeltaLat is LatLimpoX - LatLimpoY,
    DeltaLong is LongLimpoX - LongLimpoY,
    Estima is (sqrt(DeltaLat^2 + DeltaLong^2))/1000.


resolve_aestrela(Origem, Destino, Caminho/Tempo) :-
    paragem(Origem, LatX, LongX, _, _, _, _, _, _, _),
    paragem(Destino, LatY, LongY, _, _, _, _, _, _, _),
    tempoEstimado((LatX, LongX), (LatY, LongY), Estima),
	aestrela([[Origem]/0/Estima], InvCaminho/Tempo/_, Destino),
	inverso(InvCaminho, Caminho).

aestrela(Caminhos, Caminho, Destino) :-
	obtem_melhor(Caminhos, Caminho, Destino),
	Caminho = [Nodo|_]/_/_, 
    Destino == Nodo.

aestrela(Caminhos, SolucaoCaminho, Destino) :-
	obtem_melhor(Caminhos, MelhorCaminho, Destino),
	seleciona(MelhorCaminho, Caminhos, OutrosCaminhos),
	expande_aestrela(MelhorCaminho, ExpCaminhos, Destino),
	append(OutrosCaminhos, ExpCaminhos, NovoCaminhos),
    aestrela(NovoCaminhos, SolucaoCaminho, Destino).		


obtem_melhor([Caminho], Caminho, Destino) :- !.

obtem_melhor([Caminho1/Tempo1/Est1, _/Tempo2/Est2|Caminhos], MelhorCaminho, Destino) :-
	Tempo1 + Est1 =< Tempo2 + Est2, !,
	obtem_melhor([Caminho1/Tempo1/Est1|Caminhos], MelhorCaminho, Destino).
	
obtem_melhor([_|Caminhos], MelhorCaminho, Destino) :- 
	obtem_melhor(Caminhos, MelhorCaminho, Destino).

expande_aestrela(Caminho, ExpCaminhos, Destino) :-
	findall(NovoCaminho, 
    adjacente(Caminho,NovoCaminho, Destino), ExpCaminhos).

adjacente([Nodo|Caminho]/Tempo/_, [ProxNodo,Nodo|Caminho]/NovoTempo/Est, Destino) :-
	viagem(_, Nodo, ProxNodo, PassoTempo),
    \+ member(ProxNodo, Caminho),
	NovoTempo is Tempo + PassoTempo,
	paragem(ProxNodo, LatX, LongX, _, _, _, _, _, _, _),
    paragem(Destino, LatY, LongY, _, _, _, _, _, _, _),
    tempoEstimado((LatX, LongX), (LatY, LongY), Est).


seleciona(E, [E|Xs], Xs).
seleciona(E, [X|Xs], [X|Ys]) :- 
    seleciona(E, Xs, Ys).


% resolve_aestrela(183, 78, Caminho/Tempo).


% --------------------------------------------------------------------------
% Queries

% Trajeto entre 2 pontos com apenas algumas operadoras
trajetoCertasOperadoras(Origem, Destino, Caminho, TempoViagem, Operadoras) :-
    pesquisaProfundidade(Origem, Destino, Caminho, TempoViagem, Operadoras).


pesquisaProfundidade(Origem, Destino, Caminho, TempoViagem, Operadoras) :-
    paragem(Origem, _, _, _, _, _, Operadora, _, _, _),
    pertence(Operadora, Operadoras),
	profundidade_func(Origem, Destino, [(Origem, Operadora, 'Inicio')], Caminho, TempoViagem).

profundidade_func(Destino, Destino, His, Caminho, T, Operadoras) :-
	inverso(His, Caminho), 
    T is 0.

profundidade_func(Origem, Destino, His, Caminho, TempoViagem) :-
    adjacente_func(Origem, Prox, Carreira, T1),
	\+ member(Prox, His),
    ((T1 == 'N/A') -> TLimpo is 0.0 ; TLimpo is T1),
    paragem(Origem, _, _, _, _, _, Operadora, _, _, _),
    pertence(Operadora, Operadoras),
    profundidade_func(Prox, Destino, [(Prox, Operadora, Carreira)|His], Caminho, T),                 
    TempoViagem is T + TLimpo + 5.  % Adiciona 5 min entre paragens de autocarros

%trajetoCertasOperadoras(183, 595, Caminho, TempoViagem, ['Vimeca']).







% Escrever as soluções todas
escrever([]).
escrever([X|L]) :- 
    write(X), nl, escrever(L).

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

pertence( X,[X|L] ).
pertence( X,[Y|L] ) :-
    X \= Y,
    pertence( X,L ).