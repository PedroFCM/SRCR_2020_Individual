%--------------------------------------------------------------------------------------------------------
% PESQUISA EM PROFUNDIDADE

pesquisaProfundidade(Origem, Destino, Caminho, TempoViagem) :-
    findall(
        paragem(Gid, Lat, Long, Estado, TipoAbrigo, Publicidade, Operadora, Codigo, NomeRua, Freguesia),
        paragem(Gid, Lat, Long, Estado, TipoAbrigo, Publicidade, Operadora, Codigo, NomeRua, Freguesia),
        ListaParagens
    ),
	profundidade_func(Origem, Destino, [(Origem, 'Inicio')], Caminho, TempoViagem, ListaParagens).

profundidade_func(Destino, Destino, His, Caminho, T, _) :-
	inverso(His, Caminho), 
    T is 0.

profundidade_func(Origem, Destino, His, Caminho, TempoViagem, ListaParagens) :-
    adjacente(Origem, Prox, Carreira, T1, ListaParagens),
	\+ member((Prox, Carreira), His),
    ((T1 == 'N/A') -> TLimpo is 0.0 ; TLimpo is T1),
    profundidade_func(Prox, Destino, [(Prox, Carreira)|His], Caminho, T, ListaParagens),                 
    TempoViagem is T + TLimpo + 5.                      % Adiciona 5 min entre paragens de autocarros

adjacente(Paragem, ProxParagem, Carreira, TempoViagem, ListaParagens) :-
    member( 
        paragem(Paragem, _, _, _, _, _, _, _, _, _),
        ListaParagens
        ),
    member( 
        paragem(ProxParagem, _, _, _, _, _, _, _, _, _),
        ListaParagens
        ),
    viagem(Carreira, Paragem, ProxParagem, TempoViagem).

% pesquisaProfundidade(183, 608, Caminho, Tempo).
% pesquisaProfundidade(183, 595, Caminho, Tempo).

% Função de pesquisa em profundidade geral
pesquisaProfundidade_varios(Origem, Destino, [(Origem, 'Inicio')|Caminho], TempoViagem) :-
    findall(
        paragem(Gid, Lat, Long, Estado, TipoAbrigo, Publicidade, Operadora, Codigo, NomeRua, Freguesia),
        paragem(Gid, Lat, Long, Estado, TipoAbrigo, Publicidade, Operadora, Codigo, NomeRua, Freguesia),
        ListaParagens
    ),
	profundidade_func_varios(Origem, Destino, Caminho, TempoViagem, ListaParagens).

% Função de pesquisa para a Query 2 e 3
pesquisaProfundidade_varios(Origem, Destino, [(Origem, 'Inicio')|Caminho], TempoViagem, Operadoras) :-
    paragensComOperadoras(Operadoras, [], NovaListaParagens),
	profundidade_func_varios(Origem, Destino, Caminho, TempoViagem, NovaListaParagens).

% Função de pesquisa para a Query 7
pesquisaProfundidade_varios(Origem, Destino, [(Origem, 'Inicio')|Caminho], TempoViagem, 'Flag7') :-
    findall(
        paragem(G, L, Lo, E, T, 'Yes', Op, C, N, F),
        paragem(G, L, Lo, E, T, 'Yes', Op, C, N, F),
        ListaParagens
    ),
	profundidade_func_varios(Origem, Destino, Caminho, TempoViagem, ListaParagens).

% Função de pesquisa para a Query 10
pesquisaProfundidade_varios(Origem, Destino, [(Origem, 'Inicio')|Caminho], TempoViagem, 'Flag10') :-
    findall(
        paragem(G, L, Lo, 'Bom', T, Pub, Op, C, N, F),
        paragem(G, L, Lo, 'Bom', T, Pub, Op, C, N, F),
        ListaParagens
    ),
	profundidade_func_varios(Origem, Destino, Caminho, TempoViagem, ListaParagens).


% Função de pesquisa para a Query 8
pesquisaProfundidade_varios(Origem, Destino, [(Origem, 'Inicio')|Caminho], TempoViagem, AbrigosPossiveis, 'Flag8') :-
    paragensComAbrigo(AbrigosPossiveis, [], NovaListaParagens),
	profundidade_func_varios(Origem, Destino, Caminho, TempoViagem, NovaListaParagens).

profundidade_func_varios(Destino, Destino, [], T, _) :- 
    !,
    T is 0.

profundidade_func_varios(Paragem, Destino, [(ProxParagem, Carreira)|Caminho], TempoViagem, ListaParagens) :- 
    adjacente(Paragem, ProxParagem, Carreira, T1, ListaParagens),
    ((T1 == 'N/A') -> TLimpo is 0.0 ; TLimpo is T1),
    profundidade_func_varios(ProxParagem, Destino, Caminho, T, ListaParagens),
    TempoViagem is T + TLimpo + 5.                          % Adiciona 5 min entre paragens de autocarros

% pesquisaProfundidade_varios(183, 608, Caminho, Tempo).

% Lista de Paragens com as Operadoras pretendidas
paragensComOperadoras([], His, His).
paragensComOperadoras([Op|Tail], His, NovaListaParagens) :-
    findall(
        paragem(G, La, Lo, E, T, P, Op, C, N, F),
        paragem(G, La, Lo, E, T, P, Op, C, N, F),
        Lista
    ),
    append(Lista, His, R),
    paragensComOperadoras(Tail, R, NovaListaParagens).

% Lista de Paragens com Abrigo
paragensComAbrigo([], His, His).
paragensComAbrigo([Abrigo|Tail], His, NovaListaParagens) :-
    findall(
        paragem(G, La, Lo, E, Abrigo, P, Op, C, N, F),
        paragem(G, La, Lo, E, Abrigo, P, Op, C, N, F),
        Lista
    ),
    append(Lista, His, R),
    paragensComAbrigo(Tail, R, NovaListaParagens).


%--------------------------------------------------------------------------------------------------------
% PESQUISA EM LARGURA

pesquisaLargura(Origem, Destino, Caminho) :-
    findall(
        paragem(Gid, Lat, Long, Estado, TipoAbrigo, Publicidade, Operadora, Codigo, NomeRua, Freguesia),
        paragem(Gid, Lat, Long, Estado, TipoAbrigo, Publicidade, Operadora, Codigo, NomeRua, Freguesia),
        ListaParagens
    ),
    paragem(Origem, _, _, Estado, _, _, Operadora, _, NomeRua, _),
    pesquisaLargura_fun([[(Origem, 'Inicio', Operadora, Estado, NomeRua)]], Destino, C, ListaParagens),
    inverso(C, Caminho).

pesquisaLargura_fun([[(Destino, Carreira, _, _, _)|Path]|_], Destino, [(Destino, Carreira, _, _, _)|Path], _).

pesquisaLargura_fun([[(Paragem, Carreira, Operadora, Estado, NomeRua)|Path]|Paths], Destino, Caminho, ListaParagens) :-
    bagof(
        [(M, C1, Op1, E1, Nom1), (Paragem, Carreira, Operadora, Estado, NomeRua)|Path],
        (
            adjacente_largura(Paragem, M, C1, Op1, E1, Nom1, ListaParagens), 
            \+ member((M, C1, Op1, E1, Nom1), 
            [(Paragem, Carreira, Operadora, Estado, NomeRua) | Path])
        ),
        NewPaths
    ),
    append(Paths, NewPaths, Pathsl), !,
    pesquisaLargura_fun(Pathsl, Destino, Caminho, ListaParagens);
    pesquisaLargura_fun(Paths, Destino, Caminho, ListaParagens).

adjacente_largura(Paragem, ProxParagem, Carreira, Operadora, Estado, NomeRua, ListaParagens) :-
    member( 
        paragem(Paragem, _, _, _, _, _, _, _, _, _),
        ListaParagens
        ),
    member( 
        paragem(ProxParagem, _, _, Estado, _, _, Operadora, _, NomeRua, _),
        ListaParagens
        ),
    viagem(Carreira, Paragem, ProxParagem, _).

% pesquisaLargura(183, 595, Caminho).
% pesquisaLargura(183, 250, Caminho).

% pesquisaLargura(354, 79, Caminho).

pesquisaLarguraSimplificado(Origem, Destino, Caminho) :-
    findall(
        paragem(Gid, Lat, Long, Estado, TipoAbrigo, Publicidade, Operadora, Codigo, NomeRua, Freguesia),
        paragem(Gid, Lat, Long, Estado, TipoAbrigo, Publicidade, Operadora, Codigo, NomeRua, Freguesia),
        ListaParagens
    ),
    pesquisaLarguraSimplificado_fun([[(Origem, 'Inicio')]], Destino, C, ListaParagens),
    inverso(C, Caminho).

pesquisaLarguraSimplificado_fun([[(Destino, Carreira)|Path]|_], Destino, [(Destino, Carreira)|Path], _).

pesquisaLarguraSimplificado_fun([[(Paragem, Carreira)|Path]|Paths], Destino, Caminho, ListaParagens) :-
    bagof(
        [(M, C1), (Paragem, Carreira)|Path],
        (
            adjacente(Paragem, M, C1, _, ListaParagens), 
            \+ member((M, C1), 
            [(Paragem, Carreira) | Path])
        ),
        NewPaths
    ),
    append(Paths, NewPaths, Pathsl), !,
    pesquisaLarguraSimplificado_fun(Pathsl, Destino, Caminho, ListaParagens);
    pesquisaLarguraSimplificado_fun(Paths, Destino, Caminho, ListaParagens).

% pesquisaLarguraSimplificado(183, 595, Caminho).
% pesquisaLarguraSimplificado(354, 79, Caminho).



%--------------------------------------------------------------------------------------------------------

% PESQUISA ESTRELA

tempoEstimado((LatX, LongX), (LatY, LongY), Estima) :- 
    ((LatX == 'N/A') -> LatLimpoX is 999.9; LatLimpoX is LatX),
    ((LongX == 'N/A') -> LongLimpoX is 999.9; LongLimpoX is LongX),
    ((LatY == 'N/A') -> LatLimpoY is 999.9; LatLimpoY is LatY),
    ((LongY == 'N/A') -> LongLimpoY is 999.9; LongLimpoY is LongY),
    DeltaLat is LatLimpoX - LatLimpoY,
    DeltaLong is LongLimpoX - LongLimpoY,
    Estima is (sqrt(DeltaLat^2 + DeltaLong^2))/1000.


pesquisaAEstrela(Origem, Destino, Caminho, Tempo) :-            
    findall(
        paragem(Gid, Lat, Long, Estado, TipoAbrigo, Publicidade, Operadora, Codigo, NomeRua, Freguesia),
        paragem(Gid, Lat, Long, Estado, TipoAbrigo, Publicidade, Operadora, Codigo, NomeRua, Freguesia),
        ListaParagens
    ),
    resolve_aestrela((Origem, 'Inicio'), Destino, Caminho/Tempo, ListaParagens).    


resolve_aestrela((Origem, 'Inicio'), Destino, Caminho/Tempo, ListaParagens) :-
    paragem(Origem, LatX, LongX, _, _, _, _, _, _, _),
    paragem(Destino, LatY, LongY, _, _, _, _, _, _, _),
    tempoEstimado((LatX, LongX), (LatY, LongY), Estima),
	aestrela([[(Origem, 'Inicio')]/0/Estima], InvCaminho/Tempo/_, Destino, ListaParagens),
	inverso(InvCaminho, Caminho).

aestrela(Caminhos, Caminho, Destino, _) :-
	obtem_melhor(Caminhos, Caminho),
	Caminho = [(Paragem, _)|_]/_/_, 
    Destino == Paragem.

aestrela(Caminhos, SolucaoCaminho, Destino, ListaParagens) :-
	obtem_melhor(Caminhos, MelhorCaminho),
	seleciona(MelhorCaminho, Caminhos, OutrosCaminhos),
	expande_aestrela(MelhorCaminho, ExpCaminhos, Destino, ListaParagens),
	append(OutrosCaminhos, ExpCaminhos, NovoCaminhos),
    aestrela(NovoCaminhos, SolucaoCaminho, Destino, ListaParagens).		


obtem_melhor([Caminho], Caminho) :- !.

obtem_melhor([Caminho1/Tempo1/Est1, _/Tempo2/Est2|Caminhos], MelhorCaminho) :-
	Tempo1 + Est1 =< Tempo2 + Est2, !,
	obtem_melhor([Caminho1/Tempo1/Est1|Caminhos], MelhorCaminho).
	
obtem_melhor([_|Caminhos], MelhorCaminho) :- 
	obtem_melhor(Caminhos, MelhorCaminho).

expande_aestrela(Caminho, ExpCaminhos, Destino, ListaParagens) :-
	findall(
        NovoCaminho, 
        adjacente_aestrela(Caminho, NovoCaminho, Destino, ListaParagens), 
        ExpCaminhos
    ).

% A adição dos 5 min serve como tempo médio de espera entre viagens
adjacente_aestrela([(Paragem, Carreira1)|Caminho]/Tempo/_, [(ProxParagem, Carreira2), (Paragem, Carreira1)|Caminho]/NovoTempo/Est, 
Destino, ListaParagens) :-
	adjacente(Paragem, ProxParagem, Carreira2, PassoTempo, ListaParagens),
    \+ member((ProxParagem, Carreira2), Caminho),
	NovoTempo is Tempo + PassoTempo + 5,                    
	paragem(ProxParagem, LatX, LongX, _, _, _, _, _, _, _),
    paragem(Destino, LatY, LongY, _, _, _, _, _, _, _),
    tempoEstimado((LatX, LongX), (LatY, LongY), Est).

seleciona(E, [E|Xs], Xs).
seleciona(E, [X|Xs], [X|Ys]) :- 
    seleciona(E, Xs, Ys).


% pesquisaAEstrela(183, 79, Caminho, Tempo).


%--------------------------------------------------------------------------------------------------------

% PESQUISA GULOSA

pesquisaGulosa(Origem, Destino, Caminho, Tempo) :-            
    findall(
        paragem(Gid, Lat, Long, Estado, TipoAbrigo, Publicidade, Operadora, Codigo, NomeRua, Freguesia),
        paragem(Gid, Lat, Long, Estado, TipoAbrigo, Publicidade, Operadora, Codigo, NomeRua, Freguesia),
        ListaParagens
    ),
    resolve_gulosa((Origem, 'Inicio'), Destino, Caminho/Tempo, ListaParagens).    


resolve_gulosa((Origem, 'Inicio'), Destino, Caminho/Tempo, ListaParagens) :-
    paragem(Origem, LatX, LongX, _, _, _, _, _, _, _),
    paragem(Destino, LatY, LongY, _, _, _, _, _, _, _),
    tempoEstimado((LatX, LongX), (LatY, LongY), Estima),
	gulosa([[(Origem, 'Inicio')]/0/Estima], InvCaminho/Tempo/_, Destino, ListaParagens),
	inverso(InvCaminho, Caminho).

gulosa(Caminhos, Caminho, Destino, _) :-
	obtem_melhor_gulosa(Caminhos, Caminho),
	Caminho = [(Paragem, _)|_]/_/_, 
    Destino == Paragem.

gulosa(Caminhos, SolucaoCaminho, Destino, ListaParagens) :-
	obtem_melhor_gulosa(Caminhos, MelhorCaminho),
	seleciona(MelhorCaminho, Caminhos, OutrosCaminhos),
	expande_aestrela(MelhorCaminho, ExpCaminhos, Destino, ListaParagens),
	append(OutrosCaminhos, ExpCaminhos, NovoCaminhos),
    gulosa(NovoCaminhos, SolucaoCaminho, Destino, ListaParagens).		


obtem_melhor_gulosa([Caminho], Caminho) :- !.

obtem_melhor_gulosa([Caminho1/Tempo1/Est1, _/_/Est2|Caminhos], MelhorCaminho) :-
	Est1 =< Est2, !,
	obtem_melhor_gulosa([Caminho1/Tempo1/Est1|Caminhos], MelhorCaminho).
	
obtem_melhor_gulosa([_|Caminhos], MelhorCaminho) :- 
	obtem_melhor_gulosa(Caminhos, MelhorCaminho).


% pesquisaGulosa(183, 499, C, T).
% pesquisaGulosa(183, 250, C, T).