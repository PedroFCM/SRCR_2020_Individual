
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

adjacente(Nodo, ProxNodo, Carreira, TempoViagem, ListaParagens) :-
    member( 
        paragem(Nodo, _, _, _, _, _, _, _, _, _),
        ListaParagens
        ),
    member( 
        paragem(ProxNodo, _, _, _, _, _, _, _, _, _),
        ListaParagens
        ),
    viagem(Carreira, Nodo, ProxNodo, TempoViagem).

% pesquisaProfundidade(183, 608, Caminho, Tempo).
% pesquisaProfundidade(183, 595, Caminho, Tempo).

pesquisaProfundidade_varios(Origem, Destino, [(Origem, 'Inicio')|Caminho], TempoViagem) :-
    findall(
        paragem(Gid, Lat, Long, Estado, TipoAbrigo, Publicidade, Operadora, Codigo, NomeRua, Freguesia),
        paragem(Gid, Lat, Long, Estado, TipoAbrigo, Publicidade, Operadora, Codigo, NomeRua, Freguesia),
        ListaParagens
        ),
	profundidade_func_varios(Origem, Destino, Caminho, TempoViagem, ListaParagens).

profundidade_func_varios(Destino, Destino, [], T, _) :- 
    !,
    T is 0.

profundidade_func_varios(Paragem, Destino, [(ProxParagem, Carreira)|Caminho], TempoViagem, ListaParagens) :- 
    adjacente(Paragem, ProxParagem, Carreira, T1, ListaParagens),
    ((T1 == 'N/A') -> TLimpo is 0.0 ; TLimpo is T1),
    profundidade_func_varios(ProxParagem, Destino, Caminho, T, ListaParagens),
    TempoViagem is T + TLimpo + 5.

% pesquisaProfundidade_varios(183, 608, Caminho, Tempo).


%--------------------------------------------------------------------------------------------------------

% PESQUISA EM LARGURA

pesquisaLargura(Origem, Destino, Caminho) :-
    findall(
        paragem(Gid, Lat, Long, Estado, TipoAbrigo, Publicidade, Operadora, Codigo, NomeRua, Freguesia),
        paragem(Gid, Lat, Long, Estado, TipoAbrigo, Publicidade, Operadora, Codigo, NomeRua, Freguesia),
        ListaParagens
        ),
    pesquisaLargura_fun([[(Origem, 'Inicio')]], Destino, C, ListaParagens),
    inverso(C, Caminho).

pesquisaLargura_fun([[(Destino, Carreira)|Path]|_], Destino, [(Destino, Carreira)|Path], _).

pesquisaLargura_fun([[(Nodo, Carreira)|Path]|Paths], Destino, Caminho, ListaParagens) :-
    bagof(
        [(M, C1), (Nodo, Carreira)|Path],
        (
            adjacente(Nodo, M, C1, _, ListaParagens), 
            \+ member((M, C1), 
            [(Nodo, Carreira) | Path])
        ),
        NewPaths
        ),
    append(Paths, NewPaths, Pathsl), !,
    pesquisaLargura_fun(Pathsl, Destino, Caminho, ListaParagens);
    pesquisaLargura_fun(Paths, Destino, Caminho, ListaParagens).

% pesquisaLargura(183, 595, Caminho).



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

pesquisaAEstrela(Origem, Destino, Caminho, Tempo) :-            % NÃO ESTÁ A DAR
    findall(
        paragem(Gid, Lat, Long, Estado, TipoAbrigo, Publicidade, Operadora, Codigo, NomeRua, Freguesia),
        paragem(Gid, Lat, Long, Estado, TipoAbrigo, Publicidade, Operadora, Codigo, NomeRua, Freguesia),
        ListaParagens
        ),
    resolve_aestrela(Origem, Destino, Caminho/Tempo, ListaParagens).    


resolve_aestrela(Origem, Destino, Caminho/Tempo, ListaParagens) :-
    paragem(Origem, LatX, LongX, _, _, _, _, _, _, _),
    paragem(Destino, LatY, LongY, _, _, _, _, _, _, _),
    tempoEstimado((LatX, LongX), (LatY, LongY), Estima),
	aestrela([[Origem]/0/Estima], InvCaminho/Tempo/_, Destino, ListaParagens),
	inverso(InvCaminho, Caminho).

aestrela(Caminhos, Caminho, Destino, _) :-
	obtem_melhor(Caminhos, Caminho),
	Caminho = [Nodo|_]/_/_, 
    Destino == Nodo.

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

adjacente_aestrela([Nodo|Caminho]/Tempo/_, [ProxNodo,Nodo|Caminho]/NovoTempo/Est, Destino, ListaParagens) :-
	adjacente(_, Nodo, ProxNodo, PassoTempo, ListaParagens),
    \+ member(ProxNodo, Caminho),
	NovoTempo is Tempo + PassoTempo,
	paragem(ProxNodo, LatX, LongX, _, _, _, _, _, _, _),
    paragem(Destino, LatY, LongY, _, _, _, _, _, _, _),
    tempoEstimado((LatX, LongX), (LatY, LongY), Est).

seleciona(E, [E|Xs], Xs).
seleciona(E, [X|Xs], [X|Ys]) :- 
    seleciona(E, Xs, Ys).


% pesquisaAEstrela(183, 791, Caminho, Tempo).
