
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI

% Autor
% Pedro Machado, a83719


% ~/bin/sicstus -l main.pl


:- [paragens].
:- [viagens].
:- [algoritmosProcura].

% -----------------------------------------------------------------------------------------
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).



% -----------------------------------------------------------------------------------------
% Predicados usados

% paragem -> gid, latitude, longitude, estado de conservação, tipo de abrigo, abrigo com publicidade,
%            codigo de rua, nome da rua, freguesia, operadora                                              ORDEM ESTÁ MAL

% viagem -> carreira, paragemInicio(gid), paragemFim(gid), tempo de viagem (em minutos)
% as viagens são entre paragens adjacentes









% --------------------------------------------------------------------------

% Query 1
% "Calcular um trajeto entre dois pontos"

% usar o pesquisaProfundiade p.e.


% --------------------------------------------------------------------------

% Query 2
% "Selecionar apenas algumas das operadoras de transporte para um determinado percurso"


trajetoCertasOperadoras(Origem, Destino, Caminho, TempoViagem, Operadoras) :-
    pesquisaProfundidadeQ2(Origem, Destino, Caminho, TempoViagem, Operadoras).


pesquisaProfundidadeQ2(Origem, Destino, Caminho, TempoViagem, Operadoras) :-
    paragem(Origem, _, _, _, _, _, Operadora, _, _, _),
    pertence(Operadora, Operadoras),
	profundidade_func(Origem, Destino, [(Origem, Operadora, 'Inicio')], Caminho, TempoViagem).

profundidade_funcQ2(Destino, Destino, His, Caminho, T, Operadoras) :-
	inverso(His, Caminho), 
    T is 0.

profundidade_funcQ2(Origem, Destino, His, Caminho, TempoViagem) :-
    adjacente_func(Origem, Prox, Carreira, T1),
	\+ member(Prox, His),
    ((T1 == 'N/A') -> TLimpo is 0.0 ; TLimpo is T1),
    paragem(Origem, _, _, _, _, _, Operadora, _, _, _),
    pertence(Operadora, Operadoras),
    profundidade_func(Prox, Destino, [(Prox, Operadora, Carreira)|His], Caminho, T),                 
    TempoViagem is T + TLimpo + 5.  % Adiciona 5 min entre paragens de autocarros

%trajetoCertasOperadoras(183, 595, Caminho, TempoViagem, ['Vimeca']).

% -----------------------------------------------------------------------------

% Query 3
% "Excluir um ou mais operadores de transporte para o percurso"

% Trajeto entre 2 pontos  excluindo algumas operadoras
trajetoSemCertasOperadoras(Origem, Destino, Caminho, TempoViagem, Operadoras) :-
    pesquisaProfundidadeQ3(Origem, Destino, Caminho, TempoViagem, Operadoras).


pesquisaProfundidadeQ3(Origem, Destino, Caminho, TempoViagem, Operadoras) :-
    paragem(Origem, _, _, _, _, _, Operadora, _, _, _),
    \+ pertence(Operadora, Operadoras),
	profundidade_func(Origem, Destino, [(Origem, Operadora, 'Inicio')], Caminho, TempoViagem).

profundidade_funcQ3(Destino, Destino, His, Caminho, T, Operadoras) :-
	inverso(His, Caminho), 
    T is 0.

profundidade_funcQ3(Origem, Destino, His, Caminho, TempoViagem) :-
    adjacente_func(Origem, Prox, Carreira, T1),
	\+ member(Prox, His),
    ((T1 == 'N/A') -> TLimpo is 0.0 ; TLimpo is T1),
    paragem(Origem, _, _, _, _, _, Operadora, _, _, _),
    \+ pertence(Operadora, Operadoras),
    profundidade_func(Prox, Destino, [(Prox, Operadora, Carreira)|His], Caminho, T),                 
    TempoViagem is T + TLimpo + 5.  % Adiciona 5 min entre paragens de autocarros

% -----------------------------------------------------------------------------

% Query 4 
% "Identificar quais as paragens com o maior número de carreiras num determinado percurso"

paragemMaisCarreiras(Percurso, (Paragem, NumCarreiras)) :-
    paragemMaisCarreiras_fun(Percurso, [], (Paragem, NumCarreiras)).

paragemMaisCarreiras_fun([], His, (Paragem, NumCarreiras)) :-
    maisCarreiras(His, (Paragem, NumCarreiras)).

paragemMaisCarreiras_fun([ParagemId|Tail], His, Resultado) :-
    findall(C, viagem(C, ParagemId, _, _), ListaCarreirasInicio),
    findall(C, viagem(C, _, ParagemId, _), ListaCarreirasFim),
    length(ListaCarreirasInicio, Li),
    length(ListaCarreirasFim, Lf),
    (Li > Lf -> L is Li ; L is Lf),
    paragemMaisCarreiras_fun(
        Tail, 
        [(ParagemId, L)|His],
        Resultado
        ).

maisCarreiras([(ParagemId, NumCarreiras)], (ParagemId, NumCarreiras)).

maisCarreiras([(ParagemId, NumCarreiras), (P1, NC1)|Tail], Resultado) :-
    NumCarreiras =< NC1,
    maisCarreiras([(P1, NC1)|Tail], Resultado).

maisCarreiras([(ParagemId, NumCarreiras), (P1, NC1)|Tail], Resultado) :-
    NumCarreiras > NC1,
    maisCarreiras([(ParagemId, NumCarreiras)|Tail], Resultado).

% paragemMaisCarreiras([79, 44], (P, N)).

% maisCarreiras([(1,2), (3,5), (0, 100), (23, 200), (25, 2)], R).

% -----------------------------------------------------------------------------

% Query 5
% "Escolher o menor percurso (usando critério menor número de paragens)"
% Através do algoritmo AEstrela
menorPercurso(ParagemInicio, ParagemFim, Caminho, NParagens) :-
    menorPercurso_fun(ParagemInicio, ParagemFim, Caminho/NP),
    NParagens is NP + 1.        % compensar a contagem da primeira paragem

menorPercurso_fun(Origem, Destino, Caminho/Tempo) :-
	aestrela_MenorPercurso([[Origem]/0/Estima], InvCaminho/Tempo/_, Destino),               % VER SE ESTÁ A TER UM COMPORTAMENTO CORRETO
	inverso(InvCaminho, Caminho).

aestrela_MenorPercurso(Caminhos, Caminho, Destino) :-
	obtem_MenorPercurso(Caminhos, Caminho, Destino),
	Caminho = [Nodo|_]/_/_, 
    Destino == Nodo.

aestrela_MenorPercurso(Caminhos, SolucaoCaminho, Destino) :-
	obtem_MenorPercurso(Caminhos, MelhorCaminho, Destino),
	seleciona(MelhorCaminho, Caminhos, OutrosCaminhos),
	expande_aestrela_MP(MelhorCaminho, ExpCaminhos, Destino),
	append(OutrosCaminhos, ExpCaminhos, NovoCaminhos),
    aestrela_MenorPercurso(NovoCaminhos, SolucaoCaminho, Destino).		


obtem_MenorPercurso([Caminho], Caminho, Destino) :- 
    !.

obtem_MenorPercurso([Caminho1/Nparagens/Est1, _/Nparagens2/Est2|Caminhos], MelhorCaminho, Destino) :-
	Nparagens + Est1 =< Nparagens2 + Est2, 
    !,
	obtem_MenorPercurso([Caminho1/Nparagens/Est1|Caminhos], MelhorCaminho, Destino).
	
obtem_MenorPercurso([_|Caminhos], MelhorCaminho, Destino) :- 
	obtem_MenorPercurso(Caminhos, MelhorCaminho, Destino).

expande_aestrela_MP(Caminho, ExpCaminhos, Destino) :-
	findall(
        NovoCaminho, 
        adjacente_MP(Caminho, NovoCaminho, Destino),
        ExpCaminhos
        ).

adjacente_MP([Nodo|Caminho]/Nparagens/_, [ProxNodo,Nodo|Caminho]/Novo_Nparagens/Novo_Nparagens, Destino) :-
	viagem(C, Nodo, ProxNodo, _),
    \+ member(ProxNodo, Caminho),
	Novo_Nparagens is Nparagens + 1.


% ----------------------------------------------------------------------------------------------------------

% Query 6
% "Escolher o percurso mais rápido (usando critério da distância)"





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


