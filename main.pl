%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI

% Autor
% Pedro Machado, a83719


% ~/bin/sicstus -l main.pl


:- [paragens].
:- [viagens].
:- [algoritmosProcura].
:- [funcAux].

% -----------------------------------------------------------------------------------------
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).



% -----------------------------------------------------------------------------------------
/* Principais predicados usados

    paragem -> Gid, Latitude, Longitude, Estado de Conservação, Tipo de Abrigo, Abrigo com Publicidade,
            Operadora, Codigo de Rua, Nome da Rua, Freguesia                                              

    viagem -> carreira, paragemInicio(gid), paragemFim(gid), tempo de viagem (em minutos)

*/



% -----------------------------------------------------------------------------------------

% Query 1
% "Calcular um trajeto entre dois pontos"

% Resolver a Query1 através de um algoritmo de Procura em Profundidade (DFS)
% Trata-se de uma procura não informada
trajetoEntre2Pontos(Origem, Destino, 'Profundidade') :-   
    pesquisaProfundidade_varios(Origem, Destino, Caminho, TempoViagem), 
    printCaminhoTempo(Caminho, TempoViagem).

% Resolver a Query1 através de um algoritmo de Procura em Largura (BFS)
% Trata-se de uma procura não informada
trajetoEntre2Pontos(Origem, Destino, 'Largura') :-
    pesquisaLargura(Origem, Destino, Caminho),
    printCaminhoMaisInfo(Caminho).

% Resolver a Query1 através de um algoritmo de Procura em Largura (BFS)
% Trata-se de uma procura não informada
% Este algoritmo difere do anterior, pois dá resultados mais simples
trajetoEntre2Pontos(Origem, Destino, 'LarguraSimplificado') :-
    pesquisaLarguraSimplificado(Origem, Destino, Caminho),
    printLista(Caminho).

% Resolver a Query1 através de um algoritmo de Procura por a* (AEstrela)
% Trata-se de uma procura informada
trajetoEntre2Pontos(Origem, Destino, 'AEstrela') :-
    pesquisaAEstrela(Origem, Destino, Caminho, Tempo),
    printCaminhoTempo(Caminho, Tempo).

% Resolver a Query1 através de um algoritmo de Procura Gulosa (Greedy Search)
% Trata-se de uma procura informada
trajetoEntre2Pontos(Origem, Destino, 'Gulosa') :-
    pesquisaGulosa(Origem, Destino, Caminho, Tempo),
    printCaminhoTempo(Caminho, Tempo).


% trajetoEntre2Pontos(354, 79, 'Profundidade').                     trajetoEntre2Pontos(183, 595, 'Profundidade', R).
% trajetoEntre2Pontos(354, 79, 'Largura').
% trajetoEntre2Pontos(183, 250, 'LarguraSimplificado').             trajetoEntre2Pontos(183, 595, 'LarguraSimplificado', R).
% trajetoEntre2Pontos(183, 595, 'AEstrela').
% trajetoEntre2Pontos(183, 595, 'Gulosa').



% -----------------------------------------------------------------------------------------

% Query 2
% "Selecionar apenas algumas das operadoras de transporte para um determinado percurso"

trajetoCertasOperadoras(Origem, Destino, Caminho, TempoViagem, Operadoras) :-    
    pesquisaProfundidade_varios(Origem, Destino, Caminho, TempoViagem, Operadoras),
    printCaminhoTempo(Caminho, TempoViagem).


% trajetoCertasOperadoras(183, 595, Caminho, TempoViagem, ['Carris']).
% trajetoCertasOperadoras(183, 595, Caminho, TempoViagem, ['Vimeca']).


% -----------------------------------------------------------------------------------------

% Query 3
% "Excluir um ou mais operadores de transporte para o percurso"

% Retira todas as operadoras possiveis (através do "findall") para uma lista sem repetidos (com o "sort")
trajetoSemCertasOperadoras(Origem, Destino, Caminho, TempoViagem, Operadoras) :-
    findall(
        Op,
        (paragem(_, _, _, _, _, _, Op, _, _, _), \+ member(Op, Operadoras)),
        ListaOpt
    ),
    sort(ListaOpt, OperadorasPossiveis),                               
    pesquisaProfundidade_varios(Origem, Destino, Caminho, TempoViagem, OperadorasPossiveis),
    printCaminhoTempo(Caminho, TempoViagem).


% trajetoSemCertasOperadoras(183, 595, Caminho, TempoViagem, ['Carris']).
% trajetoSemCertasOperadoras(183, 595, Caminho, TempoViagem, ['Vimeca']).


% -----------------------------------------------------------------------------------------

% Query 4 
% "Identificar quais as paragens com o maior número de carreiras num determinado percurso"

% Depois de saber o percurso, faz print do trajeto escolhido e, de seguida, apresenta a paragem desse percurso 
% que possui mais carreiras
paragemMaisCarreiras(Origem, Destino) :-
    pesquisaProfundidade_varios(Origem, Destino, Caminho, TempoViagem),
    printCaminhoTempo(Caminho, TempoViagem),
    nl, write('----------------'), nl, nl,
    write('Paragem com mais carreiras...'), nl, nl,
    calculaCarreiras(Caminho, (Paragem, NumCarreiras)),
    write('Paragem -> '), format('~w', Paragem), nl,
    write('Numero de Carreiras -> '), format('~w', NumCarreiras), nl,
    write('\tTempo -> '), format('~w', TempoViagem).

% Dado um percurso determina o número de carreiras para cada paragem e apresenta a que tem mais
calculaCarreiras(Percurso, (Paragem, NumCarreiras)) :-
    calculaCarreiras_fun(Percurso, [], (Paragem, NumCarreiras)).

calculaCarreiras_fun([], His, (Paragem, NumCarreiras)) :-
    maisCarreiras(His, (Paragem, NumCarreiras)).

calculaCarreiras_fun([(ParagemId, _)|Tail], His, Resultado) :-
    findall(C, viagem(C, ParagemId, _, _), ListaCarreirasInicio),
    findall(C, viagem(C, _, ParagemId, _), ListaCarreirasFim),
    length(ListaCarreirasInicio, Li),
    length(ListaCarreirasFim, Lf),
    (Li > Lf -> L is Li ; L is Lf),
    calculaCarreiras_fun(
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


% paragemMaisCarreiras(595, 182).



% -----------------------------------------------------------------------------------------

% Query 5
% "Escolher o menor percurso (usando critério menor número de paragens)"

% Determina todas os caminhos possiveis e, de seguida, indica o que contém o menor número de paragens
menorPercurso(ParagemInicio, ParagemFim, Caminho, NParagens) :-
    findall(
        (Caminho, L),
        (pesquisaProfundidade_varios(ParagemInicio, ParagemFim, Caminho, _), length(Caminho, L)),
        ListaCaminho
    ),
    menosParagens(ListaCaminho, ([], 9999.9), (Caminho, NParagens)),
    printParagem(Caminho). 


% Dado uma lista de caminhos, indica o menor número de paragens e o respetivo caminho
menosParagens([], (Caminho, NParagens), (Caminho, NParagens)).
menosParagens([(Caminho, NParagens)|Tail], (Caminho1, NParagens1), Resultado) :- 
    NParagens =< NParagens1,
    menosParagens(Tail, (Caminho, NParagens), Resultado).
menosParagens([(Caminho, NParagens)|Tail], (Caminho1, NParagens1), Resultado) :- 
    NParagens > NParagens1,
    menosParagens(Tail, (Caminho1, NParagens1), Resultado).


% menorPercurso(183, 595, Caminho, NumeroParagens).



% -----------------------------------------------------------------------------------------

% Query 6
% "Escolher o percurso mais rápido (usando critério da distância)"

% Esta query usa uma procura informada para determinar o caminho com menor custo a nível de tempo/distncia.
% Visto que o tempo está relacionado com a distância através de uma relação: 1Km - 1min, sabemos que o 
% caminho com menor tempo terá, tabém a menor distância
trajetoMaisRapido(Origem, Destino) :-
    pesquisaAEstrela(Origem, Destino, Caminho, Tempo),
    printCaminhoTempo(Caminho, Tempo).


% trajetoMaisRapido(183, 182).



% -----------------------------------------------------------------------------------------

% Query 7
% "Escolher o percurso que passe apenas por abrigos com publicidade"

% É chamada a função de pesquisa em profundidade com a flag 'Flag7' para indicar que 
% se pretende calcular um caminho que vá de encontro com a query pedida, neste caso a 7
trajetoAbrigosPub(Origem, Destino) :-
    pesquisaProfundidade_varios(Origem, Destino, Caminho, Tempo, 'Flag7'),
    printCaminhoTempo(Caminho, Tempo).


% trajetoAbrigosPub(180, 185).


% -----------------------------------------------------------------------------------------

% Query 8
% "Escolher o percurso que passe apenas por paragens abrigadas"

% Esta query não aceita paragens com o "N/A", aceitando apenas paragens abrigadas
% É usado o findall seguido do sort para identificar
trajetoParagemAbrigada(Origem, Destino) :-
    findall(
        Abrigo,
        (paragem(_, _, _, _, Abrigo, _, _, _, _, _), Abrigo \== 'Sem Abrigo', Abrigo \== 'N/A'),
        ListaComAbrigo
    ),
    sort(ListaComAbrigo, ParagensComAbrigo),                       
    pesquisaProfundidade_varios(Origem, Destino, Caminho, Tempo, ParagensComAbrigo, 'Flag8'),
    printCaminhoTempo(Caminho, Tempo).


% trajetoParagemAbrigada(628, 39).
% trajetoParagemAbrigada(593, 180).
% trajetoParagemAbrigada(89, 597).


% -----------------------------------------------------------------------------------------

% Query 9
% "Escolher um ou mais pontos intermédios por onde o percurso deverá passar"

% Esta query não aceita quando a lista de pontos intermédios é vazia, tendo de ter pelo menos 1 ponto
trajetoComPontosInterm(Origem, Destino, Interm) :-
    findall(
        Caminho,
        (pesquisaProfundidade_varios(Origem, Destino, Caminho, _)),
        ListaCaminho
    ),
    Interm \= [],
    tiraCarreirasList(ListaCaminho, [], NovaListaCaminho),
    intermedioLista(NovaListaCaminho, [], L),
    temInterm(L, Interm, CaminhoEscolhido),
    append([Origem], CaminhoEscolhido, R),
    addUltimoElem(Destino, R, Res),
    printParagem(Res).
    
% Função que recebe uma lista de caminhos e retira para cada elemento (caminho),
% as carreiras de cada paragem. Ficamos, assim, com uma lista de caminhos representados apenas
% pelas paragens.
tiraCarreirasList([], Acc, NovaList) :-
    inverso(Acc, NovaList).
tiraCarreirasList([Caminho|Tail], Acc, NovaList) :-
    tiraCarreiras(Caminho, [], C),
    tiraCarreirasList(Tail, [C|Acc], NovaList).

tiraCarreiras([], Acc, R) :-
    inverso(Acc, R).
tiraCarreiras([(P, C)|Tail], Acc, R) :-
    tiraCarreiras(Tail, [P|Acc], R).

% Calcula os pontos intermedios de um caminho para todos os caminhos da lista
intermedioLista([], His, His).
intermedioLista([Caminho|Tail], His, R) :-
    caminhoIntermedio(Caminho, CInterm),
    intermedioLista(Tail, [CInterm|His], R).

% Calcula os pontos intermedios de um caminho
caminhoIntermedio(Caminho, Res) :-
    tiraFirst(Caminho, SemFirst),
    tiraLast(SemFirst, [], Res).

% Retira o primeiro elemento de uma lista
tiraFirst([H|Tail], Tail).

% Retira o ultimo elemento de uma lista
tiraLast([H], Acc, SemLast) :-
    inverso(Acc, SemLast).
tiraLast([H|Tail], Acc, SemLast) :-
    tiraLast(Tail, [H|Acc], SemLast).

% Verifica se a lista de pontos intermedios é sublista da lista intermedia do caminho, ou seja,
% verifica se o caminho passa nos pontos intermedios dados
temInterm([C], Interm, C) :-
    subLista(Interm, C).
temInterm([Caminho|Tail], Interm, C) :-
    \+ subLista(Interm, Caminho),
    temInterm(Tail, Interm, C).
temInterm([Caminho|Tail], Interm, Caminho) :-
    subLista(Interm, Caminho).


% trajetoComPontosInterm(183, 595, [791]).
% trajetoComPontosInterm(183, 595, []).
% trajetoComPontosInterm(183, 595, [121]).


% -----------------------------------------------------------------------------------------

% Queries Extra

% Query 10
% "Escolher um percurso que passe apenas por paragens em Bom estado de conservação"

trajetoParagemBomEst(Origem, Destino) :-
    pesquisaProfundidade_varios(Origem, Destino, Caminho, Tempo, 'Flag10'),
    printCaminhoTempo(Caminho, Tempo).


% trajetoParagemBomEst(183, 595).

% Query 11
% "Identificar qual o Nome das Ruas iniciais e finais de um trajeto"

trajetoNomeRua(Origem, Destino) :-
    paragem(Origem, _, _, _, _, _, _, _, NomeRuaOrig, _),
    paragem(Destino, _, _, _, _, _, _, _, NomeRuaDest, _),
    pesquisaProfundidade_varios(Origem, Destino, Caminho, Tempo),
    printCaminhoTempo(Caminho, Tempo),
    write('\tNome da Rua da Paragem Inicial: '),
    format('~w', NomeRuaOrig), nl,
    write('\tNome da Rua da Paragem Final: '),
    format('~w', NomeRuaDest), nl.

% trajetoNomeRua(183, 79).

% -----------------------------------------------------------------------------------------
% Funções para comparar Tempo de execução de diferentes algoritmos
trajetoEntre2Pontos(Origem, Destino, 'Profundidade', R) :-   % R em milisegundos
    statistics(runtime,[Start|_]),
    pesquisaProfundidade_varios(Origem, Destino, Caminho, TempoViagem), 
    statistics(runtime,[Stop|_]),
    R is Stop - Start,
    printCaminhoTempo(Caminho, TempoViagem).

trajetoEntre2Pontos(Origem, Destino, 'LarguraSimplificado', R) :-
    statistics(runtime,[Start|_]),
    pesquisaLarguraSimplificado(Origem, Destino, Caminho),
    statistics(runtime,[Stop|_]),
    R is Stop - Start, 
    printLista(Caminho).