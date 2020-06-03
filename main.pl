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
/* Predicados usados

% paragem -> Gid, Latitude, Longitude, Estado de Conservação, Tipo de Abrigo, Abrigo com Publicidade,
%            Operadora, Codigo de Rua, Nome da Rua, Freguesia                                              

% viagem -> carreira, paragemInicio(gid), paragemFim(gid), tempo de viagem (em minutos)
*/

% -----------------------------------------------------------------------------------------

% Query 1
% "Calcular um trajeto entre dois pontos"

trajetoEntre2Pontos(Origem, Destino, 'Profundidade') :-
    pesquisaProfundidade_varios(Origem, Destino, Caminho, TempoViagem), 
    printCaminhoTempo(Caminho, TempoViagem).

trajetoEntre2Pontos(Origem, Destino, 'Largura') :-
    pesquisaLargura(Origem, Destino, Caminho), 
    printCaminhoMaisInfo(Caminho).

trajetoEntre2Pontos(Origem, Destino, 'LarguraSimplificado') :-
    pesquisaLarguraSimplificado(Origem, Destino, Caminho), 
    printLista(Caminho).

trajetoEntre2Pontos(Origem, Destino, 'AEstrela') :-
    pesquisaAEstrela(Origem, Destino, Caminho, Tempo),
    printCaminhoTempo(Caminho, Tempo).

trajetoEntre2Pontos(Origem, Destino, 'Gulosa') :-
    pesquisaGulosa(Origem, Destino, Caminho, Tempo),
    printCaminhoTempo(Caminho, Tempo).

% trajetoEntre2Pontos(354, 79, 'Profundidade'). Este trajeto anda em 3 carreiras diferentes
% trajetoEntre2Pontos(354, 79, 'Largura').
% trajetoEntre2Pontos(183, 250, 'LarguraSimplificado').
% trajetoEntre2Pontos(183, 595, 'AEstrela').
% trajetoEntre2Pontos(183, 595, 'Gulosa').

% -----------------------------------------------------------------------------------------

% Query 2
% "Selecionar apenas algumas das operadoras de transporte para um determinado percurso"

trajetoCertasOperadoras(Origem, Destino, Caminho, TempoViagem, Operadoras) :-    
    pesquisaProfundidade_varios(Origem, Destino, Caminho, TempoViagem, Operadoras),
    printCaminhoTempo(Caminho, TempoViagem).


% trajetoCertasOperadoras(183, 595, Caminho, TempoViagem, ['Carris']).



% -----------------------------------------------------------------------------------------

% Query 3
% "Excluir um ou mais operadores de transporte para o percurso"


% Trajeto entre 2 pontos  excluindo algumas operadoras
trajetoSemCertasOperadoras(Origem, Destino, Caminho, TempoViagem, Operadoras) :-
    findall(
        Op,
        (paragem(_, _, _, _, _, _, Op, _, _, _), \+ member(Op, Operadoras)),
        ListaOpt
    ),
    sort(ListaOpt, OperadorasPossiveis),                                % retira todas as operadoras possiveis para uma lista sem repetidos 
    pesquisaProfundidade_varios(Origem, Destino, Caminho, TempoViagem, OperadorasPossiveis),
    printCaminhoTempo(Caminho, TempoViagem).

% trajetoSemCertasOperadoras(183, 595, Caminho, TempoViagem, ['Carris']).




% -----------------------------------------------------------------------------------------

% Query 4 
% "Identificar quais as paragens com o maior número de carreiras num determinado percurso"

paragemMaisCarreiras(Origem, Destino) :-
    pesquisaProfundidade_varios(Origem, Destino, Caminho, TempoViagem),
    printCaminhoTempo(Caminho, TempoViagem),
    nl, write('----------------'), nl, nl,
    write('Paragem com mais carreiras...'), nl, nl,
    calculaCarreiras(Caminho, (Paragem, NumCarreiras)),
    write('Paragem -> '), format('~w', Paragem), nl,
    write('Numero de Carreiras -> '), format('~w', NumCarreiras), nl,
    write('\tTempo -> '), format('~w', TempoViagem).


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
    maisCarreiras([(ParagemId, NumCarreiras)|Tail], Resultado).                                    % TESTAR MELHOR

% paragemMaisCarreiras(595, 182).

% calculaCarreiras([79, 44], (P, N)).
% maisCarreiras([(1,2), (3,5), (0, 100), (23, 200), (25, 2)], R).

% -----------------------------------------------------------------------------------------

% Query 5
% "Escolher o menor percurso (usando critério menor número de paragens)"
menorPercurso(ParagemInicio, ParagemFim, Caminho, NParagens) :-
    findall(
        (Caminho, L),
        (pesquisaProfundidade_varios(ParagemInicio, ParagemFim, Caminho, _), length(Caminho, L)),
        ListaCaminho
    ),
    menosParagens(ListaCaminho, ([], 9999.9), (Caminho, NParagens)). 


menosParagens([], (Caminho, NParagens), (Caminho, NParagens)).
menosParagens([(Caminho, NParagens)|Tail], (Caminho1, NParagens1), Resultado) :- 
    NParagens =< NParagens1,
    menosParagens(Tail, (Caminho, NParagens), Resultado).
menosParagens([(Caminho, NParagens)|Tail], (Caminho1, NParagens1), Resultado) :- 
    NParagens > NParagens1,
    menosParagens(Tail, (Caminho1, NParagens1), Resultado).

% menorPercurso(183, 595, C, P).


% -----------------------------------------------------------------------------------------

% Query 6
% "Escolher o percurso mais rápido (usando critério da distância)"
trajetoMaisRapido(Origem, Destino) :-
    pesquisaAEstrela(Origem, Destino, Caminho, Tempo),
    printCaminhoTempo(Caminho, Tempo).


% trajetoMaisRapido(183, 182).



% -----------------------------------------------------------------------------------------

% Query 7
% "Escolher o percurso que passe apenas por abrigos com publicidade"
trajetoAbrigosPub(Origem, Destino) :-
    pesquisaProfundidade_varios(Origem, Destino, Caminho, Tempo, 'Flag7'),
    printCaminhoTempo(Caminho, Tempo).


% trajetoAbrigosPub(183, 185).



% -----------------------------------------------------------------------------------------

% Query 8
% "Escolher o percurso que passe apenas por paragens abrigadas"
% Esta query não aceita paragens com o "N/A"
trajetoParagemAbrigada(Origem, Destino) :-
    findall(
        Abrigo,
        (paragem(_, _, _, _, Abrigo, _, _, _, _, _), Abrigo \== 'Sem Abrigo', Abrigo \== 'N/A'),
        ListaComAbrigo
    ),
    sort(ListaComAbrigo, ParagensComAbrigo),                       % retira todas as paragens possiveis para uma lista sem repetidos 
    pesquisaProfundidade_varios(Origem, Destino, Caminho, Tempo, ParagensComAbrigo, 'Flag8'),
    printCaminhoTempo(Caminho, Tempo).


% trajetoParagemAbrigada(628, 39).
% trajetoParagemAbrigada(593, 180).
% trajetoParagemAbrigada(89, 597).


% -----------------------------------------------------------------------------------------

% Query 9
% "Escolher um ou mais pontos intermédios por onde o percurso deverá passar"
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
    printParagem(CaminhoEscolhido).
    
tiraCarreirasList([], Acc, NovaList) :-
    inverso(Acc, NovaList).
tiraCarreirasList([Caminho|Tail], Acc, NovaList) :-
    tiraCarreiras(Caminho, [], C),
    tiraCarreirasList(Tail, [C|Acc], NovaList).

tiraCarreiras([], Acc, R) :-
    inverso(Acc, R).
tiraCarreiras([(P, C)|Tail], Acc, R) :-
    tiraCarreiras(Tail, [P|Acc], R).

intermedioLista([], His, His).
intermedioLista([Caminho|Tail], His, R) :-
    caminhoIntermedio(Caminho, CInterm),
    intermedioLista(Tail, [CInterm|His], R).

caminhoIntermedio(Caminho, Res) :-
    tiraFirst(Caminho, SemFirst),
    tiraLast(SemFirst, [], Res).

tiraFirst([H|Tail], Tail).

tiraLast([H], Acc, SemLast) :-
    inverso(Acc, SemLast).
tiraLast([H|Tail], Acc, SemLast) :-
    tiraLast(Tail, [H|Acc], SemLast).


temInterm([C], Interm, C) :-
    subLista(Interm, C).

temInterm([Caminho|Tail], Interm, C) :-
    \+ subLista(Interm, Caminho),
    temInterm(Interm, Tail).

temInterm([Caminho|Tail], Interm, Caminho) :-
    subLista(Interm, Caminho).



% trajetoComPontosInterm(183, 595, [791]).
% trajetoComPontosInterm(183, 595, []).
% trajetoComPontosInterm(183, 595, [121]).


