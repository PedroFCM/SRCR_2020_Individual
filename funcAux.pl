% Faz reload ao ficheiro 'main.pl'
reload :- consult('main.pl').

% Faz clear ao terminal
clear :- write('\33\[2J').

% ------------------------------------------------
% Funções auxiliares para as queries

subLista([], _).
subLista([X|XS], [X|XSS]) :-
    subLista( XS, XSS ).
subLista([X|XS], [_|XSS]) :- 
    subLista([X|XS], XSS).

% ------------------------------------------------
% Funções de print 
printParagem([]).
printParagem([Paragem|Tail]) :-
    write('Paragem -> '),
    format('~w', Paragem), nl,
    printParagem(Tail).

printCaminhoTempo(Caminho, Tempo) :-
    write('Trajeto efetuado: '), nl,
    printLista(Caminho),
    write('\tTempo de Viagem -> '), 
    format('~w', Tempo), write(' minutos'),
    nl.

printLista([]).
printLista([(Paragem, Carreira)|Tail]) :-
    write('\tParagem: '),
    format('~w', Paragem), nl,
    write('\t -> Carreira '),
    format('~w', Carreira), nl,
    printLista(Tail).


printCaminhoMaisInfo([]).
printCaminhoMaisInfo([(Paragem, Carreira, Operadora, Estado, NomeRua)|Tail]) :-
    write('Paragem: '),
    format('~w', Paragem), nl,
    write('\tCarreira -> '),
    format('~w', Carreira), nl,
    write('\tOperadora -> '),
    format('~w', Operadora), nl,
    write('\tEstado -> '),
    format('~w', Estado), nl,
    write('\tNome da Rua -> '),
    format('~w', NomeRua), nl,
    printCaminhoMaisInfo(Tail). 

% ------------------------------------------------
% Funções auxiliares gerais

inverso(Xs, Ys) :-
    inverso(Xs, [], Ys).
inverso([], Xs, Xs).
inverso([X|Xs], Ys, Zs) :-
    inverso(Xs, [X|Ys], Zs).

pertence(X,[X|_]).
pertence(X,[Y|_]) :-
    X \= Y,
    pertence(X, _).
