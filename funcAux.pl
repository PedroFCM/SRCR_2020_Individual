% Faz reload ao ficheiro 'main.pl'
reload :- consult('main.pl').

% Faz clear ao terminal
clear :- write('\33\[2J').

% ------------------------------------------------
% Funções auxiliares para as queries

% Indica se uma lista é sublista de outra, ou seja, se está contida nela.
subLista([], _).
subLista([H|Tail1], [H|Tail2]) :-
    subLista(Tail1, Tail2).
subLista([H|Tail1], [_|Tail2]) :- 
    subLista([H|Tail1], Tail2).

% ------------------------------------------------
% Funções de print 

% Dá print a uma lista de Paragens 
printParagem([]).
printParagem([Paragem|Tail]) :-
    write('Paragem -> '),
    format('~w', Paragem), nl,
    printParagem(Tail).

% Dá print a um caminho e o seu respetivo tempo
printCaminhoTempo(Caminho, Tempo) :-
    write('Trajeto efetuado: '), nl,
    printLista(Caminho),
    write('\tTempo de Viagem -> '), 
    format('~w', Tempo), write(' minutos'),
    nl.

% Dá print a uma lista de Paragens e suas respetivas carreiras
printLista([]).
printLista([(Paragem, Carreira)|Tail]) :-
    write('\tParagem: '),
    format('~w', Paragem), nl,
    write('\t -> Carreira '),
    format('~w', Carreira), nl,
    printLista(Tail).

% Dá print a uma lista de paragens, bem como a algumas das informações dessa paragem
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

% Retorna o inverso de uma lista
inverso(Xs, Ys) :-
    inverso(Xs, [], Ys).
inverso([], Xs, Xs).
inverso([X|Xs], Ys, Zs) :-
    inverso(Xs, [X|Ys], Zs).

% Verfica se um elemento pertence a uma lista
pertence(X,[X|_]).
pertence(X,[Y|_]) :-
    X \= Y,
    pertence(X, _).
