% vim: set ft=prolog:

% Neste arquivo estão especificados os predicados que devem ser implementados.
% Você pode criar predicados auxiliares quando necessário.
%
% No arquivo resame_testes.pl estão os testes para alguns predicados.
%
% Para implementar cada predicado, primeiro você deve ler e entender a
% especificação e o teste.
%
% A especificação dos parâmetros dos predicados segue o formato descrito em
% http://www.swi-prolog.org/pldoc/doc_for?object=section%282,%274.1%27,swi%28%27/doc/Manual/preddesc.html%27%29%29
%
% Um Jogo same é representado por uma lista de colunas, sem os elementos nulos
% (zeros).
% Por exemplo, o jogo
% 2 | 3 0 0 0
% 1 | 2 2 2 0
% 0 | 2 3 3 1
% --+--------
%   | 0 1 2 3
% é representado como [[2, 2, 3], [3, 2], [3, 2], [1]].
% O tamanho deste jogo é 3x4 (linhas x colunas).
%
% Uma posição no jogo é representado por uma estrutura pos com dois argumentos
% (lin, col), onde lin é o número da linha e col é o número da coluna.  No
% exemplo anterior, a posição pos(0, 1) tem cor 3, e a posição pos(1, 2) tem
% cor 2.

% Você pode utilizar os predicados definidos no arquivo resame_utils.pl
:- consult(resame_utils).

%% main(+File) is det
%
%  Carrega um jogo same do arquivo File e imprime uma resolução na saída padrão
%  ou sem-solucao se o jogo não tem solução.

main(File) :-
    writeln(File), fail.

%% solve(+Same, -Moves) is nondet
%
%  Verdadeiro se Moves é uma sequência de jogadas (lista de posições) que
%  quando realizadas ("clicadas") resolvem o jogo Same.
%  Este predicado não tem teste de unidade. Ele é testado pelo testador.

solve([], []).
solve(Same, [M|Moves]) :-
    group(Same, Group),
    remove_group(Same, Group, NewSame),
    [M|_] = Group,
    solve(NewSame, Moves).

%% group(+Same, ?Group) is nondet
%
%  Verdadeiro se Group é um grupo de Same. Group é uma lista de posições
%  (estrutura pos(lin,col)). Este predicado é não determinístico e deve ser
%  capaz de gerar todos os grupos de Same. Este predicado não deve gerar grupos
%  repetidos. Este predicado e group/3 para vão utilizar os mesmos precicados
%  auxiliares.

group(Same, Group) :- 
	findall(P, posicoes(Same,P), Pontos),
	%posicoes(Same,Pontos),
	group2(Same,Pontos,[],K),
	member(Group,K).

split(L) :- 
	split(L, S).

split([], H).

split([H|_], H).

split([H|T], H) :- 
	write(2),
	split(T, _), !.
	
group2(Same, [], Group, Group).


group2(Same, [H|T],Group, Gamb) :-
	group(Same,H,GrupoP),
	sort(GrupoP,GrupoO),
	not(member(GrupoO, Group)),
	group2(Same, T, [GrupoO|Group],Gamb), !.
    	%writeln([Same, Group]), fail.

group2(Same, [H|T], Group, Gamb) :-
	group2(Same,T,Group,Gamb).
	

%% grupo(+Same, +P, -Group) is semidet
%
%  Verdadeiro se Group é um grupo de Same que contém a posição P.
group(Same, P, Group) :- 
	%lista_de_colunas(Same, T),
	compara-vizinhos(Same, [], [P], Group).


compara-vizinhos(Same,R,[],R) :- 
	length(R,T),
	T > 1, !.
	
compara-vizinhos(Same, Vizitados , [H|T], Mesmacor) :-
	not(member(H,Vizitados)),
	findall(Elemento,vizinhos(Same,H,Elemento), Vizinhos),
	append(Vizinhos,T,NovoC),
	compara-vizinhos(Same, [H|Vizitados], NovoC, Mesmacor),!.

compara-vizinhos(Same, Vizitados, [_|T], Mesmacor) :-
        compara-vizinhos(Same, Vizitados, T, Mesmacor).

%% remove_group(+Same, +Group, -NewSame) is semidet
%
%  Verdadeiro se NewSame é obtido de Same removendo os elemento especificados
%  em Group. A remoção é feita de acordo com as regras do jogo same.
%  Dica:
%    - crie um predicado auxiliar remove_column_group, que remove os elementos
%    de uma coluna específica
remove_group(Same, Group, NewSame) :-
    writeln([Same, Group, NewSame]), fail.

%verdadeiro se vizinho é um vizinho da mesma cor
vizinhos(Same, pos(X,Y),Vizinho) :-
	Cord is X + 1,
	P1 = pos(Cord, Y),
	P2 = pos(X,Y),
	cmp(Same, P1, P2), %a cor é a mesma.
	Vizinho = P1.

vizinhos(Same, pos(X,Y),Vizinho) :-
	Cord is Y + 1,
	P1 = pos(X, Cord),
	P2 = pos(X,Y),
	cmp(Same, P1, P2), %a cor é a mesma.
	Vizinho = P1.

vizinhos(Same, pos(X,Y),Vizinho) :-
	Cord is X - 1,
	P1 = pos(Cord, Y),
	P2 = pos(X,Y),
	cmp(Same, P1, P2), %a cor é a mesma.
	Vizinho = P1.

vizinhos(Same, pos(X,Y),Vizinho) :-
	Cord is Y - 1,
	P1 = pos(X, Cord),
	P2 = pos(X,Y),
	cmp(Same, P1, P2), %a cor é a mesma.
	Vizinho = P1.

%Verdadeiro se as duas posicoes sao iguais
cmp(Same, pos(X,Y), pos(I,J)) :-
	listref(Same, pos(X,Y), Elemento),
	listref(Same, pos(I,J), Elemento2),
	Elemento =:= Elemento2.

listref(Same, pos(X,Y), Elemento) :- 
	nth0(Y,Same,Linha),
	nth0(X,Linha,Elemento).

%Verdadeiro se P e o conjunto de todas as posicoes do same
posicoes(Same, P) :-
	listref(Same, P, _).

% TESTES

teste(Vizinhos) :- 
	lista_de_colunas('3-3-3', Teste),
	%write(Teste).
	%vizinhos(Teste, pos(1,2), Vizinhos).
	%findall(A,posicoes(Teste, P)),
	mesma_cor([pos(0,0), pos(1,0),pos(2,0), pos(0,1), pos(1,1), pos(2,1),pos(0,2),pos(1,2),pos(2,2)], G).

%mesma_cor([T|R], Group) :-
%	findall(Elemento,vizinhos([[1,1,1],[1,1,1],[1,1,1]],T,Elemento), Return),
%	append(Return,Group, Nova),
%	write(Nova),
%	mesma_cor(R,Nova).

lista_de_colunas(Matriz, X) :-
	read_matrix_file(Matriz, M),
	transpose(M,X).
	
