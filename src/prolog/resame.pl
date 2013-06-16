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
	read_matrix_file(File, M),
	transpose(M, M1),
	solve(M1,R), !,
	write_jogadas(M1, R, M1), !.

write_jogadas(_,[], _) :- !.

write_jogadas(Same, [H|T], Original) :- 
	write_pos(H), nl, nl,
	group(Same, H, Group),
	remove_group(Same, Group, Removido),
	completa_same(Original, Removido, Zerado),
	not(empty(T)),
	transpose(Zerado, Trans),
	write_matrix(Trans), nl,
	write_jogadas(Removido, T, Original).

write_jogadas(Same, [H|_], Original) :-
	group(Same, H, Group),
	remove_group(Same, Group, Removido),
	completa_same(Original, Removido, Zerado),
	transpose(Zerado,Trans),	
	write_matrix(Trans), !.

write_pos(pos(X,Y)) :-
	write(X),
	write(' '),
	write(Y).
	


completa_same([H|T], SameIncompleto, NovoSame) :-
	Same = [H|T],
	length(Same, X),
	length(H, Y),
	completa_col(SameIncompleto, Y, Completado),
	completa_lin(Completado, X, NovoSame).
	
completa_lin([H|T], X, NovoSame) :-	
	Same = [H|T],
	length(Same, L),
	X1 is X - L,
	length(H, TamCol),
	completa_lin(Same, X1, TamCol, [], NovoSame).

completa_lin(_, 0, _, NovoSame, NovoSame) :- 
	not(empty(NovoSame)), !.

completa_lin(NovoSame, 0, _, _, NovoSame) :- !.

completa_lin(Same, X, TamCol, _, Retorno) :-
	coluna_nula(TamCol, NovaLinha),
	ListaNula = [NovaLinha|[]],
	append(Same, ListaNula, Novo),
	X1 is X - 1,
	completa_lin(Novo, X1, TamCol, Novo, Retorno).

completa_col(L, X, Retorno) :- 
	length(L, S),
	S =\= 0,
	completa_col(L, X, [], Retorno), !.

completa_col(L, X, Retorno) :-
	completa_col([[]|L], X, [], Retorno).

completa_col([H|T], X, NovoSame, Retorno) :-
	length(H, Tam),
	X1 is X - Tam,
	coluna_nula(X1,L),
	append(H, L, R),
	completa_col(T, X, [R|NovoSame], Retorno).

completa_col([], _, R, Retorno) :- reverse(R,Retorno).

	
	
coluna_nula(N, L) :- 
	coluna_nula(N, [], L).
	
coluna_nula(0, A, A):- !.

coluna_nula(N, NovaColuna, Retorno) :-
	N1 is N - 1,
	coluna_nula(N1, [0|NovaColuna], Retorno).	

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
	group(Same,Pontos,[],K),
	member(Group,K).

group(_, [], Group, Group).

group(Same, [H|T],Group, Retorno) :-
	group(Same,H,GrupoP),
	sort(GrupoP,GrupoO),
	not(member(GrupoO, Group)),
	group(Same, T, [GrupoO|Group],Retorno), !.
    	%writeln([Same, Group]), fail.

group(Same, [_|T], Group, Retorno) :-
	group(Same,T,Group,Retorno).
	

%% grupo(+Same, +P, -Group) is semidet
%
%  Verdadeiro se Group é um grupo de Same que contém a posição P.
group(Same, P, Group) :- 
	%lista_de_colunas(Same, T),
	compara-vizinhos(Same, [], [P], Group).


compara-vizinhos(_,R,[],R) :- 
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
	zera(Same, Group, SameZerado),
	remove_zeros(SameZerado,[], NewSame), !.

remove_zeros([], NovoSame, Novo) :-
	delete(NovoSame, [], Novo1), 
	reverse(Novo1, Novo).

remove_zeros([H|T], NovoSame, Retorno) :-
	delete(H, 0, L),
	remove_zeros(T, [L|NovoSame], Retorno).

	

zera(Same, [], Same).

zera(Same, [H|T], K) :-
	zera_pos(Same, H, [], NovoSame),
	zera(NovoSame, T, K).
	
zera_pos([], _, NovoSame, Novo) :-
	reverse(NovoSame, Novo).

zera_pos([H|T], pos(X,0), NovoSame, Retorno) :-
	setsub(H,0,X,[], H2),
	zera_pos(T, pos(X,-1), [H2|NovoSame], Retorno), !.

zera_pos([H|T], pos(X,Y), NovoSame, Retorno) :-
	Y1 is Y - 1,
	zera_pos(T, pos(X,Y1), [H|NovoSame], Retorno).
		

setsub([], _, _, Novalista, Retorno) :- reverse(Novalista, Retorno).

setsub([_|T], Valor, 0, Novalista, Retorno) :-
	setsub(T, Valor, -1, [Valor|Novalista], Retorno).

setsub([H|T], Valor, Pos, Novalista, Retorno) :-
	Pos =\= 0,
	Pos2 is Pos - 1,
	setsub(T, Valor, Pos2, [H|Novalista], Retorno).


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
