% Joao Carlos Morgado David - 89471 %
% Importar exemplo %
:- [exemplos_puzzles].

% Operacoes Basicas %
/* Todos estes predicados recebem como primeiro argumento um problema. */
termometros([Termometros|_], Termometros).
lim_linhas([_, Lim_Linhas, _], Lim_Linhas).
lim_colunas([_, _, Lim_Colunas], Lim_Colunas).

linha((Linha, _), Linha).
coluna((_, Coluna), Coluna).

% termometro_em/3 (Termometros, Posicao, Termometro).
termometro_em(Termometros, Posicao, Termometro) :-
    member(Termometro, Termometros),
    member(Posicao, Termometro), !.

% posicoes_ate/3 (Termometro, Posicao, Posicoes).
posicoes_ate(Termometro, Pos, Posicoes) :- 
    posicoes_ate(Termometro, Pos, Posicoes, []).

posicoes_ate(_, Pos, Res, [Pos|R]) :-
    sort([Pos|R], Res), !.

posicoes_ate([P|R], Pos, Posicoes, X) :-
    member(Pos, [P|R]), !,
    posicoes_ate(R, Pos, Posicoes, [P|X]), !.

% propaga/3(Puz, Pos, Posicoes).
% Posicoes sao as posicoes do termometro que contem 
% Pos no puzzle Puz, desde a sua base ate Pos.
propaga(Puz, Pos, Posicoes) :- 
    termometros(Puz, Termometros),
    termometro_em(Termometros, Pos, Termometro),
    posicoes_ate(Termometro, Pos, Posicoes).


% nao_altera_linhas_anteriors/3(Posicoes, L, Ja_Preenchidas).
nao_altera_linhas_anteriores(Ja_Preenchidas, _, Ja_Preenchidas) :- !.
nao_altera_linhas_anteriores([], _, _) :- !.

nao_altera_linhas_anteriores([P|R], L, Ja_Preenchidas) :-
    linha(P, Linha_Atual),
    (Linha_Atual >= L ; member(P, Ja_Preenchidas)),
    nao_altera_linhas_anteriores(R, L, Ja_Preenchidas).

% verifica_parcial/4(Puz, Ja_Preenchidas, Dim, Poss).
verifica_parcial(_, _ , _, []) :- !.

verifica_parcial(Puz, Ja_Preenchidas, _, Poss) :- 
    append(Ja_Preenchidas, Poss, A),
    list_to_set(A, Poss_Total),
    lim_colunas(Puz, Lim_Colunas),
    verifica_parcial2(Poss_Total, 1, Lim_Colunas).

% verifica_parcial2/3(Poss_Total, Coluna, Lim_Colunas).
verifica_parcial2(_, _, []) :- !.

verifica_parcial2(Poss_Total, Coluna, [PC|RC]) :-
    n_elementos_coluna(Poss_Total, Coluna, N),
    N =< PC,
    Col_Aux is Coluna + 1,
    verifica_parcial2(Poss_Total, Col_Aux, RC).

% n_elementos_coluna/3(Poss, Coluna, N).
% N e o numero de elementos na coluna Coluna,
% que estao na lista de possibilidades Poss.
n_elementos_coluna(Poss, Coluna, N) :-
    n_elementos_coluna(Poss, Coluna, N, 0).

n_elementos_coluna([], _, N, N) :- !.

n_elementos_coluna([P|R], Coluna, N, I) :-
    coluna(P, Col_I),
    Col_I == Coluna ->
        I_Aux is I + 1,
        n_elementos_coluna(R, Coluna, N, I_Aux)
    ;
        n_elementos_coluna(R, Coluna, N, I)
    .

% possibilidades_linha/5(Puz, Posicoes_linha, Total, Ja_Preenchidas, Possibilidades_L).

