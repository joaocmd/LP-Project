% Joao Carlos Morgado David - 89471 %

% Importar Exemplos %
:- [exemplos_puzzles].

%---------------------------------------------------------------------
% Seletores de Puzzle: termometros/2, lim_linhas/2, lim_colunas/2.
% O primeiro argumento e um puzzle, e o segundo o seu componente
% desejado.
%---------------------------------------------------------------------
termometros([Termometros|_], Termometros).
lim_linhas([_, Lim_Linhas, _], Lim_Linhas).
lim_colunas([_, _, Lim_Colunas], Lim_Colunas).

%----------------------------------------------------------------------
% Seletores Sobre Posicoes: linha/2, coluna/2.
% O primeiro argumento e uma posicao e o segundo a sua linha ou coluna.
%----------------------------------------------------------------------
linha((Linha,_), Linha).
coluna((_,Coluna), Coluna).

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
    union(Ja_Preenchidas, Poss, Union),
    lim_colunas(Puz, Lim_Colunas),
    verifica_parcial2(Union, 1, Lim_Colunas).

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

possibilidades_linha(Puz, Posicoes_linha, Total, Ja_Preenchidas, Possibilidades_L) :-
    linha_atual(Posicoes_linha, Linha_Atual),
    lim_linhas(Puz, Lim_Linhas),
    nth1(Linha_Atual, Lim_Linhas, Lim_Atual),
    findall(Combinacao, combinacao(Lim_Atual, Posicoes_linha, Combinacao), Combinacoes),
    com_ja_preenchidas(Combinacoes, C_Ja_Preenchidas),
    propaga_tudo(Puz, C_Ja_Preenchidas, C_Propagadas).

possibilidades_linha(Puz, Posicoes_linha, Total, Ja_Preenchidas, Possibilidades_L) :-


% linha_atual/2(Posicoes_linha, Linha).
linha_atual([P|_], L) :-
    linha(P, L).

% combinacao/3(N, L, Combinacao).
% Combinacao e uma combinacao de L composta por N elementos.
combinacao(0, _, []).

combinacao(N, L, [E|R]) :-
    N > 0,
    append(_, [E|L_Apos_E], L),
    N_Aux is N - 1,
    combinacao(N_Aux, L_Apos_E, R).

% com_ja_preenchidas/3(Combinacoes, Ja_Preenchidas, Com_Ja_Preenchidas).
com_ja_preenchidas(Combinacoes, Ja_Preenchidas, C_Ja_Preenchidas) :-
    com_ja_preenchidas(Combinacoes, Ja_Preenchidas, C_Ja_Preenchidas, []).
    
com_ja_preenchidas([], _, C_Ja_Preenchidas, C_Ja_Preenchidas).

com_ja_preenchidas([P|R], Ja_Preenchidas, C_Ja_Preenchidas, Res) :-
    subset(Ja_Preenchidas, P) ->
        com_ja_preenchidas(R, Ja_Preenchidas, C_Ja_Preenchidas, [P|Res])
    ;
        com_ja_preenchidas(R, Ja_Preenchidas, C_Ja_Preenchidas, Res)
    .

% propaga_tudo/3(Puz, Combinacoes, C_Propagado)
propaga_tudo(Puz, Combinacoes, C_Propagado) :-
    propaga_tudo(Puz, Combinacoes, C_Propagado, []).

propaga_tudo(_, [], C_Propagado, C_Propagado).

propaga_tudo(Puz, [P|R], C_Propagado, Res) :-
    propaga_lista(Puz, P, P_Propagado),
    propaga_tudo(Puz, R, C_Propagado, [P_Propagado|Res]).
    

% propaga_lista/2(L_Original, L__Propagada)
propaga_lista(Puz, L_Original, L_Propagada) :-
    propaga_lista(Puz, L_Original, L_Propagada, []).

propaga_lista(_, [], L_Propagada, L_Propagada) :- !.

propaga_lista(Puz, [P|R], C_Apos_Propaga, Res) :-
    propaga(Puz, P, Posicoes), 
    append(Res, Posicoes, A),
    propaga_lista(Puz, R, C_Apos_Propaga, A).
