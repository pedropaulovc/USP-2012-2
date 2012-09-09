-module(silhueta).
-compile(export_all).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                           MAINs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main() ->	main (['2', standard_io, standard_io, null]).
	
main (L) ->
	case L of
		[Alg]                               -> main([Alg, standard_io, standard_io, null]);
		[Alg, Input]                        -> main([Alg, Input, standard_io, null]);
		[Alg, Input, Output]                -> main([Alg, Input, Output, null]);
		[Alg, Input, Output, Imagem | _ ]   -> Entrada = ler_entrada(Input),
											   Saida = executa_algoritmo(Alg, Entrada),
                                               formata_saida (Saida, Output),
                                               gera_imagem (Saida, Imagem),
                                               init:stop()
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                       ALGORITMOS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

algoritmo1(L) -> algoritmo1(L, []).

algoritmo1([], Resultado) -> Resultado;
algoritmo1([H|T], Resultado) -> algoritmo1(T, uniao(Resultado, silhueta_de_edificio(H))).


algoritmo2([]) -> [];
algoritmo2([H]) -> silhueta_de_edificio(H);
algoritmo2(L) -> 
	{L1, L2} = split(L),
	uniao(algoritmo2(L1), algoritmo2(L2)).

silhueta_com_foldl(L) -> lists:foldl(fun(E, S) -> uniao(S, silhueta_de_edificio(E)) end, [], L).

silhueta_com_foldr(L) -> lists:foldr(fun(E, S) -> uniao(silhueta_de_edificio(E), S) end, [], L).

executa_algoritmo (Alg, Entrada) ->
	case Alg of 
		'1' -> 	algoritmo1 (Entrada);
		'2' -> 	algoritmo2 (Entrada);
		'R' -> 	silhueta_com_foldr (Entrada);
		'L' -> 	silhueta_com_foldl (Entrada);
		_   -> 	io:format ("~nParametro Invalido!~nDeve ser '1', '2', 'L' ou 'R'~n~n", []),
				init:stop()
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                      UNIAO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

uniao(L1, L2) -> une(L1, L2, 0, 0, []).

%Encerramos o processamento
une([], [], _, _, Resp) -> lists:reverse(Resp);


une([{X1, A1}], [], _, Alt2, [{X, _}|T]) when (X == X1) ->
	une([], [], A1, Alt2, [{X1, A1}|T]);

une([{X1, A1}|T1], L2, _, Alt2, [{X, A}|T]) when (X == X1) ->
	if
		(A1 > A) -> une(T1, L2, A1, Alt2, [{X1, A1}|T]);
		true     -> une(T1, L2, A1, Alt2, [{X, A}|T])
	end;

%Casos triviais, união de uma silhueta com outra vazia.
une([], L, Alt1, Alt2, Resp) -> une(L, [], Alt2, Alt1, Resp);
une([{C,A}|T], [], Alt1, Alt2, Resp) -> une(T, [], Alt1, Alt2, [{C,A}|Resp]);


%Se a silhueta 2 começa antes, invertemos as duas.
une([{X1, A1}|T1], [{X2, A2}|T2], Alt1, Alt2, Resp) when (X1 > X2) ->
	une([{X2, A2}|T2], [{X1, A1}|T1], Alt2, Alt1, Resp);

%Neste ponto, a silhueta 1 começa antes da 2.
une([{X1, A1}|T1], [{X2, A2}|T2], Alt1, Alt2, Resp) ->
	Novo = if 
		(A1 =< Alt2) and (Alt1 =< Alt2) -> Resp;
		(A1 =< Alt2) and (Alt1 >  Alt2) -> [{X1,Alt2}|Resp];
		(A1  > Alt2) and (Alt1 <  Alt2) -> [{X1,A1}|Resp];
		(A1  > Alt2) and (Alt1 >= Alt2) -> [{X1,A1}|Resp]
	end,
	
	une(T1, [{X2, A2}|T2], A1, Alt2, Novo).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                      UTILITARIOS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

abre_arquivo (Onde, Modo) ->
	{Status, Res} = file:open (Onde, Modo),
	case Status of
		ok   -> Res;	
		Erro -> io:format ("Impossivel abrir o arquivo ~p: ~p~n", [Onde, Erro]),
				init:stop()
	end.


split(L) -> lists:split(length(L) div 2, L).

silhueta_de_edificio({E, A, D}) -> [{E, A}, {D, 0}].
		

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                        IMAGEM
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gera_imagem (_L, null) -> ok;
gera_imagem (L, Onde) ->
	{Status, Res} = file:open (Onde, write),
	
	case Status of
		error -> io:format("Erro na geração da imagem: ~p~n", [Res]),
							init:stop();
		_	  -> ok
	end,

	M = cria_matriz(L),

	io:format(Res, "P2~n~p ~p~n~p~n", [n_cols(), n_lins(), branco()]),
	
	imprime_matriz(M, Res, 0, n_lins(), 0, n_cols()).


imprime_matriz(_M, _Handler, FimL, FimL, _AtuC, _FimC) ->
	ok;

imprime_matriz(M, Handler, AtuL, FimL, AtuC, FimC) ->
	if
		AtuC == FimC - 1 -> 
			io:format(Handler, "~p~n", [matrix:get(AtuL, FimC - 1, M)]),
			imprime_matriz(M, Handler, AtuL + 1, FimL, 0, FimC);
		true             -> 
			io:format(Handler, "~p ", [matrix:get(AtuL, AtuC, M)]),
			imprime_matriz(M, Handler, AtuL, FimL, AtuC + 1, FimC)
	end.

preenche_retangulo(Matriz, Lin1, Lin2, Col1, Col2, Valor) -> 
	preenche(Matriz, Lin1, Lin1, Lin2, Col1, Col1, Col2, Valor).

%Completou todas as linhas
preenche(M, _IniL, FimL, FimL, _IniC, _AtuC, _FimC, _Valor) -> M;

%Completou todas as colunas de uma linha
preenche(M, IniL, AtuL, FimL, IniC, FimC, FimC, Valor) ->
	preenche(M, IniL, AtuL + 1, FimL, IniC, IniC, FimC, Valor);

%Executa a operação
preenche(M, IniL, AtuL, FimL, IniC, AtuC, FimC, Valor) ->
	M2 = matrix:set(AtuL, AtuC, Valor, M),
	preenche(M2, IniL, AtuL, FimL, IniC, AtuC + 1, FimC, Valor).


cria_matriz (L) ->
	M = matrix:new(n_lins(), n_cols()),
	N = preenche_retangulo(M, base(), base() + 1, 0, n_cols(), preto()), %Base
	cria_matriz_rec(L, N).
	
cria_matriz_rec([{X1, A1}, {X2, 0}], M) -> 
	preenche_retangulo(M, base() - A1, base(), X1, X2, cinza());

cria_matriz_rec([{X1, A1}, {X2, A2}|T], M) -> 
	N = preenche_retangulo(M, base() - A1, base(), X1, X2, cinza()),
	cria_matriz_rec([{X2, A2}|T], N).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                         ENTRADA
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ler_entrada(Onde) ->
	
	Fonte = case Onde of
				standard_io -> standard_io;
				_           -> abre_arquivo (Onde, read)
			end,
	
	io:fread(Fonte, "", "~d"),
	ler_entrada_r (Fonte, []).
	
	
ler_entrada_r (Arq, Lido) ->	
	case io:fread(Arq, "", "~d~d~d") of
		{ok, [H1, H2, H3]} 	-> 	Novo = [{H1, H2, H3} | Lido], 
								ler_entrada_r(Arq, Novo);
		eof					->	lists:reverse(Lido);
		{error, Erro}		->  io:format("Erro na leitura da entrada: ~p~n", [Erro]),
								init:stop()
	end.	



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                          SAIDA
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


formata_saida (L, Onde) ->
	Handler = 	case Onde of
					standard_io	->	standard_io;
					_			->	abre_arquivo(Onde, write)
				end,	
	io:format(Handler, "~p~n", [length(L)]),
	imprime (L, Handler).


imprime([], _Handler) -> ok;
imprime([H|T], Handler) ->
	{X, Y} = H,
	io:format(Handler, "~p ~p~n", [X, Y]),
	imprime(T, Handler).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                        DEFINEs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


n_lins() -> 600.                      % n�mero de linhas da imagem
n_cols() -> 800.                      % n�mero de colunas da imagem
borda_inf() -> n_lins() - 1.          % borda inferior (�ltima linha da imagem) 
margem_inf() -> 20.                   % linhas do eixo base � borda inferior da imagem
base() -> borda_inf() - margem_inf(). % linha do eixo base 
       
branco() -> 15.                       % valor de maxval
cinza() -> 10.                        % cor da silhueta preenchida
preto() -> 0.                         % cor do eixo base



