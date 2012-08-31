-module(a).
-compile(export_all).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                           MAINs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main() ->	main ([2, standard_io, standard_io, null]).
	
%%	Entrada = ler_entrada(stdin),
%%	Saida = algoritmo2(Entrada),
%%	formata_saida_tela (Saida),
%%	init:stop().

%	
%main([Alg]) ->	main ([Alg, standard_io, standard_io, null]).

%%	Entrada = ler_entrada(stdin),
%%	Saida =	escolhe_algoritmo(Alg),
%%	formata_saida_tela (Saida),
%%	init:stop().
%	

%main ([Alg, Input]) ->	main ([Alg, Input, standard_io, null]).

%%	Entrada = ler_entrada(Input), 
%%	Saida = escolhe_algoritmo(Alg),
%%	formata_saida_tela (Saida),
%%	init:stop().


%main ([Alg, Input, Output]) ->	main ([Alg, Input, Output, null]).

%%	Entrada = ler_entrada(Input),	
%%	Saida = escolhe_algoritmo(Alg),
%%	formata_saida_arquivo (Saida, Output),
%%	init:stop().


%main ([Alg, Input, Output, Imagem]) ->	

%	Entrada = ler_entrada(Input),	
%	Saida = escolhe_algoritmo(Alg),
%	formata_saida_arquivo (Saida, Output),
%	gera_imagem (Saida, Imagem),
%	init:stop().
	

main (L) ->
	case L of
		[Alg] 							->	main([Alg, standard_io, standard_io, null]);
		[Alg, Input] 					->	main([Alg, Input, standard_io, null]);
		[Alg, Input, Output] 			->	main([Alg, Input, Output, null]);
		[Alg, Input, Output, Imagem] 	->	Entrada = ler_entrada(Input),	
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                      FUNCOES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

abre_arquivo (Onde, Modo) ->
	{Status, Res} = file:open (Onde, Modo),
	case Status of
		ok	->	Res;	
		_	->	io:format ("Impossivel abrir o arquivo ~p", [Onde]),
				init:stop()
	end.


qtd_elementos(L) -> qtd_elementos(L, 0).
qtd_elementos([], C) -> C;
qtd_elementos([_H|T], C) -> qtd_elementos(T, 1 + C).

split(L) -> lists:split(qtd_elementos(L) div 2, L).

silhueta_de_edificio({E, A, D}) -> [{E, A}, {D, 0}].
		
%uniao(ListaDePares1, ListaDePares2) -> ListaDePares3
uniao (L1, L2) -> L1++L2.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                        IMAGEM
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



gera_imagem (_L, null) -> ok;
gera_imagem (L, Path) ->
	{Status, Res} = file:open (Path, write),
	
	case Status of
		error -> io:format("~p~n", [Res]),
							init:stop();
		_	  -> ok
	end,

	M = cria_matriz(L),

	io:format(Res, "P2~n~p ~p~n~p~n", [n_cols(), n_lins(), branco()]),
	
	ForCol = fun(Lin) -> for_print(fun(Col) -> io:format(Res, "~p ", [matrix:get(Lin, Col, M)]) end, 0, n_cols(), Res) end,
	for_print(ForCol, 0, n_lins(), Res).

executa_algoritmo (Alg, Entrada) ->
	case Alg of 
		'1' -> 	algoritmo1 (Entrada);
		'2' -> 	algoritmo2 (Entrada);
		'R' -> 	silhueta_com_foldr (Entrada);
		'L' -> 	silhueta_com_foldl (Entrada);
		_ 	-> 	io:format ("~nParametro Invalido!~nDeve ser '1', '2', 'L' ou 'R'~n~n", []),
				init:stop()
	end.




for_print(_Fun, Fim, Fim, IO) -> ok;
for_print( Fun, Ini, Fim, IO) -> Fun(Ini), for_print(Fun, Ini+1, Fim, IO).

for(_Fun, Fim, Fim, Res) -> Res;
for( Fun, Ini, Fim, Res) -> Novo = Fun(Ini, Res), for(Fun, Ini+1, Fim, Novo).



preenche_retangulo(Matriz, Lin1, Lin2, Col1, Col2, Valor) -> 
	ForCol = fun(Lin, M1) -> for(fun(Col, M2) -> matrix:set(Lin, Col, Valor, M2) end, Col1, Col2, M1) end,
	for(ForCol, Lin1, Lin2, Matriz).


cria_matriz (L) ->
	M = matrix:new(n_lins(), n_cols()),
	N = preenche_retangulo(M, base(), base() + 1, 0, n_cols(), preto()), %Base
	preenche_retangulo(N, 0, 300, 0, 400, cinza()).
	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                          SAIDA
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


formata_saida (L, Onde) ->
	Handler = 	case Onde of
					standard_io	->	standard_io;
					_			->	abre_arquivo(Onde, write)
				end,	
	io:format(Handler, "~p~n", [qtd_elementos(L)]),
	imprime (L, Handler).


imprime([], _Handler) -> ok;
imprime([H|T], Handler) ->
	{X, Y} = H,
	io:format(Handler, "~p ~p~n", [X, Y]),
	imprime(T, Handler).

%formata_saida_arquivo(L, Arquivo) ->
%	io:format(Arquivo, "~p~n", [qtd_elementos(L)]),
%	imprime_arquivo(L, Arquivo).



%formata_saida_tela(L) ->
%	io:format("~p~n", [qtd_elementos(L)]),
%	imprime_tela(L).

%imprime_tela([]) -> ok;
%imprime_tela([H|T]) ->
%	{X, Y} = H,
%	io:format("~p ~p~n", [X, Y]),
%	imprime_tela(T).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                         ENTRADA
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ler_entrada(Onde) ->
	
	case Onde of
		standard_io	->	io:fread("", "~d"),
						ler_entrada_r(standard_io, []);

		_			->	io:fread(abre_arquivo (Onde, read), "", "~d"),
						ler_entrada_r (Onde, [])
	end.
	
	
ler_entrada_r (Arq, Lido) ->	
	case io:fread(Arq, "", "~d~d~d") of
		{ok, [H1, H2, H3]} 	-> 	Novo = [{H1, H2, H3} | Lido], 
								ler_entrada_r(Arq, Novo);
		eof					->	lists:reverse(Lido)
	end.	





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                        DEFINEs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




n_lins() -> 600.                      % número de linhas da imagem
n_cols() -> 800.                      % número de colunas da imagem
borda_inf() -> n_lins() - 1.          % borda inferior (última linha da imagem) 
margem_inf() -> 20.                   % linhas do eixo base à borda inferior da imagem
base() -> borda_inf() - margem_inf(). % linha do eixo base 
       
branco() -> 15.                       % valor de maxval
cinza() -> 10.                        % cor da silhueta preenchida
preto() -> 0.                         % cor do eixo base



