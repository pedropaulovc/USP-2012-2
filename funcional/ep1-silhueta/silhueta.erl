%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Alunos: 
%         Alessandro Calò          - 4325393
%         Pedro Paulo Vezzá Campos - 7538743
%
% MAC0319-2012 - Programação Funcional Contemporânea - EP1: A Silhueta de um 
% Conjunto de Edifícios
%
% Sobre o arquivo: Contém toda a implementação e documentação do EP proposto.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(silhueta).
-export([main/0, main/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                           MAINs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Função main de aridade zero: correspondente a invocar o programa utilizando
% o algoritmo 2, fazendo leitura da entrada e impressão do resultado via
% entrada/saída padrão.
main() ->	main (['2', standard_io, standard_io, null]).

% Função main de aridade um: Recebe uma lista de argumentos definindo 
% o algoritmo a ser utilizado, forma de leitura da entrada, saída dos resultados
% e o arquivo de imagem PGM a ser gerado conforme a especificação do EP.
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

% Função obrigatória algoritmo1(ListaDeTriplas) -> ListaDePares:
% Implementação do algoritmo 1 descrito no EP utilizando recursão de cauda. Faz
% uso da função auxiliar algoritmo1/2 para o cálculo efetivo do resultado.
algoritmo1(L) -> algoritmo1(L, []).

% algoritmo1(ListaDeTriplas, ListaDePares) -> ListaDePares:
% 	- ListaDeTriplas: Lista de prédios a ter a silhueta determinada
% 	- ListaDePares: A silhueta determinada até o momento
algoritmo1([], Resultado) -> Resultado;
algoritmo1([H|T], Resultado) -> algoritmo1(T, uniao(Resultado, silhueta_de_edificio(H))).

% Função obrigatória algoritmo2(ListaDeTriplas) -> ListaDePares:
% Implementação do algoritmo 2 descrito no EP.
algoritmo2([]) -> [];
algoritmo2([H]) -> silhueta_de_edificio(H);
algoritmo2(L) -> 
	{L1, L2} = split(L),
	uniao(algoritmo2(L1), algoritmo2(L2)).

% Função obrigatória silhueta_com_foldl(ListaDeTriplas) -> ListaDePares:
% Implementação alternativa para o algoritmo 1.
silhueta_com_foldl(L) -> lists:foldl(fun(E, S) -> uniao(S, silhueta_de_edificio(E)) end, [], L).

% Função obrigatória silhueta_com_foldr(ListaDeTriplas) -> ListaDePares:
% Implementação alternativa para o algoritmo 1.
silhueta_com_foldr(L) -> lists:foldr(fun(E, S) -> uniao(silhueta_de_edificio(E), S) end, [], L).

% Recebe como parâmetro um caractere do conjunto ['1', '2', 'L', 'R'] e uma 
% ListaDeTriplas e invoca do algoritmo correspondente ao caractere passando 
% a ListaDeTriplas como parâmetro.
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

% as duas listas foram percorridas totalmente. Basta retornar a lista S em ordem inversa.
une ([], [], _A1, _A2, S)	->	lists:reverse (S);

% a lista 2 foi percorrida totalmente. Basta retornar S invertida, e adicionar L1 ao final desta.
une (L1, [], _A1, _A2, S)	->	lists:reverse (S, L1);

% a lista 1 foi percorrida totalmente. Basta retornar S invertida, e adicionar L2 ao final desta.
une ([], L2, _A1, _A2, S)	->	lists:reverse (S, L2);

% caso generico: as duas silhuetas contem pelo menos um elemento cada.
% A1 e A2 sao as alturas atuais das 2 silhuetas, e S eh a silhueta resultante.
une (L1, L2, A1, A2, S)	->	
	[{X1, Y1} | T1] = L1,
	[{X2, Y2} | T2] = L2,
	case X1 < X2 of
		% X1 < X2
		true	->	case Y1 > A2 of
						% a altura de X1 eh maior que a da outra silhueta, 
						% portanto deve ser considerada
						% Obs: adicionamos a nova tupla NO INICIO da lista, 
						% por isso nas clausulas acima ela deve ser invertida.
						true	->	une (T1, L2, Y1, A2, [{X1, Y1}|S]);
						% a altura de X2 nao eh maior que a da outra silhueta, 
						% entao nao deve ser considerada
						false	->	une (T1, L2, Y1, A2, [{X1, A2}|S])
					end;
		% X1 > X2 OU X1 =:= X2
		false	->	case X1 > X2 of
						% X1 > X2
						true	->	case Y2 > A1 of
										% a altura de X2 eh maior que a da outra silhueta, 
										% portanto deve ser considerada
										true	->	une (L1, T2, A1, Y2, [{X2, Y2}|S]);
										% a altura de X2 nao eh maior que a da outra silhueta, 
										% portanto nao deve ser considerada
										false	->	une (L1, T2, A1, Y2, [{X2, A1}|S])
									end;
						% X1 =:= X2
						% Aqui, basta incluir a tupla {Xi, max(Y1, Y2)} na silhueta final, 
						% onde i pode ser 1 ou 2, tanto faz.
						false	->	case Y1 > Y2 of
										% max = Y1
										true	->	une (T1, T2, Y1, Y2, [{X1, Y1}|S]);
										% max = Y2
										false	->	une (T1, T2, Y1, Y2, [{X2, Y2}|S])
									end
					end
	end.


% Função obrigatória uniao(ListaDePares1, ListaDePares2) -> ListaDePares3:
% a funcao uniao/2 chama a funcao auxiliar une/5, que realiza a mesma tarefa.
% a funcao remove/2, que age sobre o resultado de une/5, remove desta lista as tuplas redundantes.
uniao (L1, L2) -> remove (une (L1, L2, 0, 0, []), []).


% se a lista for vazia, nao há nada a remover
remove ( [], _Res)						->	[];

%se existir só um elemento (uma tupla) restante na silhueta, ele certamente não será redundante
remove ( [{X, Y}], Res)					->	lists:reverse (Res, [{X, Y}]);

% se existirem 2 ou mais elementos restantes na silhueta, faz-se a comparacao entre os 2 primeiros,
% e se o primeiro for redundante, ele nao eh incluido na silhueta final (Res)
remove ( [{X1, Y1}, {X2, Y2}|T], Res)	->	case Y1 =:= Y2 of
												true	->	remove ([{X1, Y1}|T], Res);
												false	->	remove ([{X2, Y2}|T], [{X1, Y1} | Res])
											end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                      UTILITARIOS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Recebe como parâmetro uma string contendo o local do arquivo a ser aberto
% e o modo de abertura (read, write...) retorna como valor o descritor do arquivo
% aberto ou encerra o programa em caso de erro.
abre_arquivo (Onde, Modo) ->
	{Status, Res} = file:open (Onde, Modo),
	case Status of
		ok   -> Res;	
		Erro -> io:format ("Impossivel abrir o arquivo ~p: ~p~n", [Onde, Erro]),
				init:stop()
	end.

% Recebe como parâmetro uma lista e retorna uma tupla contendo a lista original
% dividida em duas "metades".
split(L) -> lists:split(length(L) div 2, L).

% Função obrigatória silhueta_de_edificio(Tripla) -> ListaDePares:
% Recebe como parâmetro uma tripla representando um edifício e retorna sua 
% silhueta.
silhueta_de_edificio({E, A, D}) -> [{E, A}, {D, 0}].
		

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                        IMAGEM
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Função obrigatória gera_imagem(ListaDePares, String) -> true
% Recebe uma silhueta ListaDePares, converte-a para uma imagem no formato PGM, 
% e guarda essa imagem no arquivo cujo nome é String.
% Caso não consiga gravar o resultado exibe um erro e encerra o programa.
gera_imagem (_L, null) -> true;
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

% Função auxiliar de gera_imagem/2.
% imprime_matriz(M, Handler, AtuL, FimL, AtuC, FimC):
% 	- M: Matriz preenchida com a silhueta final e a base
%  	- Handler: Descritor de arquivo ou standard_io onde será gravado o conteúdo
% da matriz
% 	- AtuL: Linha atual na varredura da matriz
% 	- FimL: Quantidade de linhas da matriz
% 	- AtuC: Coluna atual na varredura da matriz
% 	- FimC: Quantidade de colunas da matriz
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

% Função obrigatória preenche_retangulo(Matriz, Lin1, Lin2, Col1, Col2, Valor)
% -> Matriz1: Implementação da especificação do EP do preenchimento de um 
% retângulo em uma matriz. Faz uso da função auxiliar preenche/8.
preenche_retangulo(Matriz, Lin1, Lin2, Col1, Col2, Valor) -> 
	preenche(Matriz, Lin1, Lin1, Lin2, Col1, Col1, Col2, Valor).


% preenche/8: Função auxiliar de preenche_retangulo/6.
% preenche(M, _IniL, FimL, FimL, _IniC, _AtuC, _FimC, _Valor):
% 	- M: Matriz base a ser preenchida
% 	- IniL: Linha inicial do preenchimento da matriz
% 	- AtuL: Linha atual no preenchimento da matriz
% 	- FimL: Linha final do preenchimento da matriz
% 	- IniC: Coluna inicial do preenchimento da matriz
% 	- AtuC: Coluna atual no preenchimento da matriz
% 	- FimC: Coluna final do preenchimento da matriz

%Completou todas as linhas
preenche(M, _IniL, FimL, FimL, _IniC, _AtuC, _FimC, _Valor) -> M;

%Completou todas as colunas de uma linha
preenche(M, IniL, AtuL, FimL, IniC, FimC, FimC, Valor) ->
	preenche(M, IniL, AtuL + 1, FimL, IniC, IniC, FimC, Valor);

%Executa a operação
preenche(M, IniL, AtuL, FimL, IniC, AtuC, FimC, Valor) ->
	M2 = matrix:set(AtuL, AtuC, Valor, M),
	preenche(M2, IniL, AtuL, FimL, IniC, AtuC + 1, FimC, Valor).


% Função auxiliar de gera_imagem/2. Realiza uma série de chamadas à função
% preenche_retangulo/6 para preencher a matriz. Faz uso da função cria_matriz/2
% para varrer a ListaDeDuplas que recebe como parâmetro.
cria_matriz (L) ->
	M = matrix:new(n_lins(), n_cols(), branco()),
	N = preenche_retangulo(M, base(), base() + 1, 0, n_cols(), preto()), %Base
	cria_matriz(L, N).
	
% Recebe como parâmetro uma ListaDeDuplas a ser desenhada com retângulos e 
% a matriz atual contendo a silhueta até o momento. Retorna a nova matriz com
% a silhueta desenhada.
cria_matriz([{X1, A1}, {X2, _}], M) -> 
	preenche_retangulo(M, base() - A1, base(), X1, X2, cinza());

cria_matriz([{X1, A1}, {X2, A2}|T], M) -> 
	N = preenche_retangulo(M, base() - A1, base(), X1, X2, cinza()),
	cria_matriz([{X2, A2}|T], N).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                         ENTRADA
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Recebe como parâmetro uma string com o caminho de onde serão lidas as triplas
% ou standard_io caso seja feita leitura via entrada padrão. Retorna a lista de
% triplas lida. Faz uso da função auxiliar ler_entrada/3.
ler_entrada(Onde) ->
	
	Fonte = case Onde of
				standard_io -> standard_io;
				_           -> abre_arquivo (Onde, read)
			end,
	
	{ok, [N]} = io:fread(Fonte, "", "~d"),
	ler_entrada (Fonte, [], N).
	
% Recebe como parâmetros um descritor de arquivo aberto, uma ListaDeTriplas
% contendo as triplas lidas até o momento e a quantidade N de triplas a serem
% lidas. Termina a leitura quando recebe um EOF ou quando N == 0. Retorna a lista
% na ordem que foi lida.
ler_entrada(_, Lido, 0) -> lists:reverse(Lido);

ler_entrada (Arq, Lido, N) ->
	case io:fread(Arq, "", "~d~d~d") of
		{ok, [H1, H2, H3]} 	-> 	Novo = [{H1, H2, H3} | Lido], 
								ler_entrada(Arq, Novo, N - 1);
		eof					->	lists:reverse(Lido);
		{error, _}			->  ler_entrada(Arq, Lido, N)
	end.	



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                          SAIDA
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Recebe como parâmetros uma ListaDeDuplas com a silhueta finalizada e uma string
% indicando o local onde será gravado o resultado ou standard_io caso se deseje 
% imprimir na saída padrão. Imprime segundo a especificação do E e retorna ok ao
% fim da escrita. Faz uso da função auxiliar imprime/2.
formata_saida (L, Onde) ->
	Handler = 	case Onde of
					standard_io	->	standard_io;
					_			->	abre_arquivo(Onde, write)
				end,	
	io:format(Handler, "~p~n", [length(L)]),
	imprime (L, Handler).

% Recebe como parâmetros uma ListaDeDuplas com a silhueta finalizada a ser 
% impressa e um descritor de arquivo indicando onde a impressão será feita. 
% Retorna ok ao fim da escrita.
imprime([], _Handler) -> ok;
imprime([H|T], Handler) ->
	{X, Y} = H,
	io:format(Handler, "~p ~p~n", [X, Y]),
	imprime(T, Handler).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                        DEFINEs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Defines especificados pelo enunciado do EP.
n_lins() -> 600.                      % número de linhas da imagem
n_cols() -> 800.                      % número de colunas da imagem
borda_inf() -> n_lins() - 1.          % borda inferior (Última linha da imagem) 
margem_inf() -> 20.                   % linhas do eixo base à borda inferior da imagem
base() -> borda_inf() - margem_inf(). % linha do eixo base 
       
branco() -> 15.                       % valor de maxval
cinza() -> 10.                        % cor da silhueta preenchida
preto() -> 0.                         % cor do eixo base



