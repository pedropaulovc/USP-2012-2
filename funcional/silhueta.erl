-module(silhueta).
-export([main/1, split/1]).

main(ArgList) ->
	io:format("Testando~p ~n", [ArgList]).

%TODO Trocar para recursão de cauda
%algoritmo1([H|T]) -> uniao(silhueta_de_edificio(H), algoritmo1(T));
%algoritmo1([]) -> [].

%algoritmo2(ListaDeTriplas) -> ListaDePares
%silhueta_com_foldl(ListaDeTriplas) -> ListaDePares
%silhueta_com_foldr(ListaDeTriplas) -> ListaDePares
%uniao(ListaDePares1, ListaDePares2) -> ListaDePares3

qtd_elementos(L) -> qtd_elementos(L, 0).

qtd_elementos([], C) -> C;
qtd_elementos([_H|T], C) -> qtd_elementos(T, 1 + C).

split(L) -> lists:split(qtd_elementos(L) div 2, L).

silhueta_de_edificio({E, A, D}) -> [{E, A}, {D, 0}].
		
	
%gera_imagem(ListaDePares, String) -> true

%n_lins() -> 600.                      % número de linhas da imagem
%n_cols() -> 800.                      % número de colunas da imagem
%borda_inf() -> n_lins() - 1.          % borda inferior (última linha da imagem) 
%margem_inf() -> 20.                   % linhas do eixo base à borda inferior da imagem
%base() -> borda_inf() - margem_inf(). % linha do eixo base 
%       
%branco() -> 15.                       % valor de maxval
%cinza() -> 10.                        % cor da silhueta preenchida
%preto() -> 0.                         % cor do eixo base

%preenche_retangulo(Matriz, Lin1, Lin2, Col1, Col2, Valor) -> Matriz1

