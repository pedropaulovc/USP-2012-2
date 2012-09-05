-module(b).
-compile(export_all).

uniao ([], L2) -> L2;      
uniao (L1, []) -> L1;
uniao (L1, L2) ->
	Map1 = silhouette_to_map (L1),
	Map2 = silhouette_to_map (L1),
	map_to_silhouette (Map2).                %%%%%%%% ainda nao implementada

%nova funcao com 2 argumentos, o segundo é justamente o novo hight map com HightMap[i] = 0.
silhouette_to_map (L) -> stm (L, array:new(800, {default,0})).

stm ([{X,Y}], Map) -> for (Y, X, 800, Map);
stm ([{X1,Y1},{X2,Y2}|T], Map) ->
	for (Y1, X1, X2, Map),
	stm ([{X2,Y2}|T], Map).


%%%%%%Caso ele receba uma lista com 2 ou mais pares, ele faz um "for (i = X1, i <= X2, i++) Map[i] = Y1" e depois chama a função com a mesma lista menos o PRIMEIRO par.

stm ([{X,Y}], Map )	->	for (fun (I) -> array:set (I, Y, Map) end, X, 800, Map);

%%%%%% Caso ele receba uma lista com 1 par só, então Y é com certeza zero. Daí, ele faz a mesma coisa, ou seja Map[i] = 0 de i = X até i = 800.



for(_Y, _Fim, _Fim, Map)	->	Map;
for(Y, Ini, Fim, Map)	->	ModMap = array:set(Ini, Y, Map), 
for(Y, Ini+1, Fim, ModMap).



%%%%esse é o for que eu tentei entender e implementar. Parece justo?



