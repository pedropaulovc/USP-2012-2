package mac0451.similaridade;

import org.apache.lucene.search.DefaultSimilarity;

/**
 * Exercícios:
 * 
 * O objetivo desta classe é fornecer os parâmetros utilizados pelo Solr (Lucene 
 * internamente) para o cálculo da fórmula que fornecerá a relevância do documento para
 * a busca realizada. O objetivo é testar parâmetros diferentes e verificar como eles afetam
 * a relevância dos resultados. 
 * 
 * 1) Leia http://www.lucenetutorial.com/advanced-topics/scoring.html para entender
 * os parâmetros que você irá alterar.
 * 
 * 2) Utilize os valores default apresentados e veja a ordenação para uma busca. 
 * 
 * 3) Teste novas possibilidades, por exemplo:
 *     - Aumente a importância do idf no resultado final em 5 vezes
 *     - Modifique a fórmula do tf para controlar keyword stuffing
 * 
 * IMPORTANTE: Ao final de cada edição neste arquivo você deve exportar este projeto como jar
 * e substituir o arquivo mac0451-similaridade.jar do diretório solr/dist. Em seguida deve 
 * encerrar a instância do Solr atual (Ctrl+C) e em seguida reiniciá-lo (No diretório
 * solr/example execute "java -jar start.jar")  
 */
public class MinhaSimilaridade extends DefaultSimilarity {

	private static final long serialVersionUID = 2522542095417547322L;

	@Override
	public float tf(float freq) {
		return 1;
	}

	@Override
	public float idf(int docFreq, int numDocs) {
		return 1;
	}
	
	@Override
	public float coord(int overlap, int maxOverlap) {
		return 1;
	}

	@Override
	public float queryNorm(float sumOfSquaredWeights) {
		return 1;
	}

}
