package filmes.avaliador;

import org.jgap.FitnessFunction;
import org.jgap.Gene;
import org.jgap.IChromosome;

import filmes.modelo.FilmeInfo;

/**
 * Função de custo - Analisa um cromossomo e retorna o melhor custo segundo
 * o preço.
 * @author Pedro Paulo
 * Last updated: Apr 14, 2012
 *
 * TODO: Modificar a função de custo (evaluate) para considerar outros fatores
 * na avaliação.
 * 
 * Podem ser utilizados:
 * a) Quantidade de notas no IMDB ou Rotten Tomatoes
 * b) Nota ou Tomates Frescos/Podres no Rotten Tomatoes
 * c) Duracao do Filme
 * d) Data de Lancamento
 * 
 * Cuidado! Nem todos filmes tem todos os atributos disponíveis. Para
 * estes casos, serão retornados valores como 0 para notas ou tempo de 
 * duração ou a data atual para datas de lançamento desconhecidas. 
 */
public class FuncaoCustoFilmes extends FitnessFunction {

    /**
     * Implementa o avaliador de custo.
     * @param cromossomo contém os valores de entrada para a avaliação.
     * @return custo.
     */
    @Override
    protected double evaluate(IChromosome cromossomo) {
        Gene[] genes = cromossomo.getGenes();
        double soma = 0.0;
        for (Gene gene: genes) {
            FilmeInfo f = (FilmeInfo) gene.getAllele();

            //TODO: Incremente aqui a sua funcao de custo. Explore
            //os atributos que um FilmeInfo possui. No momento a
            //funcao de custo apenas soma a notas de todos os filmes
            //do cromossomo para indicar o quão "adaptado" é um 
            //cromossomo em relacao a outro.
            
            soma += f.getImdbRating();
        }

        return soma; //quanto maior, melhor
    }

}
