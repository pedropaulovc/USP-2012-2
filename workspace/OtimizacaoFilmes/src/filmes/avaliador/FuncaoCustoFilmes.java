package filmes.avaliador;

import org.jgap.FitnessFunction;
import org.jgap.Gene;
import org.jgap.IChromosome;

import filmes.modelo.FilmeInfo;

/**
 * Função de custo - Analisa um cromossomo e retorna o melhor custo segundo
 * o preço.
 * @author rodrigo
 * Last updated: Apr 14, 2012
 *
 * TODO: Modificar a função de custo (evaluate) para considerar outros fatores
 * na avaliação.
 * 
 * Podem ser utilizados:
 * a) preferência por loja: Por exemplo, Submarino.
 * b) avaliação do usuário: 0 a 10.
 * c) avaliação do EBit: Diamante, Ouro, Prata, Bronze.
 * d) número de comentários dos clientes
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
            soma += f.getImdbRating();
        }

        return soma; //quanto maior, melhor
    }

}
