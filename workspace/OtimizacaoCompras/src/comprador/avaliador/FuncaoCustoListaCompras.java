package comprador.avaliador;

import comprador.modelo.ProdutoInfo;
import org.jgap.FitnessFunction;
import org.jgap.Gene;
import org.jgap.IChromosome;

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
public class FuncaoCustoListaCompras extends FitnessFunction {

    /* Construtor */
    public FuncaoCustoListaCompras() {
    }

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
            ProdutoInfo p = (ProdutoInfo) gene.getAllele();
            soma += p.getPreco();
        }

        return soma; //quanto menor, melhor
    }

}
