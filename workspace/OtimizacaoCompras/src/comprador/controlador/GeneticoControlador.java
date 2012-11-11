package comprador.controlador;

import comprador.modelo.ProdutoInfo;
import comprador.avaliador.FuncaoCustoListaCompras;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.jgap.Chromosome;
import org.jgap.Configuration;
import org.jgap.FitnessFunction;
import org.jgap.Gene;
import org.jgap.IChromosome;
import org.jgap.Population;
import org.jgap.impl.MapGene;

/**
 * Configura e gera partes importantes do Algoritmo Genetico.
 * @author rodrigo
 * Last updated: Apr 14, 2012
 */
public class GeneticoControlador {
    /* armazena a lista de possíveis genes */
    private List<Gene> genes;
    private final Configuration conf;

    public GeneticoControlador(Configuration conf) {
        genes = new ArrayList<Gene>();
        this.conf = conf;
    }

    public void adicionaGene(List<ProdutoInfo> lista) throws Exception {
        Gene gene = null;
        Map alleles = new HashMap();

        for (ProdutoInfo p : lista) {
            if (!alleles.containsKey(p.getLoja())) {
                alleles.put(p.getLoja(), p);
            }
        }

        gene = new MapGene(conf, alleles);
        genes.add(gene);
    }

    public IChromosome defineCromossomo() throws Exception {
        Gene[] arrayGenes = genes.toArray(new Gene[genes.size()]);

        IChromosome cromossomo = new Chromosome(conf, arrayGenes);

        return cromossomo;
    }

    public FitnessFunction inializaFuncaoCusto() {
        return new FuncaoCustoListaCompras();
    }


    /**
     * @param a_pop população a ser verificada
     * @return true se todos os cromossomos na população são únicos
     *
     * Autor original:
     * @author Klaus Meffert
     * @since 3.3.1
     */
     public static boolean uniqueChromosomes(Population a_pop) {
        /* verifica se todos os cromossomos são únicos */
        for (int i = 0; i < a_pop.size() - 1; i++) {
            IChromosome c = a_pop.getChromosome(i);
            for (int j = i + 1; j < a_pop.size(); j++) {
                IChromosome c2 = a_pop.getChromosome(j);
                if (c == c2) {
                    return false;
                }
            }
        }
        return true;
    }

}
