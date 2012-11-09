package comprador;

import comprador.buscape.Result;
import comprador.controlador.BuscapeControlador;
import comprador.controlador.GeneticoControlador;
import comprador.controlador.ProdutoInfoControlador;
import comprador.modelo.ProdutoInfo;
import java.util.ArrayList;
import java.util.List;
import org.jgap.Configuration;
import org.jgap.DeltaFitnessEvaluator;
import org.jgap.FitnessFunction;
import org.jgap.Gene;
import org.jgap.Genotype;
import org.jgap.IChromosome;
import org.jgap.impl.DefaultConfiguration;

/**
 * Gerencia a execução do algoritmo genético
 * @author rodrigo
 * Last updated: Apr 14, 2012
 */
public class GeneticoMain {
    private static int MAX_ALLOWED_EVOLUTIONS = 50;
    
    /* configuração do algoritmo */
    private Configuration conf;

    /* controlador para criação das estruturas */
    private GeneticoControlador genControlador;

    /* controloador para acesso aos dados do buscape */
    private BuscapeControlador buscapeControlador;

    /**
     * Construtor: Configura o algoritmo genético com a lista de compra, no caso,
     * uma lista com os códigos dos produtos a serem comprados.
     * @param listaCompra
     * @throws Exception
     */
    public GeneticoMain(List<Integer> listaCompra) throws Exception {
        /* inicia a configuração com DefaultConfiguration (configurações mais comuns) */
        conf = new DefaultConfiguration();

        /* inicia os controladores */
        genControlador = new GeneticoControlador(conf);
        buscapeControlador = new BuscapeControlador();

        /* mantém para a próxima geração a melhor população */
        conf.setPreservFittestIndividual(true);

        /* permite que o tamanho da população possa aumentar */
        conf.setKeepPopulationSizeConstant(false);

        /* inicializa e configura a função de custo */
        FitnessFunction funcaoCusto = genControlador.inializaFuncaoCusto();
        conf.setFitnessFunction(funcaoCusto);

        /* modifica o avaliador: menor é melhor */
        Configuration.resetProperty(Configuration.PROPERTY_FITEVAL_INST);
        conf.setFitnessEvaluator(new DeltaFitnessEvaluator());

        /* Acessa o site do buscape e gera um gene e seus alelos para cada produto consultado */
        for (Integer idProduto:listaCompra) {
            Result result = buscapeControlador.acessaListaOfertas(idProduto);
            List<ProdutoInfo> produtos = ProdutoInfoControlador.retornaListaProdutos(result);
            if(produtos.size() > 0)
            	genControlador.adicionaGene(produtos);
        }

        /* define e configura a estrutura do cromossomo */
        IChromosome cromossomo = genControlador.defineCromossomo();
        conf.setSampleChromosome(cromossomo);

        /* Define o tamanho da população, ou seja, quantos cromossomos
         * compõem a população. Quanto mais cromossomos, maior o número
         * de soluções potenciais, mas maior o tempo para evoluir a população.
         */
        conf.setPopulationSize(30);
    }

    public void executa() throws Exception {
        /*
         * Cria uma população inicial aleatória de cromossomos.
         * Poderia ser lido de um arquivo (mas não foi!).
         */
        Genotype populacao = Genotype.randomInitialGenotype(conf);

        /*
         * Evolui a população. Como não sabemos a melhor resposta faremos a evolução
         * um número finito de vezes.
         */
        long startTime = System.currentTimeMillis();
        for (int i = 0; i < MAX_ALLOWED_EVOLUTIONS; i++) {
        	System.out.println("=============================");
        	System.out.println("População iteração " + i);
        	exibeMelhorSolucaoEncontrada(populacao, 0, 0);
        	System.out.println("=============================");
            if (!GeneticoControlador.uniqueChromosomes(populacao.getPopulation())) {
                throw new RuntimeException("Estado inválido na geração." + i);
            }
            populacao.evolve();
        }
        long endTime = System.currentTimeMillis();

        /* Apresenta a melhor solução */
        exibeMelhorSolucaoEncontrada(populacao, endTime, startTime);
    }

    private void exibeMelhorSolucaoEncontrada(Genotype population, long endTime, long startTime) {
        IChromosome melhorCromossomo = population.getFittestChromosome();

        Gene[] genes = melhorCromossomo.getGenes();
        for (Gene gene : genes) {
            ProdutoInfo p = (ProdutoInfo) gene.getAllele();
            System.out.println("Produto: " + p.getDescricao());
            System.out.println("Loja: " + p.getLoja());
            System.out.println("Preço: R$ " + p.getPreco());
        }
    }

    /**
     * Método Principal
     */
    public static void main(String[] args) {
        try {
            List<Integer> listaCompras = new ArrayList<Integer>();
            listaCompras.add(381239); //Câmera Digital Sony Cyber W610
            listaCompras.add(291672); //XBOX 360 Slim 4GB
            listaCompras.add(326956); //TV LCD 42 - LG
            listaCompras.add(357087); //Notebook Sony SB35FBB - Core i5
            listaCompras.add(123725); //Lavadora de Roupa 9Kg - Brastemp
            listaCompras.add(161629); //Forno de Micro-ondas MEF33
            listaCompras.add(268107); //Pen Drive Kingston DataTraveler 4GB
            listaCompras.add(380546); //Impressora Samsung ML - 2165 Mono Laser

            GeneticoMain genAlg = new GeneticoMain(listaCompras);
            genAlg.executa();
        }
        catch (Exception ex) {
            ex.printStackTrace();
        }
    } //main
}