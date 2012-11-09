package filmes;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.jgap.Configuration;
import org.jgap.DefaultFitnessEvaluator;
import org.jgap.Gene;
import org.jgap.Genotype;
import org.jgap.IChromosome;
import org.jgap.impl.DefaultConfiguration;

import filmes.controlador.GeneticoControlador;
import filmes.modelo.CategoriasFilmes;
import filmes.modelo.FilmeInfo;

/**
 * Gerencia a execução do algoritmo genético
 * 
 * @author rodrigo Last updated: Apr 14, 2012
 */
public class FilmesMain {
	private static int MAX_ALLOWED_EVOLUTIONS = 50;

	/* configuração do algoritmo */
	private Configuration conf;

	/* controlador para criação das estruturas */
	private GeneticoControlador genControlador;

	/**
	 * Construtor: Configura o algoritmo genético com a lista de compra, no
	 * caso, uma lista com os códigos dos produtos a serem comprados.
	 * 
	 * @param listaCompra
	 * @throws Exception
	 */
	public FilmesMain(Map<String, List<FilmeInfo>> categorias) throws Exception {
		/*
		 * inicia a configuração com DefaultConfiguration (configurações mais
		 * comuns)
		 */
		conf = new DefaultConfiguration();

		/* inicia os controladores */
		genControlador = new GeneticoControlador(conf);

		/* mantém para a próxima geração a melhor população */
		conf.setPreservFittestIndividual(true);

		/* permite que o tamanho da população possa aumentar */
		conf.setKeepPopulationSizeConstant(false);

		/* inicializa e configura a função de custo */
		conf.setFitnessFunction(genControlador.inializaFuncaoCusto());

		/* modifica o avaliador: maior é melhor */
		Configuration.resetProperty(Configuration.PROPERTY_FITEVAL_INST);
		conf.setFitnessEvaluator(new DefaultFitnessEvaluator());

		/* Gene: Categoria de filme */
		/* Alelo: Filmes possíveis */
		for (List<FilmeInfo> filmes : categorias.values()) {
			genControlador.adicionaGene(filmes);
		}

		/* define e configura a estrutura do cromossomo */
		IChromosome cromossomo = genControlador.defineCromossomo();
		conf.setSampleChromosome(cromossomo);

		/*
		 * Define o tamanho da população, ou seja, quantos cromossomos compõem a
		 * população. Quanto mais cromossomos, maior o número de soluções
		 * potenciais, mas maior o tempo para evoluir a população.
		 */
		conf.setPopulationSize(30);
	}

	public void executa() throws Exception {
		/*
		 * Cria uma população inicial aleatória de cromossomos. Poderia ser lido
		 * de um arquivo (mas não foi!).
		 */
		Genotype populacao = Genotype.randomInitialGenotype(conf);

		/*
		 * Evolui a população. Como não sabemos a melhor resposta faremos a
		 * evolução um número finito de vezes.
		 */
		long startTime = System.currentTimeMillis();
		for (int i = 0; i < MAX_ALLOWED_EVOLUTIONS; i++) {
			System.out.println("População iteração " + i);
			exibePopulacao(populacao);
			System.out.println("=============================");
			if (!GeneticoControlador.uniqueChromosomes(populacao
					.getPopulation())) {
				throw new RuntimeException("Estado inválido na geração." + i);
			}
			populacao.evolve();
		}
		long endTime = System.currentTimeMillis();

		/* Apresenta a melhor solução */
		exibeSolucao(populacao, endTime, startTime);
	}

	private void exibeSolucao(Genotype population, long endTime, long startTime) {
		IChromosome melhorCromossomo = population.getFittestChromosome();

		Gene[] genes = melhorCromossomo.getGenes();
		for (Gene gene : genes) {
			FilmeInfo p = (FilmeInfo) gene.getAllele();
			System.out.println("Filme: " + p.getTitle());
			System.out.println("Nota: " + p.getImdbRating());
		}
	}
	
	private void exibePopulacao(Genotype population) {
		for (IChromosome c : population.getChromosomes()){
			for (Gene gene : c.getGenes()){
				FilmeInfo p = (FilmeInfo) gene.getAllele();
				System.out.printf("%20.20s (%.1f) | ", p.getTitle(), p.getImdbRating());
			}
			System.out.println();
		}
	}

	/**
	 * Método Principal
	 */
	public static void main(String[] args) {
		try {
			Map<String, List<FilmeInfo>> categorias = new HashMap<String, List<FilmeInfo>>();

			categorias.put("Comedia", CategoriasFilmes.obterComedia());
			categorias.put("Acao", CategoriasFilmes.obterAcao());
			categorias.put("Drama", CategoriasFilmes.obterDrama());
			
			System.out.println(CategoriasFilmes.obterComedia().get(0));
			
			FilmesMain genAlg = new FilmesMain(categorias);
			genAlg.executa();
		} catch (Exception ex) {
			ex.printStackTrace();
		}
		
	} // main
}