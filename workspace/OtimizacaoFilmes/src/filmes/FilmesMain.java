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
 * @author Pedro Paulo, modificado para a versão de filmes
 */
public class FilmesMain {
	private static int MAX_ALLOWED_EVOLUTIONS = 50;

	/* configuração do algoritmo */
	private Configuration conf;

	/* controlador para criação das estruturas */
	private GeneticoControlador genControlador;

	/**
	 * Construtor: Configura o algoritmo genético as categorias dos filmes
	 * disponíveis e os filmes em cada categoria.
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

		/* Cromossomo: Conjunto de filmes, 1 de cada categoria */
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


	/**
	 * Método Principal
	 */
	public static void main(String[] args) {
		try {
			Map<String, List<FilmeInfo>> categorias = CategoriasFilmes.obterCategorias();
			Map<String, List<FilmeInfo>> escolhidas = new HashMap<String, List<FilmeInfo>>();
			
			escolhidas.put("action", categorias.get("action"));
			escolhidas.put("adventure", categorias.get("adventure"));
			escolhidas.put("animation", categorias.get("animation"));
//			escolhidas.put("biography", categorias.get("biography"));
//			escolhidas.put("comedy", categorias.get("comedy"));
//			escolhidas.put("crime", categorias.get("crime"));
//			escolhidas.put("documentary", categorias.get("documentary"));
//			escolhidas.put("drama", categorias.get("drama"));
//			escolhidas.put("family", categorias.get("family"));
//			escolhidas.put("fantasy", categorias.get("fantasy"));
//			escolhidas.put("film_noir", categorias.get("film_noir"));
//			escolhidas.put("game_show", categorias.get("game_show"));
//			escolhidas.put("history", categorias.get("history"));
//			escolhidas.put("horror", categorias.get("horror"));
//			escolhidas.put("music", categorias.get("music"));
//			escolhidas.put("musical", categorias.get("musical"));
//			escolhidas.put("mystery", categorias.get("mystery"));
//			escolhidas.put("news", categorias.get("news"));
//			escolhidas.put("reality_tv", categorias.get("reality_tv"));
//			escolhidas.put("romance", categorias.get("romance"));
//			escolhidas.put("sci_fi", categorias.get("sci_fi"));
//			escolhidas.put("sport", categorias.get("sport"));
//			escolhidas.put("talk_show", categorias.get("talk_show"));
//			escolhidas.put("thriller", categorias.get("thriller"));
//			escolhidas.put("war", categorias.get("war"));
//			escolhidas.put("western", categorias.get("western"));

			FilmesMain genAlg = new FilmesMain(escolhidas);
			genAlg.executa();
		} catch (Exception ex) {
			ex.printStackTrace();
		}
		
	} // main
	
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

	public void exibirFilmes(){
		Map<String, List<FilmeInfo>> categorias = CategoriasFilmes.obterCategorias();
		
		for(String categoria : categorias.keySet()){
			System.out.println("==== " + categoria + " ====");
			for (FilmeInfo f : categorias.get(categoria)){
				System.out.println(f);
			}
		}
	}
	
}