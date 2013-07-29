package exercicios;

import iweb2.ch3.collaborative.data.BaseDataset;
import iweb2.ch3.collaborative.data.MovieLensData;
import iweb2.ch3.collaborative.data.MovieLensDataset;
import iweb2.ch3.collaborative.data.MusicData;
import iweb2.ch3.collaborative.data.MusicUser;
import iweb2.ch3.collaborative.model.Item;
import iweb2.ch3.collaborative.model.Rating;
import iweb2.ch3.collaborative.model.User;
import iweb2.ch3.collaborative.recommender.Delphi;
import iweb2.ch3.collaborative.recommender.MovieLensDelphi;
import iweb2.ch3.collaborative.similarity.RecommendationType;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

public class Main {

	public static void main(String[] args) {
		MusicUser[] mu = MusicData.loadExample();

		// Ex 1. Usando o método getSimilarity() ache a similaridade entre os 3
		// usuários usando os dois simTypes

		int k = 0;
		double[] similaridades = new double[8];
		for(int i = 0; i < 3; i++)
			for(int j = i + 1; j < 3; j++){
				similaridades[k++] = mu[i].getSimilarity(mu[j], 0);
				similaridades[k++] = mu[i].getSimilarity(mu[j], 1);
			}
		
		BaseDataset ds = MusicData.createDataset();
			
		Delphi delphiUser = new Delphi(ds, RecommendationType.USER_BASED);
		delphiUser.setVerbose(true);
		
		
		//
		// //
		// // Show me users like X (top 5)
		// //
		MusicUser mu1 = (MusicUser) ds.pickUser("Bob");
		delphiUser.findSimilarUsers(mu1);

		MusicUser mu2 = (MusicUser) ds.pickUser("John");
		delphiUser.findSimilarUsers(mu2);

		// //
		// // Show me recommendations for user X (top 5)
		// //
		delphiUser.recommend(mu1);

		// Ex2. Fazer a recomendação acima por itens e ver as diferenças no
		// resultado

		
		MovieLensDataset mds = MovieLensData.createDataset();

		// Create the recommender
		MovieLensDelphi delphi = new MovieLensDelphi(mds);
		List<Item> itens = new ArrayList<Item>();

		itens.add(0, mds.getItem(1)); // Toy Story
		itens.add(1, mds.getItem(8)); // Babe
		itens.add(2, mds.getItem(38)); // A rede
		itens.add(3, mds.getItem(50)); // Star Wars
		itens.add(4, mds.getItem(56)); // Pulp Fiction
		itens.add(5, mds.getItem(67)); // Ace Ventura: Detetive de animais
		itens.add(6, mds.getItem(71)); // O rei leão
		itens.add(7, mds.getItem(98)); // O silencio dos inocentes
		itens.add(8, mds.getItem(154)); // Monty Python: A vida de Brian
		itens.add(9, mds.getItem(187)); // O poderoso chefão: parte II
		itens.add(10, mds.getItem(196)); // Sociendade dos poetas mortos
		itens.add(11, mds.getItem(313)); // Titanic

		User u = new User(1000);

		List<Rating> ratings = new ArrayList<Rating>();
		
		Random rand = new Random();
		for(int i = 0; i < 11; i++)
			ratings.add(new Rating(u, itens.get(i), rand.nextInt(10)));
		

		// Ex3. Use o método addRating para dar notas aos filmes acima use o
		// delphi para ver quais itens ele te recomenda (Usando o arquivo
		// movies.dat, é possível pegar
		// mais filmes para dar notas, pois quanto mais dados, melhor é a
		// precisão do algoritmo)

		for (Rating rating : ratings) {
			rating.getItem().addUserRating(rating);
		}

		delphi.recommend(u);

	}
}
