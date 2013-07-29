import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import Jama.Matrix;

public class Ranking {

	private final double DAMPING_FACTOR = 0.85;

	private List<String> params = new ArrayList<String>();

	public static void main(String[] args) {

		Ranking ranking = new Ranking();

		// Imprime os PageRanks obtidos
		System.out.println("PR(A) = " + ranking.rank("A"));
		System.out.println("PR(B) = " + ranking.rank("B"));
		System.out.println("PR(C) = " + ranking.rank("C"));

	}

	// Resolve a equa��o linear AX = B, onde A e a matrix de constantes, X s�o
	// os PageRanks que queremos
	// e B � a matriz n x 1 de fatores de amortecimento.
	public double rank(String pageId) {

		generateParamList(pageId);

		Matrix a = new Matrix(generateMatrix());

		double[][] arrB = new double[params.size()][1];

		for (int i = 0; i < params.size(); i++) {

			arrB[i][0] = 1 - DAMPING_FACTOR;

		}

		Matrix b = new Matrix(arrB);

		// Resolve a equa��o e pega os PageRanks

		Matrix x = a.solve(b);

		int ind = 0;

		int cnt = 0;

		for (Iterator<String> it = params.iterator(); it.hasNext();) {

			String curPage = (String) it.next();

			if (curPage.equals(pageId))

				ind = cnt;

			cnt++;

		}

		return x.getArray()[ind][0];

	}

	// Gera a matriz n x n, onde n � o n�mero de p�ginas relacionadas
	private double[][] generateMatrix() {

		double[][] arr = new double[params.size()][params.size()];

		for (int i = 0; i < params.size(); i++) {

			for (int j = 0; j < params.size(); j++) {

				arr[i][j] = getMultiFactor((String) params.get(i),

				(String) params.get(j));

			}

		}

		return arr;

	}

	/****************************** ATIVIDADE DE IMPLEMENTA��O *****************************************/
	// Retorna a constante da equa��o linear da vari�vel dada
	private double getMultiFactor(String sourceId, String linkId) {

		if (sourceId.equals(linkId))

			return 1;

		else {
			// Pege a lista de p�ginas que tem link pra p�gina

			// Percorra essa lista e veja se a p�gina linkId passada tem
			// rela��o.

			// Se tiver calcule a constante que deve ir para a matriz

			// Caso contr�rio devolvemos "algo", que eu j� aviso n�o � 33 ; )
			String[] linksSaida = getInboundLinks(sourceId);

			for (int i = 0; i < linksSaida.length; i++) {
				if (linksSaida[i].equals(linkId)) {
					return -1
							* (DAMPING_FACTOR / getOutboundLinks(linkId).length);
				}
			}
		}

		return 0;

	}

	/**************************************************************************************/

	// Retorna a lista de paginas relacionadas, que � tamb�m a lista de
	// parametr�s, PR(A), PR(B)...
	private void generateParamList(String pageId) {

		// Adiciona a primeira p�gina
		if (!params.contains(pageId))

			params.add(pageId);

		// Pega as p�ginas que tem links da p�gina - INBOUND PAGES
		String[] inc = getInboundLinks(pageId);

		// Adiciona os links da lista na lista dos par�metros
		for (int i = 0; i < inc.length; i++) {

			if (!params.contains(inc[i]))

				generateParamList(inc[i]);

		}

	}

	/************************* ATIVIDADE DE EXPERIMENTA��O *************************************/

	// Aqui simulamos uma cole��o de p�ginas - INBOUND LINKS
	// Retorna a lista de INBOUND LINKS de uma p�gina
	// Brinquem com a rela��o entre as p�ginas e veja o que ocorre quando
	// realizamos essas mudan�as
	private String[] getInboundLinks(String pageId) {

		Map<String, String[]> map = new HashMap<String, String[]>();

		map.put("A", new String[] { "C" });

		map.put("B", new String[] { "A", "C" });

		map.put("C", new String[] { "A", "B" });

		return (String[]) map.get(pageId);

	}

	// Aqui simulamos uma cole��o de p�ginas - OUTBOUND LINKS
	// Retorna a lista de OUTBOUND LINKS de uma p�gina
	// Brinquem com a rela��o entre as p�ginas e veja o que ocorre quando
	// realizamos essas mudan�as
	private String[] getOutboundLinks(String pageId) {

		Map<String, String[]> map = new HashMap<String, String[]>();

		map.put("A", new String[] { "B", "C" });

		map.put("B", new String[] { "C" });

		map.put("C", new String[] { "A", "B", "D", "E", "F" });

		return (String[]) map.get(pageId);

	}

	/**********************************************************************************/

}
