package servidorWeb;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;

/**
 * Implementação do interpretador de arquivos .bfhtml. Substitui código brainfuck
 * incluido entre tags <% e %> pela saída do programa executado. 
 * @author Pedro Paulo Vezzá Campos - 7538743
 */
public class Bfhtml {
	/**
	 * Função responsável por receber código html possivelmente contendo código brainfuck
	 * incluido entre tags <% e %>. Substitui os trechos de código pela saída do programa.
	 * @param entrada O código html descrito
	 * @return O novo html com o código substituído pela saída do interpretador.
	 */
	public static String interpretar(String entrada) {
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		PrintStream ps = new PrintStream(baos);
		InterpretadorBf ibf = new InterpretadorBf(1024, ps, null);
		
		StringBuffer saida = new StringBuffer();
		int inicio = 0;
		int fim = entrada.indexOf("<%");

		if (fim == -1)
			return entrada;

		while (fim != entrada.length()) {
			saida.append(entrada.substring(inicio, fim));
			inicio = fim + 2;
			fim = entrada.indexOf("%>", inicio);
			String codigo = entrada.substring(inicio, fim);

			ibf.init(1024);
			ibf.setProgram(codigo);
			ibf.start();
			saida.append(baos.toString());

			inicio = fim + 2;
			fim = entrada.indexOf("<%", inicio);
			if (fim == -1)
				fim = entrada.length();
			saida.append(entrada.substring(inicio, fim));
		}

		return saida.toString();
	}
}
