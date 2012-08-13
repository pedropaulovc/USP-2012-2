package servidorWeb;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;

public class Bfhtml {
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
