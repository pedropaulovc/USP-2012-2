package servidorWeb;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.PrintStream;

public class Bfhtml {
	public static String interpretar(String entrada) {
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		PrintStream ps = new PrintStream(baos);
		BrainfuckInterpreter bfi = new BrainfuckInterpreter(1024, ps, null);
		
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

			bfi.init(1024);
			bfi.setProgram(codigo);
			bfi.start();
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
