package servidorWeb;

import java.util.HashMap;
import java.util.Map;

/**
 * Classe representante de um cookie recebido de um cliente. Quebra-o em partes
 * para posterior uso.
 * 
 * @author Pedro Paulo Vezzá Campos - 7538743
 * 
 */
public class Cookie {
	private Map<String, String> valores = new HashMap<String, String>();

	/**
	 * Construtor do objeto, recebe uma string representando os dados do cookie.
	 * Ex: "campo1=valor1; campo2=valor2"
	 * 
	 * @param cookie
	 *            uma string representando os dados do cookie.
	 */
	public Cookie(String cookie) {
		parsearCookie(cookie);
	}

	/**
	 * Método responsável por dada uma string representando um cookie, quebrá-lo
	 * em partes e armazená-las.
	 * 
	 * @param cookie
	 *            uma string representando os dados do cookie.
	 */
	private void parsearCookie(String cookie) {
		if (cookie == null)
			return;

		String[] campos = cookie.split(";\\s+");
		for (String campo : campos) {
			String[] chaveValor = campo.split("=");
			if (chaveValor.length > 1)
				valores.put(chaveValor[0], chaveValor[1]);
		}
	}

	/**
	 * Método de obtenção do valor de uma dada chave armazenada no cookie 
	 * @param chave Chave do valor armazenado
	 * @return O valor ou null caso não exista.
	 */
	public String obterCampo(String chave) {
		return valores.get(chave);
	}
}
