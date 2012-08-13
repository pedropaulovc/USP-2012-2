package servidorWeb;

import java.util.HashMap;
import java.util.Map;

public class Cookie {
	private Map<String, String> valores = new HashMap<String, String>();

	public Cookie(String cookie) {
		parsearCookie(cookie);
	}

	private void parsearCookie(String cookie) {
		if(cookie == null)
			return;
		
		String[] campos = cookie.split(";\\s+");
		for (String campo : campos) {
			String[] chaveValor = campo.split("=");
			if (chaveValor.length > 1)
				valores.put(chaveValor[0], chaveValor[1]);
		}
	}
	
	public String obterCampo(String chave){
		return valores.get(chave);
	}
}
