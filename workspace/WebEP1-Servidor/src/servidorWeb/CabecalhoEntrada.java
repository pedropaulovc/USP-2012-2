package servidorWeb;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.StringTokenizer;

public class CabecalhoEntrada {
	private BufferedReader input;
	private Map<String, String> campos = new HashMap<String, String>();
	private String comando;
	private String url;
	private String protocolo;
	
	public CabecalhoEntrada(BufferedReader input){
		this.input = input;
	}
	
	public CabecalhoEntrada ler() throws IOException{
		String linha = input.readLine();
		if(linha == null)
			return this;

		// Extract the filename from the request line.
		StringTokenizer tokens = new StringTokenizer(linha);

		if(tokens.hasMoreTokens())
			comando = tokens.nextToken(); // GET, POST...
		if(tokens.hasMoreTokens())
			url = tokens.nextToken();
		if(tokens.hasMoreTokens())
			protocolo = tokens.nextToken();
		
		linha = input.readLine();
		while(linha != null && linha.length() > 0){
			String[] campo = linha.split(": ");
			if(campo.length >= 2)
				campos.put(campo[0], campo[1]);
			linha = input.readLine();
		}
		
		return this;
	}
	
	public String obterComando(){
		return comando;
	}
	
	public String obterUrl(){
		return url;
	}
	
	public String obterProtocolo(){
		return protocolo;
	}
	
	public String obterCampo(String campo){
		return campos.get(campo);
	}
}
