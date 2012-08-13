package servidorWeb;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.StringTokenizer;

/**
 * Classe responsável por encapsular operações de escrita em um cabeçalho HTTP
 * de saída.
 * 
 * Uso básico:
 * 
 * CabecalhoEntrada ce = new CabecalhoEntrada();
 * ce.ler().obterCampo("Cookie")
 * 
 * @author Pedro Paulo Vezzá Campos - 7538743
 */
public class CabecalhoEntrada {
	private BufferedReader input;
	private Map<String, String> campos = new HashMap<String, String>();
	private String comando;
	private String url;
	private String protocolo;
	
	/**
	 * Construtor do objeto com o buffer de entrada
	 * @param input O buffer de entrada
	 */
	public CabecalhoEntrada(BufferedReader input){
		this.input = input;
	}
	
	/**
	 * Lê os cabeçalhos recebidos do cliente.
	 * @return O próprio objeto
	 * @throws IOException
	 */
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
	
	/**
	 * Obter o comando HTTP (GET, POST, DELETE...)
	 * @return o comando recebido ou null caso não tenha sido encontrado
	 */
	public String obterComando(){
		return comando;
	}
	
	/**
	 * Obter a url relativa requisitada
	 * @return a url relativa recebida ou null caso não tenha sido encontrada
	 */
	public String obterUrl(){
		return url;
	}
	
	/**
	 * Obter o protocolo (HTTP 1.1, ...)
	 * @return o protocolo recebido ou null caso não tenha sido encontrado
	 */
	public String obterProtocolo(){
		return protocolo;
	}
	
	/**
	 * Obter um header específico
	 * @return o valor do header recebido ou null caso não exista.
	 */
	public String obterCampo(String campo){
		return campos.get(campo);
	}
}
