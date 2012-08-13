package servidorWeb;

import java.net.ServerSocket;
import java.net.Socket;

/**
 * Classe principal do servidor web, contendo o método main. Responsável por
 * aceitar novas conexões e repassá-las a um novo objeto Requisicao que
 * executará em uma thread separada.
 * 
 * @author Pedro Paulo Vezzá Campos
 * 
 */
public final class WebServer {
	public static final String CRLF = "\r\n";
	public static final String arquivoAutorizados = ".autorizados";
	public static final String nome = "Guarani/0.0.1 (Java)";

	/**
	 * Método main do projeto. Aceita novas conexões e as repass a um novo
	 * objeto (Thread) Requisicao.
	 * 
	 * @param argv
	 *            Vetor de argumentos de linha de comando
	 * @throws Exception
	 */
	public static void main(String argv[]) throws Exception {
		int porta = 8080; // Porta que o servidor ouvirá
		String diretorioBase = System.getProperty("user.dir") + "/src/www";

		System.out.println("Servidor Web iniciado." + CRLF);

		ServerSocket socket = new ServerSocket(porta); // Cria um socket

		while (true) { // Loop infinito aguardando conexões
			Socket conexaoSocket = socket.accept(); // Escuta o socket

			Requisicao request = new Requisicao(conexaoSocket, diretorioBase);

			request.run();
		}

	}
}