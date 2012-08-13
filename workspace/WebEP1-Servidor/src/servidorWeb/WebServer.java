package servidorWeb;
import java.net.ServerSocket;
import java.net.Socket;

public final class WebServer {
	public static final String CRLF = "\r\n";
	public static final String arquivoAutorizados = ".autorizados";
	public static final String nome = "Guarani/0.0.1 (Java)";
	
	public static void main(String argv[]) throws Exception {
		int porta = 8080; // Porta que o servidor ouvirá
		String diretorioBase = "/home/bcc/pedrovc/www"; // diretório onde estarão os arquivos

		System.out.println("Servidor Web iniciado." + CRLF);
		
		ServerSocket socket = new ServerSocket(porta); // Cria um socket

		while (true) { // Loop infinito aguardando conexões
			Socket conexaoSocket = socket.accept(); // Escuta o socket

			Requisicao request = new Requisicao(conexaoSocket, diretorioBase);

			request.processa();
		}

	}
}