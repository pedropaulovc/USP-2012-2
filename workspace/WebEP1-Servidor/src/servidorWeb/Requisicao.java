package servidorWeb;

import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.Socket;
import java.util.StringTokenizer;

import static servidorWeb.Cabecalho.CRLF;

final class Requisicao {
	String diretorioBase;
	Socket socket;
	private BufferedReader input;
	private DataOutputStream output;
	private Cabecalho cabecalho;

	public Requisicao(Socket socket, String diretorioBase) throws Exception {
		this.socket = socket;
		this.input = new BufferedReader(new InputStreamReader(
				socket.getInputStream()));
		this.output = new DataOutputStream(socket.getOutputStream());
		this.diretorioBase = diretorioBase;
		this.cabecalho = new Cabecalho(output);
	}

	public void processa() throws Exception {
		String requestLine = input.readLine();
		if (requestLine == null) {
			exibirErro(400, "Não há requisicao a ser processada");
			encerrar();
			return;
		}

		// Extract the filename from the request line.
		// TODO: Tratar entradas malformadas
		StringTokenizer tokens = new StringTokenizer(requestLine);

		tokens.nextToken(); // GET, POST...
		String requisicao = diretorioBase + tokens.nextToken();

		File req = new File(requisicao);

		if (req.exists())
			exibirResultado(requisicao, req);
		else 
			exibirErro(404, requisicao + " não encontrado");
		
		encerrar();
	}

	private void exibirResultado(String requisicao, File req) throws Exception {
		cabecalho.definirStatus(200);
		if (req.isDirectory())
			cabecalho.definirLinha("Content-Type: text/plain");
		else
			cabecalho.definirLinha("Content-Type: " + contentType(requisicao));
		cabecalho.enviar();
		
		if(req.isDirectory()){
			output.writeBytes("Listando diretório " + requisicao + CRLF + CRLF);
			String[] arquivos = req.list();
			if (arquivos != null)
				for (String arquivo : arquivos)
					output.writeBytes(arquivo + CRLF);
		} else if (req.isFile()) {
			sendBytes(new FileInputStream(req), output);
		}

	}

	private void exibirErro(int codigo, String mensagem) throws IOException {
		cabecalho.definirStatus(codigo).definirLinha("Content-Type: text/plain")
				.enviar();
		output.writeBytes(mensagem + CRLF);
	}

	private void encerrar() throws IOException {
		output.close();
		input.close();
		socket.close();
	}

	private static void sendBytes(FileInputStream fis, OutputStream os)
			throws Exception {
		byte[] buffer = new byte[1024];
		int bytes = 0;
		while ((bytes = fis.read(buffer)) != -1) {
			os.write(buffer, 0, bytes);
		}
	}

	private static String contentType(String fileName) {
		if (fileName.endsWith(".htm") || fileName.endsWith(".html")) {
			return "text/html";
		}
		if (fileName.endsWith(".jpg") || fileName.endsWith(".jpeg")) {
			return "image/jpeg";
		}
		if (fileName.endsWith(".gif")) {
			return "image/gif";
		}
		if (fileName.endsWith(".txt")) {
			return "text/plain";
		}
		if (fileName.endsWith(".pdf")) {
			return "application/pdf";
		}
		return "application/octet-stream";
	}
}