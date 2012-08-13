package servidorWeb;

import static servidorWeb.CabecalhoSaida.CRLF;
import static servidorWeb.WebServer.arquivoAutorizados;

import java.io.BufferedReader;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.Socket;
import java.nio.MappedByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.List;

final class Requisicao extends Thread{
	String diretorioBase;
	Socket socket;
	private BufferedReader input;
	private DataOutputStream output;
	private CabecalhoSaida cabecalhoSaida;
	private CabecalhoEntrada cabecalhoEntrada;

	public Requisicao(Socket socket, String diretorioBase) throws Exception {
		this.socket = socket;
		this.input = new BufferedReader(new InputStreamReader(
				socket.getInputStream()));
		this.output = new DataOutputStream(socket.getOutputStream());
		this.diretorioBase = diretorioBase;
		this.cabecalhoSaida = new CabecalhoSaida(output);
		this.cabecalhoEntrada = new CabecalhoEntrada(input);
	}

	public void run(){
		try {
			processa();
		} catch (Exception e) {
			System.err.println("Excecao ocorrida: " + e);
		}
	}
	
	public void processa() throws Exception {
		cabecalhoEntrada.ler();
		processarCookies();

		if (cabecalhoEntrada.obterUrl() == null) {
			exibirErro(400, "Não há requisicao a ser processada");
			encerrar();
			return;
		}

		System.out.println(cabecalhoEntrada.obterComando() + " "
				+ cabecalhoEntrada.obterUrl() + " "
				+ cabecalhoEntrada.obterProtocolo());

		String requisicao = diretorioBase + cabecalhoEntrada.obterUrl();

		File req = new File(requisicao);

		if (req.exists()) {
			if (!necessitaAutenticacao(req.getAbsolutePath())
					|| autenticar(req.getAbsolutePath()))
				exibirResultado(requisicao, req);
		} else
			exibirErro(404, requisicao + " não encontrado");

		encerrar();
	}

	private void processarCookies() {
		Cookie cookie = new Cookie(cabecalhoEntrada.obterCampo("Cookie"));
		String qtdVisitas = cookie.obterCampo("qtd_visitas");

		if (qtdVisitas == null)
			qtdVisitas = "0";

		cabecalhoSaida.definirLinha("Set-Cookie: qtd_visitas="
				+ (Integer.parseInt(qtdVisitas) + 1));
	}

	private boolean autenticar(String diretorio) throws IOException {
		BufferedReader br = new BufferedReader(new InputStreamReader(
				new DataInputStream(new FileInputStream(diretorio + "/"
						+ arquivoAutorizados))));

		String linha;
		List<String> autorizados = new ArrayList<String>();
		while ((linha = br.readLine()) != null)
			autorizados.add("Basic " + linha);
		br.close();

		String credenciais = cabecalhoEntrada.obterCampo("Authorization");
		if (credenciais == null || !autorizados.contains(credenciais)) {
			cabecalhoSaida
					.definirStatus(401)
					.definirLinha("Content-Type: text/plain")
					.definirLinha(
							"WWW-Authenticate: Basic realm=\"Forneca credenciais para continuar\"")
					.enviar();
			output.writeBytes("Área restrita do site.");
			return false;
		}

		return true;
	}

	private void exibirResultado(String requisicao, File req) throws Exception {
		if (req.isDirectory())
			exibirDiretorio(requisicao, req);
		else if (requisicao.endsWith(".bfhtml"))
			interpretarScript(requisicao, req);
		else
			enviarArquivo(requisicao, req);
	}

	private void interpretarScript(String requisicao, File req) throws IOException {
		cabecalhoSaida.definirStatus(200);
		cabecalhoSaida.definirLinha("Content-Type: text/html");
		cabecalhoSaida.enviar();
		
		FileInputStream stream = new FileInputStream(req);
		  try {
		    FileChannel fc = stream.getChannel();
		    MappedByteBuffer bb = fc.map(FileChannel.MapMode.READ_ONLY, 0, fc.size());
		    /* Instead of using default, pass in a decoder. */
		    String arquivo = Charset.defaultCharset().decode(bb).toString();
		    
		    output.writeBytes(Bfhtml.interpretar(arquivo));
		  }
		  finally {
		    stream.close();
		  }
	}

	private void exibirDiretorio(String requisicao, File req)
			throws IOException {
		cabecalhoSaida.definirStatus(200);
		cabecalhoSaida.definirLinha("Content-Type: text/plain");
		cabecalhoSaida.enviar();

		output.writeBytes("Listando diretório " + requisicao + CRLF + CRLF);
		String[] arquivos = req.list();
		if (arquivos != null)
			for (String arquivo : arquivos)
				output.writeBytes(arquivo + CRLF);
	}

	private void enviarArquivo(String requisicao, File req) throws Exception {
		cabecalhoSaida.definirStatus(200);
		cabecalhoSaida.definirLinha("Content-Type: " + contentType(requisicao));
		cabecalhoSaida.enviar();

		sendBytes(new FileInputStream(req), output);
	}

	private boolean necessitaAutenticacao(String diretorio) {
		File autorizados = new File(diretorio + "/" + arquivoAutorizados);
		return autorizados.exists();
	}

	private void exibirErro(int codigo, String mensagem) throws IOException {
		cabecalhoSaida.definirStatus(codigo)
				.definirLinha("Content-Type: text/plain").enviar();
		output.writeBytes(mensagem + CRLF);
	}

	public void encerrar() throws IOException {
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
		if (fileName.endsWith(".txt") || fileName.endsWith(".css")) {
			return "text/plain";
		}
		if (fileName.endsWith(".pdf")) {
			return "application/pdf";
		}
		return "application/octet-stream";
	}
}