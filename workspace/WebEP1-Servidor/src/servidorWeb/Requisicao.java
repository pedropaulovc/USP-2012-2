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

/**
 * Classe representante de uma requisição HTTP. É uma thread reponsável por
 * interpretar a requisição e responder de maneira adequada, inclusive recebendo
 * e enviando cabeçalhos HTTP. É capaz de interpretar pedidos de: Listar
 * diretório; Enviar arquivos estáticos; Executar a interpretação de arquivos
 * .bfhtml; Suporta um contador de acesso implementado usando Cookies;
 * Autenticação simples HTTP.
 * 
 * @author Pedro Paulo Vezzá Campos - 7538743
 * 
 */
final class Requisicao extends Thread {
	String diretorioBase;
	Socket socket;
	private BufferedReader input;
	private DataOutputStream output;
	private CabecalhoSaida cabecalhoSaida;
	private CabecalhoEntrada cabecalhoEntrada;

	/**
	 * Construtor do objeto.
	 * 
	 * @param socket
	 *            O socket de comunicação com o cliente.
	 * @param diretorioBase
	 *            O diretório de trabalho, de onde serão buscados os arquivos.
	 * @throws Exception
	 */
	public Requisicao(Socket socket, String diretorioBase) throws Exception {
		this.socket = socket;
		this.input = new BufferedReader(new InputStreamReader(socket
				.getInputStream()));
		this.output = new DataOutputStream(socket.getOutputStream());
		this.diretorioBase = diretorioBase;
		this.cabecalhoSaida = new CabecalhoSaida(output);
		this.cabecalhoEntrada = new CabecalhoEntrada(input);
	}

	/**
	 * Método run da thread. Responsável por invocar o processamento da
	 * requisição e encerramento dos recursos alocados.
	 */
	public void run() {
		try {
			processa();
			encerrar();
		} catch (Exception e) {
		}
	}

	/**
	 * Método de processamento da requisição. Utilizando objetos auxiliares
	 * analisa a requisição recebida para decidir qual resposta será utilizada.
	 * Invoca a análise de cookies, autenticação do usuário (Caso necessário) e
	 * retorna erros em requisições mal formadas.
	 * 
	 * @throws Exception
	 */
	public void processa() throws Exception {
		cabecalhoEntrada.ler();
		processarCookies();

		if (cabecalhoEntrada.obterUrl() == null) {
			enviarErro(400, "Não há requisicao a ser processada");
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
			enviarErro(404, requisicao + " não encontrado");
	}

	/**
	 * Função responsável por realizar o controle e atualização do cookie
	 * contador de acessos.
	 */
	private void processarCookies() {
		Cookie cookie = new Cookie(cabecalhoEntrada.obterCampo("Cookie"));
		String qtdVisitas = cookie.obterCampo("qtd_visitas");

		if (qtdVisitas == null)
			qtdVisitas = "0";

		cabecalhoSaida.definirLinha("Set-Cookie: qtd_visitas="
				+ (Integer.parseInt(qtdVisitas) + 1));
	}

	/**
	 * Método responsável por enviar um desafio de autenticação (401) HTTP
	 * simples ao cliente caso ele não tenha fornecido uma credencial válida.
	 * 
	 * @param diretorio
	 *            Diretório onde será localizado o arquivo contendo os usuários
	 *            autenticados.
	 * @return true caso o usuário tenha se autenticado com sucesso ou false
	 *         caso contrário.
	 * @throws IOException
	 */
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

	/**
	 * Função responsável por invocar uma operação de exibição de algum arquivo
	 * ao usuário. Verifica se deve-se listar um diretório, enviar um arquivo ou
	 * interpretar um script.
	 * 
	 * @param requisicao
	 *            O caminho do arquivo requisitado
	 * @param req
	 *            O file handler corrspondente ao usuário.
	 * @throws Exception
	 */
	private void exibirResultado(String requisicao, File req) throws Exception {
		if (req.isDirectory())
			exibirDiretorio(requisicao, req);
		else if (requisicao.endsWith(".bfhtml"))
			interpretarScript(requisicao, req);
		else
			enviarArquivo(requisicao, req);
	}

	/**
	 * Função responsável por invocar o interpretador em um arquivo bfhtml e
	 * enviar seu resultado ao usuário.
	 * 
	 * @param requisicao
	 *            O caminho do arquivo requisitado
	 * @param req
	 *            O file handler corrspondente ao usuário.
	 * @throws IOException
	 */
	private void interpretarScript(String requisicao, File req)
			throws IOException {
		cabecalhoSaida.definirStatus(200);
		cabecalhoSaida.definirLinha("Content-Type: text/html");
		cabecalhoSaida.enviar();

		FileInputStream stream = new FileInputStream(req);
		try {
			FileChannel fc = stream.getChannel();
			MappedByteBuffer bb = fc.map(FileChannel.MapMode.READ_ONLY, 0, fc
					.size());
			/* Instead of using default, pass in a decoder. */
			String arquivo = Charset.defaultCharset().decode(bb).toString();

			output.writeBytes(Bfhtml.interpretar(arquivo));
		} finally {
			stream.close();
		}
	}

	/**
	 * Método responsável por enviar ao usuário uma listagem do diretório
	 * requisitado.
	 * 
	 * @param requisicao
	 *            O caminho do arquivo requisitado
	 * @param req
	 *            O file handler corrspondente ao usuário.
	 * @throws IOException
	 */
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

	/**
	 * Método responsável por enviar ao usuário um arquivo estático.
	 * 
	 * @param requisicao
	 *            O caminho do arquivo requisitado
	 * @param req
	 *            O file handler corrspondente ao usuário.
	 * @throws Exception
	 */
	private void enviarArquivo(String requisicao, File req) throws Exception {
		cabecalhoSaida.definirStatus(200);
		cabecalhoSaida.definirLinha("Content-Type: " + contentType(requisicao));
		cabecalhoSaida.enviar();

		sendBytes(new FileInputStream(req), output);
	}

	/**
	 * Função responsável por verificar se há a necessidade de autenticar o
	 * usuário para ter acesso ao recurso.
	 * 
	 * @param diretorio
	 *            O diretório do recurso pedido
	 * @return true caso haja um arquivo .autorizados no diretorio e false caso
	 *         contrário.
	 */
	private boolean necessitaAutenticacao(String diretorio) {
		File autorizados = new File(diretorio + "/" + arquivoAutorizados);
		return autorizados.exists();
	}

	/**
	 * Método responsável por enviar ao cliente um código de erro HTTP (4xx ou
	 * 5xx) seguido de uma mensagem explicativa.
	 * 
	 * @param codigo
	 *            O código a ser enviado.
	 * @param mensagem
	 *            A mensagem a ser enviada em seguida.
	 * @throws IOException
	 */
	private void enviarErro(int codigo, String mensagem) throws IOException {
		cabecalhoSaida.definirStatus(codigo).definirLinha(
				"Content-Type: text/plain").enviar();
		output.writeBytes(mensagem + CRLF);
	}

	/**
	 * Função responsável por desalocar os recursos ocupados pelo objeto.
	 * 
	 * @throws IOException
	 */
	public void encerrar() throws IOException {
		output.close();
		input.close();
		socket.close();
	}

	/**
	 * Função responsável por quebrar o envio de arquivos grandes em partes
	 * menores.
	 * 
	 * @param fis
	 *            O FileInputStream contendo o arquivo a ser enviado
	 * @param os
	 *            O OutputStream onde o arquivo será enviado
	 * @throws Exception
	 */
	private static void sendBytes(FileInputStream fis, OutputStream os)
			throws Exception {
		byte[] buffer = new byte[1024];
		int bytes = 0;
		while ((bytes = fis.read(buffer)) != -1) {
			os.write(buffer, 0, bytes);
		}
	}

	/**
	 * Função responsável por retornar o tipo MIME de um dado arquivo.
	 * 
	 * @param fileName
	 *            O nome do arquivo a ser processado.
	 * @return O tipo MIME determinado.
	 */
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