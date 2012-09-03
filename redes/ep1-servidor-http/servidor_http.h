/**
* Aluno: Pedro Paulo Vezzá Campos - 7538743
* MAC0448-2012 - Programação para Redes de Computadores - Tarefa 1: Servidor Web
* Sobre o arquivo: Aqui são declarados e documentados todos os protótipos das 
* funções e estruturas relevantes ao servidor web a serem implementados no 
* arquivo servidor_http.c. 
*/

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <limits.h>
#include <time.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <netdb.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <arpa/inet.h>

/**
 * Tamanho máximo de uma string lida ou enviada pelo servidor de uma vez.
 */
#define MAXLINE 4096
/**
 * Nome e versão do servidor, a ser enviada nos cabeçalhos HTTP
 */
#define NOME_SERVIDOR "Tupi/0.0.1"
/**
 * Quantidade máxima de campos armazenáveis vindos de uma requisição POST.
 */
#define MAXPOST 32

/**
 * Tipos implementados de requisição HTTP: GET e POST.
 */
typedef enum {
	GET,
	POST,
	REQ_INVALIDA
} tipo_requisicao;

/**
 * Versões do protocolo HTTP conhecidos pelo servidor.
 */
typedef enum {
	HTTP10,
	HTTP11,
	PROT_INVALIDO
} protocolo_http;


/**
 * Estrutura que encapsula as informações relevantes ao servidor lidas do 
 * cabeçalho enviado pelo cliente. Contém:
 *  - tipo: O tipo da requisição (GET, POST)
 *  - caminho: O endereço pedido pelo cliente (Ex. /formulario.html)
 *  - protocolo: O tipo de protocolo enviado pelo cliente.
 */
typedef struct {
	tipo_requisicao tipo;
	char caminho[MAXLINE + 1];
	protocolo_http protocolo;
} requisicao;


/**
 * Estrutura que encapsula as informações a serem enviadas ao cliente como 
 * resposta do servidor. Contém:
 *  - status: O código de status da requisição (200, 404)
 *  - protocolo: O protocolo a ser utilizado pelo servidor na resposta
 *  - arquivo: O arquivo a ser enviado pelo servidor no caso de uma requisição
 * GET. Caso status não seja 200, deve conter o caminho da página de erro a ser
 * enviada pelo servidor.
 */
typedef struct {
	int status;
	protocolo_http protocolo;
	char arquivo[PATH_MAX + MAXLINE + 1];
} resposta;


/**
 * Caminho absoluto do diretório www escolhido na execução do servidor. Caso
 * tenha sido omitido, é escolhido o mesmo diretório de execução do programa.
 */
char diretorio_www[PATH_MAX + 1];
/**
 * Buffer a ser utilizado nas operações de IO do servidor.
 */
char buffer[MAXLINE + 1];
/**
 * Espaço ocupado atualmente no buffer.
 */
int tam_buffer;
/**
 * O descritor do socket utilizado nesta conexão com o cliente.
 */
int socket_conexao;
/**
 * Vetor de chaves dos campos POST enviados. Ex: Numa requisição que envia:
 * "campo1=valor1&campo2=valor2", chaves_post[0] == "campo1" e chaves_post[1] ==
 *"campo2".
 */
char chaves_post[MAXPOST][MAXLINE + 1];
/**
 * Vetor de valores dos campos POST enviados. Ex: Numa requisição que envia:
 * "campo1=valor1&campo2=valor2", chaves_post[0] == "valor1" e chaves_post[1] ==
 *"valor2".
 */
char valores_post[MAXPOST][MAXLINE + 1];
/**
 * A quantidade de campos lidos de uma requisição POST.
 */
int qtd_campos_post;

/**
 * Recebe como parâmetro a linha inicial da requisição do cliente (Ex. "GET / 
 * HTTP/1.1"), parseia a linha e produz como resultado uma struct 
 * do tipo requisicao contendo as informações lidas.
 */
requisicao interpretar_requisicao(char requisicao[]);
/**
 * Recebe como parâmetro uma struct do tipo resposta devidamente populada 
 * e o tipo da requisição pedida pelo cliente. Com estes dados envia todos os
 * cabeçalhos relevantes ao cliente, tais como: resultado da requisição (200 OK),
 * Date, Server, Last-Modificed, Etag, Accept-Ranges, 
 * Content-Length, Vary, Connection e Content-Type. Termina o envio de cabeçalhos
 * com o envio de uma quebra de linha (\r\n) extra.
 */
void enviar_resposta(resposta resp, tipo_requisicao tipo);
/**
 * Recebe o caminho para o arquivo a ser enviado após os cabeçalhos. Divide a
 * operação em blocos de MAXLINE bytes.
 */
void enviar_arquivo(char nome_arquivo[]);
/**
 * Recebe como parâmetro o nome de um arquivo que se deseja obter o Content-Type
 * correspondente retorna uma string estaticamente alocada contendo o valor
 * correspondente. Aceita arquivos .htm(l), .jp(e)g, .gif, .txt, .pdf. Para outros
 * arquivos retorna application/octet-stream.
 */
char* obter_content_type(char nome_arquivo[]);
/**
 * Recebe como parâmetro um caminho para o diretório de onde serão servidos os
 * arquivos pelo servidor web. Caso receba NULL como parâmetro define o diretório
 * de execução como sendo o diretório www.
 */
void definir_diretorio_www(char diretorio[]);
/**
 * Recebe como parâmetrox uma string "string" de tamanho "tam_string" e armazena
 * no buffer do servidor. Caso o buffer não tenha capacidade vaga o suficiente,
 * envia seu conteúdo atual e o substitui pelo conteúdo de "string". O 
 * comportamento para valores de "tam_string" maiores que MAXLINE é
 * indeterminado.
 */
void escrever_buffer(char string[], int tam_string);
/**
 * Envia os conteúdos atualmente contidos no buffer para o cliente e marca o 
 * buffer como vazio novamente.
 */
void enviar_buffer();
/**
 * Função principal do servidor web. Recebe como parâmetro o descritor do socket
 * utilizado pelo programa para comunicação com o cliente. Com o auxílio das
 * funções descritas neste módulo é responsável por ler a requisição do cliente,
 * interpretar o pedido, e enviar a resposta adequada. Aceita requisições GET e
 * POST. No primeiro caso, envia o arquivo requisitado caso exista, o index.html
 * caso seja requisitado um diretório ou uma página de erro 404.html caso o 
 * arquivo não exista. No segundo caso, envia como resposta os campos lidos do
 * usuário.
 * 
 * Obs: A página 404.html deve residir no diretório www (O de execução do 
 * programa, por default. 
 */
void executar_servidor_http(int socket);
/**
 * Função responsável por ler uma linha terminada com \r\n ou \n do socket e 
 * escrever na string passada por parâmetro linha. O tamanho máximo da string
 * é definida no parâmetro tam_max. linha ao final da execução é uma string 
 * terminada com \0 e sem os caracteres de quebra de linha.
 */
int ler_linha(char linha[], int tam_max);
/**
 * Dada uma linha contendo o conteúdo da requisição POST vinda do cliente
 * (Ex. "campo1=valor1&campo2=valor2") faz o parsing desta string e popula as
 * variáveis qtd_campos_post, chaves_post e valores_post com os valores lidos
 * desta string.
 */
void interpretar_post(char linha[]);
/**
 * Envia de volta ao usuário os conteúdos armazenados nas variáveis 
 * qtd_campos_post, chaves_post e valores_post em um formato tabular em texto 
 * puro.
 */
void enviar_conteudo_post();

