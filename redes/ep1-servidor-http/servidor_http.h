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
 * cabeçalhos relevantes ao cliente.
 *
 */
void enviar_resposta(resposta resp, tipo_requisicao tipo);
/**
 *
 *
 *
 *
 */
void enviar_arquivo(char nome_arquivo[]);
/**
 *
 *
 *
 *
 */
char* obter_content_type(char nome_arquivo[]);
/**
 *
 *
 *
 *
 */
void definir_diretorio_www(char diretorio[]);
/**
 *
 *
 *
 *
 */
void escrever_buffer(char string[], int tam_string);
/**
 *
 *
 *
 *
 */
void enviar_buffer();
/**
 *
 *
 *
 *
 */
void executar_servidor_http(int socket);
/**
 *
 *
 *
 *
 */
int ler_linha(char linha[], int tam);
/**
 *
 *
 *
 *
 */
void interpretar_post(char linha[]);
/**
 *
 *
 *
 *
 */
void enviar_conteudo_post();

