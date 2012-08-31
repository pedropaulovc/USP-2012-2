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


#define MAXLINE 4096
#define NOME_SERVIDOR "Tupi/0.0.1"
#define MAXPOST 32

typedef enum {
	GET,
	POST,
	REQ_INVALIDA
} tipo_requisicao;

typedef enum {
	HTTP10,
	HTTP11,
	PROT_INVALIDO
} protocolo_http;


typedef struct {
	tipo_requisicao tipo;
	char caminho[MAXLINE + 1];
	protocolo_http protocolo;
} requisicao;


typedef struct {
	int status;
	protocolo_http protocolo;
	char arquivo[PATH_MAX + MAXLINE + 1];
} resposta;

char diretorio_www[PATH_MAX + 1];
char buffer[MAXLINE + 1];
int tam_buffer;
int socket_conexao;
char chaves_post[MAXPOST][MAXLINE + 1];
char valores_post[MAXPOST][MAXLINE + 1];
int qtd_campos_post;

requisicao interpretar_requisicao(char requisicao[]);
void enviar_resposta(resposta resp, tipo_requisicao tipo);
void enviar_arquivo(char nome_arquivo[]);
char* obter_content_type(char nome_arquivo[]);
void definir_diretorio_www(char diretorio[]);
void escrever_buffer(char string[], int tam_string);
void enviar_buffer();
void executar_servidor_http(int socket);
int ler_linha(char linha[], int tam);
void interpretar_post(char linha[]);
void enviar_conteudo_post();

