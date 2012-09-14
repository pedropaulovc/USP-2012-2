/* Por Prof. Daniel Batista <batista@ime.usp.br>
 * Em 12/08/2012
 * Modificações pontuais por Pedro Paulo Vezzá Campos - 7538743
 * Em 01/09/2012
 */

#include "main_servidor.h"

int main (int argc, char **argv) {
	int connfd;
	pid_t childpid;
	
	if (argc < 1) {
		fprintf(stderr,"Uso: %s <Porta>\n\n",argv[0]);
		fprintf(stderr,"Vai rodar um servidor de bate-papo na porta <Porta> TCP\n");
		exit(1);
	}

	printf("[Servidor no ar. Aguardando conexoes na porta %s]\n",argv[1]);
	printf("[Servindo arquivos do diretório %s]\n",diretorio_www);
	printf("[Para finalizar, pressione CTRL+c ou rode um kill ou killall]\n");
	
	for (;;) {
		if ((connfd = accept(listen_tcp, (struct sockaddr *) NULL, NULL)) == -1 ) {
			perror("accept :(\n");
			exit(5);
		}
		
		if ( (childpid = fork()) == 0) {
			/**** PROCESSO FILHO ****/
			printf("[%d: Conexao aberta]\n", getpid());
			close(listen_tcp);

			executar_servidor_http(connfd);

			printf("[%d: Conexao fechada]\n", getpid());
			exit(0);
		}
		/**** PROCESSO PAI ****/
		close(connfd);
	}
	exit(0);
}

void iniciar_escuta_tcp(){
	if ((listen_tcp = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
		perror("socket TCP :(\n");
		exit(2);
	}

	bzero(&listen_tcp, sizeof(servaddr));
	servaddr.sin_family      = AF_INET;
	servaddr.sin_addr.s_addr = htonl(INADDR_ANY);
	servaddr.sin_port        = htons(atoi(argv[1]));
	
	if (bind(listen_tcp, (struct sockaddr *)&servaddr, sizeof(servaddr)) == -1) {
		perror("bind TCP :(\n");
		exit(3);
	}
	
	if (listen(listen_tcp, LISTENQ) == -1) {
		perror("listen :(\n");
		exit(4);
	}

}

void iniciar_escuta_udp(){
	struct sockaddr_in servaddr;

	if ((listen_udp = socket(AF_INET, SOCK_DGRAM, 0)) == -1) {
		perror("socket UDP :(");
		exit(2);
	}

	bzero(&servaddr, sizeof(servaddr));
	servaddr.sin_family      = AF_INET;
	servaddr.sin_addr.s_addr = htonl(INADDR_ANY);
	servaddr.sin_port        = htons(atoi(argv[1]));

	if (bind(listenfd, (struct sockaddr *)&servaddr, sizeof(servaddr)) == -1) {
		perror("bind UDP :(");
		exit(3);
	}

}
