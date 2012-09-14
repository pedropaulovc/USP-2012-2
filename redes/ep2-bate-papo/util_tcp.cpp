#include "util_tcp.h"

int iniciar_escuta_tcp(int porta){
	int listen_tcp;
	struct sockaddr_in servaddr;
	
	if ((listen_tcp = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
		perror("socket TCP :(\n");
		exit(2);
	}

	bzero(&servaddr, sizeof(servaddr));
	servaddr.sin_family      = AF_INET;
	servaddr.sin_addr.s_addr = htonl(INADDR_ANY);
	servaddr.sin_port        = htons(porta);
	
	if (bind(listen_tcp, (struct sockaddr *)&servaddr, sizeof(servaddr)) == -1) {
		perror("bind TCP :(\n");
		exit(3);
	}
	
	if (listen(listen_tcp, LISTENQ) == -1) {
		perror("listen :(\n");
		exit(4);
	}
	
	return listen_tcp;
}

int aceitar_conexao(int listen_tcp){
	int connfd;
	
	if ((connfd = accept(listen_tcp, (struct sockaddr *) NULL, NULL)) == -1 ) {
		perror("accept :(\n");
		exit(5);
	}
	
	return connfd;
}
	
