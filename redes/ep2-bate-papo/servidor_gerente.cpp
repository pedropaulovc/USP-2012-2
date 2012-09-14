#include "servidor_gerente.h"

void processar_requisicao(int socket){
	char buffer[MAXLINE + 1];
	char *resto = NULL;
	char *token;
	char *ptr = buffer;

	int lido = read(socket_conexao, buffer, MAXLINE);
	buffer[lido] = '\0';
	
	token = strtok_r(ptr, " ", &resto);
	if(!token)
		return;

	if(strcmp(token, "CAD_NICK") == 0)
		req.tipo = GET;
	else if (strcmp(token, "POST") == 0)
		req.tipo = POST;

	ptr = resto;
	token = strtok_r(ptr, " ", &resto);
	if(!token)
		return req;
	
}

/*
• Cliente informa o nick do usuario;
• Servidor responde se o nick foi aceito ou não;
• Cliente pede para o servidor a lista de todos os nicks online;
• Servidor retorna a lista dos nicks;
• Cliente envia uma mensagem para um nick espec ́fico; ı
• Cliente solicita a desconexao;
• Cliente solicita o envio de um arquivo binario para um nick especifico;
• Servidor confirma que o nick aceitou o envio do arquivo e permite uma conexao TCP direta entre
  os usuarios para o envio deste arquivo.
*/
void executar_gerente(int porta){
	struct pollfd fds[2];
	int listen_tcp = iniciar_escuta_tcp(porta);

	fds[0].events = POLLIN;
	fds[1].events = POLLIN;
	fds[0].fd = aceitar_conexao(listen_tcp);
	fds[1].fd = aceitar_conexao(listen_tcp);
	
	int atual = 0;
	while (true) {
		outro = atual + 1 % 2;
		int res = poll(fds, 2, -1);

		if ((fds[atual].revents & POLLIN)){
			processar_requisicao(fds[atual].fd);
			atual = outro;
		}

		if ((fds[outro].revents & POLLIN)){
			processar_requisicao(fds[outro].fd);
		}
	}
		
}


