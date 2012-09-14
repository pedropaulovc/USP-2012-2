#include "servidor_gerente.h"

int processar_mensagem(char *linha){
	char *resto = NULL;
	char *token;
	char *ptr = linha;
	
	token = strtok_r(ptr, " ", &resto);
	if(!token)
		return REQ_INCOMPLETA;
	
	string origem = string(token);
	
	if(nicks.find(origem) == nicks.end())
		return NICK_ORIGEM_DESCONHECIDO;
		
	ptr = resto;
	token = strtok_r(ptr, " ", &resto);
	
	if(!token)
		return REQ_INCOMPLETA;
	
	string destino = string(token);
	
	if(nicks.find(destino) == nicks.end())
		return NICK_DESTINO_DESCONHECIDO;
	
	ptr = resto;
	token = strtok_r(ptr, " ", &resto);
	
	if(!token)
		return REQ_INCOMPLETA;

	string mensagem = string(token);	
	
	return ENV_OK;
}

void processar_requisicao(int socket){
	char buffer[MAXLINE + 1];
	string resposta;
	char *resto = NULL;
	char *token;
	char *ptr = buffer;

	int lido = read(socket, buffer, MAXLINE);
	buffer[lido] = '\0';
	for(int i = 0; i < lido; i++)
		if(buffer[i] == '\n' || buffer[i] == '\r')
			buffer[i] = '\0';
	
	printf("%d: Recebeu %s\n", getpid(), buffer);
	
	token = strtok_r(ptr, " ", &resto);
	if(!token)
		return;

	if(strcmp(token, "CAD") == 0){
		ptr = resto;
		token = strtok_r(ptr, " ", &resto);
		if(!token)
			resposta = "ERRO_REQ_INCOMPLETA";
		else if(nicks.find(string(token)) != nicks.end())
			resposta = "CAD_NEG";
		else {
			nicks.insert(string(token));
			resposta = "CAD_OK";
		}
		
	} else if (strcmp(token, "LIST") == 0){
		 for (set<string>::iterator it = nicks.begin(); it != nicks.end(); it++)
		 	resposta += *it + " ";
		 	
	} else if (strcmp(token, "ENV") == 0){
		ptr = resto;
		token = strtok_r(ptr, " ", &resto);
		
		int resultado = processar_mensagem(token);
		if(resultado == REQ_INCOMPLETA)
			resposta = "ERRO_REQ_INCOMPLETA";
		else if(resultado == NICK_ORIGEM_DESCONHECIDO)
			resposta = "ERRO_NICK_ORIGEM_DESCONHECIDO";
		else if(resultado == NICK_DESTINO_DESCONHECIDO)
			resposta = "ERRO_NICK_DESTINO_DESCONHECIDO";
		else
			resposta = "ENV_OK";
	
	} else if (strcmp(token, "FIM") == 0) {
		ptr = resto;
		token = strtok_r(ptr, " ", &resto);
		if(!token)
			resposta = "ERRO_REQ_INCOMPLETA";
		else if(nicks.find(string(token)) == nicks.end())
			resposta = "ERRO_NICK_DESCONHECIDO";
		else {
			nicks.erase(string(token));
			resposta = "FIM_OK";
		}
		
	} else {
		resposta = "ERRO_REQ_DESCONHECIDA";
	}
	
	printf("%d: Respondeu %s\n", getpid(), resposta.c_str());
	write(socket, resposta.c_str(), resposta.size());
}

/*
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
	
	while (true) {
		if(poll(fds, 2, -1) < 0)
			continue;

		if ((fds[0].revents & POLLIN))
			processar_requisicao(fds[0].fd);
		
		if ((fds[1].revents & POLLIN))
			processar_requisicao(fds[1].fd);
	}
		
}


