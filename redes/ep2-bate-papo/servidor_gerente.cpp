#include "servidor_gerente.h"

map<string, int> nicks_socket;

string processar_mensagem(char *linha){
	char *resto = NULL;
	char *token;
	char *ptr = linha;
	
	string msg_encaminhada = "MSG ";
	msg_encaminhada += string(linha);
	msg_encaminhada += "\r\n";
	
	token = strtok_r(ptr, " ", &resto);
	
	if(!token)
		return "ERRO_REQ_INCOMPLETA";
	
	string id = string(token);
	
	ptr = resto;
	token = strtok_r(ptr, " ", &resto);
	
	string origem = string(token);

	if(nicks_socket.find(origem) == nicks_socket.end())
		return "ERRO_NICK_ORIGEM_DESCONHECIDO";
		
	ptr = resto;
	token = strtok_r(ptr, " ", &resto);
	
	if(!token)
		return "REQ_INCOMPLETA";
	
	string destino = string(token);
	
	if(nicks_socket.find(destino) == nicks_socket.end())
		return "NICK_DESTINO_DESCONHECIDO";
	
	string mensagem = string(resto);	
	
	write(nicks_socket[destino], msg_encaminhada.c_str(), msg_encaminhada.size());

	printf("[%d: Mensagem id '%s' de '%s' para '%s' conteudo '%s']\n", getpid(), 
		id.c_str(), origem.c_str(), destino.c_str(), mensagem.c_str());
	
	return "MSG_OK " + id;
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

	if(strcmp(token, "CON") == 0){
		ptr = resto;
		token = strtok_r(ptr, " ", &resto);
		if(!token)
			resposta = "ERRO_REQ_INCOMPLETA";
		else if(nicks_socket.find(string(token)) != nicks_socket.end())
			resposta = "CON_NEG";
		else {
			nicks_socket[string(token)] = socket;
			resposta = "CON_OK";
		}
		
	} else if (strcmp(token, "LIST") == 0){
		map<string, int>::iterator it;
		 for (it = nicks_socket.begin(); it != nicks_socket.end(); it++)
		 	resposta += (*it).first + " ";
		 	
	} else if (strcmp(token, "MSG") == 0){
		resposta = processar_mensagem(resto);
	
	} else if (strcmp(token, "FIM") == 0) {
		ptr = resto;
		token = strtok_r(ptr, " ", &resto);
		if(!token)
			resposta = "ERRO_REQ_INCOMPLETA";
		else if(nicks_socket.find(string(token)) == nicks_socket.end())
			resposta = "ERRO_NICK_DESCONHECIDO";
		else {
			nicks_socket.erase(string(token));
			resposta = "FIM_OK";
		}
		
	} else {
		resposta = "ERRO_REQ_DESCONHECIDA";
	}
	
	resposta += "\r\n";
	printf("%d: Respondeu %s", getpid(), resposta.c_str());
	write(socket, resposta.c_str(), resposta.size());
}

/*
• Cliente solicita o MSGio de um arquivo binario para um nick especifico;
• Servidor confirma que o nick aceitou o MSGio do arquivo e permite uma conexao TCP direta entre
  os usuarios para o MSGio deste arquivo.
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


