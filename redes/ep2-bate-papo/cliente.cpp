#include "cliente.h"

int servidor_sd;
struct sockaddr servidor_ip;
socklen_t servidor_len;
string nick;
bool conectou = false;
int id_msg = 0;
int porta_arquivos;

void exibir_menu(){
	puts("Comandos disponiveis:");
	puts("@user msg         - Envia a mensagem msg ao usuario user");
	puts("/file user arq    - Enviar o arquivo arq ao usuario user");
	puts("/list             - Lista todos os usuarios disponiveis");
	puts("/help             - Exibir esta ajuda");
	puts("/exit             - Sair do bate-papo");
}

void processar_comando_cliente(){
	int rc;
	string lido, requisicao, tmp, destino, arquivo;
	stringstream ss; 
	
	getline(cin, lido);
	
	if(lido.size() == 0)
		return;
	else if(strncmp(lido.c_str(), "/exit", 5) == 0)
		requisicao = "FIM " + nick;
	else if(strncmp(lido.c_str(), "/list", 5) == 0)
		requisicao = "LIST";
	else if(strncmp(lido.c_str(), "/help", 5) == 0)
		exibir_menu();
	else if(strncmp(lido.c_str(), "@", 1) == 0){
		size_t fim_nick_destino = lido.find(" ");
		ss << "MSG " 
			<< id_msg++ 
			<< " " 
			<< nick 
			<< " " 
			<< lido.substr(1, fim_nick_destino);
		if (fim_nick_destino != string::npos)
			ss << lido.substr(fim_nick_destino + 1, string::npos);
		requisicao = ss.str();
	} else if(strncmp(lido.c_str(), "/file", 5) == 0){
		ss << lido;
		ss >> tmp;
		ss >> destino;
		ss >> arquivo;
		ss.clear();
		ss.str("");
		
		ss << "ARQ "
			<< nick << " "
			<< destino << " "
			<< arquivo;
			
		requisicao = ss.str();
	}
	
	if(requisicao.size() == 0){
		printf("Comando desconhecido!");
		return;
	}
	
	requisicao += "\r\n";
	
	rc = sendto(servidor_sd, requisicao.c_str(), requisicao.length(), 0, &servidor_ip, servidor_len);
	if (rc < 0)
		perror("  send() failed");
}

void receber_arquivo(const char *nome_arquivo, const char *ip){
  int rc, on = 1;
  struct sockaddr_in servaddr;
  char buffer[MAXLINE];

  int socket_arquivo = socket(AF_INET, SOCK_STREAM, 0);
  if (socket_arquivo < 0)
  {
    perror("socket() servidor failed");
    exit(-1);
  }

  /*************************************************************/
  /* Allow socket descriptor to be reuseable                   */
  /*************************************************************/
  rc = setsockopt(socket_arquivo, SOL_SOCKET,  SO_REUSEADDR,
                  (char *)&on, sizeof(on));
  if (rc < 0)
  {
    perror("setsockopt() tcp failed");
    close(socket_arquivo);
    exit(-1);
  }

  /*************************************************************/
  /* Pegando o endereço do servidor                            */
  /*************************************************************/

  inet_pton(AF_INET, ip, &servaddr.sin_addr);

  /*************************************************************/
  /* Bind the socket                                           */
  /*************************************************************/
  memset(&servaddr, 0, sizeof(servaddr));
  servaddr.sin_family      = AF_INET;
  servaddr.sin_port        = htons(porta_arquivos);

  /*************************************************************/
  /* Conectando ao servidor                                    */
  /*************************************************************/
  
  while(connect(socket_arquivo, (struct sockaddr *) &servaddr, sizeof(servaddr)) < 0);
  
  if(rc < 0)
  	perror("problema connect()");
  
  
  FILE *arquivo = fopen(nome_arquivo, "wb");
  
  while((rc = recvfrom(socket_arquivo, buffer, sizeof(buffer), 0, NULL, NULL)) > 0){
  	fwrite(buffer, 1, rc, arquivo);
  }
  
  close(socket_arquivo);
}

void enviar_arquivo(string nome_arquivo){
	int listen_tcp;
	int connfd;
	struct sockaddr_in servaddr;
	char buffer[MAXLINE];
	int rc, on = 1;
	
	stringstream ss;
	ss << nome_arquivo;
	ss >> nome_arquivo;
	
	FILE *arquivo = fopen(nome_arquivo.c_str(), "rb");


	if ((listen_tcp = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
		perror("socket TCP :(\n");
		exit(2);
	}

	rc = setsockopt(listen_tcp, SOL_SOCKET,  SO_REUSEADDR,
		          (char *)&on, sizeof(on));
	if (rc < 0)
	{
		perror("setsockopt() tcp failed");
		close(listen_tcp);
		exit(-1);
	}


	bzero(&servaddr, sizeof(servaddr));
	servaddr.sin_family      = AF_INET;
	servaddr.sin_addr.s_addr = htonl(INADDR_ANY);
	servaddr.sin_port        = htons(porta_arquivos);

	if (bind(listen_tcp, (struct sockaddr *)&servaddr, sizeof(servaddr)) == -1) {
		perror("bind TCP :(\n");
		exit(3);
	}

	if (listen(listen_tcp, 1) == -1) {
		perror("listen :(\n");
		exit(4);
	}

	if ((connfd = accept(listen_tcp, (struct sockaddr *) NULL, NULL)) == -1 ) {
		perror("accept :(\n");
		exit(5);
	}
	
	rc = setsockopt(connfd, SOL_SOCKET,  SO_REUSEADDR,
		          (char *)&on, sizeof(on));
	if (rc < 0)
	{
		perror("setsockopt() tcp failed");
		close(connfd);
		exit(-1);
	}

	if(!arquivo){
		fprintf(stderr, "abrir %s :(\n", nome_arquivo.c_str());
		return;
	}

	int lido;
	while(!feof(arquivo)){
		lido = fread(buffer, 1, MAXLINE, arquivo);
		write(connfd, buffer, lido);
	}
	
	
	close(connfd);
	close(listen_tcp);
	fclose(arquivo);
}

void exibir_mensagem(char *msg){
	char *resto = NULL;
	char *token;
	char *ptr = msg;
	
	token = strtok_r(ptr, " ", &resto);
	ptr = resto;
	token = strtok_r(ptr, " ", &resto);
	
	string id = string(token);
	
	ptr = resto;
	token = strtok_r(ptr, " ", &resto);
	
	string origem = string(token);

	ptr = resto;
	token = strtok_r(ptr, " ", &resto);
	
	string destino = string(token);
	
	int len = strlen(resto);
	resto[len - 1] = '\0';
	resto[len - 2] = '\0';
	string mensagem = string(resto);
	
	cout << "#" << origem << ": " << mensagem << endl;
}

void processar_chamada_servidor(){
	char lido[MAXLINE + 1];
	int rc;
	
	rc = recvfrom(servidor_sd, lido, sizeof(lido), 0, &servidor_ip, &servidor_len);
	if(rc == 0){
		fprintf(stderr, "Servidor caiu! Encerrando.\n");
		exit(-1);
	}
	lido[max(rc, 0)] = '\0';
	
	if(strncmp(lido, "FIM_OK", 6) == 0)
		exit(0);
	else if(strncmp(lido, "MSG_OK", 6) == 0)
		return;
	else if(strncmp(lido, "MSG", 3) == 0)
		exibir_mensagem(lido);
	else if(strncmp(lido, "NICK_DESTINO_DESCONHECIDO", 25) == 0)
		printf("Erro! Nick desconhecido.\n");
	else if(strncmp(lido, "ARQ_OK", 6) == 0){
		printf("Iniciando transferencia de %s", lido + 7);
		enviar_arquivo(string(lido + 7));
		printf("Transferencia concluida\n");
	} else if(strncmp(lido, "ARQ_NEG", 6) == 0){
		printf("Transferência de %s negada.\n", lido + 6);
	} else if(strncmp(lido, "ARQ", 3) == 0) {
		stringstream ss;
		string tmp, origem, destino, arquivo, ip;
		char resp = '\0';
		ss << string(lido);
		
		ss >> tmp >> origem >> destino >> arquivo >> ip;
		
		while(resp != 'y' && resp != 'n'){
			cout << "Voce aceita receber o arquivo " << arquivo 
			 << " de " << origem << "? [y/n]" << endl;
			cin >> resp;
		}
		
		string resultado;
		if(resp == 'y')
			resultado = "ARQ_OK ";
		else
			resultado = "ARQ_NEG ";
		resultado += arquivo;
		
		sendto(servidor_sd, resultado.c_str(), resultado.length(), 0, &servidor_ip, servidor_len);

		if(resp == 'y')	{
			receber_arquivo(arquivo.c_str(), ip.c_str());
			cout << "Arquivo recebido." << endl;
		}
	}
	else
		printf(lido);
}

void cadastrar_nick(){
  int    rc;
  struct pollfd fds[1];
  int    nfds = 1;
  char buffer[MAXLINE + 1];
  buffer[0] = '\0';	
  
  memset(fds, 0 , sizeof(fds));
  fds[0].fd = servidor_sd;
  fds[0].events = POLLIN;
  

	while(strcmp("CON_OK\r\n", buffer) != 0){
		if(buffer[0] == '\0')
			printf("Escolha o nick que gostaria de utilizar: ");
		else
			printf("Nick fornecido ja esta sendo usado. Escolha outro: ");
		
		cin >> nick;

		sprintf(buffer, "CON %s", nick.c_str());
		rc = sendto(servidor_sd, buffer, strlen(buffer), 0, &servidor_ip, servidor_len);
	
		poll(fds, nfds, -1);
		rc = recvfrom(servidor_sd, buffer, sizeof(buffer), 0, &servidor_ip, &servidor_len);
		buffer[max(rc, 0)] = '\0';
	}
	
	printf("OK! Seja bem-vindo ao servidor!\n");
}

void receber_requisicoes(){  
  int    rc;
  bool   end_server = false;
  struct pollfd fds[MAX_FDS];
  int    nfds = 2, current_size = 0, i;
  /*************************************************************/
  /* Initialize the pollfd structure                           */
  /*************************************************************/
  memset(fds, 0 , sizeof(fds));

  /*************************************************************/
  /* Set up the initial listening socket                        */
  /*************************************************************/
  fds[0].fd = servidor_sd;
  fds[0].events = POLLIN;
  fds[1].fd = fileno(stdin);
  fds[1].events = POLLIN;
 
  cadastrar_nick();
  exibir_menu();

  /*************************************************************/
  /* Loop waiting for incoming connects or for incoming data   */
  /* on any of the connected sockets.                          */
  /*************************************************************/
  do
  {
    /***********************************************************/
    /* Call poll() and wait 3 minutes for it to complete.      */
    /***********************************************************/
    rc = poll(fds, nfds, -1);

    /***********************************************************/
    /* Check to see if the poll call failed.                   */
    /***********************************************************/
    if (rc < 0)
    {
      perror("  poll() failed");
      break;
    }


    /***********************************************************/
    /* One or more descriptors are readable.  Need to          */
    /* determine which ones they are.                          */
    /***********************************************************/
    current_size = nfds;
    for (i = 0; i < current_size; i++)
    {
      /*********************************************************/
      /* Loop through to find the descriptors that returned    */
      /* POLLIN and determine whether it's the listening       */
      /* or the active connection.                             */
      /*********************************************************/
      if(fds[i].revents == 0)
        continue;
      
      if (fds[i].revents & POLLHUP)
      {
        fprintf(stderr, "Servidor caiu! Encerrando.\n");
        close(fds[i].fd);
        exit(-1);
      }
      
      /*********************************************************/
      /* If revents is not POLLIN, it's an unexpected result,  */
      /* log and end the server.                               */
      /*********************************************************/
      if(!((fds[i].revents & POLLIN) | (fds[i].revents & POLLOUT)))
      {
        fprintf(stderr, "  Error! revents = %d\n", fds[i].revents);
        end_server = true;
        break;

      }
      if (fds[i].fd == servidor_sd && (fds[i].revents & POLLIN))
      {
        /*******************************************************/
        /* Listening descriptor is readable.                   */
        /*******************************************************/

        /*******************************************************/
        /* Accept all incoming connections that are            */
        /* queued up on the listening socket before we         */
        /* loop back and call poll again.                      */
        /*******************************************************/
 		
 		processar_chamada_servidor();
 		
      /*********************************************************/
      /* This is not the listening socket, therefore an        */
      /* existing connection must be readable                  */
      /*********************************************************/
      }
      else if((fds[i].fd == fileno(stdin)) && (fds[i].revents & POLLIN))
      {
        /*******************************************************/
        /* Receive all incoming data on this socket            */
        /* before we loop back and call poll again.            */
        /*******************************************************/
		
		processar_comando_cliente();
		

        /*******************************************************/
        /* If the close_conn flag was turned on, we need       */
        /* to clean up this active connection. This           */
        /* clean up process includes removing the              */
        /* descriptor.                                         */
        /*******************************************************/

      } else {
      	printf("Não devia acontecer: revents[%d] = %d\n", i, fds[i].revents);
      }  /* End of existing connection is readable             */
    } /* End of loop through pollable descriptors              */

  } while (end_server == false); /* End of serving running.    */

  /*************************************************************/
  /* Clean up all of the sockets that are open                  */
  /*************************************************************/
  for (i = 0; i < nfds; i++)
  {
    if(fds[i].fd >= 0)
      close(fds[i].fd);
  }
}


void iniciar_cliente_udp(char *ip, int porta){
	int rc, flags, on = 1;
	struct sockaddr_in servaddr;
	struct  hostent *hptr;

  /*************************************************************/
  /* Pegando o endereço do servidor                            */
  /*************************************************************/
	if ((hptr = gethostbyname(ip)) == NULL) {
		fprintf(stderr,"gethostbyname :(\n");
		exit(1);
	}
	
	if (hptr->h_addrtype != AF_INET) {
		fprintf(stderr,"h_addrtype :(\n");
		exit(1);
	}

	if ((servidor_sd = socket(AF_INET, SOCK_DGRAM, 0)) == -1) {
		perror("socket UDP :(");
		exit(-1);
	}

  /*************************************************************/
  /* Allow socket descriptor to be reuseable                   */
  /*************************************************************/
  rc = setsockopt(servidor_sd, SOL_SOCKET,  SO_REUSEADDR,
                  (char *)&on, sizeof(on));
  if (rc < 0)
  {
    perror("setsockopt() tcp failed");
    close(servidor_sd);
    exit(-1);
  }

  flags = fcntl(servidor_sd, F_GETFL, 0);
  rc = fcntl(servidor_sd, F_SETFL, flags | O_NONBLOCK);

  if (rc < 0)
  {
    perror("ioctl() servidor failed");
    close(servidor_sd);
    exit(-1);
  }


	bzero(&servaddr, sizeof(servaddr));
	servaddr.sin_family      = AF_INET;
	servaddr.sin_port        = htons(porta);
	memcpy(&servaddr.sin_addr, hptr->h_addr_list[0], hptr->h_length);


	servidor_len = sizeof(servidor_ip);
	memcpy(&servidor_ip, &servaddr, servidor_len);

	
	receber_requisicoes();
}

void iniciar_cliente_tcp(char *ip, int porta){
	int rc, flags, on = 1;
  struct sockaddr_in servaddr;
  struct  hostent *hptr;

  /*************************************************************/
  /* Create an AF_INET stream socket to receive incoming       */
  /* connections on                                            */
  /*************************************************************/
  servidor_sd = socket(AF_INET, SOCK_STREAM, 0);
  if (servidor_sd < 0)
  {
    perror("socket() servidor failed");
    exit(-1);
  }

  /*************************************************************/
  /* Allow socket descriptor to be reuseable                   */
  /*************************************************************/
  rc = setsockopt(servidor_sd, SOL_SOCKET,  SO_REUSEADDR,
                  (char *)&on, sizeof(on));
  if (rc < 0)
  {
    perror("setsockopt() tcp failed");
    close(servidor_sd);
    exit(-1);
  }

  /*************************************************************/
  /* Set socket to be nonblocking. All of the sockets for    */
  /* the incoming connections will also be nonblocking since  */
  /* they will inherit that state from the listening socket.   */
  /*************************************************************/


  flags = fcntl(servidor_sd, F_GETFL, 0);
  rc = fcntl(servidor_sd, F_SETFL, flags | O_NONBLOCK);

  if (rc < 0)
  {
    perror("ioctl() servidor failed");
    close(servidor_sd);
    exit(-1);
  }

  /*************************************************************/
  /* Pegando o endereço do servidor                            */
  /*************************************************************/
	if ((hptr = gethostbyname(ip)) == NULL) {
		fprintf(stderr,"gethostbyname :(\n");
		exit(1);
	}
	
	if (hptr->h_addrtype != AF_INET) {
		fprintf(stderr,"h_addrtype :(\n");
		exit(1);
	}

  /*************************************************************/
  /* Bind the socket                                           */
  /*************************************************************/
  memset(&servaddr, 0, sizeof(servaddr));
  servaddr.sin_family      = AF_INET;
  servaddr.sin_port        = htons(porta);
  memcpy(&servaddr.sin_addr, hptr->h_addr_list[0], hptr->h_length);

  /*************************************************************/
  /* Conectando ao servidor                                    */
  /*************************************************************/
  
  rc = connect(servidor_sd, (struct sockaddr *) &servaddr, sizeof(servaddr));
  receber_requisicoes();
}



int main (int argc, char **argv) {

	if (argc < 4) {
		fprintf(stderr,"Uso: %s <tcp|udp> <IP servidor> <Porta>\n\n",argv[0]);
		fprintf(stderr,"Vai rodar um cliente de bate-papo na porta <Porta> TCP ou");
		fprintf(stderr,"udp dependendo do parâmetro passado via linha de comando\n");
		exit(1);
	}

	int protocolo = -1;
	if(strcmp(argv[1], "udp") == 0)
		protocolo = UDP;
	else if (strcmp(argv[1], "tcp") == 0)
		protocolo = TCP;
	
	if(protocolo == -1){
		fprintf(stderr,"Protocolo desconhecido. Encerrando.\n");
		exit(2);
	}
		
	printf("[Bem vindo ao sistema de bate-papo yaIRC]!\n");
	printf("[Para finalizar, pressione CTRL+c ou rode um kill ou killall]\n");
	
	porta_arquivos = atoi(argv[3]) + 1;

	if(protocolo == TCP)
		iniciar_cliente_tcp(argv[2], atoi(argv[3]));
	else
		iniciar_cliente_udp(argv[2], atoi(argv[3]));

	printf("[%d: Encerrando cliente]\n", getpid());
	exit(0);
}

