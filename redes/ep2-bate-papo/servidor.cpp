#include "servidor.h"

map<string, int> nicks_socket;
map<string, struct sockaddr> nicks_ip;

string encaminhar_mensagem(char *linha){
	char *resto = NULL;
	char *token;
	char *ptr = linha;
	int rc;
	
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
	rc = sendto(nicks_socket[destino], msg_encaminhada.c_str(), msg_encaminhada.size(), 
			0, &(nicks_ip[destino]), sizeof(nicks_ip[destino]));
			
	if (rc < 0)
		perror("  send() failed");

	printf("[%d: Mensagem id '%s' de '%s' para '%s' conteudo '%s']\n", getpid(), 
		id.c_str(), origem.c_str(), destino.c_str(), mensagem.c_str());
	
	return "MSG_OK " + id;
}

int processar(int socket, char *buffer, struct sockaddr *origem, socklen_t origem_len){
	string resposta;
	char *resto = NULL;
	char *token;
	char *ptr = buffer;

	int lido = strlen(buffer);
	for(int i = 0; i < lido; i++)
		if(buffer[i] == '\n' || buffer[i] == '\r')
			buffer[i] = '\0';

	printf("%d: Recebeu %s\n", getpid(), buffer);
	
	token = strtok_r(ptr, " ", &resto);
	if(!token)
		return 1;

	if(strcmp(token, "CON") == 0){
		ptr = resto;
		token = strtok_r(ptr, " ", &resto);
		if(!token)
			resposta = "ERRO_REQ_INCOMPLETA";
		else if(nicks_socket.find(string(token)) != nicks_socket.end())
			resposta = "CON_NEG";
		else {
			nicks_socket[string(token)] = socket;
			nicks_ip[string(token)] = *origem;
			resposta = "CON_OK";
		}
		
	} else if (strcmp(token, "LIST") == 0){
		map<string, int>::iterator it;
		 for (it = nicks_socket.begin(); it != nicks_socket.end(); it++)
		 	resposta += (*it).first + " ";
	} else if (strcmp(token, "MSG") == 0){
		resposta = encaminhar_mensagem(resto);
	} else if (strcmp(token, "FIM") == 0) {
		ptr = resto;
		token = strtok_r(ptr, " ", &resto);
		if(!token)
			resposta = "ERRO_REQ_INCOMPLETA";
		else if(nicks_socket.find(string(token)) == nicks_socket.end())
			resposta = "ERRO_NICK_DESCONHECIDO";
		else {
			nicks_socket.erase(string(token));
			nicks_ip.erase(string(token));
			resposta = "FIM_OK";
		}
	} else {
		resposta = "ERRO_REQ_DESCONHECIDA";
	}
	
	resposta += "\r\n";
	printf("%d: Respondeu %s", getpid(), resposta.c_str());
	return sendto(socket, resposta.c_str(), resposta.size(), 0, origem, origem_len);
}

//Código adaptado de http://publib.boulder.ibm.com/infocenter/iseries/v6r1m0/index.jsp?topic=/rzab6/example.htm

bool tratar_requisicao(int fd){
	bool   close_conn = false;
	char   buffer[MAXLINE + 1];
	int    len, rc;
	struct sockaddr origem;
	socklen_t origem_len = sizeof(origem);
	bzero(&origem, origem_len);

    do
    {
      /*****************************************************/
      /* Receive data on this connection until the         */
      /* recv fails with EWOULDBLOCK. If any other        */
      /* failure occurs, we will close the                 */
      /* connection.                                       */
      /*****************************************************/
      
      rc = recvfrom(fd, buffer, sizeof(buffer), 0, &origem, &origem_len);
      
      if (rc < 0)
      {
        if (!(errno == EWOULDBLOCK || errno == EAGAIN))
        {
          perror("  recv() failed");
          close_conn = true;
        }
        break;
      }

      /*****************************************************/
      /* Check to see if the connection has been           */
      /* closed by the client                              */
      /*****************************************************/
      if (rc == 0)
      {
        printf("  Connection closed\n");
        close_conn = true;
        
        map<string, int>::iterator it;
		for (it = nicks_socket.begin(); it != nicks_socket.end(); it++)
			if((*it).second == fd){
				nicks_socket.erase((*it).first);
				nicks_ip.erase((*it).first);
			}
        
        break;
      }
      
      buffer[rc] = '\0';
      
      /*****************************************************/
      /* Data was received                                 */
      /*****************************************************/
      len = rc;
      printf("  %d bytes received\n", len);

      /*****************************************************/
      /* Process the data                                  */
      /*****************************************************/
      rc = processar(fd, buffer, &origem, origem_len);

      if (rc < 0)
      {
        perror("  send() failed");
        close_conn = true;
        break;
      }

    } while(true);

	return close_conn;
}

void iniciar_servidor (int porta) {
  int    flags, rc, on = 1;
  int    listen_sd = -1, new_sd = -1, listen_udp = -1;
  bool   close_conn, end_server = false, compress_array = false;
  struct sockaddr_in   addr;
  struct pollfd fds[MAX_FDS];
  int    nfds = 2, current_size = 0, i, j;

  /*************************************************************/
  /* Create an AF_INET stream socket to receive incoming       */
  /* connections on                                            */
  /*************************************************************/
  listen_sd = socket(AF_INET, SOCK_STREAM, 0);
  if (listen_sd < 0)
  {
    perror("socket() tcp failed");
    exit(-1);
  }

	listen_udp = socket(AF_INET, SOCK_DGRAM, 0);
	if (listen_udp < 0) {
		perror("socket() udp failed");
		exit(-1);
	}

  /*************************************************************/
  /* Allow socket descriptor to be reuseable                   */
  /*************************************************************/
  rc = setsockopt(listen_sd, SOL_SOCKET,  SO_REUSEADDR,
                  (char *)&on, sizeof(on));
  if (rc < 0)
  {
    perror("setsockopt() tcp failed");
    close(listen_sd);
    exit(-1);
  }

  rc = setsockopt(listen_udp, SOL_SOCKET,  SO_REUSEADDR,
                  (char *)&on, sizeof(on));
  if (rc < 0)
  {
    perror("setsockopt() udp failed");
    close(listen_sd);
    exit(-1);
  }

  /*************************************************************/
  /* Set socket to be nonblocking. All of the sockets for    */
  /* the incoming connections will also be nonblocking since  */
  /* they will inherit that state from the listening socket.   */
  /*************************************************************/

  flags = fcntl(listen_sd, F_GETFL, 0);
  rc = fcntl(listen_sd, F_SETFL, flags | O_NONBLOCK);

  if (rc < 0)
  {
    perror("ioctl() tcp failed");
    close(listen_sd);
    exit(-1);
  }

  flags = fcntl(listen_udp, F_GETFL, 0);
  rc = fcntl(listen_udp, F_SETFL, flags | O_NONBLOCK);

  if (rc < 0)
  {
    perror("ioctl() upd failed");
    close(listen_sd);
    exit(-1);
  }

  /*************************************************************/
  /* Bind the socket                                           */
  /*************************************************************/
  memset(&addr, 0, sizeof(addr));
  addr.sin_family      = AF_INET;
  addr.sin_addr.s_addr = htonl(INADDR_ANY);
  addr.sin_port        = htons(porta);
  rc = bind(listen_sd, (struct sockaddr *)&addr, sizeof(addr));
  if (rc < 0)
  {
    perror("bind() tcp failed");
    close(listen_sd);
    exit(-1);
  }

  rc = bind(listen_udp, (struct sockaddr *)&addr, sizeof(addr));
  if (rc < 0)
  {
    perror("bind() udp failed");
    close(listen_udp);
    exit(-1);
  }


  /*************************************************************/
  /* Set the listen back log                                   */
  /*************************************************************/
  rc = listen(listen_sd, MAX_BACKLOG);
  if (rc < 0)
  {
    perror("listen() failed");
    close(listen_sd);
    exit(-1);
  }

  /*************************************************************/
  /* Initialize the pollfd structure                           */
  /*************************************************************/
  memset(fds, 0 , sizeof(fds));

  /*************************************************************/
  /* Set up the initial listening socket                        */
  /*************************************************************/
  fds[0].fd = listen_sd;
  fds[0].events = POLLIN;
  fds[1].fd = listen_udp;
  fds[1].events = POLLIN;
  
	

  /*************************************************************/
  /* Loop waiting for incoming connects or for incoming data   */
  /* on any of the connected sockets.                          */
  /*************************************************************/
  do
  {
    /***********************************************************/
    /* Call poll() and wait 3 minutes for it to complete.      */
    /***********************************************************/
    printf("Waiting on poll()...\n");
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

      /*********************************************************/
      /* If revents is not POLLIN, it's an unexpected result,  */
      /* log and end the server.                               */
      /*********************************************************/
      if (fds[i].revents & POLLHUP)
      {
        printf("  Descriptor %d hanged up\n", fds[i].fd);
        close(fds[i].fd);
        fds[i].fd = -1;
        compress_array = true;
        continue;
      }
        
      if(fds[i].revents != POLLIN)
      {
        printf("  Error! revents = %d\n", fds[i].revents);
        end_server = true;
        break;
      }
      if (fds[i].fd == listen_sd && nfds < MAX_FDS)
      {
        /*******************************************************/
        /* Listening descriptor is readable.                   */
        /*******************************************************/
        printf("  Listening socket is readable\n");

        /*******************************************************/
        /* Accept all incoming connections that are            */
        /* queued up on the listening socket before we         */
        /* loop back and call poll again.                      */
        /*******************************************************/
        do
        {
          /*****************************************************/
          /* Accept each incoming connection. If              */
          /* accept fails with EWOULDBLOCK, then we            */
          /* have accepted all of them. Any other             */
          /* failure on accept will cause us to end the        */
          /* server.                                           */
          /*****************************************************/
          new_sd = accept(listen_sd, NULL, NULL);
          if (new_sd < 0)
          {
            if (!(errno == EWOULDBLOCK || errno == EAGAIN))
            {
              perror("  accept() failed");
              end_server = true;
            }
            break;
          }

          /*****************************************************/
          /* Add the new incoming connection to the            */
          /* pollfd structure                                  */
          /*****************************************************/
          printf("  New incoming connection - %d\n", new_sd);
          fds[nfds].fd = new_sd;
          fds[nfds].events = POLLIN;
          flags = fcntl(new_sd, F_GETFL, 0);
          rc = fcntl(new_sd, F_SETFL, flags | O_NONBLOCK);
          nfds++;

          /*****************************************************/
          /* Loop back up and accept another incoming          */
          /* connection                                        */
          /*****************************************************/
        } while (new_sd != -1 && nfds < MAX_FDS);
      }

      /*********************************************************/
      /* This is not the listening socket, therefore an        */
      /* existing connection must be readable                  */
      /*********************************************************/

      else
      {
        printf("  Descriptor %d is readable\n", fds[i].fd);
        close_conn = false;
        /*******************************************************/
        /* Receive all incoming data on this socket            */
        /* before we loop back and call poll again.            */
        /*******************************************************/

		close_conn = tratar_requisicao(fds[i].fd);

        /*******************************************************/
        /* If the close_conn flag was turned on, we need       */
        /* to clean up this active connection. This           */
        /* clean up process includes removing the              */
        /* descriptor.                                         */
        /*******************************************************/
        if (close_conn)
        {
          close(fds[i].fd);
          fds[i].fd = -1;
          compress_array = true;
        }


      }  /* End of existing connection is readable             */
    } /* End of loop through pollable descriptors              */

    /***********************************************************/
    /* If the compress_array flag was turned on, we need       */
    /* to squeeze together the array and decrement the number  */
    /* of file descriptors. We do not need to move back the    */
    /* events and revents fields because the events will always*/
    /* be POLLIN in this case, and revents is output.          */
    /***********************************************************/
    if (compress_array)
    {
      compress_array = false;
      for (i = 0; i < nfds; i++)
      {
        if (fds[i].fd == -1)
        {
          for(j = i; j < nfds; j++)
          {
            fds[j].fd = fds[j+1].fd;
          }
          nfds--;
        }
      }
    }

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




/*
• Cliente solicita o MSGio de um arquivo binario para um nick especifico;
• Servidor confirma que o nick aceitou o MSGio do arquivo e permite uma conexao TCP direta entre
  os usuarios para o MSGio deste arquivo.
*/

int main (int argc, char **argv) {
	if (argc < 2) {
		fprintf(stderr,"Uso: %s <Porta>\n\n",argv[0]);
		fprintf(stderr,"Vai rodar um servidor de bate-papo na porta <Porta> TCP e UDP\n");
		exit(1);
	}

	printf("[Servidor no ar. Aguardando conexoes na porta %s]\n",argv[1]);
	printf("[Para finalizar, pressione CTRL+c ou rode um kill ou killall]\n");

	iniciar_servidor (atoi(argv[1]));

	printf("[%d: Encerrando servidor]\n", getpid());
	exit(0);
}

