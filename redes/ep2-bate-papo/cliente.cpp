#include "cliente.h"

int servidor_sd;
struct sockaddr servidor_ip;
socklen_t servidor_len;

void processar_comando_cliente(){
	int rc;
	string buffer;
	getline(cin, buffer);
	
	if(buffer.size() == 0)
		return;
	
	rc = sendto(servidor_sd, buffer.c_str(), buffer.length(), 0, &servidor_ip, servidor_len);
	if (rc < 0)
		perror("  send() failed");
}

void processar_chamada_servidor(){
	char buffer[MAXLINE + 1];
	int rc;
	
	rc = recvfrom(servidor_sd, buffer, sizeof(buffer), 0, &servidor_ip, &servidor_len);	
	buffer[rc-2] = '\0';
	buffer[rc-1] = '\0';
	buffer[rc] = '\0';
	
	printf("%s\n", buffer);
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
		
	printf("[Cliente no ar na porta %s. Aguardando comandos e/ou mensagens de outros usuarios]\n",argv[2]);
	printf("[Para finalizar, pressione CTRL+c ou rode um kill ou killall]\n");

	if(protocolo == TCP)
		iniciar_cliente_tcp(argv[2], atoi(argv[3]));
	else
		iniciar_cliente_udp(argv[2], atoi(argv[3]));

	printf("[%d: Encerrando cliente]\n", getpid());
	exit(0);
}

