#include "cliente.h"

void iniciar_cliente_tcp(int porta){
	

}

void iniciar_cliente_udp(int porta){


}

int main (int argc, char **argv) {
   struct  hostent *hptr;
   char    enderecoIPServidor[INET_ADDRSTRLEN];


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
		
	if ((hptr = gethostbyname(argv[2])) == NULL) {
		fprintf(stderr,"gethostbyname :(\n");
		exit(1);
	}
	
	if (hptr->h_addrtype != AF_INET) {
		fprintf(stderr,"h_addrtype :(\n");
		exit(1);
	}
	
	if ( (inet_ntop(AF_INET, hptr->h_addr_list[0], enderecoIPServidor, sizeof(enderecoIPServidor))) == NULL) {
		fprintf(stderr,"inet_ntop :(\n");
		exit (1);
	}


	printf("[Cliente no ar na porta %s. Aguardando comandos e/ou mensagens de outros usuários]\n",argv[2]);
	printf("[Para finalizar, pressione CTRL+c ou rode um kill ou killall]\n");

	if(protocolo == TCP)
		iniciar_cliente_tcp (atoi(argv[1]));
	else
		iniciar_cliente_udp (atoi(argv[1]));

	printf("[%d: Encerrando servidor]\n", getpid());
	exit(0);
}

