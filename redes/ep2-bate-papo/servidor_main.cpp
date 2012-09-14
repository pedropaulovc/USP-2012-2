/* Por Prof. Daniel Batista <batista@ime.usp.br>
 * Em 12/08/2012
 * Modificações pontuais por Pedro Paulo Vezzá Campos - 7538743
 * Em 01/09/2012
 */


#include <stdio.h>
#include <stdlib.h>
#include <sys/ioctl.h>
#include <sys/poll.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <netinet/in.h>
#include <errno.h>
#include <signal.h>
#include <unistd.h>

#include "servidor_main.h"

pid_t pid_tcp, pid_udp;

void terminar(int signum){
	if(pid_tcp > 0)
		kill(pid_tcp, SIGTERM);
	if(pid_udp > 0)
		kill(pid_udp, SIGTERM);
}

int main (int argc, char **argv) {
	if (argc < 2) {
		fprintf(stderr,"Uso: %s <Porta>\n\n",argv[0]);
		fprintf(stderr,"Vai rodar um servidor de bate-papo na porta <Porta> TCP e UDP\n");
		exit(1);
	}

	printf("[Servidor no ar. Aguardando conexoes na porta %s]\n",argv[1]);
	printf("[Para finalizar, pressione CTRL+c ou rode um kill ou killall]\n");

	if ((pid_tcp = fork()) == 0) {
		/**** PROCESSO FILHO - INTERFACE TCP ****/
		printf("[%d: Iniciando interface TCP]\n", getpid());

		iniciar_servidor_tcp (atoi(argv[1]));

		printf("[%d: Encerrando interface TCP]\n", getpid());
		exit(0);
	}
	
	if((pid_udp = fork()) == 0) {
		/**** PROCESSO FILHO - INTERFACE UDP ****/
		printf("[%d: Iniciando interface UDP]\n", getpid());

		/**** TODO *****/

		printf("[%d: Encerrando interface UDP]\n", getpid());
		exit(0);
	}
	
	/**** PROCESSO PAI - GERENTE DE CONEXÕES ****/
	printf("[%d: Iniciando gerente]\n", getpid());
	
	signal(SIGTERM, terminar);
	signal(SIGKILL, terminar);
	
	int x;
	while(scanf("%d", &x) || true);
	
	//executar_gerente(atoi(argv[1]) + 1);
	
	printf("[%d: Encerrando gerente]\n", getpid());
	exit(0);
}



