/* Por Prof. Daniel Batista <batista@ime.usp.br>
 * Em 12/08/2012
 * Modificações pontuais por Pedro Paulo Vezzá Campos - 7538743
 * Em 01/09/2012
 */

#include "servidor_main.h"


int main (int argc, char **argv) {
	if (argc < 2) {
		fprintf(stderr,"Uso: %s <Porta>\n\n",argv[0]);
		fprintf(stderr,"Vai rodar um servidor de bate-papo na porta <Porta> TCP e UDP\n");
		exit(1);
	}

	printf("[Servidor no ar. Aguardando conexoes na porta %s]\n",argv[1]);
	printf("[Para finalizar, pressione CTRL+c ou rode um kill ou killall]\n");

	if (fork() == 0) {
		/**** PROCESSO FILHO - INTERFACE TCP ****/
		printf("[%d: Iniciando interface TCP]\n", getpid());

		/**** TODO *****/

		printf("[%d: Encerrando interface TCP]\n", getpid());
		exit(0);
	}
	
	if(fork() == 0) {
		/**** PROCESSO FILHO - INTERFACE UDP ****/
		printf("[%d: Iniciando interface UDP]\n", getpid());

		/**** TODO *****/

		printf("[%d: Encerrando interface UDP]\n", getpid());
		exit(0);
	}
	
	/**** PROCESSO PAI - GERENTE DE CONEXÕES ****/
	printf("[%d: Iniciando gerente]\n", getpid());
	
	executar_gerente(atoi(argv[1]) + 1);
	
	printf("[%d: Encerrando gerente]\n", getpid());
	exit(0);
}



