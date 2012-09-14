#include "util_udp.h"

int iniciar_escuta_udp(int porta){
	int listen_udp;
	struct sockaddr_in servaddr;

	if ((listen_udp = socket(AF_INET, SOCK_DGRAM, 0)) == -1) {
		perror("socket UDP :(");
		exit(2);
	}

	bzero(&servaddr, sizeof(servaddr));
	servaddr.sin_family      = AF_INET;
	servaddr.sin_addr.s_addr = htonl(INADDR_ANY);
	servaddr.sin_port        = htons(porta);

	if (bind(listen_udp, (struct sockaddr *)&servaddr, sizeof(servaddr)) == -1) {
		perror("bind UDP :(");
		exit(3);
	}
	
	return listen_udp;
}

