#ifndef _UTIL_TCP
#define _UTIL_TCP

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <netdb.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <time.h>
#include <unistd.h>

#define LISTENQ 1

int iniciar_escuta_tcp(int porta);
int aceitar_conexao(int listen_tcp);

#endif
