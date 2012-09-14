#ifndef _MAIN_SERVIDOR_H
#define _MAIN_SERVIDOR_H

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

#include "servidor_gerente.h"
#include "servidor_tcp.h"
#include "servidor_udp.h"
#include "util_tcp.h"
#include "util_udp.h"

#define LISTENQ 1
#define MAXDATASIZE 100
#ifndef MAXLINE
#define MAXLINE 4095
#endif


#endif
