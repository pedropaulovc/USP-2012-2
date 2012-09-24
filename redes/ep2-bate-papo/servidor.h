#ifndef _SERVIDOR_GERENTE_H
#define _SERVIDOR_GERENTE_H

#include <poll.h>
#include <fcntl.h>
#include <map>
#include <string>
#include <iostream>
#include <sys/ioctl.h>
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

using namespace std;

#ifndef MAXLINE
#define MAXLINE 4095
#endif

#define MAX_FDS 256
#define MAX_BACKLOG 5


#endif

