#ifndef _SERVIDOR_TCP_H
#define _SERVIDOR_TCP_H

#include <poll.h>
#include <fcntl.h>
#include <map>
#include <string>
#include <iostream>
#include <sys/ioctl.h>
#include "util_tcp.h"

using namespace std;

#ifndef MAXLINE
#define MAXLINE 4095
#endif

#define MAX_FDS 256
#define MAX_BACKLOG 5

void iniciar_servidor_tcp (int porta);

#endif

