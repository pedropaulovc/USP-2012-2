#ifndef _SERVIDOR_GERENTE_H
#define _SERVIDOR_GERENTE_H

#include <poll.h>
#include "util_tcp.h"

#define MAXLINE 4095

#define CAD 0
#define CAD_OK 1
#define CAD_NEG 2
#define LIST 3
#define MSG 4
#define DESC 5
#define ENV 6
#define ENV_OK 7
#define ENV_NEG 8


void executar_gerente(int porta);

#endif

