#ifndef _SERVIDOR_GERENTE_H
#define _SERVIDOR_GERENTE_H

#include <poll.h>
#include <set>
#include <string>
#include <iostream>
#include "util_tcp.h"

using namespace std;

#define MAXLINE 4095

#define REQ_INCOMPLETA -1
#define NICK_ORIGEM_DESCONHECIDO -2
#define NICK_DESTINO_DESCONHECIDO -3
#define ENV_OK 0

set<string> nicks;

void executar_gerente(int porta);

#endif

