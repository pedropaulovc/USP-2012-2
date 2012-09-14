#ifndef _SERVIDOR_GERENTE_H
#define _SERVIDOR_GERENTE_H

#include <poll.h>
#include <map>
#include <string>
#include <iostream>
#include "util_tcp.h"

using namespace std;

#ifndef MAXLINE
#define MAXLINE 4095
#endif


void executar_gerente(int porta);

#endif

