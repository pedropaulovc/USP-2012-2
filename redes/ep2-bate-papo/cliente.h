#ifndef _CLIENTE_H
#define _CLIENTE_H

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
#include <string>
#include <sstream>

using namespace std;

#define TCP 0
#define UDP 1

#define MAX_FDS 2

#ifndef MAXLINE
#define MAXLINE 1024
#endif

#endif
