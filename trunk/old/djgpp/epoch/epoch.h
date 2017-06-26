#ifndef _EPOCH_H_
#define _EPOCH_H_

#include <string>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>  // for the unblicking thing...
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <errno.h>
#include <iostream.h>

#include "esocket.h"
#include "eplayer.h"
#include "game.h"

#define MAJORVERSION        1
#define MINORVERSION        0
#define REVISION            0

#define MAX_CLIENTS 1024
#define MAX_GAMES 1024

#endif



