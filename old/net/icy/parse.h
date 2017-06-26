#ifndef _TM7_PARSE_H
#define _TM7_PARSE_H

#include <string>
#include "aux.h"
#include <iostream.h>
#include "linkbank.h"

void parse(string);

extern linkbank * froodsays;

void load_action_list(const char *);

int eatme(string);

#define ACTION_LIST "control.icy"

#endif
