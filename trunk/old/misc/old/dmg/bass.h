#ifndef __TM7_BASS_H
#define __TM7_BASS_H

#include "dmg.h"
#include "midi.h"

void construct_measure_bass(int root, int measure, track * t, scheme & s);

void bass4_f1(int,int,track*,scheme);
void bass4_f2(int,int,track*,scheme);
void bass4_f3(int,int,track*,scheme);
void bass4_f4(int,int,track*,scheme);

extern void (*bass4[])(int,int,track*,scheme); /* track star, ha ha */

#define NUM_BASS4 4



#endif
