#include <string>
//#include <fstream.h>
#include <stdio.h>
#include <iostream.h>
//#include <iomanip.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <values.h>

#define RECURSEMAX 1024
//#define HIGH 10
//#define TIMES 25000
int main (int argc, char ** argv) {
    int HIGH = atoi(argv[1]);
    int TIMES = atoi(argv[2]);
    srandom(time(NULL));
    int*stat = new int[HIGH];
    for (int x = 0;x < HIGH;x++) stat[x]=0;
    for (int x = 0;x < TIMES;x++) {
      int n = (int)((float(random())/float(MAXINT))*(HIGH));
      stat[n]++;
    }
    for (int x = 0;x < HIGH; x++) {
      printf("[%d]: %.2f%%\n",x,(stat[x]/float(TIMES))*100);
    }
    exit(0);
}
