
#include <stdio.h>

int main (int argc, char ** argv) {

  if (argc >= 3) {
    char * a = argv[1];
    char * b = argv[2];
    
    printf ("%d\n",
	    ml_bestalignment(strlen(a), a,
			     strlen(b), b));
  } else {
    printf("test str1 str2\n");
  }

}
