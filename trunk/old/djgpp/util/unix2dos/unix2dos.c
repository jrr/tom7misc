

#include <stdio.h>
     int
main
(int c,
     char**
v) {  int z; FILE * in = stdin; FILE *
out = stdout; if (c > 1 &&
!(in = fopen(v[1
],"r"
"b"))) { printf("%s: Can't open %s.\n",v[0],v[
1]); abort(
);}if (c > 2 && !(out = fopen(v[2],"wb"))){printf
("%s: Can't write to %s.\n", v
[0],v[1]);abort
();}while     (EOF != (
z = fgetc(in)
)) if (z=='\n')   fprintf(
out,"\r\n"
);else
   fputc(z             ,out);
}

