
#include <string>
#include <stdlib.h>
#include <iostream.h>
#include <stdio.h>

main (int c, char**argv) {
  if (c>1) {
    string prefix,file;
    prefix = argv[1];
    int z;

    while(getline(cin,file))
      {
	FILE * in = fopen(file.c_str(),"rb+");
	FILE * out = fopen((prefix + file).c_str(),"wb+");
	if (!(in && out)) {
	  printf("#### ERROR: (%s)->(%s)\n", file.c_str(), (prefix+file).c_str());
	  if (in) fclose(in);
	  if (out) fclose(out);
	  continue;
	}

	while     (EOF != (
			   z = fgetc(in)
			   )) if (z=='\r') {} 
			   else
			     fputc(z             ,out);

	fclose(in); fclose(out);
      }

    exit(0);
  } else {
    cerr << "Give a default out path and a file list on stdin:\n"
	 << argv[0] << " < filelist l:\\unix\\\n";
    exit(-1);
  }

}

