
#include <string>
#include <stdlib.h>
#include <iostream.h>
#

main (int c, char**argv) {
     if (c>1) {
        string command,file;
        for (int x=1;x<c;x++) command += argv[x] + string(" ");

        while(getline(cin,file))
            system(string(command+file).c_str());
     exit(0);
     } else {
        cerr << "You must give a program to run on the command line, ie:\n"
                "\n    rm\n"
                "or\n"
                  "    rm -f\n";
     exit(-1);
     }

}
