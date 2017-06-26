#include <string>
#include <iostream.h>
#include <fstream.h>

main () {
int b;
string d;
ifstream c("buildnum.h");
c >> d; // #ifndef
c >> d; // __WM_BUILDNUM_H
c >> d; // #define
c >> d; // __WM_BUILDNUM_H
c >> d; // #define
c >> d; // _WM_BUILDNUM
c >> b;
c.close();
ofstream a("buildnum.h");
a << "#ifndef __WM_BUILDNUM_H\n"
        "#define __WM_BUILDNUM_H\n"
        "#define _WM_BUILDNUM " << (b+1) << "\n"
        "#endif\n";
a.close();
exit(0);
}
