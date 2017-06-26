
#include "slick.h"
#include "allegro.h"
#include <iostream.h>


void slick_info() {
     slick_cpu();
     slick_os();
}

void slick_os() {
     switch (os_type) {
     case OSTYPE_UNKNOWN: cout << "DOS/unknown"; break;
     case OSTYPE_WIN3:    cout << "Windows 3"; break;
     case OSTYPE_WIN95:   cout << "Windows 95"; break;
     case OSTYPE_WINNT:   cout << "Win NT"; break;
     case OSTYPE_OS2:     cout << "OS/2"; break;
     case OSTYPE_WARP:    cout << "OS/2 Warp 3"; break;
     case OSTYPE_DOSEMU:  cout << "Linux DosEMU"; break;
     case OSTYPE_OPENDOS: cout << "Caldera OPENDOS"; break;
     default: break;
     }
     cout << endl;
}

void slick_cpu() {
     check_cpu();
     cout << cpu_vendor<< ": ";
     cout << "80" << cpu_family << "86 ";
     switch (cpu_model) {
        case 14: cout << "(Cyrix)"; break;
        case 0: case 3: case 1: cout << "(DX)"; break;
        case 2: cout << "(SX)"; break;
        default: break;
     }
     cout << " " << (cpu_fpu?"FPU":"no FPU")
          << ", " <<(cpu_mmx?"MMX":"no MMX")
          << ", " <<(cpu_cpuid?"(cpuid)":"(?)")<<endl;
}
