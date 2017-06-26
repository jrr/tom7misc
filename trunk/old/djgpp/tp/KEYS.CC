/*** This is for TP - This is the key handler ***/

#include "tp.h"

int handlekeys(int key) {
if (key == (key & 0xFF)) {

        switch (key) {
          case 'q': case 'Q': denit();

          default: break;
        }
} else { // key > 255
  denit();


}
}
