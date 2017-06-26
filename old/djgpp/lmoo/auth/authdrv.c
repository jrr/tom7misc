#include <global.h>

int main (int argc, char**argv) {
     wint key={0,0},block,plaintext = ((wint){0x65181830,0xDEADBEEF});
     switch (argc) {
     case 1:
     break;
     case 3:
         plaintext = keyfromtext(argv[2]);
     case 2:
         key = keyfromtext(argv[1]);
     break;
     default:
       printf ("%s [key [plaintext]]\n",argv[0]);
       exit(-1);
     }

       printf ("Key: ");
       print_wint(key);
       printf ("In:  ");
       print_wint(plaintext);
       printf ("Out: ");
       print_wint( block = enc_block( plaintext, key ));
       printf ("Dec: ");
       print_wint(         dec_block( block    , key ));


return 0;
}
