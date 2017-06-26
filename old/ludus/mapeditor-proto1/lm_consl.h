#ifndef __ludus_map_console_
#define __ludus_map_console_

#define keyin() (readkey() & 0xff)

#include <string.h>

// Console stuff

void console_on(void) {
char a,byebye;
char cons_buffer[512];
char cons_cmd[512];
// Go to text mode:
show_mouse(NULL);
set_gfx_mode(GFX_TEXT, 80, 25,0,0);
printf ("Hello, I am your console speaking.\r\n");
printf ("Enter to quit. HELP for available commands.\r\n");
// Main console loop:
byebye=0;
do {
  cons_gets(cons_buffer);
//  printf ("Oh yeah. %s\r\n",cons_buffer);
  cons_cmd[0] = 0;
  if(index(cons_buffer,' ')) {
     strncpy(cons_cmd,cons_buffer, (int) index(cons_buffer,' ') - (int) &cons_buffer);
    } else {                             //     index
     strcpy(cons_cmd,cons_buffer);
     }
//  printf ("Your command: %s\r\n",cons_cmd);

//  Split it:
//  make an array of pointers within the string
//  replaces spaces with \0

if (! stricmp(cons_cmd,"save")) {
     save_map("map.dat");
     printf ("Saved map as %s.\r\n","map.dat");
  }
  else if (cons_cmd[0] == 0) byebye=1; // let's go.
  else if (! stricmp(cons_cmd,"help")) {
    printf ("Commands: \r\n");
    printf ("SAVE   HELP\r\n");
  } else {
    printf ("Unknown command: %s\r\n",cons_cmd);
  }
} while (!byebye);

// Back to Game:
set_gfx_mode(GFX_AUTODETECT, 640, 480,0,0);
set_pallete(local_pallete);
show_mouse(screen);
blit_offscreen();
}

void cons_gets(char * dest) {
  // Gotta write our own goddamn function
//  char dieme = 0;
  char a;
  int idx=0;
  while (1) {
    while (!keypressed()) {}  // maybe make it sparkle or something.
    // Get key
    a = readkey() & 0xff;
    putch(a);
    switch (a) {
      case 13: // CR
       putch(10);
       dest[idx]=0;
       return; break;
      case 8:  // BS
       dest[idx--]=0;
       putch(32); putch(8); // erase and space
       break;
      default:
       dest[idx++]=a;

      }
    }

}

#endif
