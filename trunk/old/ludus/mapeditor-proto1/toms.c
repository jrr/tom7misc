#include <stdio.h>
#include <conio.h>
#include <stdlib.h>

char a;
int b;
int fg;
int bg;

void main(Void) {
clrscr();

textcolor(14);
for (b=1;b<100;b++) {
fg = b % 14 + 1;
bg = abs(fg-10);
if (bg==fg) bg--;
textcolor(fg);
textbackground(bg);
ScreenSetCursor(0,0);
cprintf("HEY %d %d",fg,bg);
printf("Yo. %d \r\n",b);
while (!kbhit()) {
ScreenSetCursor(13,12);
cprintf(" %d ",rand());
}
a = getch();
printf("%s\r\n",&a);
}

}
