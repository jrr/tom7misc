//---------------------------------------------------------------------------
#include <vcl\vcl.h>
#include <stdlib.h>
#pragma hdrstop

#define MAX(c1,c2) ((c1 < c2) ? c2 : c1)
#define MIN(c1,c2) ((c1 < c2) ? c1 : c2)
#define SQUARED(c1) (c1 * c1)
#include "poopserv.h"
#include "sqstuf.h"
//---------------------------------------------------------------------------


void __fastcall TForm1::Checksquares(int x, int y, int game) {
// Checks for squares. ONLY the player that moved
// there, and ONLY squares involving the new X
// and Y coordinates.
int idx = games[game].board[x][y];
int xx, yy, dx, dy;


for (xx=0;xx<=7;xx++)
 for (yy=0;yy<=7;yy++) {
   if (games[game].board[xx][yy] == idx) {
     if (hassquare(x,y,xx,yy,idx,game)) {
   //    drawsquare (x,y,xx,yy,idx);
       dx = abs(xx - x);
       dy = abs(yy - y);
       score(idx,SQUARED((MAX(dx, dy) + 1)),game);
     }
   }
 }
}
void __fastcall TForm1::score(int player, int points, int game) {
 // mumblemumble
games[game].score[player] += points;
}

int __fastcall TForm1::hassquare(int x1, int y1, int x2, int y2, int idx, int game){
 // checks if player (idx) has a square with
 // the given diagonals.

 if (x1 == x2 && y1 == y2) return 0;
 // Otherwise it counts a single point as
 // a square (which it is, in some ways)
int dx, dy, offset, nx1, nx2, ny1, ny2;

dx = x2 - x1;
dy = y2 - y1;
  offset = dx - dy;
  if (offset % 2 == 0) {
    ny1 = y1 - (offset / 2);
    ny2 = y2 + (offset / 2);
    nx1 = x2 - (offset / 2);
    nx2 = x1 + (offset / 2);
    if ((MAX(MAX(nx1,nx2),MAX(ny1,ny2)) > 7)
       || (MIN(MIN(nx1,nx2),MIN(ny1,ny2))) < 0) return 0;
    // COULD be a square... check if he owns them.
//if (idx) { Application->MessageBox ("Hi", "Hi", MB_OK); }
    if (games[game].board[nx1][ny1] == idx && games[game].board[nx2][ny2] == idx) {
       return 1;
       } else {
       return 0;
       }
  } else { return 0; }

}
#if 0
void __fastcall TForm1::ReChecksquares() {
int x, y;
int idx, xx, yy;
for(x=0;x<8;x++)
 for(y=0;y<8;y++)
  if (idx = board[x][y])
   for (xx=x;xx<=7;xx++)
    for (yy=y;yy<=7;yy++)
     if (board[xx][yy] == idx)
      if (hassquare(x,y,xx,yy,idx))
       drawsquare (x,y,xx,yy,idx);
}
#endif
