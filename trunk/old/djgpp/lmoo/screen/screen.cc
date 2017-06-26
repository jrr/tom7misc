#include <stdio.h>
#include <allegro.h>
#include <global.h>

/* FIX THESE: */

char * messages[5] = {
"~YYel~ylow ~RRe~rd dog~B Blue~b boy~G Green~g th~eumb",
"~bAtom ~rHeart ~yMother",
"~WUniversity of Shiny Bug",
"~eFerris Wheel ~bEXTRACTOR!",
"~BF~bancy ~GP~gants",
};

void mousemove(int);

 volatile int m_oldx=0,m_oldy=0;

int oldpos=0;

int main () {
     int k=0,m=0,otw=0,mtw=0;
//     unsigned int a=1,b=2,c=3,d=4,e=5,m=0;
     int pos,tx=50,ty=50,tw,tdx=1,tdy=1,otx=0,oty=0;

//     mouse_callback = mousemove; // allegro callback


     allegro_init();
     install_keyboard();
     install_timer();
     install_mouse();

     text_init();
     dirt_init();

     while (!keypressed()) {
          show_mouse(NULL);

     tw = ftextlen(bigfont,messages[m]);
     mtw = ftextlen(bigfont,messages[0]);

if (k != retrace_count) {

     rectfill(buffer,otx,oty,otx+mtw,oty+22,0);

if (!(k%100)) {
     m++; m%=5;
     tw = ftextlen(bigfont,messages[m]);
     otw = tw;
}
     tx += tdx;
     ty += tdy;
     if ((tx + tw) > 600) tdx = -1;
     if (tx < 20)        tdx = 1;
     if ((ty + 20) > 400) tdy = -1;
     if (ty < 20)        tdy = 1;

     k = retrace_count;

     ftextout(buffer,bigfont,messages[m],tx,ty);

     dirtyspritemove(tx, ty, otx, oty, mtw, 20);

     otx=tx; oty=ty;

} 
          pos = mouse_pos;
          freeze_mouse_flag = 1;
        if (pos != oldpos) {

             dirtyspritemove(oldpos>>16,oldpos&65535,pos>>16,pos&65535,16,16);
             oldpos = pos;
        }


          freeze_mouse_flag = 0;
          show_mouse(buffer);
          dirty_draw();

     }

     set_gfx_mode(GFX_TEXT,0,0,0,0);
return 0;
}

#if 0
void dirty_draw() {
     if (!dirties) { num_nochange++; // DEBUG
                     return;}      // woo!

     if (dirties>MAX_DIRTY) {  // Too much activity. Redraw Everything.
         blit(buffer,screen,0,0,0,0,640,480);
         num_fullscreen++;
     } else {                  // woo!
          int x;
          for (x=0;x<dirties;x++)
               blit(buffer,screen,dirt[x].x,dirt[x].y,
                                  dirt[x].x,dirt[x].y,
                                  dirt[x].w, dirt[x].h );
          num_dirty++;
     }
     dirties = 0; // Uh... gotta do that.
}
#endif
