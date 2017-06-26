#include "mape.h"
#include <fstream.h>

/***** limp functions for the map editor ********/

string * TOOLBAR_CODE = 0;

void toolbarproc(int idx) {
     if (TOOLBAR_CODE)
      eval_limp((string)"(eval (" + TOOLBAR_CODE[idx] + (string)"))");
     
};

ext_limp_token map_limp_toggle_layer(stringqueue & oh) {
     if (oh.length() < 1) {
          limpeprint("Usage: ~gtoggle-layer ~Blayernum");
          return (LIMP_ERROR_TOKEN);
     }
     switch(atoi(oh.stuff[0].c_str())) {
     case 0: DRAWLAYER0 = !DRAWLAYER0; break;
     case 1: DRAWLAYER1 = !DRAWLAYER1; break;
     case 2: DRAWLAYER2 = !DRAWLAYER2; break;
     default: 
          limpeprint("~rtoggle-layer: ~elayer out of range.");
          return (LIMP_ERROR_TOKEN);
     }
     force_fs();
     return ((ext_limp_token){(string)"OK",LIMP_VALUE});
}

ext_limp_token map_limp_add_loop(stringqueue & oh) {
     if (oh.length() < 3) {
          limpeprint("Usage: ~gadd-loop ~Bticks tile1 tile2 ~e[~Btile3 ~e...]");
          return (LIMP_ERROR_TOKEN);
     }
     if (oh.length() > 17) {
          limpeprint("~radd-loop: ~Rloops can be up to 16 tiles long (use an ~Bobject!)");
          return (LIMP_ERROR_TOKEN);
     }
     tile_loop * midget = new tile_loop;
     midget -> link = loophead;
     midget -> interval = atoi(oh.stuff[0].c_str());
     for (uint x=1;x<oh.length();x++) {
          midget -> loop[x-1] = atoi(oh.stuff[x].c_str());
     }
     midget -> length = oh.length()-1;
     midget -> next = 1;

     midget -> tileset = 0; /* FIXME ******** */

     loophead = midget;
     return ((ext_limp_token){(string)"OK",LIMP_VALUE});
}

ext_limp_token map_limp_status(stringqueue & oh) {
     if (oh.length() < 1 || oh.length() > 2) {
          limpeprint("Usage: ~gtoolbar ~Bmessage ~e[~Bticks (1/72 sec)~e]");
          return (LIMP_ERROR_TOKEN);
     }
     int t = (oh.length()==2)?atoi(oh.stuff[1].c_str()):144;
     status(oh.stuff[0],t);
     return ((ext_limp_token){(string)"OK",LIMP_VALUE});
}

ext_limp_token map_limp_toolbar(stringqueue & oh) {
     /* format is x y iconidx command iconidx command ... */

     if (oh.length() < 4 || (1&oh.length())) {
          limpeprint("Usage: ~gtoolbar ~Bx y iconidx command1 ~e[~Biconidx command2 ~e...]");
          return (LIMP_ERROR_TOKEN);          
     }

     int x = atoi(oh.stuff[0].c_str());
     int y = atoi(oh.stuff[1].c_str());

     if (x < 0 || x > 630 || y < 0 || y > 470) {
          limpeprint("toolbar: ~rcoordinates are off the screen!");
          return (LIMP_ERROR_TOKEN);
     }

     int num = (oh.length() - 2)>>1;

     for (int i=0;i<num;i++) {
          int zz = atoi(oh.stuff[2+(i<<1)].c_str());
          if (zz < 0 || zz > LAST_TOOLICON) {
            limpeprint((string)"toolbar: ~rpoor iconidx (max is ~B" + itos(LAST_TOOLICON-1) + (string)"~r)");
            return (LIMP_ERROR_TOKEN);
          }
     }
     /* checks out ok */

     if (windows[W_TOOLBAR].style) { // destroy_window FIXME
               cprint("~gtoolbar: ~bwarning: ~etoolbar already exists.");
               }


     delete TOOLBAR_CODE;
     TOOLBAR_CODE = new string[num];

     lw_description meh;
     meh.color = C_BLUE3;
     meh.title = "~0tools";

     windows[W_TOOLBAR] = create_generalized_lwin(x, y, (num << 4)
          + 3 + 3 /* lb rb */, 16 + 8 + 3 /* tb lb */, meh);

     for (int i=0;i<num;i++) {
          TOOLBAR_CODE[i] = oh.stuff[3+(i<<1)];
          lw_button * sung = new lw_button;
          sung -> flags = BUTTON_DRAW_PIC;
          sung -> id = i;
          sung -> proc = toolbarproc;
          sung -> pic = toolico[atoi(oh.stuff[2+(i<<1)].c_str())];
          add_widget(&windows[W_TOOLBAR],sung,LW_BUTTON,i<<4,0,16,16);
     }

     windows[W_TOOLBAR].paint(&windows[W_TOOLBAR]);
     win_draw_windows();
     force_fs();

//     limpeprint("Not yet implemented...");
     return ((ext_limp_token){(string)"OK",LIMP_VALUE});
}


ext_limp_token map_limp_set_paintlayer(stringqueue & oh) {
     int z;
     if (oh.length() != 1) {
          limpeprint("set-paintlayer ~glayernum");
          return (LIMP_ERROR_TOKEN);
     }
     z = atoi(oh.stuff[0].c_str());
     if (z < 0 || z > 3) {
          limpeprint("set-paintlayer ~G(layer from ~00~G to ~03~G)");
          return (LIMP_ERROR_TOKEN);
     }
     paintlayer = z;
     return ((ext_limp_token){(string)"OK",LIMP_VALUE});
}

ext_limp_token map_limp_quit(stringqueue &) {
     windows[0].killme++;
     return ((ext_limp_token){(string)"OK",LIMP_VALUE});
}

ext_limp_token map_limp_fill_layer(stringqueue & eh) {
     /* fills map layer arg1 from (arg2,arg3) to (arg2+arg4,arg3+arg5)
        with tile [arg6] or current tile */
     string t;
     if (eh.length() == 6) {
         t = eh.stuff[5];
     } else if (eh.length() == 5) {
         t = (string)"'" + itos(drawtile)+(string)"'";
     } else {
          limpeprint("fill-layer layernum x y w h [tilenum]");
          return (LIMP_ERROR_TOKEN);
     }
     int l=atoi(eh.stuff[0].c_str()),
         xs=atoi(eh.stuff[1].c_str()),
         ys=atoi(eh.stuff[2].c_str()),
         w=atoi(eh.stuff[3].c_str()),
         h=atoi(eh.stuff[4].c_str());
     if (l > 2 || l < 0) {
          limpeprint("layer isn't {0,1,2}");
          return (LIMP_ERROR_TOKEN);
     }
     if (w < 0 || h < 0) {
          limpeprint("(w,h) must be nonnegative");
          return (LIMP_ERROR_TOKEN);
     }

     for (int y=ys;y<(ys+h);y++)
     for (int x=xs;x<(xs+w);x++) {

/*
          limpdprint((string)"Do: (" + itos(x) + (string)","
                              + itos(y) + (string)") -> "
                              + itos(t));
*/

          if (x >= 0 && x < windows[0].map.w
           && y >= 0 && y < windows[0].map.h) {
               windows[0].map.dat[l][(y*windows[0].map.w)+x] =
                    atoi(evaltoken(t).c_str());
                   }
     }
     map_draw_windows();
     force_fs();
     return ((ext_limp_token){(string)"OK",LIMP_VALUE});
}

ext_limp_token map_limp_help(stringqueue &) {
     cprint((string)"Read the source.");
     return ((ext_limp_token){(string)"OK",LIMP_VALUE});
}

ext_limp_token map_limp_test(stringqueue & shite) {
     for (uint x=0;x<shite.length();x++) {
          cprint(itos(x) + (string)" >> " + shite.stuff[x]);
     }
    return ((ext_limp_token){(string)"YES",LIMP_VALUE});
}

ext_limp_token map_limp_use(stringqueue & oh) {
     if (oh.length() !=1) {
         limp_error("usage: ~Buse `~efilename~B`");
         return (LIMP_ERROR_TOKEN);
     } else {
     FILE * inways;
     string filename = oh.stuff[0];
     if (!(inways = fopen(filename.c_str(),"rb") )) {
         limp_error((string)"Can't open ~B" + filename + (string)"~0.");
         return (LIMP_ERROR_TOKEN);
     }

     int c;
     string s;
     while ( (c=getc(inways)) != EOF ) s+=(char)c;
     fclose (inways);

     post_evaluate = (string)"(, " + s + (string)" )";

     return ((ext_limp_token){(string)"YES",LIMP_VALUE});
     }
}

ext_limp_token map_limp_set_tile(stringqueue & oh) {
     int x=0,y=0,l=0,t=drawtile;
     if (oh.length() == 2) {
          x = atoi(oh.stuff[0].c_str());
          y = atoi(oh.stuff[1].c_str());
     } else if (oh.length() == 3) {
          x = atoi(oh.stuff[0].c_str());
          y = atoi(oh.stuff[1].c_str());
          t = atoi(oh.stuff[2].c_str());
     } else if (oh.length() == 4) {
          x = atoi(oh.stuff[0].c_str());
          y = atoi(oh.stuff[1].c_str());
          t = atoi(oh.stuff[2].c_str());
          l = atoi(oh.stuff[3].c_str());
     } else {
      limp_error("Usage: ~bset-tile ~gx y ~B[~gtilenum ~B[~glayernum ~B]]");
      return (LIMP_ERROR_TOKEN);
     }
         if (windows[0].map.dat[l][x + (windows[0].map.w*y)] != t)
            { windows[0].map.dat[l][x + (windows[0].map.w*y)] = t;
//          thisisdirty((mx&self->data[0]) -32,(my&self->data[1]) -32,32,32);
          force_fs(); /////////// FIXME!!!!   ************ VERY DUMB
          rootpaint(&windows[0]);    // This could be made faster, by only drawing
          map_draw_windows();       // the tile that changed. Big deal.
            }
      return ((ext_limp_token){(string)"YES",LIMP_VALUE});
}

// THIS DOES NOT WORK! *************** FIXME!! **********************

ext_limp_token map_limp_resize_map(stringqueue & oh) {
     // resize-map newx newy [startx [starty [filltile]]]
     if (oh.length() > 5 || oh.length() < 2) {
      limp_error("Usage: ~Bresize-map ~Gnew-x-size new-y-size"
                  " ~B[~Gx-start(0) ~B[~Gy-start(0)"
                  " ~B[~Gfilltile(currenttile)~B]]]");
      return (LIMP_ERROR_TOKEN);
     }
     int newx = atoi(oh.stuff[0].c_str()),
         newy = atoi(oh.stuff[1].c_str()),
         /*startx = 0, starty=0,*/ destx=0,desty=0,
         filltile = drawtile;
     if (oh.length() > 2) {
        destx = atoi(oh.stuff[2].c_str());
        if (oh.length() > 3) {
           desty = atoi(oh.stuff[3].c_str());
           if (oh.length() > 4) filltile = atoi(oh.stuff[4].c_str());
        }
     }
     ushort *gbv[3];
     uchar  *poo;
     gbv[0] = (ushort*) malloc(sizeof(ushort)*newx*newy);
     gbv[1] = (ushort*) malloc(sizeof(ushort)*newx*newy);
     gbv[2] = (ushort*) malloc(sizeof(ushort)*newx*newy);
     poo    = (uchar *) malloc(newx*newy);
     if (!(poo && gbv[0] && gbv[1] && gbv[2])) {
      limp_error("I can't even allocate memory for a map that size.");
      free(poo);      /* in case we got some. ok to free null pointers */
      free(gbv[0]);
      free(gbv[1]);
      free(gbv[2]);
      return (LIMP_ERROR_TOKEN);
     }
     // all right! copy over...
     for (int i=0;i<(newx*newy);i++) {gbv[0][i]=filltile;
                                      gbv[1][i]=gbv[2][i]=0;
                                      poo[i]=0;}
     #define MAP windows[0].map
/*
     int stdx=destx,
         stdy=desty;

     for (int l=0;l<3;l++)  {
     desty=stdy;
     for (int srcy=starty;srcy<MAP.h;srcy++) {
       destx=stdx;
       for (int srcx=startx;srcx<MAP.w;srcx++) {
          if (desty < newy && desty >= 0 && destx< newx && destx >= 0
           && srcy < MAP.h && srcy  >= 0 && srcx < MAP.w&& srcx  >= 0)
          gbv[l][desty*newx + destx] = MAP.dat[l][srcy*MAP.w + srcx];
          destx++;
       }
       desty++;
    }
   }
*/
   free(MAP.dat[0]);
   free(MAP.dat[1]);
   free(MAP.dat[2]);
   free(MAP.clip);
   MAP.dat[0] = gbv[0];
   MAP.dat[1] = gbv[1];
   MAP.dat[2] = gbv[2];
   MAP.clip = poo;
   MAP.w = newx;
   MAP.h = newy;

#undef MAP

   windows[0].data[0] =
   windows[0].data[1] = 0;
   rootpaint(&windows[0]);
   map_draw_windows();
   force_fs();

   default_mapvariables();


   return ((ext_limp_token){(string)"YES",LIMP_VALUE});
}


ext_limp_token map_limp_randomlist(stringqueue & oh) {
     if (!oh.length()) {
          limp_error("Usage ~brandomlist ~gitem item item ...");
          return (LIMP_ERROR_TOKEN);
     }

     // otherwise, choose a random element:

     int idx = (int)((rand()/float(RAND_MAX))*oh.length());
//     limpdprint((string)"Element ~R" + itos(idx));
     return ((ext_limp_token){oh.stuff[idx%oh.length()],LIMP_VALUE});
}

ext_limp_token map_limp_screen_capture(stringqueue & oh) {
     if (oh.length() != 1) {
      limp_error("Usage: ~bscreen-capture ~gfilename");
      return (LIMP_ERROR_TOKEN);
     }
     if (! save_pcx((char*)oh.stuff[0].c_str(), buffer, _current_pallete))
     return ((ext_limp_token){(string)"YES",LIMP_VALUE});
     else return (LIMP_ERROR_TOKEN);
}

ext_limp_token map_limp_refresh_screen(stringqueue &) {
     rootpaint(&windows[0]);
     /* maybe cycle through all windows, do paint functions? */
     map_draw_windows();
     force_fs();
     return ((ext_limp_token){(string)"YES",LIMP_VALUE});
}

ext_limp_token map_limp_bind(stringqueue & oh) {
    if (oh.length() == 1) {
      // print the current keybinding.
      int k = map_keynum(oh.stuff[0]);
      if (k == -1) limpeprint((string)"~g"+oh.stuff[0] +(string)"~0 is not a key.");
      else {
          if (bindkeys[k] != "") {
            cprint((string)"~g" +oh.stuff[0]+ "~0 -> ~b" + bindkeys[k]);
          } else cprint((string)"~g" +oh.stuff[0]+"~0 is not bound.");
      }
      return ((ext_limp_token){(string)"YES",LIMP_VALUE});
    } else if (oh.length() != 2) {
      limp_error("usage: ~Bbind `~ekey~B` `~elimp expression~B`");
      return (LIMP_ERROR_TOKEN);
    } else {
      /* add to bind list */
      if (add_keybind(oh.stuff[0],oh.stuff[1])) {
          return ((ext_limp_token){(string)"YES",LIMP_VALUE});
      } else return (LIMP_ERROR_TOKEN);
    }
}

ext_limp_token map_limp_toggle_onlyroot(stringqueue &) {
     // toggles visibility of all layers but root window: (tab key)
     if (draw_only_root)
          onlyroot_off();
     else onlyroot_on ();
     return ((ext_limp_token){(string)"YES",LIMP_VALUE});
}

ext_limp_token map_limp_show_console(stringqueue &) {
          windows[W_CONSOLE].flags |= LWIN_FL_ACTIVE;
          windows[W_CONSOLE].paint(&windows[W_CONSOLE]);
          draw_only_root = 0;
          map_draw_windows();
          force_fs();
     return ((ext_limp_token){(string)"YES",LIMP_VALUE});
}

ext_limp_token map_limp_get_file(stringqueue & oh) {

      char nooper[128] = {".\\"};
      string caption = "Select File";
      const char * ext = 0;
      switch(oh.length()) {
          case 3:
               ext = oh.stuff[2].c_str();
          case 2:
               strncpy(nooper,oh.stuff[1].c_str(),80);
          case 1:
               caption = oh.stuff[0];
          case 0: break;
          default:
          limp_error("Usage: ~bget-file ~e[~gcaption ~e[~gpath ~e[~gext~e]]]");
          return LIMP_ERROR_TOKEN;
      }

      freeze_mouse_flag = 0;
      show_mouse(0); /* hide old mouse */
      /* blast screen over: */
      force_fs();                 /* nasty! *********** */
      dirty_draw();

      show_mouse(screen);
      int a = 
      (file_select((char*)caption.c_str(),nooper,(char*)ext));

      show_mouse(NULL);
      freeze_mouse_flag = 1;
      force_fs();

      if (a) {
          return((ext_limp_token)
               {(string)nooper,LIMP_VALUE});
      } else  /* otherwise, cancelled: */
      return ((ext_limp_token){(string)"\x08THROW CANCEL",LIMP_VALUE});
      /* Throws exception CANCEL */
}


ext_limp_token map_limp_debug_allbindings(stringqueue & ) {
     int x=0;
     for (int u=0;u<NUM_BINDKEYS;u++)
          if (bindkeys[u] != "") { x++;
               cprint((string)"~b" + map_numtokey(u) +(string)": ~G" + bindkeys[u]);}
          return ((ext_limp_token){itos(x),LIMP_VALUE});
}

ext_limp_token map_limp_save_bindings(stringqueue & oh) {
     if (oh.length() == 1) {
     ofstream smack(oh.stuff[0].c_str());
     if (!smack) {
          limp_error((string)"save-bindings: ~rCan't open file ~g" + oh.stuff[0]);
          return LIMP_ERROR_TOKEN;
     }
       for (int u=0;u<NUM_BINDKEYS;u++) {
          if (bindkeys[u] != "") {
               smack << "(bind " << map_numtokey(u) << "  '" << bindkeys[u] << ")\n";
//               cprint((string)"~b" + itos(u) +(string)": ~G" + bindkeys[u]);
               }
       }
         smack.close();
         return ((ext_limp_token){(string)"YES",LIMP_VALUE});
     } else {
          limp_error("Usage: ~rsave-bindings ~gfilename");
          return (LIMP_ERROR_TOKEN);
     }
}


ext_limp_token map_limp_toggle_bluestuff(stringqueue & ) {
      if (draw_clip && paintlayer==3) paintlayer = 0; // ????????? fixme?
      draw_clip ^= 1;
      windows[0].paint(&windows[0]);
      map_draw_windows();
      force_fs();
      return ((ext_limp_token){(string)"YES",LIMP_VALUE});
}

ext_limp_token map_limp_new_map(stringqueue & oh) {
     // new-map newx newy [filltile]
     if (oh.length() != 3 && oh.length() != 2) {
      limp_error("Usage: ~Bnew-map ~Gnew-x-size new-y-size"
                  " ~B[~Gfilltile(currenttile)~B]");
      return (LIMP_ERROR_TOKEN);
     }
     int newx = atoi(oh.stuff[0].c_str()),
         newy = atoi(oh.stuff[1].c_str()),
         filltile = drawtile;
     if (oh.length() > 2) {
        filltile = atoi(oh.stuff[2].c_str());
     }
     #define MAP windows[0].map
   free(MAP.dat[0]);
   free(MAP.dat[1]);
   free(MAP.dat[2]);
   free(MAP.clip);
     MAP.dat[0] = (ushort*) malloc(sizeof(ushort)*newx*newy);
     MAP.dat[1] = (ushort*) malloc(sizeof(ushort)*newx*newy);
     MAP.dat[2] = (ushort*) malloc(sizeof(ushort)*newx*newy);
     MAP.clip   = (uchar *) malloc(newx*newy);
     if (!(MAP.clip && MAP.dat[0] && MAP.dat[1] && MAP.dat[2])) {
      limp_error("I can't even allocate memory for a map that size.");
      free(MAP.clip);   /* in case we got some. ok to free null pointers */
      free(MAP.dat[0]);
      free(MAP.dat[1]);
      free(MAP.dat[2]);
      MAP.w = MAP.h = 0;
   windows[0].data[0] =
   windows[0].data[1] = 0;
   rootpaint(&windows[0]);
   map_draw_windows();
   force_fs();
      return (LIMP_ERROR_TOKEN);
     }
     // all right! copy over...
     for (int i=0;i<(newx*newy);i++) {MAP.dat[0][i]=filltile;
                                      MAP.dat[1][i]=MAP.dat[2][i]=0;
                                      MAP.clip[i]=0;}

   MAP.w = newx;
   MAP.h = newy;

#undef MAP

   windows[0].data[0] =
   windows[0].data[1] = 0;
   rootpaint(&windows[0]);
   map_draw_windows();
   force_fs();
     return ((ext_limp_token){(string)"YES",LIMP_VALUE});
}


ext_limp_token map_limp_mouse_xtile(stringqueue &) {
      return ((ext_limp_token){itos(((windows[0].data[0])+mouse_x)>>5),LIMP_VALUE});
}

ext_limp_token map_limp_mouse_ytile(stringqueue &) {
      return ((ext_limp_token){itos(((windows[0].data[1])+mouse_y)>>5),LIMP_VALUE});
}

ext_limp_token map_limp_load_map(stringqueue & oh) {
string mapname,tileset,defmidi,palfile,mapinfo,objfile,shortname;
uchar flags1,flags2,flags3;
if (oh.length()!=1) {
      limp_error("Usage: ~Bload-map ~Gfilename");
     return (LIMP_ERROR_TOKEN);
}
lmap mip =    load_map(mapname,
              tileset,
              defmidi,
              palfile,
              mapinfo,
              objfile,
              shortname,
              flags1,
              flags2,
              flags3,
              oh.stuff[0]);

if (mip.h) {
     limp_global_set("TILESET",tileset);
     limp_global_set("DEFMIDI",defmidi);
     limp_global_set("PALFILE",palfile);
     limp_global_set("MAPINFO",mapinfo);
     limp_global_set("TILESET",tileset);
     limp_global_set("FLAGS1" ,itos(flags1));
     limp_global_set("FLAGS2" ,itos(flags2));
     limp_global_set("FLAGS3" ,itos(flags3));

     free(windows[0].map.dat[0]);
     free(windows[0].map.dat[1]);
     free(windows[0].map.dat[2]);
     free(windows[0].map.clip  );
     windows[0].map.dat[0] = mip.dat[0];
     windows[0].map.dat[1] = mip.dat[1];
     windows[0].map.dat[2] = mip.dat[2];
     windows[0].map.clip = mip.clip;
     windows[0].map.w = mip.w;
     windows[0].map.h = mip.h;
     rootpaint(&windows[0]);
     windows[0].data[0] = 0;
     windows[0].data[1] = 0;
     map_draw_windows();
     force_fs();
     return ((ext_limp_token){(string)"YES",LIMP_VALUE});
} else {
     /* FIXME: */
     limp_error("~rload-map~e: ~b some sort of file reading error (hehe, sorry)");
     return (LIMP_ERROR_TOKEN);
}
}

ext_limp_token map_limp_save_map(stringqueue & oh) {
     if (oh.length() != 1) {
      limp_error("Usage: ~bsave-map ~gfilename");
      return (LIMP_ERROR_TOKEN);
     }
     string palfile = limp_getval("PALFILE"),
            tileset = limp_getval("TILESET"),
            defmidi = limp_getval("DEFMIDI"),
            mapname = limp_getval("MAPNAME"),
            mapinfo = limp_getval("MAPINFO"),
            filename, objfile;
     uchar  flags1  = atoi(limp_getval("FLAGS1").c_str()),
            flags2  = atoi(limp_getval("FLAGS2").c_str()),
            flags3  = atoi(limp_getval("FLAGS3").c_str());
     objfile = getfilebase(filename = oh.stuff[0]);
//     filename = objfile = oh.stuff[0];

     /****** FIXME:
          make filename/objfile use the 8 significant chars of the
          filename, not the possibly absolute path given as an 
          argument.
     */

     FILE * outfile;
     if (!(outfile = fopen ((filename).c_str(),"wb+"))) {
      limp_error((string)"I can't open a file called '" + filename + (string) "'.");
      return (LIMP_ERROR_TOKEN);
     }

     // start writing it out.
     fprintf(outfile,"*MAP*");
     fputc(0,outfile); fputc(1,outfile); /* version 01 */
     puteightnul(outfile,objfile ); /* FIXME: mean filebase(filename) */
     puteightnul(outfile,palfile );
     puteightnul(outfile,tileset );
     puteightnul(outfile,defmidi );
     puteightnul(outfile,objfile );
#define MAP windows[0].map
     putword(outfile,MAP.w);
     putword(outfile,MAP.h);
     putdword(outfile,
         ftell(outfile)+4+1+1+1+4+4+mapname.length()+1+mapinfo.length()+1);
     fputc(flags1,outfile);
     fputc(flags2,outfile);
     fputc(flags3,outfile);
     putdword(outfile,
         ftell(outfile)+4+4);
     putdword(outfile,
         ftell(outfile)+4+mapname.length()+1);
     fprintf(outfile,mapname.c_str()); fputc(0,outfile);
     fprintf(outfile,mapinfo.c_str()); fputc(0,outfile);
     for(uint y=0;y<3;y++)
       for(int x=0;x<(MAP.w*MAP.h);x++)
          putword(outfile,MAP.dat[y][x]);
     for(int x=0;x<(MAP.w*MAP.h);x++)
          fputc(MAP.clip[x],outfile);
     fclose(outfile); /* BEFORE we return, heh. */
     return ((ext_limp_token){(string)"YES",LIMP_VALUE});
#undef MAP
}

string getfilebase(string in) {
     /* given d:\djgpp\lmoo\map\megahyper.lmp, return megahyper */
     int flag = 0;
     string it;
     for (uint x=0;x<in.length();x++) {
          switch (in[x]) {
          case '\\':
            flag = 0; it = "";
            break;
          case '.': flag = 1;
            break;
          default:
              if (!flag) it += in[x];
          }
     }
     return it;
}
