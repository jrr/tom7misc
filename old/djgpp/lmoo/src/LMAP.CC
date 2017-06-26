#include "map.h"

lmap load_map(string & mapname,
              string & tileset,
              string & defmidi,
              string & palfile,
              string & mapinfo,
              string & objfile,
              string & shortname,
              uchar & flags1,
              uchar & flags2,
              uchar & flags3,
              string filename) {

     lmap smoot;
     smoot.w = smoot.h = 0;
     smoot.dat[0] = smoot.dat[1] = smoot.dat[2] = 0;
     smoot.clip = 0;
     FILE * inways;

//     cprint("open it...");
     if (! (inways = fopen(filename.c_str(),"rb"))) return smoot;
     for (uint x=0;x<7;x++) {
          if (fgetc(inways) != "*MAP*\x00\x01"[x]) return smoot; // not magic
     }
//     cprint("get some names");
     if (geteightsnul(inways,shortname)) return smoot;
     if (geteightsnul(inways,palfile)) return smoot;
     if (geteightsnul(inways,tileset)) return smoot;
     if (geteightsnul(inways,defmidi)) return smoot;
     if (geteightsnul(inways,objfile)) return smoot;
//     cprint(tileset);
//     cprint(defmidi);
//     cprint(objfile);
     int width,height;
//     cprint("get some words");
     if (fgetword(inways,width)) return smoot;
     if (fgetword(inways,height)) return smoot;
     int mapdataP, longnameP, infoP;
//     cprint("get a dword");
     if (fgetdword(inways,mapdataP)) return smoot;
     flags1 = fgetc(inways); // check for EOF?  /******* FIXME *********/
     flags2 = fgetc(inways);
     flags3 = fgetc(inways);
     if (fgetdword(inways,longnameP)) return smoot;
     if (fgetdword(inways,infoP)) return smoot;

     /**** FIXME: */
     if (fgetstringat(inways,mapname,longnameP)) return smoot;
     if (fgetstringat(inways,mapinfo,infoP)) return smoot;
//     cprint("also fseek");
     if (fseek(inways,mapdataP,SEEK_SET)) return smoot;
//VERB     cprint((string)"~0<~bConsole~0> ~eLoading ~B" + mapname + (string)" ~e(~G"+ itos(width) + (string)" ~eby ~G" + itos(height)+ (string)"~e).");
     smoot.dat[0] = (ushort*)malloc(width*height*sizeof(ushort));
     smoot.dat[1] = (ushort*)malloc(width*height*sizeof(ushort));
     smoot.dat[2] = (ushort*)malloc(width*height*sizeof(ushort));
     smoot.clip   = (uchar *)malloc(width*height);
//     cprint("malloc");
     if (!(smoot.dat[0] && smoot.dat[1] && smoot.dat[2] && smoot.clip)) {
freeout: /* sorry */
          free(smoot.dat[0]);
          free(smoot.dat[1]);
          free(smoot.dat[2]);
          free(smoot.clip);
          return smoot;
     }

     for (int layer=0;layer<3;layer++)
{ //    cprint("layer...");
       for (int ctr=0;ctr<(width*height);ctr++) {
          int a,b;
          if (EOF == (a = fgetc(inways))) goto freeout;
          if (EOF == (b = fgetc(inways))) goto freeout;
          smoot.dat[layer][ctr] = (a<<8)|b;
       }}
//     cprint("clip");
       for (int ctr=0;ctr<(width*height);ctr++) {
          int a;
          if (EOF == (a = fgetc(inways))) goto freeout;
          smoot.clip[ctr] = a;
       }
     fclose(inways);
//     cprint("done!");
     smoot.w = width;
     smoot.h = height;

     return smoot;
}

int fgetstringat(FILE * inways, string & super, int loc) {
     if (fseek(inways,loc,SEEK_SET)) return 1;
     super = "";
     int a;
     while (EOF !=(a=fgetc(inways))) {
          if (!a) return 0;
          super+=(char)a;
     } return 1;
}

int fgetdword(FILE * in, int & w) {
     int a; w=0;
     int s=4;
     while (s--) {
     w <<= 8;
     if (EOF == (a=getc(in))) return 1;
//     cprint((string)"Byte: " +itos(a));
     w |= a;
     }
     return 0;
}

int fgetword(FILE * in, int & w) {
     int a; w=0;
     if (EOF == (a=getc(in))) return 1;
     w = a<<8;
     if (EOF == (a=getc(in))) return 1;
     w |= a;
     return 0;
}

int geteightsnul(FILE * in, string & out) {
     out = "";
     uint x=8; int a;
     while(x--) { if (EOF == (a = getc(in))) return 1; else out += (char)a; }
     if (EOF == (a = getc(in))) return 1; else return 0;
}

