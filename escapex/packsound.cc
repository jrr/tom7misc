#include "escapex.h"
#include "level.h"
#include "../cc-lib/sdl/sdlutil.h"

#include "util.h"
#include "font.h"
#include "extent.h"
#include "draw.h"
#include "pngsave.h"

#include <iostream>
#include <fstream>

/* XXX necessary? */
SDL_Surface * screen;
string self;

int main (int argc, char ** argv) {

  /* change to location of binary, so that we can find the
     images needed. */
  if (argc > 0) {
    string wd = util::pathof(argv[0]);
    util::changedir(wd);

#   if WIN32
    /* on win32, the ".exe" may or may not
       be present. Also, the file may have
       been invoked in any CaSe. */
    self = util::lcase(util::fileof(argv[0]));
    self = util::ensureext(self, ".exe");
#   else
    self = util::fileof(argv[0]);
#   endif

  }

  /* XXX it's not implausible that we could pack the wave files
     into one wave, like with animation. */
  if (argc != 3) {
    printf("\n"
	   "Usage: packsound description.pack basename\n");
    printf("Creates a packed PNG graphic from each of the files in\n"
	   "description.pack. Produces:\n"
	   "   basename_enum.h (enum basename_t)\n"
	   "   basename_decs.h (declares static array of sound data)\n"
	   "   basename_load.h (code that loads the sounds)\n"
	   "\n"
	   "See packsound.cc for details.\n\n");
    /* XXX generate also the list of wave files so that we can
       add them to releasefiles and the installer */
    return 1;
  }

  /* we don't really need any SDL modules */
  if (SDL_Init (SDL_INIT_NOPARACHUTE) < 0) {
    printf("Unable to initialize SDL. (%s)\n", SDL_GetError());
    return 1;
  }


  string descname = argv[1];
  string basename = argv[2];

  string enumname  = basename + "_enum.h";
  string decname   = basename + "_decs.h";
  string loadname  = basename + "_load.h";
  FILE * enums = fopen(enumname.c_str(), "w");
  FILE * decs  = fopen(decname.c_str(), "w");
  FILE * load  = fopen(loadname.c_str(), "w");

  if (! (enums && decs && load)) {
    printf("Can't open output files.\n");
    return -1;
  }

  /* write generated warnings, preludes */
  fprintf(load, 
	  "/* Generated by packpng from %s. DO NOT EDIT */\n",
	  descname.c_str());

  fprintf(enums,
	  "/* Generated by packpng from %s. DO NOT EDIT */\n"
	  "\n"
	  "enum %s_t {\n",
	  descname.c_str(),
	  basename.c_str());

  fprintf(decs,
	  "/* Generated by packpng from %s. DO NOT EDIT */\n",
	  descname.c_str());

  int n = 0;
  {
    string name;
    string file;

    ifstream desc(descname.c_str());
    while (desc >> name >> file) {
      fprintf(enums, "     %s,\n", name.c_str());
      
      /* use 'DIRSEP' instead of / */
      string f = util::replace(file, "/", "\" DIRSEP \"");

      /* XXX specialize to platform */
      fprintf(load,
	      "   %s_data[%d] = Mix_LoadWAV(\"%s\");\n"
	      "   if (!%s_data[%d]) {\n"
	      "      printf(\"Couldn't load sound: '%s'\\n\");\n"
	      "      return;\n"
	      "   }\n"
	      ,
	      basename.c_str(), n, f.c_str(),
	      basename.c_str(), n, f.c_str());

      n++;
    }
  }

  string num_enum = (string)"NUM_" + util::ucase(basename);

  /* postlude for enums */
  fprintf(enums,
	  "     %s,\n"
	  "};\n\n",
	  num_enum.c_str());

  fprintf(decs, "static Mix_Chunk * %s_data[%s] = {0};\n\n",
	  basename.c_str(), 
	  num_enum.c_str());


  fprintf(load, "\n");

  fclose(load);
  fclose(enums);
  fclose(decs);

  return 0;
}
