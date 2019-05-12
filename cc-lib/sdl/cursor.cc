
#include "cursor.h"

#include "SDL.h"
// #include "base/logging.h"

#include <vector>

using namespace std;


SDL_Cursor *Cursor::MakeCursor(int width, int height, int hot_x, int hot_y,
			       const char *ascii_img) {
  if (width % 8) return nullptr;
  std::vector<Uint8> mask(width * height / 8, 0);
  std::vector<Uint8> data(width * height / 8, 0);

  for (int i = 0; i < (width * height) / 8; i++) {
    Uint8 db = 0, mb = 0;
    for (int b = 0; b < 8; b++) {
      db <<= 1;
      mb <<= 1;
      const int c = ascii_img[i * 8 + b];
      switch (c) {
        case '#':
          db |= 0b1;
          mb |= 0b1;
          break;
        case '-':
          mb |= 0b1;
          break;
        case ' ':
          break;
      }
    }
    data[i] = db;
    mask[i] = mb;
  }
  
  return SDL_CreateCursor(data.data(), mask.data(), width, height,
			  hot_x, hot_y);
}

SDL_Cursor *Cursor::MakeArrow() {
  return MakeCursor(
      32, 32, 0, 0,
      "#                               "
      "##                              "
      "#-#                             "
      "#--#                            "
      "#---#                           "
      "#----#                          "
      "#-----#                         "
      "#------#                        "
      "#-------#                       "
      "#--------#                      "
      "#-----#####                     "
      "#--#--#                         "
      "#-# #--#                        "
      "##  #--#                        "
      "#    #--#                       "
      "     #--#                       "
      "      #--#                      "
      "      #--#                      "
      "       ##                       "
      "                                "
      "                                "
      "                                "
      "                                "
      "                                "
      "                                "
      "                                "
      "                                "
      "                                "
      "                                "
      "                                "
      "                                "
      "                                ");
}

SDL_Cursor *Cursor::MakeBucket() {
  return MakeCursor(
      32, 32, 2, 15,
      "        ######                  "
      "       ##    ##                 "
      "       ##     ##                "
      "      ######  ##                "
      "    #####---####                "
      "  #######----###                "
      " #####-##-----##                "
      "#####--###-----##               "
      "####--##-##-----##              "
      "###----###-------##             "
      "##----------------##            "
      "####---------------#            "
      "### #-------------#             "
      "###  #-----------#              "
      "###   #---------#               "
      " ##    #-------#                "
      "  #     #-----#                 "
      "         #---#                  "
      "          #-#                   "
      "           #                    "
      "                                "
      "                                "
      "                                "
      "                                "
      "                                "
      "                                "
      "                                "
      "                                "
      "                                "
      "                                "
      "                                "
      "                                ");
}
