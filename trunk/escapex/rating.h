#ifndef __RATING_H
#define __RATING_H

#include <string>
#include "drawable.h"

struct Player;

/* some slack for later... */
#define RATINGBYTES 6

struct Rating {
  string tostring();
  
  static Rating *FromString(const string &s);

  static Rating *Create();

  int difficulty = 0;
  int style = 0;
  int rigidity = 0;
  /* should only be true if same player has solved */
  int cooked = 0;
};


struct RateScreen : public Drawable {
  static RateScreen *Create(Player *p, Level *l, string levmd);

  /* pops up a menu to rate the level identified
     by md5 string levmd. It's rated by player p,
     and the level ptr is just used to display a
     thumbnail and some information.

     if the player is not registered, shows an error.

     if the player has already rated this level, this
     allows him to edit his old rating.
     
     this call manages the rating in the player's file,
     saving the file if necessary, and updating the
     rating on the internet.

     doesn't free player or level objects.
  */
  virtual void rate() = 0;

  virtual ~RateScreen() {}

  virtual void draw() = 0;
  virtual void screenresize() = 0;

  virtual void setmessage(string) = 0;

  Drawable *below;
};


#endif

