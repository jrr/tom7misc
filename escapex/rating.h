#ifndef _ESCAPE_RATING_H
#define _ESCAPE_RATING_H

#include <string>
#include "drawable.h"

struct Player;
struct Level;

struct Rating {
  std::string ToString() const;

  static Rating *FromString(const std::string &s);

  static Rating *Create();

  int difficulty = 0;
  int style = 0;
  int rigidity = 0;
  /* should only be true if same player has solved */
  int cooked = 0;
};


struct RateScreen : public Drawable {
  static RateScreen *Create(Player *p, const Level *l,
                            const std::string &levmd);

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
  virtual void Rate() = 0;

  virtual ~RateScreen() {}

  void Draw() override = 0;
  void ScreenResize() override = 0;

  virtual void SetMessage(const std::string &s) = 0;

  Drawable *below = nullptr;
};


#endif

