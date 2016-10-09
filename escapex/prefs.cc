
#include "escapex.h"
#include "player.h"
#include "prefs.h"
#include "menu.h"
#include "draw.h"
#include "message.h"
#include "chars.h"

#define IND (fon->width)

void Prefs::show(Player *plr) {

  /* ------- user info -------- */
  /* XXX allow changing of name,
     which should notify the server */

  VSpace spacer((int)(fon->height * 1.5f));

  /* ------- game prefs ------- */
  Label game;
  game.text = PICS BARLEFT BAR BAR BARRIGHT POP
               GREEN " Game Settings " POP
              PICS BARLEFT BAR BAR BARRIGHT POP ;

  Toggle askrate;
  askrate.indent = IND;
  askrate.disabled = !plr->webid;
  askrate.question = "Prompt to Rate";
  askrate.checked = getbool(plr, PREF_ASKRATE);
  askrate.explanation =
    "If checked, then the game will prompt you to rate\n"
    "a level after solving it for the first time.\n"
    GREY "(Default: Checked)";

  Toggle showtut;
  showtut.indent = IND;
  showtut.disabled = false;
  showtut.question = "Show Tutorial";
  showtut.checked = getbool(plr, PREF_SHOWTUT);
  showtut.explanation =
    "Show the tutorial on the main menu.\n"
    GREY "(Default: Checked)";

  Toggle backup;
  backup.indent = IND;
  backup.disabled = false;
  backup.question = "Backup Player";
  backup.checked = getbool(plr, PREF_BACKUP_PLAYER);
  backup.explanation =
    "Back up player files every few days. They can be\n"
    "recovered from the player selection screen.\n"
    GREY "(Default: Checked)";

#if 0
  Slider animspeed(0, ANIMATION_MAX_SPEED, 22);
  animspeed.indent = IND;
  animspeed.pos = getint(plr, PREF_ANIMATION_SPEED);
  animspeed.question = "Animation Speed";
  animspeed.low = "slower";
  animspeed.high = "faster    off ^^";
  animspeed.explanation =
    "Speed of animation in the game. If set to maximum,\n"
    "animation will be disabled for snappiest possible action.";
#endif

  Toggle animon;
  animon.indent = IND;
  animon.checked = getbool(plr, PREF_ANIMATION_ENABLED);
  animon.question = "Enable Animation";
  animon.explanation =
    "Draw animations when playing. Recommended unless you are\n"
    "extremely impatient.\n"
    GREY "(Default: Checked)";

  Toggle optsol;
  optsol.indent = IND;
  optsol.checked = getbool(plr, PREF_OPTIMIZE_SOLUTIONS);
  optsol.question = "Optimize Solutions";
  optsol.explanation =
    "After you solve a level, the game will automatically try\n"
    "to optimize your solution to make it shorter.\n"
    GREY "(Default: Checked)";

  /* ------- network ---------- */
  Label network;
  network.text =
    PICS BARLEFT BAR BAR BARRIGHT POP
       GREEN " Network Settings " POP
    PICS BARLEFT BAR BAR BARRIGHT POP;

  TextInput servername;
  servername.indent = IND;
  servername.question = "Server:";
  servername.input = getstring(plr, PREF_SERVER);
  servername.explanation =
    "This is the internet name of the server that Escape connects\n"
    "to in order to upgrade, register, and get new levels.\n"
    GREY "(Default: escape.spacebar.org)";

  Toggle altconnect;
  altconnect.indent = IND;
  altconnect.checked = getbool(plr, PREF_ALTCONNECT);
  altconnect.question = "Alternate Connect";
  altconnect.explanation =
    "If this option is selected, then Escape will connect on an\n"
    "alternate port, which may bypass troublesome proxies.\n"
    GREY "(Default: Unchecked)";

  Toggle debugnet;
  debugnet.indent = IND;
  debugnet.checked = getbool(plr, PREF_DEBUG_NET);
  debugnet.question = "Debug Network";
  debugnet.explanation =
    "Records debugging information about the networking process\n"
    "in a file. " RED "NOT RECOMMENDED" POP " for normal users.\n"
    GREY "(Default: Unchecked)";

  Okay ok;
  ok.text = "Change Preferences";

  Cancel can;

  PtrList<MenuItem> *l = nullptr;

  PtrList<MenuItem>::push(l, &can);
  PtrList<MenuItem>::push(l, &ok);
  PtrList<MenuItem>::push(l, &spacer);

  PtrList<MenuItem>::push(l, &debugnet);
  PtrList<MenuItem>::push(l, &altconnect);
  PtrList<MenuItem>::push(l, &servername);
  PtrList<MenuItem>::push(l, &network);
  PtrList<MenuItem>::push(l, &spacer);

  PtrList<MenuItem>::push(l, &optsol);
  PtrList<MenuItem>::push(l, &animon);
  PtrList<MenuItem>::push(l, &backup);
  PtrList<MenuItem>::push(l, &showtut);
  PtrList<MenuItem>::push(l, &askrate);
  PtrList<MenuItem>::push(l, &game);


  std::unique_ptr<Menu> mm = Menu::Create(0, "Escape Preferences Menu", l, false);

  InputResultKind res = mm->menuize();

  PtrList<MenuItem>::diminish(l);
  mm.reset();

  /* XXX check InputResultKind::QUIT */
  if (res == InputResultKind::OK) {
    putstring(plr, PREF_SERVER, servername.input);
    putbool(plr, PREF_ALTCONNECT, altconnect.checked);
    putbool(plr, PREF_ASKRATE, askrate.checked);
    putbool(plr, PREF_SHOWTUT, showtut.checked);
    putbool(plr, PREF_BACKUP_PLAYER, backup.checked);
    putbool(plr, PREF_DEBUG_NET, debugnet.checked);
    putbool(plr, PREF_ANIMATION_ENABLED, animon.checked);
    putbool(plr, PREF_OPTIMIZE_SOLUTIONS, optsol.checked);

    /* prefs may have changed, so write */
    plr->WriteFile();
  }
}

void Prefs::defaults(Player *plr) {

  Chunks *ch = plr->GetChunks();

  if (!ch->Get(PREF_SERVER))
    putstring(plr, PREF_SERVER, DEFAULT_SERVER);

  if (!ch->Get(PREF_ALTCONNECT))
    putbool(plr, PREF_ALTCONNECT, false);

  if (!ch->Get(PREF_ASKRATE))
    putbool(plr, PREF_ASKRATE, true);

  if (!ch->Get(PREF_SHOWTUT))
    putbool(plr, PREF_SHOWTUT, true);

  if (!ch->Get(PREF_BACKUP_PLAYER))
    putbool(plr, PREF_BACKUP_PLAYER, true);

  if (!ch->Get(PREF_DEBUG_NET))
    putbool(plr, PREF_DEBUG_NET, false);

  if (!ch->Get(PREF_ANIMATION_ENABLED))
    putbool(plr, PREF_ANIMATION_ENABLED, true);

  if (!ch->Get(PREF_OPTIMIZE_SOLUTIONS))
    putbool(plr, PREF_OPTIMIZE_SOLUTIONS, true);

}

void Prefs::putstring(Player *plr, uint32 key, string s) {
  plr->GetChunks()->Insert(new Chunk(key, s));
}

void Prefs::putbool(Player *plr, uint32 key, bool b) {
  plr->GetChunks()->Insert(new Chunk(key, b));
}

void Prefs::putint(Player *plr, uint32 key, int32 i) {
  plr->GetChunks()->Insert(new Chunk(key, i));
}

int32 Prefs::getint(Player *plr, uint32 k) {
  Chunk *c = plr->GetChunks()->Get(k);

  if (c && c->type == CT_INT32) {
    return c->i;
  } else {
    printf("c: %p\n", c);
    if (c) printf("c->type %d\n", c->type);
    Message::Bug(0, "int pref unavailable: " RED + itos(k));
    return 0;
  }
}

bool Prefs::getbool(Player *plr, uint32 k) {
  Chunk *c = plr->GetChunks()->Get(k);

  if (c && c->type == CT_BOOL) {
    return (bool)c->i;
  } else {
    Message::Bug(0, "bool pref unavailable: " RED + itos(k));
    return false;
  }
}

string Prefs::getstring(Player *plr, uint32 k) {
  Chunk *c = plr->GetChunks()->Get(k);

  if (c && c->type == CT_STRING) {
    return c->s;
  } else {
    Message::Bug(0, "string pref unavailable: " RED + itos(k));
    return "";
  }
}
