#include "prefs.h"

#include "escapex.h"
#include "player.h"
#include "menu.h"
#include "draw.h"
#include "message.h"
#include "chars.h"
#include "../cc-lib/util.h"

#define IND (fon->width)

void Prefs::Show(Player *plr) {

  /* ------- user info -------- */
  /* XXX allow changing of name,
     which should notify the server */

  VSpace spacer((int)(fon->height * 1.5f));

  /* ------- game prefs ------- */
  Label game;
  game.text = PICS BARLEFT BAR BAR BARRIGHT POP
               GREEN " Game Settings " POP
              PICS BARLEFT BAR BAR BARRIGHT POP;

  Toggle askrate;
  askrate.indent = IND;
  askrate.disabled = !plr->webid;
  askrate.question = "Prompt to Rate";
  askrate.checked = GetBool(plr, PREF_ASKRATE);
  askrate.explanation =
    "If checked, then the game will prompt you to rate\n"
    "a level after solving it for the first time.\n"
    GREY "(Default: Checked)";

  Toggle showtut;
  showtut.indent = IND;
  showtut.disabled = false;
  showtut.question = "Show Tutorial";
  showtut.checked = GetBool(plr, PREF_SHOWTUT);
  showtut.explanation =
    "Show the tutorial on the main menu.\n"
    GREY "(Default: Checked)";

  Toggle backup;
  backup.indent = IND;
  backup.disabled = false;
  backup.question = "Backup Player";
  backup.checked = GetBool(plr, PREF_BACKUP_PLAYER);
  backup.explanation =
    "Back up player files every few days. They can be\n"
    "recovered from the player selection screen.\n"
    GREY "(Default: Checked)";

#if 0
  Slider animspeed(0, ANIMATION_MAX_SPEED, 22);
  animspeed.indent = IND;
  animspeed.pos = GetInt(plr, PREF_ANIMATION_SPEED);
  animspeed.question = "Animation Speed";
  animspeed.low = "slower";
  animspeed.high = "faster    off ^^";
  animspeed.explanation =
    "Speed of animation in the game. If set to maximum,\n"
    "animation will be disabled for snappiest possible action.";
#endif

  Toggle animon;
  animon.indent = IND;
  animon.checked = GetBool(plr, PREF_ANIMATION_ENABLED);
  animon.question = "Enable Animation";
  animon.explanation =
    "Draw animations when playing. Recommended unless you are\n"
    "extremely impatient.\n"
    GREY "(Default: Checked)";

  Toggle optsol;
  optsol.indent = IND;
  optsol.checked = GetBool(plr, PREF_OPTIMIZE_SOLUTIONS);
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
  servername.input = GetString(plr, PREF_SERVER);
  servername.explanation =
    "This is the internet name of the server that Escape connects\n"
    "to in order to upgrade, register, and get new levels.\n"
    GREY "(Default: escape.spacebar.org)";

  Toggle altconnect;
  altconnect.indent = IND;
  altconnect.checked = GetBool(plr, PREF_ALTCONNECT);
  altconnect.question = "Alternate Connect";
  altconnect.explanation =
    "If this option is selected, then Escape will connect on an\n"
    "alternate port, which may bypass troublesome proxies.\n"
    GREY "(Default: Unchecked)";

  Toggle debugnet;
  debugnet.indent = IND;
  debugnet.checked = GetBool(plr, PREF_DEBUG_NET);
  debugnet.question = "Debug Network";
  debugnet.explanation =
    "Records debugging information about the networking process\n"
    "in a file. " RED "NOT RECOMMENDED" POP " for normal users.\n"
    GREY "(Default: Unchecked)";

  Okay ok;
  ok.text = "Change Preferences";

  Cancel can;

  vector<MenuItem *> items = {
    &game,
    &askrate,
    &showtut,
    &backup,
    &animon,
    &optsol,

    &spacer,
    &network,
    &servername,
    &altconnect,
    &debugnet,

    &spacer,
    &ok,
    &can,
  };

  std::unique_ptr<Menu> mm =
    Menu::Create(0, "Escape Preferences Menu", items, false);
  InputResultKind res = mm->menuize();
  mm.reset();

  /* XXX check InputResultKind::QUIT */
  if (res == InputResultKind::OK) {
    PutString(plr, PREF_SERVER, servername.input);
    PutBool(plr, PREF_ALTCONNECT, altconnect.checked);
    PutBool(plr, PREF_ASKRATE, askrate.checked);
    PutBool(plr, PREF_SHOWTUT, showtut.checked);
    PutBool(plr, PREF_BACKUP_PLAYER, backup.checked);
    PutBool(plr, PREF_DEBUG_NET, debugnet.checked);
    PutBool(plr, PREF_ANIMATION_ENABLED, animon.checked);
    PutBool(plr, PREF_OPTIMIZE_SOLUTIONS, optsol.checked);

    /* prefs may have changed, so write */
    plr->WriteFile();
  }
}

void Prefs::Defaults(Player *plr) {

  Chunks *ch = plr->GetChunks();

  if (!ch->Get(PREF_SERVER))
    PutString(plr, PREF_SERVER, DEFAULT_SERVER);

  if (!ch->Get(PREF_ALTCONNECT))
    PutBool(plr, PREF_ALTCONNECT, false);

  if (!ch->Get(PREF_ASKRATE))
    PutBool(plr, PREF_ASKRATE, true);

  if (!ch->Get(PREF_SHOWTUT))
    PutBool(plr, PREF_SHOWTUT, true);

  if (!ch->Get(PREF_BACKUP_PLAYER))
    PutBool(plr, PREF_BACKUP_PLAYER, true);

  if (!ch->Get(PREF_DEBUG_NET))
    PutBool(plr, PREF_DEBUG_NET, false);

  if (!ch->Get(PREF_ANIMATION_ENABLED))
    PutBool(plr, PREF_ANIMATION_ENABLED, true);

  if (!ch->Get(PREF_OPTIMIZE_SOLUTIONS))
    PutBool(plr, PREF_OPTIMIZE_SOLUTIONS, true);

}

void Prefs::PutString(Player *plr, uint32 key, string s) {
  plr->GetChunks()->Insert(Chunk(key, s));
}

void Prefs::PutBool(Player *plr, uint32 key, bool b) {
  plr->GetChunks()->Insert(Chunk(key, b));
}

void Prefs::PutInt(Player *plr, uint32 key, int32 i) {
  plr->GetChunks()->Insert(Chunk(key, i));
}

int32 Prefs::GetInt(Player *plr, uint32 k) {
  const Chunk *c = plr->GetChunks()->Get(k);

  if (c && c->type == CT_INT32) {
    return c->i;
  } else {
    printf("c: %p\n", c);
    if (c) printf("c->type %d\n", c->type);
    Message::Bug(0, "int pref unavailable: " RED + Util::itos(k));
    return 0;
  }
}

bool Prefs::GetBool(Player *plr, uint32 k) {
  const Chunk *c = plr->GetChunks()->Get(k);

  if (c && c->type == CT_BOOL) {
    return (bool)c->i;
  } else {
    Message::Bug(0, "bool pref unavailable: " RED + Util::itos(k));
    return false;
  }
}

string Prefs::GetString(Player *plr, uint32 k) {
  const Chunk *c = plr->GetChunks()->Get(k);

  if (c && c->type == CT_STRING) {
    return c->s;
  } else {
    Message::Bug(0, "string pref unavailable: " RED + Util::itos(k));
    return "";
  }
}
