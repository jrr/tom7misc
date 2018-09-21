
#include "game-database.h"
#include "../cc-lib/base/logging.h"

GameDB::GameDB() {
  // Assumes cart is called name.nes, movie name.fm7.
  auto Insert1P =
    [this](const string &name,
	   int xloc, int yloc, int lives) -> Game * {
      Game g;
      g.romfile = name + ".nes";
      g.moviefile = name + ".fm7";
      g.is_two_player = false;
      g.p1.xloc = xloc;
      g.p1.yloc = yloc;
      g.p1.lives = lives;
      auto ins = db_.emplace(name, std::move(g));
      CHECK(ins.second) << name;
      return &ins.first->second;
    };

  // Assumes name2p.fm7.
  auto Insert2P =
    [this](const string &name,
	   Game::Player p1, Game::Player p2) -> Game * {
      Game g;
      g.romfile = name + ".nes";
      g.moviefile = name + "2p.fm7";
      g.is_two_player = true;
      g.p1 = p1;
      g.p2 = p2;

      auto ins = db_.emplace(name, std::move(g));
      CHECK(ins.second) << name;
      return &ins.first->second;
    };
  
  // 423 comes from xloc, 425 found by guessing (!) since
  // the other sprite pairs were separated by two bytes. Seems
  // to be a lot of sprite pairs in ram, so maybe just need
  // to have a higher limit here.
  {
    // There are not "lives", but there are continue credits.
    // Should these count?
    Game *sword = Insert1P("swordmaster", 0x423, 0x425, -1);
    // Modyfing this reflects it immediately.
    sword->p1.health = 0x042f;
    // Health displays as 1+value.
    sword->allow_zero_health = OBool::TRUE;

    // No visible timer.
  }

  // With 0x04fb,0x04f9, warping has effect (within some small
  // region at least), but y location is not on the player. So
  // I think this does not qualify as working. x looks good.
  // TODO: Search for a working y, or is there something weird
  // with scroll?
  {
    // No lives in this game, at least as far as I got...
    Game *fester = Insert1P("festersquest", 0x04fb, -1, -1);
    // Autolives finds it. Modifying works.
    fester->p1.health = 0x4e9;
    fester->allow_zero_health = OBool::FALSE;
  }

  // Tracks player sprite, but no warping.
  //
  // 0x031 (xloc) does seem to control some aspect
  // of the player position (allowing some warping),
  // but this game is pretty weird (isometric).
  {
    // Found in autolives. Modifying works.
    Game *solstice = Insert1P("solstice", 0x728, 0x72c, 0x789);
    solstice->allow_zero_lives = OBool::TRUE;
  }

  // Verified, with warping. Found manually...
  // There are lots of high-scoring linkages, so maybe
  // we just need to search deeper? 0x788 is the top
  // xloc.
  {
    // Lives found in FCEUX. Modifying works.
    Game *athena = Insert1P("athena", 0x788, 0x75a, 0x009b);
    athena->p1.health = 0x0095;
    // Zero health?
    athena->allow_zero_lives = OBool::TRUE;

    // Timer is multibyte BCD: 0x91, 0x92 = 0M:SS
  }
    
  // 0x04d9,0x051b works but no warping. there may be
  // better pairs in there; didn't check a lot.
  {
    // No "lives" in this game.
    Game *die = Insert1P("diehard", 0x04d9, 0x051b, -1);
    // Found in FCEUX. Modifying works.
    die->p1.health = 0x00ac;
    die->allow_zero_health = OBool::FALSE;
  }

  // Verified. Warping works.
  {
    Game *gradius = Insert1P("gradius", 0x360, 0x320, 0x0020);
    gradius->allow_zero_lives = OBool::TRUE;
  }

  // Verified. Warping works.
  {
    // Lives found by autolives. Modifying works.
    Game *kage = Insert1P("kage", 0x053, 0x054, 0x002b);
    kage->allow_zero_lives = OBool::TRUE;
    // Note: It is possible to get something a little like health in
    // this game (you change color).
  }

  {
    // Game does not have "lives."
    Game *were = Insert1P("werewolf", -1, -1, -1);
    // Found in FCEUX, also autolives. Modifying works.
    were->p1.health = 0x00BC;
    were->allow_zero_health = OBool::FALSE;

    // Death when it reaches 0. Unsigned.
    were->timer = 0x00BF;
  }

  // Verified. Warping just moves the feet, but the head will
  // reattach when switching rooms. The head is also
  // separately warpable (0x0707,0x0704), but doesn't seem to
  // be the "master" location.
  {
    // There are no "lives" in this game.
    Game *dead = Insert1P("deadlytowers", 0x0703, 0x0700, -1);
    // Found in FCEUX. Setting to 0 instantly kills you.
    // Seems like autolives should be able to find this, but
    // I guess typical per-frame damage is too high.
    dead->p1.health = 0x009a;
    dead->allow_zero_health = OBool::FALSE;
  }

  // Verified positions.
  {
    // Modifying lives works, although it's treated as signed,
    // so a value like 0x90 is game over on the next death.
    Game *mario = Insert1P("mario", 0x86, 0xCE, 0x075a);
    mario->allow_zero_lives = OBool::TRUE;

    // Timer is a multibyte quantity in BCD:
    // 0x7F8, 0x7F9, 0x7FA = 0S 0S 0S
  }

  // From wiki.
  {
    Game *mega = Insert1P("megaman2", 0x460, 0x4A0, 0x00A8);
    mega->p1.health = 0x06c0;
    // can set this to zero and it is reflected in the UI
    // with zero bars, but if health drops to zero normally,
    // you die.
    mega->allow_zero_health = OBool::FALSE;
    mega->allow_zero_lives = OBool::FALSE;
  }
  
  // From wiki.
  // Autocamera works but is VERY SLOW.
  {
    // Autocamera finds this (among others); modifying works.
    Game *lolo = Insert1P("lolo", 0x6D, 0x6F, 0x0057);
    lolo->allow_zero_lives = OBool::FALSE;
    // No visible timer.
  }
  
  // Has horizontal and vertical scrolling.
  // Verified. (wiki says 0x51,0x52 which is not right?)
  {
    // No "lives" in this game.
    Insert1P("metroid", 0x30E, 0x30D, -1);
    // Health is stored in two different bytes using a weird
    // BCD-like encoding. What's displayed on screen as 83
    // is stored at 0x106 and 0x107 as: 30 08 (?).
    // Would make sense to protect both of these values, but
    // weird to think of either one alone as "health." :/
  }

  // From glEnd.
  {
    // No "lives" in this game.
    Insert1P("zelda", 0x70, 0x84, -1);
    // Health is complicated. The low nybble is the current number of
    // full hearts. High nibble holds the number of heart containers.
    // 670 holds half-hearts (0-ff gamut, weirdly).
    // Seems wrong to consider this "health" in the normal sense,
    // without some masking. Autolives does find this.
    // zelda->p1.health = 0x066F;

    // No visible timer.
  }

  // Verified. Warping definitely moves the sprite, though
  // it also causes physics to get desynced.
  {
    // No "lives" in this game?
    Game *rocket = Insert1P("rocketeer", 0x40c, 0x419, -1);
    // Found in fceux. Modifying works and is reflected immediately.
    // Set to 0x7e for lulz
    rocket->p1.health = 0x5c5;
    rocket->allow_zero_health = OBool::TRUE;
  }

  // Verified. Screen coordinates. Warping works great!
  {
    // Autolives finds 0x5f for lives, which is like "bombs left."
    // Good counter-example, since we lose control when it goes
    // to zero, but this is clearly good.
    // Lives found in FCEUX. Modifying works.
    Game *gyro = Insert1P("gyromite", 0x609, 0x608, 0x0039);
    gyro->allow_zero_lives = OBool::FALSE;

    // Timer is multibyte BCD, 0x5E 0x5D = 0S SS
  }
    
  // Has horizontal and vertical scrolling.
  // Verified. Prescroll coordinates. Warping can cause
  // glitches/locks.
  {
    // Lives found in FCEUX. Modifying these works and takes
    // effect immediately.
    Game *little = Insert1P("littlemermaid", 0x330, 0x360, 0xB0);
    // Autolives finds this.
    little->p1.health = 0xB1;
    little->allow_zero_health = OBool::FALSE;
    little->allow_zero_lives = OBool::TRUE;
    // No visible timer.
  }

  // Verified. Warping works great. This game does split
  // scrolling for a bottom menu, so it always appears to be
  // (close to) 0,0.
  {
    // Found in FCEUX. Modifying works.
    Game *back = Insert1P("backtothefuture", 0x3a2, 0x3a7, 0x0393);
    back->allow_zero_lives = OBool::FALSE;
    // In this game there's the photograph which fades when you don't
    // collect clocks. This could be considered "health." But there
    // doesn't seem to be a single simple memory location associated
    // with it.
  }

  // Verified. Warping works.
  {
    // Lives found in FCEUX. Modifying works.
    Game *adv = Insert1P("adventureisland", 0x584, 0x5d3, 0x003f);
    // Autolives finds this. Modifying works. In this game it
    // decrements on its own, so it's kind of like a timer.
    adv->p1.health = 0x0076;
    // Displays as one tick mark.
    adv->allow_zero_health = OBool::TRUE;
    // Death with 0x3f=1 is game over.
    // Weirdly, if you have 3f=0 when you die, it's fine.
    adv->allow_zero_lives = OBool::FALSE;

    // No visible timer, though life does count down on its own
    // if you're not moving (or something?)
  }
    
  // Verified. Warping works well.
  {
    // No "lives" in this game. (Well there is stuff like
    // feathers, water/barrel..)
    Game *kid = Insert1P("kidicarus", 0x723, 0x720, -1);
    kid->allow_zero_health = OBool::FALSE;
  }

  // Verified. Warping works well. I think it has a sprite
  // to do split-scrolling like mario (0x203,0x200).
  {
    // Lives found in FCEUX. Modying lives works (takes effect immediately).
    Game *ninja = Insert1P("ninjagaiden", 0x086, 0x08a, 0x0076);
    // Modifying this works. Can set to 0, but you die if it goes
    // to zero organically.
    ninja->p1.health = 0x0065;
    ninja->allow_zero_health = OBool::FALSE;
    ninja->allow_zero_lives = OBool::TRUE;

    ninja->timer = 0x0063;
    // Autolives detects 0x06a4 as a timer sometimes; it appears
    // to be part of the music playback (synchronized with some drum
    // beats).
  }

  // Found in FCEUX. Warping works.
  {
    // Lives found with FCEUX. Modifying works.
    Game *bad = Insert1P("baddudes", 0x2a6, 0x2a4, 0x2be);
    bad->allow_zero_lives = OBool::FALSE;
    // 0x0204 appears to be the game timer.
    // Found by autolives. Modifying works.
    bad->p1.health = 0x02ab;
    bad->allow_zero_health = OBool::FALSE;

    bad->timer = 0x0204;
  }

  // Found manually in FCEUX. Warping works.
  {
    // There are continues in this game, but not "lives"?
    Game *jackie = Insert1P("jackiechan", 0x610, 0x620, -1);
    jackie->allow_zero_health = OBool::FALSE;
    // Autolives finds. Modifying works.
    jackie->p1.health = 0x702;
  }

  // 0x370,0x330 is the cue ball. Lots of balls detected, but
  // not the cursor? Would not be surprised if some weird polar
  // representation is used for this game.
  {
    // Found in fceux. Modifying works.
    Game *lunar = Insert1P("lunarpool", -1, -1, 0x01C3);
    // Found by autolives. Shot counter is like lives;
    // after three without sinking a ball, you lose a ball.
    lunar->p1.health = 0x01c5;
    lunar->allow_zero_health = OBool::FALSE;
    lunar->allow_zero_lives = OBool::TRUE;
  }
    

  // Doesn't work... or every sprite has its own memory loc
  // and the right address is too deep?
  // Looks like while there are many sprites associated with
  // the player, the master location is stored as coarse/fine
  // (just guessing)... 0x028 is the x game-time number.
  // Lives found in FCEUX. Modifying works.
  {
    Game *bomb = Insert1P("bomberman", -1, -1, 0x0068);
    bomb->allow_zero_lives = OBool::TRUE;

    bomb->timer = 0x0093;
  }

  // Several x addresses for sprites:
  // 0x243, 247, 24b, 343, 347, 34b
  //
  // 0x617 seems to be the player's x tile coordinate
  // This one is pretty tricky because the player's y coordinate is
  // basically pinned to the center of the screen, except like when
  // you fall onto spikes
  {
    // Lives found with FCEUX, allows modification.
    Game *cliff = Insert1P("cliffhanger", -1, -1, 0x0406);
    // Good, allows modification. Found by autolives.
    cliff->p1.health = 0x0405;
    cliff->allow_zero_health = OBool::TRUE;
    cliff->allow_zero_lives = OBool::TRUE;
  }
    
  // Has split x-scrolling, but also appears to use some
  // mapper tricks to do vertical scrolling?
  // Despite these having weirdly distant locations, it
  // does check out, and warping works.
  {
    // 361 is good (can modify it), but it also gets copied
    // into 0x219 after a few frames.
    Game *duck = Insert1P("ducktales", 0x720, 0x640, 0x361);
    duck->allow_zero_lives = OBool::TRUE;
    // There is also health in this game, but it's not stored
    // in an obvious way (e.g. with value "3" when you have
    // 3 health).
  }

  // Doesn't work. has split scrolling. Difficult game.
  Insert1P("gauntlet2", -1, -1, -1);

  // autocamera finds this now. verified with warping.
  {
    // No 'lives' in this game.
    Game *strider = Insert1P("strider", 0x508, 0x50b, -1);
    // Autolives finds this. Modifying works.
    strider->p1.health = 0x0569;
    strider->allow_zero_health = OBool::FALSE;
  }

  // Verified. xlocs finds 0x09e, which is good. The y
  // coordinate's value weirdly remaind like 24 pixels above
  // the player, but warping works great, so I guess it's just
  // a weird one...
  {
    // No "lives" in this game.
    Game *fax = Insert1P("faxanadu", 0x09e, 0x0a1, -1);
    // Found in FCEUX. Modifying works. Copied to 0x399.
    fax->p1.health = 0x0431;
    fax->allow_zero_health = OBool::TRUE;
  }

  using Player = Game::Player;
  
  // Verified.
  {
    // Both 1p and 2p find 0xFF as the highest-scoring value;
    // this just hard-locks the game if set to 1.
    Game *contra = 
      Insert2P("contra",
	       Player{0x334, 0x31A, 0x0032},
	       Player{0x335, 0x31B, 0x0033});
    (void)contra;
    // XXX set allowed-zero to the correct value
  }

  // Found in FCEUX and searching. These work with warping
  // too. But y is the y coord negated! (Maybe it's using
  // cartesian axes?) We don't expect autocamera to find this,
  // but it appears to be the best answer for this game.
  // 
  // 1p: 0x08e, 0x09e (negated)
  // 2p: 0x084, 0x09f (negated)
  {
    // No "lives"
    Game *rivercity = Insert2P("rivercity",
			       Player{0x08e, 0x09e, -1},
			       Player{0x084, 0x09f, -1});
    rivercity->p1.health = 0x04bf;
    rivercity->p2.health = 0x04c0;
    // Another weird thing: In this game you can have zero health,
    // but you don't die unless you are knocked down.
    rivercity->allow_zero_health = OBool::TRUE;
  }
  
  // 2p. Warping works great. Lots of enemy sprite locations are
  // detected too.
  {
    // Lives found by autolives; modifying works.
    // Weirdly it also finds 0x401, which is the current level.
    // Indeed making it 0 starts the intro movie, but why would
    // it ever be decremented in normal play?
    Game *bubble =
      Insert2P("bubblebobble",
	       Player{0x203, 0x200, 0x002e},
	       Player{0x20b, 0x208, 0x0042});
    bubble->allow_zero_lives = OBool::FALSE;

    // Counts down to zero, then "Hurry!" and the monster comes.
    // Not sure if this really fits the model of timer, though,
    // since it doesn't kill you.
    // bubble->timer = 0x40D;
  }

  // 2P. There are lots of locations that track the components
  // of the monster sprites. But these two mostly allow
  // warping.
  {
    // No "lives" in this game I think.
    Game *rampage =
      Insert2P("rampage",
	       Player{0x102, 0x103, -1},
	       Player{0x12e, 0x12f, -1});
    rampage->p1.health = 0x012b;
    rampage->p2.health = 0x0157;
    // Setting to zero health immediately kills the player.
    rampage->allow_zero_health = OBool::FALSE;
  }
}

vector<Game> GameDB::GetAll() const {
  vector<Game> res;
  res.reserve(db_.size());
  for (const auto &p : db_) res.push_back(p.second);
  return res;
}

vector<Game> GameDB::GetMatching(const std::vector<string> &vec) const {
  vector<Game> res;
  res.reserve(vec.size());
  for (const string &s : vec) {
    auto it = db_.find(s);
    CHECK(it != db_.end()) << "Could not find " << s << " in database.";
    res.push_back(it->second);
  }
  return res;
}
