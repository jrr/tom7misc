
/* XXX This is deprecated (in favor of the bookmarks menu) 
   and should be deleted. */

#include "smanage.h"
#include "extent.h"
#include "message.h"
#include "prompt.h"
#include "textscroll.h"
#include "client.h"
#include "http.h"
#include "../cc-lib/md5.h"
#include "../cc-lib/base64.h"
#include "menu.h"
#include "optimize.h"
#include "play.h"

/* XXX getrgb */
#define VCRSELCOLOR 0xFF224422

typedef PtrList<NamedSolution> nslist;

/* entries in the selector */
struct nsentry {
  
  /* stays at the top */
  bool def;
  NamedSolution ns;

  bool verifies;

  static int height() { return TILEH + 8; }
  void draw(int x, int y, bool sel) {
    int yy = ((TILEH + 8) - fon->height) >> 1;

    string color = sel?YELLOW:WHITE;

    /* XXX add date */
    /* PERF precompute */
    string s;
    
    if (ns.bookmark) s = PICS BOOKMARKPIC POP " ";
    else s = PICS THUMBICON POP "  ";
    
    s += 
      color + font::pad(ns.name, 32) + POP
      + font::pad("(" GREEN + itos(ns.sol->length), -6) + POP + " moves)"
      "  ";

    if (ns.bookmark) {
      s += GREY "(bookmark)" POP;
    } else {
      s += GREY "by " POP + font::pad(ns.author, 16);
    }

    fon->draw(x, yy + y, s);

    if (!verifies && !ns.bookmark) {
      int x1 = x - 4;
      int x2 = x + 8 + fon->sizex(s);
      int y1 = yy + y + (fon->height >> 1);
      int y2 = y1;

      /* XXX better with some alpha.. */
      sdlutil::drawline(screen, x1, y1 - 1, x2, y2 - 1, 190, 0, 0);
      sdlutil::drawline(screen, x1, y1 - 0, x2, y2 - 0, 190, 0, 0);
      // sdlutil::drawline(screen, x1, y1 + 1, x2, y2 + 1, 190, 0, 0);
    }
  }

  /* we're not really "selecting" anything here. */
  int convert() { return 0; }

  bool matches(char k) {
    return ns.name.length() > 0 && (ns.name[0]|32) == (k|32);
  }

  static int none() { return 0; }

  static void swap(nsentry * l, nsentry * r) {
#   define SWAP(f) { const auto f ## _tmp = l->f; l->f = r->f; r->f = f ## _tmp; }
    SWAP(ns);
    SWAP(def);
    SWAP(verifies);
#   undef SWAP
  }
  
  static int cmp_default(const nsentry & l,
			 const nsentry & r) {
    if (l.def && !r.def) return -1;
    if (r.def && !l.def) return 1;

    if (l.ns.sol->length < r.ns.sol->length) return -1;
    else if (l.ns.sol->length == r.ns.sol->length) {
      if (l.ns.name < r.ns.name) return -1;
      else if (l.ns.name == r.ns.name) {
	if (l.ns.author < r.ns.author) return -1;
	else if (l.ns.author == r.ns.author) return 0;
	else return 1;
      } else return 1;
    } else return 1;
  }

};


typedef Selector<nsentry, int> nsel;

struct smreal : public Drawable {

  virtual ~smreal() {}
  void destroy() {
    sel->destroy();
    delete this;
  }

  /* Drawable */
  virtual void draw() {
    sdlutil::clearsurface(screen, BGCOLOR);
    /* XXX should drawsmall like load.
       need to move that stuff to some separate class
       so I can reuse it (also when rating, commenting) */
  }

  void resort() {
    sel->sort(nsentry::cmp_default);
  }

  virtual void screenresize() {}

  static smreal * create(Player *p, string lmd5, Level *lev) {
    /* get the solution set */

    nslist * solset = p->solutionset(lmd5);
    
    /* there are no solutions to manage! */
    if (!solset) return 0;


    smreal * sm = new smreal;
    if (!sm) return 0;
    int len = solset->length();
    
    sm->sel = nsel::create(len);

    sm->sel->title =
      "                                "
      "Managing solutions. " POP "\n"
      "                                "
      PICS BARLEFT BAR BAR BAR BAR BAR BAR BAR BAR BAR BAR 
      BAR BAR BAR BAR BAR BAR BAR
      BARRIGHT POP "\n"
      BLUE "enter" POP " to watch solution. "
      BLUE "del" POP " to delete. "
      BLUE "ctrl-r" POP " to rename. "
      BLUE "ctrl-i" POP " makes default. "
      "\n"
      BLUE "ctrl-o" POP " optimizes a solution.";
      
    
    if (p->webid)
      sm->sel->title += "\n"
      BLUE "ctrl-u" POP " to upload to server. "
      BLUE "ctrl-d" POP " to get new solutions from the server. "
      ;


    /* now initialize the nsentries  */
    for (int i = 0; i < len; i++) {
      Solution *s = solset->head->sol->clone();
      sm->sel->items[i].def = (i == 0);
      sm->sel->items[i].ns.name = solset->head->name;
      sm->sel->items[i].ns.author = solset->head->author;
      sm->sel->items[i].ns.date = solset->head->date;
      sm->sel->items[i].ns.bookmark = solset->head->bookmark;
      sm->sel->items[i].ns.sol = s;
      sm->sel->items[i].verifies = Level::verify(lev, s);

      solset = solset->next;
    }

    sm->resort();

    return sm;
  }

  nsel * sel;

  static void downloadsolutions(Player *plr, smreal * sm, string lmd5, Level *lev);

};

void smanage::promptupload(Drawable *below,
			   Player *plr, string lmd5, 
			   Solution *sol, string msg,
			   string name_,
			   bool speedrec) {

  string s;
  Client::quick_txdraw td;

  label message;
  message.text = msg;

  int IND = 2;

  textinput name;
  name.indent = IND;
  name.question = "Name:";
  name.input = name_;
  name.disabled = speedrec;
  name.explanation =
    "Name this solution, briefly.";
  
  toggle speed;
  speed.indent = IND;
  speed.disabled = speedrec;
  speed.checked = true;
  speed.question = "Speed Record Only";
  speed.explanation =
    "Upload this solution as a speed record entry only.\n"
    "It will be deleted if someone beats it.";

  textbox desc(42, 7);
  desc.indent = IND;
  desc.question = "Description. " GREY "(optional)" POP;
  desc.explanation =
    speedrec?
    "If you want, you can describe your solution here.\n"
    "It's fine to upload a speed record without comment.":
    "Describe your solution here. This is inserted as a comment\n"
    "with the level. This is optional, but if the solution isn't\n"
    "interesting somehow, why are you uploading it?\n";

  vspace spacer((int)(fon->height * 1.5f));
  vspace spacer2((int)(fon->height * 1.5f));


  okay ok;
  ok.text = "Upload Solution";

  cancel can;
  can.text = "Cancel";
  
  PtrList<MenuItem> * l = 0;

  PtrList<MenuItem>::push(l, &can);
  PtrList<MenuItem>::push(l, &ok);
  PtrList<MenuItem>::push(l, &spacer);

  PtrList<MenuItem>::push(l, &speed);
  PtrList<MenuItem>::push(l, &desc);
  PtrList<MenuItem>::push(l, &name);
  PtrList<MenuItem>::push(l, &spacer2);
  PtrList<MenuItem>::push(l, &message);

  /* display menu */
  menu * mm = menu::create(below, "Upload solution?", l, false);
  resultkind res = mm->menuize();
  PtrList<MenuItem>::diminish(l);
  mm->destroy();

  if (res == MR_OK) {

    if (speed.checked ||
	Message::quick(0, "Are you sure you want to upload\n"
		       "   this solution without marking\n"
		       "   it as a " BLUE "speedrun" POP "?\n"
		       "\n"
		       "   You should only do this if the\n"
		       "   solution is interesting somehow.\n"
		       "\n",

		       "Upload anyway",
		       "Cancel")) {

      HTTP * hh = Client::connect(plr, td.tx, &td);
    
      if (!hh) { 
	Message::no(&td, "Couldn't connect!");
	return;
      }
    
      Extent<HTTP> eh(hh);

      string res;

      string solcont = sol->tostring();
    
      formalist * fl = 0;
    
      /* XXX seems necessary! but in aphasia cgi? */
      formalist::pusharg(fl, "dummy", "dummy");
      formalist::pusharg(fl, "id", itos(plr->webid));
      formalist::pusharg(fl, "seql", itos(plr->webseql));
      formalist::pusharg(fl, "seqh", itos(plr->webseqh));
      formalist::pusharg(fl, "md", MD5::Ascii(lmd5));
      formalist::pusharg(fl, "name", name.input);
      formalist::pusharg(fl, "speedonly", speed.checked?"1":"0");
      formalist::pusharg(fl, "desc", desc.get_text());
      formalist::pushfile(fl, "sol", "sol.esx", solcont);

      td.say("Uploading..");
      td.draw();


      string out;
      if (Client::rpcput(hh, UPLOADSOL_RPC, fl, out)) {
	if (speedrec)
	  Message::quick(&td, GREEN "Success! the record is yours!" POP,
			 "OK", "", PICS THUMBICON POP);
	else
	  Message::quick(&td, GREEN "Success!" POP,
			 "OK", "", PICS THUMBICON POP);
      } else {
	Message::no(&td, RED "Upload failed: " + 
		    out + POP);
      }

      formalist::diminish(fl);    
    }
  }

}

/* XXX this should be documented in protocol.txt */
void smreal::downloadsolutions(Player *plr, smreal * sm, string lmd5, Level *lev) {
  string s;
  Client::quick_txdraw td;
  
  HTTP * hh = Client::connect(plr, td.tx, &td);

  if (!hh) { 
    Message::no(&td, "Couldn't connect!");
    return;
  }

  Extent<HTTP> eh(hh);
  /* XXX register callback.. */


  httpresult hr = hh->get(ALLSOLS_URL + MD5::Ascii(lmd5), s);
  if (hr == HT_OK) {
    /* parse result. see protocol.txt */
    int nsols = util::stoi(util::getline(s));

    td.say("OK. Solutions on server: " GREEN + itos(nsols) + POP);

    /* get them! */
    for (int i = 0; i < nsols; i++) {
      string line1 = util::getline(s);
      string author = util::getline(s);
      string moves = Base64::Decode(util::getline(s));
      
      /* this is the solution id, which we don't need */
      (void) util::stoi(util::chop(line1));
      int date = util::stoi(util::chop(line1));
      string name = util::losewhitel(line1);

      Solution *s = Solution::fromstring(moves);
      if (!s) {
	Message::no(&td, "Bad solution on server!");
	return;
      }

      Extent<Solution> es(s);

      /* now check if we've got it */
      /* PERF each of the following things is O(n)
	 with bad constants, but we don't expect 
	 many solutions */
      for (int n = 0; n < sm->sel->number; n++) {
	/* use string equality.
	   could be wrong since the rle-encoded
	   string for a solution is not unique.
	   (but then we just store two copies of
	   the same solution, no big deal) */
	if (moves == sm->sel->items[n].ns.sol->tostring()) {
	  /* toss it */
	  goto toss_it;
	} 
      }

      /* keep it */
      {
	int idx = sm->sel->number;
	sm->sel->resize(idx + 1);
	/* never the default */
	sm->sel->items[idx].def = false;
	sm->sel->items[idx].ns.name = name;
	sm->sel->items[idx].ns.sol = s;
	sm->sel->items[idx].ns.author = author;
	sm->sel->items[idx].ns.date = date;
	/* these are always full solutions, not bookmarks */
	sm->sel->items[idx].ns.bookmark = false;
	sm->sel->items[idx].verifies = Level::verify(lev, s);

	/* now this belongs to sm->sel */
	es.release();

      }
	
    toss_it:;
    }

    return;

  } else {
    Message::no(&td, "Couldn't get solutions");
    return;
  }
    
}

void smanage::manage(Player *plr, string lmd5, Level *lev) {
  
  smreal * sm = smreal::create(plr, lmd5, lev);
  if (!sm) {
    Message::bug(0, "Couldn't create solution manager!");
    return;
  }

  Extent<smreal> es(sm);

  sm->sel->redraw();

  SDL_Event event;
  
  while (SDL_WaitEvent(&event) >= 0) {

    switch (event.type) {
    case SDL_KEYDOWN: {
      int key = event.key.keysym.sym;
      
      switch (key) {
      case SDLK_o: {
	if (!(event.key.keysym.mod & KMOD_CTRL)) break;
	int i = sm->sel->selected;

	/* one of these two will survive */
	Solution *sol = sm->sel->items[i].ns.sol;
	Solution *tmp = Optimize::opt(lev, sol);

	if (tmp->length < sol->length) {
	  printf("putting sol!\n");
	  sm->sel->items[i].ns.sol = tmp;
	  Message::quick(sm, "Optimized " RED +
			 itos(sol->length) + POP " " LRARROW " "
			 GREEN + itos(tmp->length) + POP ".",
			 "OK", "", PICS THUMBICON POP);
	  sol->destroy();
	} else {
	  Message::no(sm, "No optimization possible.");
	  tmp->destroy();
	}
	sm->sel->redraw();
      }
	break;
      case SDLK_u:
	if (!(event.key.keysym.mod & KMOD_CTRL)) break;	
	if (!plr->webid) break;

	if (sm->sel->items[sm->sel->selected].ns.sol->verified &&
	    !sm->sel->items[sm->sel->selected].ns.bookmark) {
	  smanage::promptupload(0, plr, lmd5, 
				sm->sel->items[sm->sel->selected].ns.sol,
				"Please only upload interesting solutions or speedruns.",
				sm->sel->items[sm->sel->selected].ns.name,
				false);
	} else {
	  Message::no(sm, "You can't upload bookmarks or non-validating "
		      "solutions.");
	}

	sm->sel->redraw();

	continue;
      case SDLK_d:
	if (!(event.key.keysym.mod & KMOD_CTRL)) break;	
	/* get solutions from web */
	smreal::downloadsolutions(plr, sm, lmd5, lev);

	sm->resort();
	sm->sel->redraw();
	
	continue;
      case SDLK_r:
	if (!(event.key.keysym.mod & KMOD_CTRL)) break;
	/* FALLTHROUGH */
      case SDLK_F2: {
	/* rename. */

	string sn = prompt::ask(sm,
				"Solution name: ",
				sm->sel->items[sm->sel->selected].ns.name);
				  
	  
	sm->sel->items[sm->sel->selected].ns.name = sn;

	/* XX resort? would have to track cursor */
	sm->sel->redraw();
	continue;
      }
      case SDLK_DELETE: ;
	/* shouldn't delete last solution */
	if (sm->sel->number > 1) {

	  int i = sm->sel->selected;

	  if (Message::quick(sm,
			     "Really delete '" YELLOW + 
			     sm->sel->items[i].ns.name + POP "'?",
			     "Delete",
			     "Cancel")) {

	    if (i == 0) {
	      /* need to make something else the default */
	      sm->sel->items[1].def = true;
	    }

	    /* now remove this one by overwriting it with the
	       last one. first delete the sol since we own it */
	    sm->sel->items[i].ns.sol->destroy();
	    
	    /* might be overwriting self, but that's no problem */
	    sm->sel->items[i].ns = sm->sel->items[sm->sel->number - 1].ns;
	    sm->sel->items[i].verifies = 
	      sm->sel->items[sm->sel->number - 1].verifies;
	      
	    sm->sel->number--;
	    sm->sel->selected = 0;
	  }
	} else Message::no(sm, "Can't delete last solution!");

	sm->resort();
	sm->sel->redraw();
	continue;

      case SDLK_i:
	if (!(event.key.keysym.mod & KMOD_CTRL)) break;
      case SDLK_INSERT: ;
	/* just make default. this is easy */
	sm->sel->items[0].def = false;
	sm->sel->items[sm->sel->selected].def = true;
	sm->sel->selected = 0;
	sm->resort();
	sm->sel->redraw();
	continue;
      }
    }
    }
    
    /* key wasn't handled above */
    switch (sm->sel->doevent(event).type) {
    case nsel::PE_SELECTED:
      /* show it */
      playback(plr, lev, &sm->sel->items[sm->sel->selected].ns);
      sm->sel->redraw();
      break;
    case nsel::PE_EXIT:
    case nsel::PE_CANCEL: {
      /* done. write changes back into player file */
      /* create solset list, preserving order */
      nslist * solset = 0;
      for (int i = sm->sel->number - 1;
	  i >= 0; 
	  i--) {
	solset = new nslist(new NamedSolution(sm->sel->items[i].ns.sol,
					      sm->sel->items[i].ns.name,
					      sm->sel->items[i].ns.author,
					      sm->sel->items[i].ns.date,
					      sm->sel->items[i].ns.bookmark),
			    solset);
      }
      
      plr->setsolutionset(lmd5, solset);
      plr->writefile();

      return;
    }
    case nsel::PE_NONE:
    default:
      /* ignore */
      break;
    }
  }
}

/* XXX add "slowmo" */
enum pbstate {
  PB_PAUSED,
  PB_PLAYING,
  PB_PLAYONEMOVE,
};

#define VCRHEIGHT 80

struct pb : public Drawable {

  virtual ~pb() {}
 
  void drawvcr() {
    int y = screen->h - VCRHEIGHT;
    int yf = y + TILEH + 4;
    int x = dr.margin; /* center? */
    int skip = TILEW + 8;

    /* clear first. */
    {
      SDL_Rect r;
      r.w = skip * 5;
      r.h = (yf - y) + (fon->height * 2) + 2;
      r.x = x;
      r.y = y - 2;
      SDL_FillRect(screen, &r, 0);
    }

    /* draw background for active button */
    {
      SDL_Rect r;
      r.w = skip - 4;
      r.h = TILEH + 4;
      r.y = y - 2;
      r.x = 0;
      switch (state) {
      case PB_PAUSED: r.x = x - 2 + (skip * 3); break;
      case PB_PLAYING: r.x = x - 2 + (skip * 2); break;
      case PB_PLAYONEMOVE: r.x = x - 2 + (skip * 4); break;
      }
      SDL_FillRect(screen, &r, VCRSELCOLOR);
    }

    drawing::drawtileu(x, y, TU_FREVBUTTON); 
    fon->drawcenter(x + (TILEW>>1), yf, "ctrl\n" LLARROW); x += skip;
    drawing::drawtileu(x, y, TU_REVBUTTON);
    fon->drawcenter(x + (TILEW>>1), yf, LLARROW); x += skip;
    drawing::drawtileu(x, y, TU_PLAYBUTTON);
    fon->drawcenter(x + (TILEW>>1), yf, "spc"); x += skip;
    drawing::drawtileu(x, y, TU_PAUSEBUTTON);
    fon->drawcenter(x + (TILEW>>1), yf, "spc"); x += skip;
    drawing::drawtileu(x, y, TU_FWDBUTTON);
    fon->drawcenter(x + (TILEW>>1), yf, LRARROW); x += skip;
    drawing::drawtileu(x, y, TU_FFWDBUTTON);
    fon->drawcenter(x + (TILEW>>1), yf, "ctrl\n" LRARROW); x += skip;
    SDL_Flip(screen);
  }

  /* Drawable */
  void draw() override {
    sdlutil::clearsurface(screen, BGCOLOR);
    dr.setscroll();
    dr.drawlev();
    dr.drawextra();

    fon->draw(screen->w - (fon->width * 5), 2,
	      itos(soli));

    #if 0
    /* good for seeing where two versions diverge */
    fon->draw(screen->w - (fon->width * 64), 2,
	      MD5::Ascii(MD5::Hash(dr.lev->tostring())));
    #endif

    drawvcr();
  }

  void screenresize() override {
    dr.margin = 12;
    dr.posx = 0;
    dr.posy = fon->height * 2;
    dr.width = screen->w - dr.posx;
    dr.height = (screen->h - dr.posy) - VCRHEIGHT;
    dr.setscroll();
    dirty->matchscreen();
  }

  pb() : dirty(Dirt::create()) {
  }

  pbstate state;
  drawing dr;
  std::unique_ptr<Dirt> dirty;
  /* index into solution */
  int soli;
};

void smanage::playback(Player *plr, Level *lev, NamedSolution *ns) {
  pb p;

  p.dr.lev = lev->clone();
  Extent<Level> el(p.dr.lev);

  Solution *sol = ns->sol->clone();
  Extent<Solution> es(sol);
  p.soli = 0;


  disamb * ctx = disamb::create(lev);
  Extent<disamb> edc(ctx);

  p.state = PB_PLAYING;
  p.screenresize();
  p.draw();

  for (;;) {

    /* act according to the state */
    switch (p.state) {
    case PB_PLAYONEMOVE:
    case PB_PLAYING:
      /* get move, execute it. */
      if (p.soli < sol->length) {
	dir move = sol->dirs[p.soli];
	p.soli++;

	/* and execute it, waiting for the animation to play. */
	play::animatemove(p.dr, ctx, p.dirty.get(), move);
	/* track player */
	p.dr.setscroll();
	p.draw();

      } else {
	/* if out of moves, we're done */
	p.state = PB_PAUSED;
      }

      /* pause if advancing just one. */
      if (p.state == PB_PLAYONEMOVE) {
	p.state = PB_PAUSED;
	p.drawvcr();
      }
      break;
    case PB_PAUSED:
      /* do nothing. */
      SDL_Delay(50);
      break;
    }
    
    /* process keyboard input */
    /* XXX also allow mouseclicks on vcr */
    SDL_Event event;
    while (SDL_PollEvent(&event)) {
      
      if (handle_video_event(&p, event)) continue;

      switch (event.type) {
      case SDL_KEYDOWN: {
	int key = event.key.keysym.sym;
	/* breaking from here will allow the key to be
	   treated as a search */

	switch (key) {
	case SDLK_u:
	case SDLK_LEFT: {
	  bool fast = (event.key.keysym.mod & KMOD_CTRL);
	  
	  int n = util::minimum(p.soli, fast?10:1);
	  /* undo and pause */

	  if (n > 0) {
	    /* rewind to beginning */
	    p.dr.lev->destroy();
	    p.dr.lev = lev->clone();
	    el.replace(p.dr.lev);
	    
	    /* play back solutions up to this point */
	    p.soli -= n;
	    for (int i = 0; i < p.soli; i++) {
	      p.dr.lev->move(sol->dirs[i]);
	    }
	  }
	  
	  p.state = PB_PAUSED;
	  p.draw();
	  break;
	}
	#if 0
	case SDLK_8:
	  if (event.key.keysym.mod & KMOD_CTRL) {
	    writefile("smanage_saved.esx", p.dr.lev->tostring());
	    Message::quick(0,
			   GREEN "wrote level for debugging "
			   RED "(DO NOT SUBMIT IT)" POP POP,
			   "OK", "", PICS THUMBICON POP);
	  }
	  break;
        #endif
	case SDLK_f:
	case SDLK_RIGHT:
	  if (event.key.keysym.mod & KMOD_CTRL) {

	    int n = util::minimum(sol->length - p.soli, 10);

	    if (n > 0) {
	      /* rewind to beginning */
	      p.dr.lev->destroy();
	      p.dr.lev = lev->clone();
	      el.replace(p.dr.lev);
	    
	      /* play back solutions up to this point */
	      p.soli += n;
	      for (int i = 0; i < p.soli; i++) {
		p.dr.lev->move(sol->dirs[i]);
	      }
	    }
	  
	    p.state = PB_PAUSED;
	    p.draw();
	  } else {
	    /* XXX maybe do something else if playing?
	       (skip a few frames and keep playing?) */
	    p.state = PB_PLAYONEMOVE;
	  }
	  p.draw();
	  break;
	case SDLK_SPACE: {
	  /* XXX start over if paused and done */
	  switch (p.state) {
	  case PB_PAUSED: p.state = PB_PLAYING; break;
	  case PB_PLAYING: p.state = PB_PAUSED; break;
	  case PB_PLAYONEMOVE: break;
	  }
	  p.drawvcr();
	  break;
	} 
	case SDLK_ESCAPE: return;
	}
	break;
      }

      case SDL_QUIT: return;
      }
    }
  }
}
