#include "ifc.h"
#include "scrobblec.h"
#include "dictionary.h"

#define SCROB ":Scrobble!z@z.org PRIVMSG"
#define ROUNDTIME 42
#define BETWEENTIME 15
#define BETWEENGAMETIME 25
#define SCORESPERLINE 4
#define GAMESCORE 300

extern char id_scrobblec[];
char id_scrobblec[] = "$Id: scrobblec.cc,v 1.4 2001/06/22 20:28:04 tom7 Exp $";

char getmagic();

const char tilevalues [ 26 ] = {
  /* a  b  c  d  e  f  g  h  i  j  k  l  m  n  o */
     1, 3, 3, 2, 1, 4, 2, 4, 1, 8, 5, 1, 3, 1, 1, 
  /* p  q  r  s  t  u  v  w  x  y  z  */
     3,10, 1, 1, 1, 1, 4, 4, 8, 4,10
};

extern dict wordlist;


void scrobblec :: speak(user * u, string style, string msg) {

  if (msg[0] == '!') {

    if (lowercase(msg) == "!help") {
      botspeakf(u, "[HELP] Er... actually there's no help yet. ;)");
    } else {
      botspeakf(u, "Unknown command '%s' :(", msg.c_str());      
    }

  } else if (msg[0] == '\001') {
    /* ctcp codes should just go through */

    channel :: speak(u, style, msg);
    
  } else switch (state) {

  case STATE_GETANSWERS:

    if (msg[0] == '-') {
      /* specially formatted message bypasses the bot */
      int skippy = (msg.length() > 1 && msg[1] == ' ') ? 2 : 1;
      channel :: speak(u, style, msg.substr(skippy, msg.length() - skippy));
    } else {
      /* process this person's answer */

      int force = 0;

      if (msg[0] == '@') {
	force = 1;
	msg = msg.substr(1,msg.length()-1);
      }

      string entry = lowercase(word(msg, 0));

      if (wordlist.lookup(entry)) {

	if (entry.length() > 2 && 
	    (signed)entry.length() <= boardlen && 
	    valid(entry)) {

	  const char * mess = "";

	  int s = score(entry);

	  /* possibly make this that user's best */

	  int ln = tomhash(lowercase(u->nick));

	  for (list<scplayer> * t = players;
	       t;
	       t = t -> next) {

	    if (t->data->nickhash == ln) {

	      if ((!t->data->thisround) ||
		  t->data->wordscore <= s || 
		  force) {

		if (t->data->thisround) {
		  if (force) mess = " (forced)";
		  else mess = " (new best)";
		}
		
		t->data->thisround = true;
		t->data->wordscore = s;
		t->data->bestword  = entry;
		t->data->repeated  = false;

	      }

	      goto foundit;
	    }
	    
	  }

	  /* didn't find this player! */

	  listpush(players, new scplayer(u->nick, ln, entry, s));


	  botspeakf(u, "You've been added to the game! :)");

	  mess = " (added to game)";

	    foundit:
	  
	  botspeakf(u, "'%s' scores %d%s. :)",
		    entry.c_str(),
		    s,
		    mess);
	  
	} else {
	  
	  botspeakf(u, "'%s' is invalid. :P",
		    entry . c_str());

	}

      } else {
	botspeakf(u, "'%s' is not in my wordlist. :(",
		  entry . c_str());
      }

    }
    break;
  case STATE_BETWEENROUNDS:

    if (msg[0] == '?') {
      /* also show to channel */
      channel::speak(u,style,msg);

      string entry = lowercase(word(msg.substr(1, msg.length()-1), 0));

      if (wordlist.lookup(entry)) {

	if (entry.length() > 2 && 
	    (signed)entry.length() <= boardlen && 
	    valid(entry)) {

	  int s = score(entry);
	  botspeakf(0,
		    "'%s' was worth %d points!", entry.c_str(), s);

	} else {
	  botspeakf(0,
		    "'%s' was not valid.",
		    entry.c_str());
	}
      } else {
	botspeakf(0,
		  "'%s' isn't in the wordlist.", entry.c_str());

      }
    }
    break;
  default: /* ? */
    channel :: speak(u, style, msg);
  }
}

void scrobblec :: changenick(user * u , string s) {


  channel :: changenick(u, s);
}


int scrobblec :: join (user * u, string kw) {
  if (!  channel :: join (u, kw) ) return 0;
    
  u -> nicksendf("NOTICE", ":Thanks for joining Scrobble! If you need "
		 "instructions, just type \"!help\" (others won't see "
		 "your message).");
  
  if (state == STATE_GETANSWERS) {

    printboard(u);

  }

  return 1; /* always succeed if parent succeeded */
}

int scrobblec :: part (user * u) {
  if (! channel :: part (u)) return 0;

 
  /* set to 'disappeared' mode, etc. */
  return 1;
}

void scrobblec :: quit (user * u, string msg) {



  /* set to 'disappeared' mode, etc. */
  channel :: quit(u, msg);
}

void scrobblec :: sendnames (user * u) {
  string nameslist;

  for (list<presence> * ul = users; ul ; ul = ul -> next) {
    nameslist += (string) ((ul->data->flags & P_CHOP)?"@":"")
      + ul->data->usr->nick + (string)" ";
  }

  nameslist += " @Scrobble";
  
  u -> nicksendf ("353", "= %s :%s",
		  name.c_str(),
		  nameslist.c_str());
  
  u -> nicksendf ("366", "%s :End of /NAMES list.", name.c_str());
}

void scrobblec :: botspeakf( user * u, const char * msg, ... ) {
  
  static char boofer[1024];
  
  int y = sprintf(boofer, SCROB " %s :",
		  name.c_str());

  va_list al;
  va_start (al, msg);
  /* int x = y + */ vsnprintf(boofer+y, 1000-y, msg, al);
  va_end (al);

  if (u) u -> sendf(boofer); /* specific user */
  else sendf(boofer);        /* whole channel */
  
}

void scrobblec :: timer ( int t ) {



  switch(state) {
  case STATE_BEGINGAME:
  printf("(%d) STATE_BEGINGAME: %d\n", t, roundend);
    /* why are we in this state? Begin a game! */

    if ( t <= roundend ) break;
    
      clearplayers();
    
  case STATE_BEGINROUND:
    printf("(%d) STATE_BEGINROUND: %d\n", t, roundend);
    botspeakf(0, 
	      "Beginning a new %s!", 
	      (state==STATE_BEGINGAME)?"game":"round");

    generateboard();
    printboard();

    state = STATE_GETANSWERS;
    
    roundend = t + ROUNDTIME;
    timewarning = 0;


    break;
  case STATE_GETANSWERS:
  printf("(%d) STATE_GETANSWERS: %d\n", t, roundend);
    if (!timewarning && (roundend - t) < 8) {
      botspeakf(0, "Less than 8 seconds left!");
      timewarning ++;
    }

    if (t > roundend) {
      botspeakf(0, "The round has ended!");

      state = STATE_DISPLAYSCORES;
    }
    
    break;
  case STATE_DISPLAYSCORES: {
  printf("(%d) STATE_DISPLAYSCORES: %d\n", t, roundend);
    /* print out all the scores */

    /* first perform exciting n**2 compare to find repeated words */

    int count = 0;
    unsigned int maxnick=4;
    for (list<scplayer> * tt = players; tt; tt = tt -> next) {

      if (tt->data->thisround && (!tt->data->repeated)) {
	count ++;
	/* check for matches */

	bool markem = false;

	for (list<scplayer> * u = tt -> next; u; u = u -> next)
	  if (u->data->thisround 
	      && (tt->data->bestword ==
		  u->data->bestword)) u->data->repeated = markem = true;

	  
        tt->data->repeated = markem;

      }

    }


    if (count) 
      botspeakf(0, "Here are the scores for this round:");
    else 
      botspeakf(0, "What, the letters were that hard? :/");

    for (list<scplayer> * tt = players; tt; tt = tt -> next)
      if (tt->data->thisround && (maxnick < tt->data->nick.length()))
	maxnick = tt->data->nick.length();

    int gameover = 0;
    for (list<scplayer> * tt = players; tt; tt = tt -> next) {

      if (tt->data->thisround) {
	botspeakf(0, "%-*s scored %3d with %s%s",
		  maxnick,
		  tt->data->nick.c_str(),
		  tt->data->wordscore >> (tt->data->repeated?1:0),
		  uppercase(tt->data->bestword).c_str(),
		  tt->data->repeated?" (repeated!)":"!");
	tt->data->score += tt->data->wordscore >> (tt->data->repeated?1:0);

	if (tt->data->score > GAMESCORE) gameover = 1;
      }
    }

    /* now check score totals */

    list<scplayer> * ll = players;

    while(ll) {
      string bleh;
      int ct = SCORESPERLINE;
      
      while (ct--) {

	if (ll) { 
	  bleh += 
	    ll->data->nick + 
	    string(": ") + 
	    itos(ll->data->score) +
	    string("  ");
	  ll = ll -> next; 
	} else break;
      }

      botspeakf(0, "%s", bleh.c_str());
    }

    if (gameover) {
      botspeakf(0, "The game is over!!! We'll start a new one in just a few moments.");
      roundend = t + BETWEENGAMETIME;
      state = STATE_BEGINGAME;

    } else {
      state = STATE_BETWEENROUNDS;

      roundend = t + BETWEENTIME;

      /* clear round flag */
      for (list<scplayer> * tt = players; tt; tt = tt -> next) 
	tt->data->thisround = false;
    }
    
    break;
  }
  case STATE_BETWEENGAMES:
  printf("(%d) STATE_BETWEENGAMES: %d\n", t, roundend);

  break;
  case STATE_BETWEENROUNDS:
  printf("(%d) STATE_BETWEENROUNDS: %d\n", t, roundend);

    if (t > roundend) {
      botspeakf(0, "OK! Get ready!");

      state = STATE_BEGINROUND;
    }

  default:; /* ? */
  }

}


/* want a lowercase string */
bool scrobblec :: valid (string m ) {
  char ltrs[13];
  char * f;
  const char * c = m.c_str();
  memcpy(ltrs, letters, boardlen);
  ltrs[boardlen] = 0;

  if (strchr(c, '.')) return 0; /* period is our marker char, so
				   it can't appear in the test string */

  for (unsigned int s = 0 ; s < m.length() ; s ++)    
    if (( f = strchr(ltrs, *c++)))
      * f = '.';
    else return 0;

  return 1;
}

#define VOWELS    "aaeeiioou!!!"
#define VOWELSLEN 9

#define CONSONANTS    "qwrrttyppssddfghjkllzxccvbbnnmm!!!"
#define CONSONANTSLEN 31

void scrobblec :: generateboard() {

  int bl = boardlen = int((random() / (float)RAND_MAX) * 7. + 5);

  float vowelpct = .25 + (random() / (float)RAND_MAX) * .3;

  int getvowels = int(boardlen * vowelpct);

  if (0) botspeakf(0, "For this board of length %d, there'll be %d vowels.",
		   boardlen, getvowels);
  
  int x;
  for (x=0; getvowels --; x ++) {
    bl --;
    letters[x] = VOWELS[int((random() / (float)RAND_MAX) * VOWELSLEN)];
  }

  for (; bl --; x++)
    letters[x] = CONSONANTS[int((random() / (float)RAND_MAX) * CONSONANTSLEN)];

  /* generate the board itself */
  for (int y=0; y < boardlen; y ++)
    board[y] = getmagic();

}

char getmagic() { /* NOTHING, DLS, TLS, DWS, TWS */

  float rarr = random() / (float) RAND_MAX;

       if (rarr < .03) return BRD_TWS;
  else if (rarr < .10) return BRD_DWS;
  else if (rarr < .15) return BRD_TLS;
  else if (rarr < .23) return BRD_DLS;
  else return BRD_NORMAL;

}

void scrobblec :: printboard(user * u = 0) {

  string b;
  for (int y=0; y < boardlen ; y ++) {
    b += '[';
    b += " dtDT"[board[y]];
    b += ']';
  }

  botspeakf(u, "Here's your board: %s", b.c_str());
 

  string s;
  for (int y=0; y < boardlen ; y ++) s += letters[y];
  
  botspeakf(0, "The letters: [%s]", s.c_str());


  if (!u) {
    /* set topic */
    topic = b + string(" (") + s + string(")");
    sendf(":Scrobble!x@x.org TOPIC %s :%s (%s)",
	  name.c_str(),
	  b.c_str(),
	  s.c_str());
  }

}

static char tilemult[] = {1, 2, 3, 1, 1};
static char wordmult[] = {1, 1, 1, 2, 3};

/* assumes word valid, all lowercase */
int scrobblec :: score(string s) {

  int best = -1;
  for (uint x=0; x <= (boardlen - s.length()); x++) {
    int multiplier = 1;
    int total = 0;
    for (uint y = 0 ; y < s.length(); y++) {

      if (0) printf("***] total += %d * %d\n",
		    tilevalues[s[y]-'a'],
		    tilemult[board[x+y]]);
      total += tilevalues[s[y]-'a'] * tilemult[board[x+y]];
      multiplier               *= wordmult[board[x+y]];
    }

    if (0) printf("*] At position x=%d, '%s' scores %d (%d * %d).\n", x,
		  s.c_str(),
		  total * multiplier,
		  total,
		  multiplier);
    
    if (total * multiplier > best) best = total * multiplier;
  }
  
  return best;
}

void scrobblec::clearplayers() {
  /* clears all the players (after a game, for instance) */


  while(players) {
    printf(")) (scrobblec) clearplayers players = %p\n", players);
    delete players->data;
    
    players->remove(players);

  }


}
