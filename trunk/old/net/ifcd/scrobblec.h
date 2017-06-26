
#ifndef __TOM7_SCROBBLEC_H
#define __TOM7_SCROBBLEC_H
/* $Id: scrobblec.h,v 1.1 2001/04/08 04:00:23 tom7 Exp $ */

#define STATE_BEGINGAME 1
#define STATE_GETANSWERS 2
#define STATE_DISPLAYSCORES 3
#define STATE_BETWEENROUNDS 4
#define STATE_BEGINROUND 5
#define STATE_BETWEENGAMES 6

#define BRD_NORMAL 0
#define BRD_DLS    1
#define BRD_TLS    2
#define BRD_DWS    3 
#define BRD_TWS    4

struct scplayer { /* for a whole game */
  string nick; /* avoid user * because of people dying and
		  rejoining... there's probably a better way
		  (especially since we have access to all the server
		  internals), but these are the semantics that people
		  are used to, at least */

  int nickhash;

  int score;

  bool thisround;

  /* this stuff applies to this round only (when thisround is set) { */

  string bestword; /* best word so far, or most recently !force'd word */
  int wordscore;   /* score of bestword */
  
  bool repeated; /* true if someone else settles on same bestword 
		    (set during scoring phase) */
  bool maxscorer;
  /* } */

  scplayer (string nn, int nh, string ww, int ws) 
    : nick(nn),
      nickhash(nh),
      score(0),
      thisround(true),
      bestword(ww),
      wordscore(ws),
      repeated(false),
      maxscorer(false) {}

  ~scplayer () {}

};

struct scrobblec : public channel {

  scrobblec(string & n)  { 
    name = n; 
    modeflags = C_TOPIC | C_WANTTIMER;
    state = STATE_BEGINGAME;
    players = 0;
    roundend = 0;
  }

  /*
    virtual void kick(user * kicker, user * kickme, string msg);
    
    virtual void sendf(const char *, ...);
    virtual void nicksendf(const char *, const char *, ...);

    virtual void sendexceptf (user * except, const char * msg, ...);
  */

  virtual void speak(user * u, string style, string msg);

  virtual void changenick(user *, string);

  /*
    virtual void settopic(user *, string);
    
    virtual list<presence> * inchannel(user * u);
    
    virtual void modechange(user *, string);
    virtual void sendmode(user *);
  */    

  virtual int  join (user *,string);
  
  virtual int  part (user *);
  
  virtual void quit (user *,string);

  virtual void sendnames (user *);

  virtual void timer (int);

  void botspeakf (user * u, const char * , ... );

  /*
    virtual void sendtopic (user *);
    
    virtual void expunge(list<presence> *);
  */

  //  virtual ~channel();

  int state, roundend, timewarning;

  /* for each board */

  char board[12];
  char letters[12];
  int boardlen; /* which is the same as letterslen */


  list<scplayer> * players;

  bool valid(string entry);
  void generateboard();
  void printboard(user * u = 0);

  int score (string);

  void clearplayers();

  ~scrobblec() {
    clearplayers();

  }

};

#endif
