#ifndef __TOM7_IFC_H
#define __TOM7_IFC_H
/* $Id: ifc.h,v 1.1 2001/04/08 04:00:23 tom7 Exp $ */

#include <iostream.h>
#include <signal.h>
#include <stdio.h>
#include <unistd.h>
#include <time.h>
#include <string>
#include <stdarg.h>

#include "tsocket.h"
#include "aux.h"
#include "tom.h"

/* YES!! */
inline const char * operator-(string & s) {return s.c_str(); }

#define HASHSIZE 17389 
           /* 2000th prime */
/* don't think that's kind of a lot? */

#define DEFAULT_PORT 6667
#define HOST "128.2.164.133"
#define VERSION "0.90"

#define NICKWAITTIME 20 /* seconds after changing nick to change again */

#define SERVERNAME "irc.spacebar.org"

#define P_CHOP  1
#define P_VOICE 2

#define C_MODERATED    1
#define C_TOPIC        2
#define C_SECRET       4
// #define C_MODERATED 8
#define C_KEYWORD     16
#define C_LIMIT       32
#define C_OPFIRSTUSER 64
#define C_WANTTIMER   128

#define U_INVISIBLE     1
#define U_OP            2
#define U_AWAY          4
#define U_DISCONNECTING 8
#define U_WALLOPS       16
#define U_SMSG          32


template <class T> struct list {
  T * data;
  list<T> * next, * prev;
  list (T * d,list<T> * n = 0, 
	list<T> * p = 0) : data(d), next(n), prev(p) {}
  
  /* note: semantics are NOT to delete the data, since it might be
     involved in some other list. Therefore,

     delete h->data;
     h->remove(head);

  */
  void remove(list<T> *& head) {

    if (head == this) head = head->next;

    if (next) next->prev = prev;
    if (prev) prev->next = next;
    next = prev = 0;

    data = 0; /* DEBUG: avoid stale pointers */

    delete this;

  }

  ~list () { if (next) next->prev=0; 
             if (prev) prev->next=0;
	     delete next; 
	     delete prev; 
  }

  void validate () {
    printf("*** %p->validate()\n", this);
    for (list<T> * i = this; i; i = i -> next) {
      printf("  [%p<-][%p][->%p]",i->prev, i, i->next);
      if (i->next && i->next->prev != i)
	printf(" *DISPARITY 1*");
      if (i->prev && i->prev->next != i)
	printf(" *DISPARITY 2*");
      
      printf("\n");
    }
    printf("*** -> 0\n");
  }
  
};

template <class T> void listpush (list<T> *& l, T * dat);
template <class T> void listpush (list<T> *& l, T * dat) {
  /*  printf("---listpush\n"); */
  /*  l->validate(); */
  /*  printf("listpush(%p,%p)\n", l, dat); */
  l = new list<T>(dat, l);
  if (l->next) l->next->prev = l;
  /*  l->validate(); */
  /*  printf("---listpush out\n"); */
}

struct pending {

  pending(string h, tsocket * s) : hostname(h), sock(s), pingtime(0) {}
  
  string nick;
  string hostname;
  string realname;
  tsocket * sock; /* doesn't destruct */

  int pingtime;

};

struct channel;

struct user {
  user(int a) :  flags(0), channels(0), pingtime(0), blocknickchangetime(0)
    {sock = new tsocket(a);}
  user(pending & p) :  flags(0), channels(0), pingtime(0), blocknickchangetime(0) {
    
    nick     = p.nick;
    hostname = p.hostname;
    realname = p.realname;
 
    sock     = p.sock;

    connecttime = idlesince = time(0);
    
  }

  void nicksendf (const char * code, const char * msg, ...);
  void sendf (const char * msg, ...);

 ~user() { delete sock; }
  tsocket * sock;

  /* user info */
  string hostname;
  string nick;
  string realname;
  string away;
  int flags;
  int idlesince, connecttime;
  list<channel> * channels;
  int pingtime;
  int blocknickchangetime;
};

struct presence {
  
  presence (user * u) : usr(u), flags(0) {}
  
  user * usr;
  int flags;

};

struct channel {
  channel(string & n) 
    : name(n), 
      modeflags(C_OPFIRSTUSER), 
      members(0),
      bans(0), 
      users(0) { }

  channel() 
    : name("#ERROR"), 
      modeflags(C_OPFIRSTUSER), 
      members(0), 
      bans(0), 
      users(0) 
    {} /* for derived classes */

  virtual void kick(user * kicker, user * kickme, string msg);

  virtual void sendf(const char *, ...);
  virtual void nicksendf(const char *, const char *, ...);

  virtual void speak(user * u, string style, string msg);
  virtual void sendexceptf (user * except, const char * msg, ...);

  virtual void settopic(user *, string);

  virtual list<presence> * inchannel(user * u);

  virtual void changenick(user *, string);

  virtual void modechange(user *, string);
  virtual void sendmode(user *);

  virtual int  join (user *,string);
  virtual int  part (user *);
  virtual void quit (user *,string);
  virtual void sendnames (user *);
  virtual void sendtopic (user *);
  
  virtual void expunge(list<presence> *);

  virtual void timer(int); /* send tick every second if
			      C_WANTTIMER is set */

  virtual ~channel();

  string name;
  int modeflags;
  string topic, keyword;
  int limit;
  int members;
  list<string> * bans;
  list<presence> * users;

};

extern list<user>    ** USERS   ;
extern list<channel> ** CHANNELS;

extern list<pending>  * PENDING ; /* list of unregistered sockets */

extern int serversock;

void acceptsockets();
void serverloop();
int init();

void addpending(int, string addressssss);
user * adduser(pending&);

void pendingparse(list<pending> *& p, string & s);
void userparse (list<user> *&, string &);

int nickused(string & s);

user * getuser(const string & u);

void logon(user * u);

channel * getchannel(string &);
channel * createchannel(string &); /* makechannel.cc */
void destroy_channel(channel * that, string name);

string validnick(string in);

void deluser(list<user> *& up, string msg);

char * oneliner();

bool systemnick(string);

#endif
