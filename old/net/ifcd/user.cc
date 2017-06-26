
#include "ifc.h"

extern char id_user[];
char id_user[] = "$Id: user.cc,v 1.1 2001/04/08 04:00:23 tom7 Exp $";

/* DUDE. Pending users can sit in the list indefinitely and block
   nicks. FIXME */

int nickused(string & n) {
  
  if (getuser(n)) return 1;

  /* also check pending list! */

  for ( list<pending> * p = PENDING; p ; p = p -> next ) {

    if (!strcasecmp(p->data->nick.c_str(),n.c_str())) return 1;

  }

  return 0;

}

user * adduser(pending & p) {

  int h = tomhash(lowercase(p.nick)) % HASHSIZE;
  
  printf("(USERS[%d]) listpush(%p,new user(p))\n",h,USERS[h]);
  listpush(USERS[h], new user(p));

  /* do on-connect stuff */

  user * u = USERS[h]->data;

  /*  u -> sock -> validate (); */

  //  u -> sendf("DAMMIT!!\n");

  logon(u);
 
  
  return u;
  
}

void logon(user * u) {

  /* FIXME: read actual motd file */

  /*  system ("/bin/touch /tmp/xxx"); */

  u -> nicksendf ("001",":Welcome to the Internet Froodnet Chatnet");
  u -> nicksendf ("002",":Your host is IFCD version " VERSION);
  u -> nicksendf ("003",":This server was created.");
  u -> nicksendf ("004",SERVERNAME " diosw biklmnopstv");
  u -> nicksendf ("251",":There are some users.");
  u -> nicksendf ("252","0 :operators online.");
  u -> nicksendf ("254","0 :channels formed.");
  u -> nicksendf ("255",":I have some clients and a server.");
  u -> nicksendf ("375",":MOTD");
  u -> nicksendf ("372",":   While ifcd is in development, everything is logged");
  u -> nicksendf ("372",":   and consequently not very private. Use DCC chat");
  u -> nicksendf ("372",":   for private communication.");
  u -> nicksendf ("372",": ");
  u -> nicksendf ("372",": IFCD source code: http://irc.spacebar.org/ifcd/");
  u -> nicksendf ("372",": ");
  u -> nicksendf ("372",": %s", oneliner());
  u -> nicksendf ("372",": ");
  u -> nicksendf ("376",": Welcome to IFC.");

  printf("[User logged on at <user *> %p\n", u);

}

static char boofer[1024];

void user :: nicksendf (const char * code, const char * msg, ...) {

  int y = sprintf(boofer, ":%s %s %s ",
		  SERVERNAME,
		  code,
		  nick.c_str());

  va_list al;
  va_start (al, msg);
  int x = y + vsnprintf(boofer+y, 1000-y, msg, al);
  va_end (al);

  boofer[x++] = '\r';
  boofer[x++] = '\n';

  for (int a=0;a<x;a++) printf("%c",boofer[a]);

  if (! sock->writepacket(boofer,x))
    sock -> disconnect ();

}

void user :: sendf (const char * msg, ...) {

  va_list al;
  va_start (al, msg);
  int x = vsnprintf(boofer, 1000, msg, al);
  va_end (al);

  boofer[x++] = '\r';
  boofer[x++] = '\n';

  for (int a=0;a<x;a++) printf("%c",boofer[a]);

  if (! sock->writepacket(boofer,x))
    sock -> disconnect ();

}


user * getuser(const string & u) {
  for(list<user> * l = USERS[tomhash(lowercase(u))%HASHSIZE];
      l;
      l = l -> next) {
    if (!strcasecmp(l->data->nick.c_str(),u.c_str())) return l->data;
  }
  return 0; /* can't find it */
}

void userparse(list<user> *& up, string & s) {

  user * u = up -> data;

  printf("<%s> (%s)\n", u->nick.c_str(), s.c_str());
  
  string cmd = lowercase(word(s,0));
			
  int code = tomhash(cmd);
  
  switch (code) {
  case 0x014CC5A0:       /* privmsg */
  case 0x36AED1FE:       /* notice */
    {
      u->idlesince = time(0);
      string target = word(s,1);
      string msg    = lastcolons(0,s);
      if (target[0] == '#') {
	
	channel * c = getchannel(target);
	if (c) {
	  c -> speak(u, (cmd[0]=='n')?"NOTICE":"PRIVMSG", msg);

	} else {
	  u -> nicksendf ("401", "%s :No such channel.",
			  target.c_str());
	}

      } else {

	user * dest = getuser(target);
	if (!dest) {
	  u -> nicksendf ("401", "%s :No such nick.",
			  target.c_str());
	} else {
	  dest -> sendf (":%s!%s %s %s :%s",
			 u->nick.c_str(),
			 u->hostname.c_str(),
			 (cmd[0]=='n')?"NOTICE":"PRIVMSG",
			 target.c_str(),
			 msg.c_str());
	}
      }
    }
    break;
  case 0x60C00386:       /* join */
    {
      string target = word(s,1);
      
      if (target[0] != '#') {
	u -> nicksendf("403", "%s :No such channel", target.c_str());
	break;
      }

      channel * c = getchannel(target);

      if (!c) {
	c = createchannel(target);
      }

      c->join(u,word(s,2)); /* send possible keyword */
      
      break;
    }
  case 0x28001580:       /* part */
    {
      string target = lastcolons(0,s);
      
      channel * c=0;

      if (target[0] != '#' || !(c = getchannel(target))) {
	u -> nicksendf("403", "%s :No such channel", target.c_str());
	break;
      }

      
      c->part(u);
      
      c=0;
      /* c may be stale (deleted) */
      
      break;
    }
  case 0x20A719D2:       /* nick */
    {
      /* change nicks */
      string nn = validnick(lastcolons(0,s));

      /* check if their last attempt at changing nicks was too
	 recent */

      if (time(0) <= u -> blocknickchangetime) {

	u -> nicksendf("438", "%s :Nick change too fast. Please wait %d seconds.",
		       nn.c_str(),
		       u -> blocknickchangetime - time(0));

	break;
      }

      if (nickused(nn) || systemnick(lowercase(nn))) {
	
	u -> nicksendf("433", "%s :You can't have that one. It's taken.",
		       nn.c_str());
	
      } else {

	/* ok, fine... */

	/* for all channels */

	for (list<channel> * cc = u->channels ; cc ; cc = cc -> next) {
	  cc->data->changenick(u,nn);
	}

	u -> sendf(":%s!%s NICK :%s",
		   u->nick.c_str(),
		   u->hostname.c_str(),
		   nn.c_str());
	
	list<user> * nnn = up->next;

	up->remove(USERS[tomhash(lowercase(u->nick))%HASHSIZE]);
	u->nick = nn;
	listpush(USERS[tomhash(lowercase(nn))%HASHSIZE], u);

	up = nnn;

	u -> blocknickchangetime = time(0) + NICKWAITTIME;

      }
       
      break;
    }
  case 0x25BAC7E6:       /* whois */
    {
      string target = word(s,1);
      user * dest = getuser(target);
      if (!dest) {

	u -> nicksendf ("401", "%s :No such nick.",
			target.c_str());
      } else {
	
	int at = dest->hostname.find('@');
	target = dest->nick; /* use official capitalization */
	u -> nicksendf ("311", "%s %s %s * :%s",
			target.c_str(),
			dest->hostname.substr(0,at).c_str(),
			dest->hostname.c_str() + at + 1,
			dest->realname.c_str());

	if (dest -> flags & U_AWAY)
	  u -> nicksendf("301", "%s :%s",
			 target.c_str(),
			 dest->away.c_str());

	u -> nicksendf ("317", "%s %d %d :seconds idle, signon time",
			target.c_str(),
			time(0) - dest->idlesince,
			dest->connecttime);

	if (dest -> flags & U_OP)
	  u -> nicksendf ("313", "%s :is an irc operator!",
			  target.c_str());

	u -> nicksendf ("312", "%s %s :(%s)",
			target.c_str(),
			SERVERNAME,
			oneliner());
      }
      u -> nicksendf ("318", "%s :End of /WHOIS list.", target.c_str());
      break;
    }
    
  case 0x545E1B3E:       /* mode */
    {
    
      /* get destination (channel or nick) */

      string dest = word(s,1);

      if (dest[0] == '#') {
	/* channel */
	
	channel * c = getchannel(dest);
	
	if (c) {
	  
	  string mode = wordsat(s,2);
	  printf("modestring: [%s]\n", mode.c_str());
	  if (mode != "") {

	    c -> modechange(u, mode);
	  
	  } else {
	    /* just checking */

	    c -> sendmode(u);

	  }
	} else {

	  u -> nicksendf("403", "%s :No such channel!", dest.c_str());
	  
	}

      } else {
	/* user */
	string mode = lastcolons(0,s);
	if (!strcasecmp(dest.c_str(), u->nick.c_str())) {

	  /* changing umode */
	  
	  string mode = lastcolons(0,s);

	  string minuses, pluses, errorchars;

	  if (mode != "" && mode != dest) {
	    int op = -1;
	    
	    for (uint v = 0; v < mode.length(); v++) {
	      switch(tolower(mode[v])) {
	      case '+':
		op = 1;
		break;
	      case '-':
		op = 0;
		break;
	      case 'i':
		if (0 <= (signed)minuses.find('i')
		    || 0 <= (signed)pluses.find('i')) continue;

		if (op == 0 && (u->flags & U_INVISIBLE)) {
		  minuses += 'i';
		  u->flags &= ~U_INVISIBLE;
		  
		} else if (op == 1 && !(u->flags & U_INVISIBLE)) {
		  pluses += 'i';
		  u->flags |= U_INVISIBLE;
		}
		break;
	      case 'o':
		if (0 <= (signed)minuses.find('o')
		    || 0 <= (signed)pluses.find('o')) continue;
		
		if (op == 0 && (u->flags & U_OP)) {
		  u->flags &= ~U_OP;
		  minuses += 'i';
		}
		break;
	      case 'w':
		if (0 <= (signed)minuses.find('w')
		    || 0 <= (signed)pluses.find('w')) continue;
		if (op == 0 && (u->flags & U_WALLOPS)) {
		  minuses += 'w';
		  u->flags &= ~U_WALLOPS;
		} else if (op == 1 && !(u->flags & U_WALLOPS)) {
		  pluses += 'w';
		  u->flags |= U_WALLOPS;
		}
		break;
	      case 's':
		if (0 <= (signed)minuses.find('s')
		    || 0 <= (signed)pluses.find('s')) continue;
		if (op == 0 && (u->flags & U_SMSG)) {
		  minuses += 's';
		  u->flags &= ~U_SMSG;
		} else if (op == 1 && !(u->flags & U_SMSG)) {
		  pluses += 's';
		  u->flags |= U_SMSG;
		}
		
		break;
	      default:
		errorchars += mode[v];
	      }
	    }
		   
	    if (errorchars != "") {
	      u -> sendf("472", "%s :are unknown umode chars to me.",
			 errorchars.c_str());
	    }
	    
	    string modechange;

	    if (pluses  != "") modechange  = (string)"+" + pluses;
	    if (minuses != "") modechange += (string)"-" + minuses;

	    if (modechange != "")
	      u -> sendf(":%s MODE %s :%s",
			 dest.c_str(), dest.c_str(),
			 modechange.c_str());
	    
	  } else {
	    /* send umode */

	    string modechars;
	    
	    if (u->flags & U_INVISIBLE) modechars += 'i';
	    if (u->flags & U_OP) modechars += 'o';
	    if (u->flags & U_WALLOPS) modechars += 'w';
	    if (u->flags & U_SMSG) modechars += 's';
	    
	    if (modechars != "") modechars = (string)"+" + modechars;

	    u -> nicksendf("221", ":%s", modechars.c_str());
	    
	  }
	  
	} else {


	  u -> nicksendf("502", ":Can't do that to other users.");

	}

      }
    }
    break;
  case 0x4B6000CA:       /* topic */
    {
      string chan = word(s,1);

      channel * c = getchannel(chan);

      if (c) {

	if (word(s,2) != "") {

	  string newtopic = lastcolons(0,s);

	  c -> settopic(u,newtopic);
	  
	} else {
	  c->sendtopic(u);
	  
	}
	
      } else {
	u -> nicksendf("442", "%s :Can't access topic", chan.c_str());
      }
      
    }
    break;
  case 0x62FC96EA:       /* quit */
    {
      string msg = lastcolons(0,s);
      if (msg == "") msg = "*";

      deluser(up,msg);
      
    }
    break;
  case 0x68A74C70:       /* kick */
    { 
      string chan = word(s,1);
      channel * c = getchannel(chan);
      if (c) {
	string usr = word(s,2);
	user * k = getuser(usr);
	if (k)
	  c -> kick(u, k, lastcolons(0,s));
	else {
	  u -> nicksendf("401", "%s :No suck nick", chan.c_str());

	}
      } else {
	u -> nicksendf ("403", "%s :No such channel", chan.c_str());
      }
    }
    break;
  case 0x610BEA34:       /* ison */
       
    break;
  case 0x70CD990B:       /* whowas */
    {
      string nick = lastcolons(0,s);
       
      u -> nicksendf("406", "%s :Whowas is not supported.",
		     nick.c_str());
      u -> nicksendf("369", ": End of WHOWAS.");
    }
    break;
  case 0x1AA23C8F:       /* list */
    {
      u -> nicksendf("321", "Channel :Users  Name");
    
      int MAX = 150;
    
      for (int x=0; x < HASHSIZE; x++) {
	for (list<channel> * c = CHANNELS[x] ; c ; c = c -> next) {
	  if (!(MAX--)) goto list_done;
	  if (!(c->data->modeflags & C_SECRET))
	    u -> nicksendf("322", "%s %d :%s",
			   c->data->name.c_str(),
			   c->data->members,
			   c->data->topic.c_str());
	}
      }
    list_done:
      u -> nicksendf("323", ":End of channel list (or max exceeded).");
    }
    break;
  case 0x08A40972:       /* names */
    {
      string chan = word(s,1);

      channel * c = getchannel(chan);

      if (c) {
	if (!(c->modeflags & C_SECRET) || (c->inchannel(u)))
	  c->sendnames(u);
	else
	  u->nicksendf ("366", "%s :Can't list names.", chan.c_str());
      } else
	u -> nicksendf ("366", "%s :No names.", chan.c_str());
 
    }
    break;
  case 0x17AD06D6:       /* userhost */
    {
      string aa = wordsat(s,1);
      string out;
      while (aa != "") {
	
	user * targ;
	if ((targ = getuser(word(aa,0)))) {
	  
	  out 
	    += targ->nick + string("=")
	    +  string((targ->flags&U_AWAY)?"-":"+")
	    +  targ->hostname + string(" ");
	  
	} 
	aa = wordsat(aa,1);
      }
      
    u -> nicksendf("302", ":%s", out.c_str());

    }
    
    break;
  case 0x0FCA9FFB:       /* version */
    {
      u -> nicksendf("351", "ifcd" VERSION " %s :Mechanic",
		     SERVERNAME);
    }
    break;
  case 0x17DC000F:       /* away */
    {
      printf("-----[%s]---\n",s.c_str());

      // why does string s = lastcolons(0,s); not work?

      string gs = lastcolons(0,s);

      printf("AWAY: %s\n", gs.c_str());
       
      if (gs == "" || gs == word(s,0)) {
	// set not-away
	u -> flags &= ~U_AWAY;
	u -> nicksendf ("305", ":Now you're not away.");
	
      } else {
	 
	u -> flags |= U_AWAY;
	u -> away = gs;
	u -> nicksendf ("306", ":Now you're away.");
      }
    }
    
    printf("Leaving *AWAY*\n");

    break;
  case 0x3FAEA8E9:       /* users */
    u -> nicksendf("446", ":USERS is disabled.");
    break;
  case 0x50A6C20F:       /* summon */
    u -> nicksendf("445", ":SUMMON is disabled.");
    break;
  case 0x3D9E0002:       /* pass */
    /* FALLTHRU */
  case 0x3FAEB901:       /* user */
    u -> nicksendf("462", ":Can't reregister. Duh.");
    break;
  case 0x38800268:       /* pong */
    /* set ping timeout to 0 */
    u -> pingtime = 0;

    break;
  case 0x361C01BA:       /* ping */
    {
    string g = lastcolons(0,s);

    u -> sendf(":%s PONG %s :%s", SERVERNAME, SERVERNAME,
	       g.c_str());
      }
    break;   
  default: 
    u -> nicksendf("421", "%s :Unknown/Unimplemented command", 
		   cmd.c_str());
    break;
  }

}

string validnick(string in) {
  string out;

  for (uint x=0;x<in.length() && out.length() < 16;x++) {

    if (((in[x]|32) >= 'a' && (in[x]|32) <= 'z')
	|| (in[x] >= '0' && in[x] <= '9')
	|| strchr("[]|-_`", in[x])) out += in[x];

  }

  if (out.length() == 0) return "_";
  return out;
}

void deluser(list<user> *& up, string msg) {
  
  if (up->data->flags & U_DISCONNECTING) {
    printf("!!! RECURSIVE DELUSER\n"); /* prevent reentrancy */
    return;
  }
  /* part from all channels */
  up->data->flags |= U_DISCONNECTING;


  /* let this be a lesson to you all: This is WRONG */
  /* the quit method modifies the structure we're traversing
     (list of channels we're in) */
#if 0
  for (list<channel> * cc = up->data->channels ; cc ; cc = cc -> next) {

    cc -> data -> quit(up->data,msg);

  }
#endif
  
  /* better: quit removes user from the channel list, so this eventually
     terminates */
  while(up->data->channels) {
    printf("--- deluser: quitting from %p\n", up->data->channels);
    up->data->channels->data->quit(up->data,msg);
    /*
      printf("--- deluser: now validate");
      up->data->channels->validate();
    */
  }
  
  delete up->data->sock;
  up->data->sock = 0;
  up->remove(USERS[tomhash(lowercase(up->data->nick))%HASHSIZE]);

  up=0;
}
