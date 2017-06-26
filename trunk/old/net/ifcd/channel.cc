
#include "ifc.h"

extern char id_channel[];
char id_channel[] = "$Id: channel.cc,v 1.1 2001/04/08 04:00:23 tom7 Exp $";

void channel :: changenick(user * u, string dest) {
  
  sendexceptf(u,":%s!%s NICK :%s",
	      u->nick.c_str(),
	      u->hostname.c_str(),
	      dest.c_str());

}

void channel :: speak(user * u, string style, string msg) {
  list<presence> * pl = inchannel(u);
  presence * p;
  if (!pl || !(p = pl->data)
      || ((modeflags & C_MODERATED) && !(p->flags & (P_VOICE|P_CHOP)))) {
    u -> nicksendf("404", "%s :Channel doesn't want to hear it.",
		   name.c_str());
    return;
  }

  /* otherwise ... */

  sendexceptf(u,":%s!%s %s %s :%s", 
	      u->nick.c_str(),
	      u->hostname.c_str(),
	      style.c_str(),
	      name.c_str(),
	      msg.c_str());
}

list<presence> * channel :: inchannel(user * u) {
  
  for (list<presence> * ul = users; ul ; ul = ul -> next)
    if (ul->data->usr == u)
      return ul;
  return 0;
  
}

int channel :: join(user * u, string keyw) {
  /* if they're in the channel, forget it. */
  
  if (inchannel(u)) {
    printf("User is already in channel.\n");
    return 0;
  }

  /* is the channel overfull? */

  if ((modeflags & C_LIMIT) && (members >= limit)) {
    
    u -> nicksendf("471", "%s :Cannot join channel (limit exceeded).",
		   name.c_str());
    return 0;
  }

  /* now check keyword */

  if (modeflags & C_KEYWORD) {
  
    if (keyw != keyword) {
      u -> nicksendf("475", "%s :Cannot join channel (need keyword).",
		     name.c_str());
      return 0;
    }
  }

  /* check bans */

  for (list<string> * bb = bans ; bb ; bb = bb -> next) {
    if (wmatch(*bb->data, u->nick + string("!") + u->hostname)) {
      printf("*** %s!%s matches ban %s\n", u->nick.c_str(),
	     u->hostname.c_str(), bb->data->c_str());
      u -> nicksendf("474", "%s :Cannot join channel (banned!).",
		     name.c_str());
      return 0;
    }
  }

  /* otherwise... */

  presence * pp = new presence(u);
  if (!users && (modeflags & C_OPFIRSTUSER)) 
    pp -> flags |= P_CHOP;                 /* op first user */


  listpush(users, pp);
  listpush(u->channels, this);


  /* everyone gets same join message, including joiner */
  sendf(":%s!%s JOIN :%s",
	u->nick.c_str(),
	u->hostname.c_str(),
	name.c_str());

  /*   sendtopic(u); */    /* wrong... don't send topic */
  sendnames(u);
  
  members ++;

  return 1;

}

#define GETARG \
   string arg = word(args,0);			\
   args = wordsat(args,1);
//   if (arg == "") break; /* no arg */		


#define MODECHECK(z) \
   if (0 <= (signed)minuses.find((z))			\
   ||  0 <= (signed)pluses.find((z))) continue;

#define REGMODE(l,f) \
   case l:						\
       MODECHECK(l)                                     \
   if (op == 0 && (modeflags & (f))) {			\
     minuses += (l);					\
     modeflags &= ~(f);					\
   } else if (op == 1 && !(modeflags & (f))) {		\
     pluses += (l);					\
     modeflags |= (f);					\
   } break;


#define BANSALLOWED 3
#define OPSALLOWED  4

void channel :: modechange(user * u, string m) {

  /* ok, here we go! */

  int bansleft   = BANSALLOWED;
  int opsleft    = OPSALLOWED;
  int voicesleft = OPSALLOWED;

  list<presence> * p = inchannel(u);

  if (!p) {

    u -> nicksendf ("442", "%s :You're not even on that channel",
		    name.c_str());
    return;
  }

  if (!(p->data->flags & P_CHOP)) {
    
    u -> nicksendf ("482", "%s :You're not a channel operator",
		    name.c_str());
    return;
  }

  string modes = lowercase(word(m,0));
  string args  = lowercase(wordsat(m,1));

  int op = -1;

  string pluses,minuses;
  string plusargs,minusargs;
  string errorchars;
  
  for (uint x = 0 ; x < modes.length() ; x++) {
    switch(modes[x]) {
    case '+': op = 1; break;
    case '-': op = 0; break;
      REGMODE('t', C_TOPIC);
      REGMODE('s', C_SECRET);
      REGMODE('m', C_MODERATED);
      
    case 'i':
      u -> nicksendf("NOTICE", ":No +i on this server.");
      break;
    case 'n':
      u -> nicksendf("NOTICE", ":Can't change cmode +n on this server.");
      break;
    case 'l':
      {
	MODECHECK('l');
	if (op == 0) {
	  modeflags &= ~ C_LIMIT;
	  limit = 0;
	  minuses += 'l';
	} else if (op == 1) {
	  GETARG;
	  int ulimit = atoi(arg.c_str());
	  if ( ulimit > 0 && ulimit <= 32768 ) {
	    /* ok. */
	    limit = ulimit;
	    modeflags |= C_LIMIT;
	    pluses += 'l';
	    plusargs += string(" ") + itos(ulimit);
	  }
	}
	break;
      }
    case 'k':
      {
	MODECHECK('k');
	if (op == 0) {
	  modeflags &= ~ C_KEYWORD;
	  keyword = "";
	  minuses += 'k';
	} else if (op == 1) {
	  GETARG;
	  arg = lowercase(validnick(arg));
	  if (arg != "") {
	    /* ok. */
	    keyword = arg;
	    modeflags |= C_KEYWORD;
	    pluses += 'k';
	    plusargs += string(" ") + arg;
	  }
	}

	break;
      }
    case 'b':
      {
	//	MODECHECK('b');
	GETARG;

	if (!(bansleft--)) break;

	arg = lowercase(arg);
	if (arg == "") {
	  bansleft = 0; /* last ban command now */

	  /* checking ban list. */
	  for (list<string> * bb = bans; bb; bb = bb -> next) {

	    u -> nicksendf("367", "%s :%s %s 90210",
			   name.c_str(), 
			   bb->data->c_str(),
			   SERVERNAME);
	  }

	  u -> nicksendf ("368", "%s :End of ban list for %s.",
			  name.c_str(),
			  name.c_str());
		      
	} else {
	  
	  if (op == 0) {

	    for (list<string> * bb = bans; bb; bb = bb -> next) {
	      /* find ban */
	      
	      if (arg == *bb->data) {
		minuses += 'b';
		minusargs += string(" ") + arg;
		delete bb->data;
		bb->remove(bans);
		break;
	      }
	      
	    }
	    
	  } else if (op == 1) {
	    
	    if (-1 == (signed)arg.find('!')) arg += string("!*@*");
	    else if (-1 == (signed)arg.find('@')) arg += string("@*");

	    for (list<string> * bb = bans; bb; bb = bb -> next) {
	      /* does this ban already exist? */

	      if (arg == *bb->data) {
		goto mode_ban_out;
	      }
	      
	    }
	    /* doesn't... */
	    
	    listpush(bans, new string(arg));

	    pluses += 'b';
	    plusargs += string(" ") + arg;

	  }
	}

      mode_ban_out:;
	break;
      }
    case 'o':
      {
	GETARG;
	if (!(opsleft--)) break;
	list<presence> * pd = inchannel(getuser(arg));

	if (!pd) {

	  u -> nicksendf("401", "%s :No such nick / Not in channel.", 
			 arg.c_str());

	  break;
	}

	if (op == 0) {
	  
	  if (pd -> data -> flags & P_CHOP) {
	    pd -> data -> flags &= ~ P_CHOP;
	    minuses += 'o';
	    minusargs += string(" ") + pd -> data -> usr -> nick.c_str();
	  }

	} else if (op == 1) {

	  if (!(pd -> data -> flags & P_CHOP)) {
	    pd -> data -> flags |= P_CHOP;
	    pluses += 'o';
	    plusargs += string(" ") + pd -> data -> usr -> nick.c_str();
	  }

	}

	break;
      }
    case 'v':
      {
	GETARG;
	if (!(voicesleft--)) break;

	list<presence> * pd = inchannel(getuser(arg));

	if (!pd) {

	  u -> nicksendf("401", "%s :No such nick / Not in channel.", 
			 arg.c_str());

	  break;
	}

	if (op == 0) {
	  
	  if (pd -> data -> flags & P_VOICE) {
	    pd -> data -> flags &= ~ P_VOICE;
	    minuses += 'v';
	    minusargs += string(" ") + pd -> data -> usr -> nick.c_str();
	  }

	} else if (op == 1) {

	  if (!(pd -> data -> flags & P_VOICE)) {
	    pd -> data ->flags |= P_VOICE;
	    pluses += 'v';
	    plusargs += string(" ") + pd -> data -> usr -> nick.c_str();
	  }

	}
		
	break;
      }
    default:
      errorchars += modes[x];
    }
  }

  if (errorchars != "")
    u -> sendf("472", "%s :are unknown umode chars to me.",
	       errorchars.c_str());

  string outie;

  if (pluses != "") outie = (string)"+" + pluses;
  if (minuses != "") outie += (string)"-" + minuses;

  outie += plusargs + minusargs;

  if (outie != "")
    sendf(":%s!%s MODE %s %s",
	  u->nick.c_str(),
	  u->hostname.c_str(),
	  name.c_str(),
	  outie.c_str());

}

#undef REGMODE(u,l)

void channel :: sendmode(user * u) {

  /* new semantics: must be in channel to see any channel modes */


  list<presence> * p = inchannel(u);

  if (!p) {
    u -> nicksendf("442", "%s :Not in channel (default secret semantics).", 
		   name.c_str());
    return;
  }

  string modetext = "n";
  string modeparms;

  if (modeflags & C_MODERATED) modetext += 'm';
  if (modeflags & C_TOPIC) modetext += 't';
  if (modeflags & C_SECRET) modetext += 's';
  if (modeflags & C_KEYWORD) {
    modetext += 'k'; 
    modeparms += (string)" " + keyword;
  }
  if (modeflags & C_LIMIT) {
    modetext += 'l';
    modeparms += (string)" " + itos(limit);
  }

  u -> nicksendf("324", "%s +%s%s", 
		 name.c_str(),
		 modetext.c_str(), modeparms.c_str());

}

void channel :: settopic(user * u, string top) {

  list<presence> * p = inchannel(u);

  if (!p) {
    u -> nicksendf("442", "%s :Not in channel.", name.c_str());
    return;
  }

  if (modeflags & C_TOPIC) {
    
    if (!(p->data->flags & P_CHOP)) {
      nicksendf("482", "%s :Not channel operator. (Use the force, %s!)",
		name.c_str(), u->nick.c_str());
      return;
    }

  } 

  topic = top;

  /* tell everyone */

  sendf(":%s!%s TOPIC %s :%s", u->nick.c_str(),
	u->hostname.c_str(),
	name.c_str(),
	top.c_str());
}

void channel :: kick(user * u, user * k, string msg) {
  list<presence> * pu = inchannel(u);
  list<presence> * pk = inchannel(k);

  if (!pu || !(pu->data->flags & P_CHOP)) {
    u -> nicksendf("482", "%s :You're not channel operator!",
		   name.c_str());
    return;
  }

  if (!pk) {
    u -> nicksendf("441", "%s %s :He or she isn't on that channel!",
		   k->nick.c_str(), name.c_str());
    return;
  }

  sendf(":%s!%s KICK %s %s :%s",
	u->nick.c_str(),
	u->hostname.c_str(),

	name.c_str(),
	k->nick.c_str(),
	msg.c_str());

  expunge(pk);

}

void channel :: sendtopic(user * u) {
  u -> nicksendf ("332", "%s :%s", 
		  name.c_str(),
		  topic.c_str());
  u -> nicksendf ("333", "%s Motorcade 90210", name.c_str());
}

void channel :: sendnames(user * u) {
  string nameslist;

  for (list<presence> * ul = users; ul ; ul = ul -> next) {
    nameslist += (string) ((ul->data->flags & P_CHOP)?"@":"")
      + ul->data->usr->nick + (string)" ";
  }
  
  u -> nicksendf ("353", "= %s :%s",
		  name.c_str(),
		  nameslist.c_str());
  
  u -> nicksendf ("366", "%s :End of /NAMES list.", name.c_str());
}

void channel :: expunge(list<presence> * p) {

  if (!p) { printf("EXPUNGING null?\n"); return; }

  //  users -> validate();
  
  user * uu = p->data->usr;

  printf("expunge: Removing %p from %p\n", p, users);
  p->remove(users);
  printf("expunge: ok, now users is %p\n", users);

  printf("expunge: Removing user %p from channel\n", uu);
  for(list<channel> * ppp = uu->channels ; ppp ; ppp = ppp->next) {
    if (ppp->data == this) {
      ppp->remove(uu->channels);
      goto expunge_didit;
    }
  }
  printf("expunge: Failed trying to remove channel from user's channel list!\n");
  abort();
 expunge_didit:
  printf("expunge: ok.\n");
  //  users -> validate();

  members --;

  if (!users) destroy_channel(this, name); 

}

void channel :: quit(user * u, string msg) {
  sendf(":%s!%s QUIT :%s",
	u->nick.c_str(),
	u->hostname.c_str(),
	msg.c_str());
	
  expunge(inchannel(u));
}

int channel :: part(user * u) {
  list<presence> * p;
  if (! (p = inchannel(u))) {
    u -> nicksendf ("442", "%s :You're not even in that channel.",
		    name.c_str());
    return 0;
  }

  sendf(":%s!%s PART %s",
	u->nick.c_str(),
	u->hostname.c_str(),
	name.c_str());

  expunge(p);
  return 1;
}

channel * getchannel(string & s) {

  for(list<channel> * l = CHANNELS[tomhash(lowercase(s))%HASHSIZE];
      l;
      l = l -> next) {
    if (!strcasecmp(l->data->name.c_str(),s.c_str())) return l->data;
  }
  return 0; /* can't find it */
}

static char boofer[1024];

/* raw msg to everyone in channel c */
void channel :: sendf (const char * msg, ...) {

  va_list al;
  va_start (al, msg);
  int x = vsnprintf(boofer, 1000, msg, al);
  va_end (al);

  boofer[x++] = '\r';
  boofer[x++] = '\n';

  printf("BROADCAST[%s]: ", name.c_str());
  for (int a=0;a<x;a++) printf("%c",boofer[a]);

  for (list<presence> * ul = users; ul ; ul = ul -> next) {
    /*    printf("-> (u = %p)\n", ul -> data -> usr); */
    if (!(ul->data->usr->flags & U_DISCONNECTING)) 
      if (! ul->data->usr->sock->writepacket(boofer,x))
	ul -> data->usr -> sock -> disconnect ();    
    
  }
}

void channel :: sendexceptf (user * except, const char * msg, ...) {
  
  va_list al;
  va_start (al, msg);
  int x = vsnprintf(boofer, 1000, msg, al);
  va_end (al);

  boofer[x++] = '\r';
  boofer[x++] = '\n';

  printf("BROADCAST[%s]: ", name.c_str());
  for (int a=0;a<x;a++) printf("%c",boofer[a]);

  for (list<presence> * ul = users; ul ; ul = ul -> next) {
    if (except != ul->data->usr && 
	!(ul->data->usr->flags & U_DISCONNECTING))
      if (! ul->data->usr->sock->writepacket(boofer,x))
	ul -> data->usr -> sock -> disconnect ();    
  }
}


void channel :: nicksendf (const char * code, const char * msg, ...) {

  for (list<presence> * ul = users; ul ; ul = ul -> next) {
    
    int y = sprintf(boofer, ":%s %s %s ",
		    SERVERNAME,
		    code,
		    ul->data->usr->nick.c_str());

    va_list al;
    va_start (al, msg);
    int x = y + vsnprintf(boofer+y, 1000-y, msg, al);
    va_end (al);

    boofer[x++] = '\r';
    boofer[x++] = '\n';

    //    for (int a=0;a<x;a++) printf("%c",boofer[a]);
    if (!(ul->data->usr->flags & U_DISCONNECTING))
      if (! ul->data->usr->sock->writepacket(boofer,x))
	ul->data->usr -> sock -> disconnect ();
  }
}

/* only call this if there are *no refs* to this channel left
   (is no users left in it) */

void destroy_channel(channel * that, string name) {

  printf("--> DESTROYING channel %s\n", name.c_str());
  /* find it in the channel list; remove it */
    
  int idx = tomhash(lowercase(name))%HASHSIZE;

  for(list<channel> * l = CHANNELS[idx];
      l;
      l = l -> next) {
    if (l->data == that) {
      /* gotcha */
      printf("Channel %s is done for.\n", name.c_str());

      //      CHANNELS[idx] -> validate();

      l -> remove(CHANNELS[idx]);

      //      CHANNELS[idx] -> validate();

      /* the *remove* deletes the item in the list but not the channel 
	 itself */

      delete that;

      return;
    }
  }
    
  printf("destroy_channel: ERROR: HAVE CHANNEL POINTER, BUT CHANNEL IS NOT IN HASHTABLE\n");
  abort();
}

/* derived classes which make a new destructor should prolly call
   this one too to clean up the bans */
channel :: ~ channel () {
  if (users) {
    printf("!!!!!! WARNING!!!!!! Deleted a channel which still had\n"
	   "a non-nul 'users' list!\n");
    users->validate();
    abort();
  }
  
  /* should delete all the bans */
  
  while (bans) {
    delete bans->data;
    bans->remove(bans);
  }
}

void channel :: timer (int) { printf("[ERR] Don't call the channel class timer.\n"); }
