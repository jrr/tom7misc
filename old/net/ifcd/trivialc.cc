
#include "ifc.h"
#include "trivialc.h"
#include "dictionary.h"

/* here's a minimal channel extension, which would be a good starting
   point for another new channel extension. See also scrobblec.cc,
   which is a game channel.
   
   In this extension, everything you type is seen backwards by
   everyone else.

   Additionally, saying ?word in the channel will ask the dictionary
   if the word is in the included wordlist.

*/

extern char id_trivialc[];
char id_trivialc[] = "$Id: trivialc.cc,v 1.2 2001/04/09 21:12:07 tom7 Exp $";

extern dict wordlist;

string sreverse(string in);

void trivialc :: speak (user * u, string style, string msg) {

  printf("trivialc: entered speak\n");
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
	      sreverse(msg).c_str());

  if(msg[0] == '?') {
    if (wordlist.lookup(lowercase(string(msg.c_str()+1)))) {
      sendf(":%s!%s PRIVMSG %s :[Bot] That's in my list.",
	    u->nick.c_str(),
	    u->hostname.c_str(),
	    name.c_str());
      
    } else {
      sendf(":%s!%s PRIVMSG %s :[Bot] That's not in my list.",
	    u->nick.c_str(),
	    u->hostname.c_str(),
	    name.c_str());
    }
  }
    
}

int trivialc :: join (user * u, string s) {
  channel :: join (u, s);
}

string sreverse(string in) {
  string oot;
  for (unsigned int i = in.length(); i -- ; ) oot += in[i];
  return oot;
}
