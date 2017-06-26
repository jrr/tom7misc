
/* this handles the possibility that the created channel will be of
   a special derived channel class. We keep this here so that we don't
   have to recompile the channel file for each added derivative, and
   keep derivative dependencies limited to this file and the files they
   appear in */

/* includes */

#include "ifc.h"
#include "trivialc.h"    /* trivial derived channel */
#include "scrobblec.h"   /* scrobble derived channel */
#include "dictionary.h"

extern char id_makechannel[];
char id_makechannel[] = "$Id: makechannel.cc,v 1.1 2001/04/08 04:00:23 tom7 Exp $";


dict wordlist("wordlist.txt");

channel * createchannel(string & s) {

  channel * n;

  if (s == "#backwards") {

    n = new trivialc(s);

  } else if (s == "#scrobble") { 
    
    n = new scrobblec(s);

  } else {
    
    n = new channel(s);
    
  }

    listpush(CHANNELS[tomhash(lowercase(s))%HASHSIZE], n);
    
    return n;

}

/* FIXME: there should be a system for registering
   system nicknames here. Right now a chained if taints our
   asymptotic complexity and code beauty... */
bool systemnick(string in) {

  if (in == "scrobble") return true;

  return false;
}
