#include "ifc.h"

extern char id_pending[];
char id_pending[] = "$Id: pending.cc,v 1.1 2001/04/08 04:00:23 tom7 Exp $";

static char boofer[1024];
void pendingparse(list<pending> *& p, string & s) {


  printf("[%p] sends [%s]\n", p, s.c_str());

  string cmd = word(s,0);

  if (cmd == "NICK") {
    string nn = lastcolons(0,s);
    if (!(nickused(nn) || systemnick(lowercase(nn))) ) {
      
      p->data->nick = nn;
      
    } else {

      
      int x = snprintf(boofer, 1000, 
		       ":%s 433 * %s :Can't have that nick.",
		       SERVERNAME,
		       nn.c_str());

      boofer[x++] = '\r';
      boofer[x++] = '\n';
      
      if (! p->data->sock->writepacket(boofer,x))
	p->data->sock -> disconnect ();
      
      
      //      macon.ca.us.blah 433 oldnick newnick :Already in use

    }
    
  } else if (cmd == "USER") {

    string usr = word(s,1);

    p->data->hostname = usr + string("@") + p->data->hostname;
    p->data->realname = lastcolons(0,s);
    
  } else if (cmd == "PASS"){
    // ignore 
    ;
  } else if (cmd == "PONG"){
    // also ignore
    ;
  } else {
    
    int x = snprintf(boofer, 1000, 
		     ":%s 451 :You have to register.\r\n",
		     SERVERNAME);
	 
    if (! p->data->sock->writepacket(boofer,x))
      p->data->sock -> disconnect ();
 

  }
  
  if (p->data->nick != "" && p->data->realname != "") {
    printf("Registered %s!%s\n",
	   p->data->nick.c_str(),
	   p->data->hostname.c_str());
    
    tsocket * tt = p->data->sock;

    tt->validate();

    adduser(*p->data);

    tt->validate();

    list<pending> * t = p->next;
    printf("list<pending>(%p)->remove(%p);\n", p, PENDING);
    p->remove(PENDING); /* remove from pending list, cos they're not
			   pending now */

    printf("(remove done)\n");

    printf("{ now PENDING=%p }\n", PENDING);
    tt->validate();
    p = t;
  }

}

void addpending(int newsock, string clientaddr) {

  printf("listpush(%p,new pending...);\n", PENDING);

  listpush(PENDING, new pending(clientaddr, new tsocket(newsock)));

  printf("[ NEW CONNECTION: PENDING=%p ]\n", PENDING);

}
