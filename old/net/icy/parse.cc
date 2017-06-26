
#include "parse.h"
#include <string>
#include "aux.h"
#include <stdlib.h>
#include "tom.h"
#include "icy.h"
#include <stdio.h>
#include <fstream.h>

extern char * iendl;

linkbank * froodsays = 0;

string current_topic;
string infostring;
string lastsaid;

// int retardo=0;

extern string DEFCHANNEL;
extern string DEFNICK;
extern string NICKPASS;

int should_op(string);

enum { ACTION_NOOP, ACTION_OP, ACTION_KICK, ACTION_EAT, };

struct action_node {
  int action;
  string wildcard;
  action_node * next;
  action_node(int i,string s,action_node*a) :
    action(i), wildcard(s), next(a) {}
};

action_node * controllist = 0;
action_node * eatlist = 0;

struct stringlist {
  string s;
  stringlist * next;
  stringlist (string ss, stringlist * nn) : s(ss), next(nn) {}
  static int pop(stringlist *& s, string & out) {
    if (s) {
      stringlist * tmp = s;
      out = s->s;
      s = s -> next;
      delete tmp;
      return 1;
    } else {
      return 0;
    }
  }
};

int doops(stringlist *& friends,
	  string channel, int n) {
  int i;
  do {
    /* get up to n elements off the list. */

    string ostring = " +";
    string fstring = "";

    for(i=0; i < n; i ++) {
      string oo = "";
      if (!stringlist::pop(friends, oo)) break;
      ostring += "o";
      fstring += " ";
      fstring += oo;
    }

    if (i) say ((string)"MODE " + channel + ostring + fstring);

  } while (i == n);
}

void load_action_list(const char* fname) {
  ifstream smack(fname);
  if (!smack) return;
  string id, wc;

  /* delete old list */
  while (controllist) {
    action_node * tmp = controllist->next;
    delete controllist;
    controllist = tmp;
  }
  while (eatlist) {
    action_node * tmp = eatlist->next;
    delete eatlist;
    eatlist = tmp;
  }

  cout << " (( reads " << fname << " ))"  << iendl;
  while (smack >> id >> wc) {
    /*    cout << '[' << id << "]===>[" << wc << "]" << iendl; */
    if (id == "OP") 
      controllist = new action_node(ACTION_OP,wc,controllist);
    else if (id == "EAT") {
      controllist = new action_node(ACTION_EAT,wc,controllist);
      eatlist = new action_node(ACTION_EAT,wc,eatlist);
    } else if (id == "KICK")
      controllist = new action_node(ACTION_KICK,wc,controllist);
    else
      controllist = new action_node(ACTION_NOOP,wc,controllist);
  }
  smack.close();
}

void parse(string in) {
  static int moddy = 0; 
  string first = word(in,1);
  int num;
  static stringlist * oplist = 0;

 
  if ("PING" == word(in,0)) {
    say("PONG :PONG PONG!");
    cout << "[Ping/Pong]" << iendl;
    if (dospeak) {
      if (froodsays->changed && !moddy) {
	froodsays->savetofile("icy-back.txt");
	infostring = froodsays->info(15);
      } 
    }
    if (++moddy > 35) moddy = 0;
    return;
  }

  if ((num = atoi(first.c_str()))) {
    /* numeric code */
    switch (num) {
    case 301: /* Frood is away */
    case 375:
    case 255:
    case 254:
    case 253:
    case 252:
    case 251:
    case   4:
    case   3:
    case   2:
    case 376:
    case 372:
      /* ignore motd, other crap */
      break;
    case 513: 
      say((string)"PONG " + word(in,-1));
      break;
    case   1:
      info ("Connected, Registered!");
      say((string)"JOIN " + DEFCHANNEL);
      say((string)"PRIVMSG ChanServ :OP " + DEFCHANNEL + (string)" " + DEFNICK);
      break;
    case 332:
      /* get topic. */
      current_topic = lastcolons(1,in);
      break;

    case 315:
      /* end of WHO list */
      doops(oplist, word(in, 3), 3);
      break;
    case 352:
      /* WHO line */
      {
	string channel = word(in,3);
	if (word(in, 8).length() == 1) {
	  string u = word(in, 7) + (string)"!" + word(in, 4) + (string)"@" + word(in, 5);
	  if (should_op(u)) {
	    cout << "YES-OP: " << u << iendl;
	    /*	    say ((string)"MODE " + channel + (string)" +o " + word(in, 7)); */
	    oplist = new stringlist(word(in,7), oplist);
	  } else {
	    cout << "WON'T-OP: " << u << iendl;
	  }
	}
      }
      break;
    default:
      cout << "[" << num << "]: " << in << iendl;
    }

  } else { /*non-numeric*/
    if (first == "PRIVMSG") {
      string nick = word(in,0);
      int oper = eatme(nick);
      string message = lastcolons(1,in);
      if (word(message,0) == "\001ACTION") 
	printf(" * %s %s%s",in.substr(1,in.find('!',0)-1).c_str(),
	       message.c_str()+8,iendl);	  
      else
	printf("<%s> %s%s",in.substr(1,in.find('!',0)-1).c_str(),
	       message.c_str(),iendl);

      string cmd = word(message,0);
      if (dospeak && cmd == "!SPEAK") {
	// speak!
	string tox = word(message,1);
	/* only can send to a channel or other user
	   if an operator */
	if (oper || (lowercase(tox) == lowercase(nick)))
	  say((string)"PRIVMSG " + tox + string(" :") +
	      froodsays->construct());
      }

      /* let anybody reload the control list
	 (might have added myself at a new IP and don't
	 want to restart it...) */
      if (dospeak && cmd == "!CONTROL") {
	load_action_list(ACTION_LIST);
	say((string)"WHO " + DEFCHANNEL);

      }

      if (oper) {
	if (dospeak && cmd == "!SAY") {
	  say((string)"PRIVMSG " + word(message,1) +
	      (string)" :" + 
	      (string)(char*)(1+message.c_str()+
			      message.find(' ',1+ message.find(' ',0))));


	} else if (cmd == "!RAW") {



	}

	if (dospeak && cmd == "!SAVE") {
	  froodsays->savetofile(word(message,1).c_str());
	  /*
	    } else if (cmd == "!WORDS") {
	    froodsays->delete_me(atoi(word(message,1).c_str()));
	  */
	} else if (dospeak && cmd == "!INFO") {
	  int xxx = atoi(word(message,2).c_str())?:5;
	  say((string)"PRIVMSG " + word(message,1) + string(" :") +
	      froodsays->info(xxx));
	} else {
	  if (dospeak && (uint)-1 == message.find("ACTION") && 
	      message[0] != '\001' && 
	      message[0] != '!') { 
	    froodsays->addphrase(message);
	    lastsaid = message;
	  }
	}
      }
    } else if (first == "MODE") {

      cout << "[MODE] " << in << iendl;

      string chn = word(in,2);
      string mode = word(in,3);
      int j = 4;
      int dir = 1; /* assume + */
      for(unsigned int i = 0; i < mode.length(); i++) {
	switch(mode[i]) {
	case '+':
	  dir = 1;
	  break;
	case '-':
	  dir = 0;
	  break;
	case 'o':
	  
	  /* was it me opped/deopped? */
	  if (word(in,j) == DEFNICK) {

	    if (!dir) {
	      /* attempt to use chanserv to get ops back. */
	      
	      cout << "[MODE] I was de-opped!!" << iendl;
	      say ((string)"PRIVMSG ChanServ :OP " + chn + (string)" " + DEFNICK);
	    } else {

	      /* when I get ops, list users on the channel so that I can op them if
		 they need it. */

	      cout << "[MODE] Opped. Checking for unopped friends..." << iendl;
	      say ((string)"WHO " + chn);

	    }

	  }

	  j++;
	  break;

	  /* consume a word, but ignore */
	case 'b':
	case 'v':
	  j++;
	  break;
	default: ;
	  /* ?? */
	}
      }

    } else if (first == "NOTICE") {
      cout << in << iendl;
      /* check if nickserv is complaining for me to register. */

      if (word(in,0).substr(0, 10) == ":NickServ!" &&
	  word(in,3) == ":please") {
	
	cout << "[NOTICE]: registering with nickserv." << iendl;
	say((string)"PRIVMSG Nickserv :identify " + NICKPASS);

      }

      /* 
	 -NickServ- This nickname is registered and protected.  If it is your
 
	 -NickServ- nick, type /msg NickServ IDENTIFY password.  Otherwise,
 
	 -NickServ- please choose a different nick.
      */

    } else if (first == "TOPIC") {
      current_topic = lastcolons(1,in);
      cout << "TOPIC -> " << current_topic << iendl;
    } else if (first == "QUIT") {
      cout << " << " << word(in,0) << " quits ("
	   << lastcolons(1,in) << ")" << iendl;
    } else if (first == "JOIN") {
      cout << " >> " << word(in,0) << " joins "
	   << word(in,2) << iendl;
    } else if (first == "PART") {
      cout << " >> " << word(in,0) << " leaves "
	   << word(in,2) << iendl;
    } else  cout << "?[" << first << "]: " << in << iendl;

    if (first == "JOIN") {
      if (should_op(word(in,0))) {
	string channel = (char*)(word(in,2).c_str()+1);
	string nick    = word(in,0);
	nick = nick.substr(1,nick.find("!",0)-1);
	say ((string)"MODE " + channel + (string)" +o " +nick);
	cout << " << OP >> " << word(in,0) << iendl;
      }
    }
  } /* numeric or not? */
}

int should_op(string s) {
  //  cout << "should_op(" << s << ")?" << iendl;
  for(action_node * temp = controllist;temp;temp = temp->next) {
    //  cout << '[' << temp->wildcard << ']' << iendl;
    if ((temp->action == ACTION_OP || temp->action == ACTION_EAT)
	&& wmatch(temp->wildcard,s)) return 1;
  }
  return 0;
}

int eatme(string s) {
  for(action_node * temp = eatlist;temp;temp = temp->next) {
    //  cout << '[' << temp->wildcard << ']' << iendl;
    if ((temp->action == ACTION_EAT)
	&& wmatch(temp->wildcard,s)) return 1;
  }
  return 0;
}
