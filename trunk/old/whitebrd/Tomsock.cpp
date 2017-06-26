// Tomsock.cpp: implementation of the Tomsock class.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "whitebrd.h"
#include "Tomsock.h"
#include "whitebrdDoc.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

Tomsock::Tomsock() {
	queue = qtail =0;
	iqueue = iqtail = 0;
	connected = 0;
	waitingforconnection = 0;
	doc = 0;
}

Tomsock::~Tomsock() {
	if (connected) Close();
}

void Tomsock::init() {
	Create();
}

void Tomsock::OnConnect(int ecode) {
	switch (ecode) {
	case 0:
		// ok!
		//MsgBox ("Connected!","ERROR",MB_OK);
		doc->madeconnection();
		connected=1;
		break;
	default:
		//MsgBox ("Unable to connect!","ERROR",MB_OK);
		connected=0;
	}
}

void MsgBox(const char*a,const char*b=0,int c=MB_OK) {
	CWnd crap;
	crap.MessageBox(a,b,c);
}

int Tomsock::sendmsg(string s) {
	// add to the queue.

	if (!connected) return SEND_ERROR;

	if (!queue) {
		// nothing in outqueue. Try to send immediately:
		int a;
		if (0 > (a = Send(s.c_str(),s.length(),0))) {
			switch (a) {
			case WSAEINPROGRESS:
			case WSAENOBUFS:
			case WSAEWOULDBLOCK:
				// couldn't send, but not the end of the world:
				addtoqueue(s);
				return SEND_QUEUED;
			default:
				// disconnect.
				Close();
				connected=0;
				return SEND_ERROR;
			}
		
		} else if (a < s.length()) {
			// didn't send the whole thing!
			addtoqueue((char*)(s.c_str() + a));
			return SEND_QUEUED;
		} else return SEND_SENT;
	} else {
		// add directly to queue:
		addtoqueue(s);
		return SEND_QUEUED;
	} 
	
}

void Tomsock::addtoqueue(string s) {
	// adds a string s to the output queue.
	stringq * newn = new stringq(s,0);
	if (!queue) queue = newn;
	else qtail->next = newn;
	qtail = newn;
}

void Tomsock::clearqueue(){
	while (queue) {
		stringq * tmp = queue->next;
		delete queue;
		queue = tmp;
	}
	queue = qtail = 0;

	while (iqueue) {
		stringq * tmp = iqueue->next;
		delete iqueue;
		iqueue = tmp;
	}
	iqueue = iqtail = 0;

}

void Tomsock::OnSend(int n) { // Callback for CAsyncSocket
	if (n) {
		Close();
		connected=0;
		clearqueue();
		return;
	}

	// ready to send some data!
	int cont = 1;
	while (queue) {
		// got something to send, actually.
		int a;
		if (0 > (a = Send(queue->st.c_str(),queue->st.length(),0))) {
			switch (a) {
			case WSAEINPROGRESS:
			case WSAENOBUFS:
			case WSAEWOULDBLOCK:
				// couldn't send... again?
				return;
			default:
				// disconnect.
				Close();
				connected=0;
				return;
			}
		} else if (a < queue->st.length()) {
			// didn't send the whole thing!
			queue->st = ((char*)(queue->st.c_str()+a));
			return; // (don't try to send more?)
		} else {
			// sent it OK!
			stringq *oldnode = queue;
			if (queue == qtail) {
				// one item:
				queue=qtail=0;
			}
			else { queue = queue -> next; }
			delete oldnode;
		}
	} 
}

string Tomsock::getmsg() {
	if (!iqueue) return "";
	string retme = iqueue->st;
	stringq *oldnode = iqueue;
	if (iqueue == iqtail) {
		// one item:
		iqueue=iqtail=0;
	}
	else { iqueue = iqueue -> next; }
	delete oldnode;	
	return retme;
}

void Tomsock :: OnReceive (int e) {
	static char inbuf[1024];
	static string saveme;
	int smut;
	if (e) {
		Close();
		connected=0;
		clearqueue();
	return;
	}

	for (;;) {

  smut = Receive(inbuf,1023,0);
  if (!smut) {
	Close();
	connected=0;
	clearqueue();	  
  } else if (smut < 0) {
    switch (GetLastError()) {
    case WSAEWOULDBLOCK: // means no string.
		
	return;
    default:
      Close();
		connected=0;
		clearqueue();
    return;
	}
  }
  // got some data. null-terminate it.

  inbuf[smut]=0;
  for (int x=0; x < smut; x++) {
    if (inbuf[x] == '\n') {
      addtoreturnqueue(saveme);
      saveme = "";
    } else saveme += inbuf[x];
  }

	}
}

void Tomsock::addtoreturnqueue(string s) {
	// adds a string s to the output queue.
	stringq * newn = new stringq(s,0);
	if (!iqueue) iqueue = newn;
	else iqtail->next = newn;
	iqtail = newn;
}
#if 0
int Tomsock::beserver(int port) {
	// be a server!
	Close();
	clearqueue();
	connected=0;

	if (!Bind(port)) {
		MsgBox("Can't Bind!");
		return 0;
	}
	
	if (!Listen(1)) { // just one connection here.
		MsgBox("Can't Listen!");
		return 0;
	}

	waitingforconnection=1;
}

void onAccept(int ecode) {

	if (!waitingforconnection) return;
	
	if (ecode) {
		waitingforconnection = 0;
		MsgBox("onAccept error code.");
		return;
	}

		

}
#endif