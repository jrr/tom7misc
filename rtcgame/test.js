
const SERVER_URL = 'http://spacebar.org/f/a/rtcgame';
const XSSI_HEADER = ")]}'\n";
// From url, etc?
const ROOM_NAME = 'test';
const POLL_MS = 5000;

// XXX debugging
let stop_running = false;

// Approximately 0.
let timeOrigin = window.performance.now();

// ?
let RTCPEER_ARGS = {
  iceServers: [{urls: ['stun:stun.l.google.com:19302']}]
};

/*
  XMLHttpRequest but as a promise.
  Resolve is called with the string containing the response.

  obj argument:
  headers: string list
  method: string; "GET", "POST", etc.
  url: string
  body: string
*/
const request = obj => {
  return new Promise((resolve, reject) => {
    let xhr = new XMLHttpRequest();
    xhr.open(obj.method || "GET", obj.url);
    if (obj.headers) {
      Object.keys(obj.headers).forEach(key => {
        xhr.setRequestHeader(key, obj.headers[key]);
      });
    }
    xhr.onload = () => {
      if (xhr.status >= 200 && xhr.status < 300) {
        resolve(xhr.response);
      } else {
        reject(xhr.statusText);
      }
    };
    xhr.onerror = () => reject(xhr.statusText);
    xhr.send(obj.body);
  });
};

/* Like above, but wrapped to use a standard protocol:
   - Always uses POST
   - params given as {key: value} object, url-encoded
   Expects response to have fixed XSSI header. Parses the json. */
const requestJSON = (url, params) => {
  // Encode a post body suitable for application/x-www-form-urlencoded.
  let kvs = [];
  for (let o in params) {
    kvs.push(encodeURIComponent(o) + '=' + encodeURIComponent(params[o]));
  }

  let obj = {url: url,
	     body: kvs.join('&'),
	     method: 'POST',
	     headers: {'Content-Type': 'application/x-www-form-urlencoded'}};

  return request(obj).
      then(res => {
	if (res.indexOf(XSSI_HEADER) == 0) {
	  let r = res.substr(XSSI_HEADER.length);
	  return JSON.parse(r);
	} else {
	  throw 'no XSSI header in response';
	}
      });
};


// Unfortunately there are two ways we may become connected: We
// initiated the connection, or someone initiated a connection to us.
const MsgType = Object.freeze({
  CHAT: 1,
  PING: 2,
  PING_RESPONSE: 3,
  CONNECTIVITY: 4,
});


// Unfortunately there are two ways we may become connected: We
// initiated the connection, or someone initiated a connection to us.
const PeerType = Object.freeze({
  I_CALL: 1,
  THEY_CALL: 2,
});

// Wrapper around a timestamp and period for implementing
// functionality like window.setTimeout.
class Periodically {
  constructor(periodMs) {
    if (periodMs <= 0) throw 'precondition';
    this.periodMs = periodMs;
    this.nextRun = window.performance.now();
    this.paused = false;
  }

  // Return true if periodMs has elapsed since the last run.
  // If this function returns true, we assume the caller does
  // the associated action now (and so move the next run time
  // forward).
  shouldRun() {
    if (this.paused) return;
    let n = window.performance.now();
    if (n >= this.nextRun) {
      this.nextRun = n + this.periodMs;
      return true;
    }
    return false;
  }

  pause() {
    this.paused = true;
  }

  reset() {
    this.paused = false;
    this.nextRun = window.performance.now() + this.periodMs;
  }
};

// One thing we have to do is decide who is going to call whom.
// We can get into a mess if both sides try to initiate a connection
// at the same time, and then e.g. both abort when it seems the other
// is making the connection! We establish a global ordering using
// uids: The player with the lexicographically earlier uid makes
// the call (reads the other's offer and sends an answer to it).
//
// PERF: Rather than have 0xFFFFF receive all calls, we could use
// some function like "is the peer closer going up (modulo radix)
// or down?" which would keep the call/receive load balanced for
// any given participant.
function getPeerType(puid) {
  if (!myUid) throw 'precondition: must join first!';
  return myUid < puid ? PeerType.I_CALL : PeerType.THEY_CALL;
}

// A Peer is a connection (possibly in progress, or failed) with
// a player.
class Peer {
  // Always have the player's uid when creating a peer, either
  // with the answer or the poll response (which contains all
  // outstanding players).
  constructor(puid) {
    this.puid = puid;
    this.peerType = getPeerType(puid);
    // Initialized by factory function.
    this.connection = null;
    this.channel = null;

    // Set when receiving a ping response.
    // TODO: This can be a time series...
    this.lastPing = Infinity;

    this.periodicallyPing = new Periodically(1000);
  }

  deliverAnswer(answer) {
    if (this.peerType == PeerType.THEY_CALL) {
      if (this.connection == null)
	throw 'in wrong state?';
      this.connection.setRemoteDescription({'type': 'answer',
					    'sdp': answer});
    } else {
      console.log('unimplemented: deliverAnswer when I_CALL');
    }
  }

  // After we've set the local description, this gets called
  // whenever the ice gathering state changes. We're waiting
  // for it to be complete so that we can send a single answer
  // with all ICE candidates.
  //
  // Arguments are the peer and offer uids.
  sendAnswerRemotely(puid, ouid) {
    // Only send a complete answer with all ice candidates.
    if (this.connection.iceGatheringState == 'complete') {
      let desc = this.connection.localDescription;
      if (desc.type != 'answer')
	throw 'Expected an answer-type description?';

      let enc = encodeSdp(desc.sdp);
      let params = {'to': puid, 'o': ouid, 'a': enc};
      /* result is ignored... */
      requestJSON(SERVER_URL + '/answer/' + myUid + '/' + mySeq,
		  params);
      console.log('Sending answer to ' + puid + ' ' + desc.sdp);
    } else {
      console.log('Not sending answer yet because ICE state is ' +
		  this.connection.iceGatheringState);
    }
  }

  // Send a message already in json form, if we have a data
  // channel.
  sendJson(json) {
    if (this.channel &&
	this.channel.readyState === 'open') {
      this.channel.send(json);
    }
  }

  sendMessage(msg) {
    this.sendJson(JSON.stringify(msg));
  }
  
  // Process a message sent BY this peer.
  processMessage(data) {
    let json = JSON.parse(data);
    let now = window.performance.now();
    switch (json['t']) {
      // Handle network timing stuff first.
    case MsgType.PING:
      // All we do for a PING is echo it back to the same peer
      // as a PING_RESPONSE.
      //
      // Treat the payload as number, though. There's no specific
      // risk to echoing back a huge arbitrary payload, but it's not
      // supposed to happen and could be a venue for abuse.
      let p = 0 + json['p'];
      this.sendMessage({'t': MsgType.PING_RESPONSE, 'p': p});
      break;

    case MsgType.PING_RESPONSE:
      // When we get a ping response, we assume it's the last ping
      // we sent, and so the difference between now and the timestamp
      // therein is the round trip latency.
      let ms = now - (0 + json['p']);
      this.lastPing = ms;
      console.log('ping rtt: ' + ms);
      break;

    case MsgType.CHAT:
      pushChat(this.puid, json['msg']);
      drawChats();
      break;

    case MsgType.CONNECTIVITY:
      let row = json['row']
      let player = players[this.puid];
      if (!player) throw 'players should be superset of peers';
      player.connectivityTo = {};
      for (let ouid in row) {
	let ct = row[ouid];
	// Player learned about this peer before me; add it...
	maybeAddPlayer(ouid);
	// Rebase to my own timeOrigin.
	let a = now - ct.a;
	console.log('atime: ' + a);
	player.connectivityTo[ouid] = {c: ct.c, p: ct.p, a: a};
      }
      break;
    }
  }

  // Called periodically.
  periodic() {
    if (this.channel) {
      if (this.periodicallyPing.shouldRun()) {
	this.sendMessage({'t': MsgType.PING, 'p': window.performance.now()});
      }

      // TODO: Send connectivity info.
    }
  }

};
let peers = {};


// ?
const PlayerType = Object.freeze({
  ME: 1,
  OTHER: 2, 
});

const Connectivity = Object.freeze({
  // Haven't heard anything yet.
  UNKNOWN: 1,
  SELF: 2,
  NEVER_CONNECTED: 3,
  CONNECTED: 4,
  DISCONNECTED: 5,
});

// A player is any UID we know about on the network, including ourselves.
// We can learn about these by polling the server, or from other peers.
//
// No explicit connection to the peer, but they use the same uid key.
let players = {};
class Player {
  constructor(puid) {
    this.playerType = (puid === myUid) ? PlayerType.ME : PlayerType.OTHER;
    this.puid = puid;
    // Map from other uid to object:
    //  { c : Connectivity,
    //    p : number, RTT in msec,
    //    a : number, time that awol began (in msec since time origin) }
    //
    // AWOL begins when we are waiting to hear from a peer (i.e., not
    // actively connecting or connected).
    // It is effectively now() for connected peers.
    this.connectivityTo = {};
  }

  // TODO: I think we should avoid this function, since it doesn't
  // have any good way to compute AWOL time for a player it doesn't
  // know about. Instead, places where we can learn about a new player
  // could just add it to the map.
  getConnectivity(ouid) {
    // Unclear whether we represent this explicitly in the map or
    // rely on this special behavior?
    if (this.puid == ouid) return { c: Connectivity.SELF, p: 0,
				    a: window.performance.now() };
    const ct = this.connectivityTo[ouid];
    if (ct) {
      return ct;
    } else {
      return { c: Connectivity.UNKNOWN, p: Infinity,
	       a: window.performance.now() };
    }
  }
};

function addSelfPlayer() {
  if (myUid == '') throw 'precondition';
  players[myUid] = new Player(myUid);
  updateMyConnectivity();
}

function maybeAddPlayer(puid) {
  if (players[puid]) return;
  players[puid] = new Player(puid);
}

// Updates my own connectivity/ping maps (for the player that corresponds
// to me.)
function updateMyConnectivity() {
  if (myUid == '') throw 'precondition';
  let me = players[myUid];
  if (!me) throw 'precondition'

  // We assume that peers is a subset of players..
  for (puid in peers) {
    if (!players[puid]) throw 'peers should be a subset of players';
  }

  let now = window.performance.now();
  for (puid in players) {
    let peer = peers[puid];
    let player = players[puid];
    if (puid == myUid) {
      // Self treated specially (no peer).
      me.connectivityTo[puid] = { c: Connectivity.SELF, p: 0, a: now };
    } else {
      let peer = peers[puid];

      let good = peer &&
	  peer.connection &&
	  (peer.connection.connectionState == 'connected') &&
	  peer.channel &&
	  (peer.channel.readyState == 'open');
      
      if (good) {
	me.connectivityTo[puid] = { c: Connectivity.CONNECTED,
				    p: peer.lastPing || Infinity,
				    a: now };

      } else {
	// No peer, or the connection is pending/broken.
	// Might be useful to add more fine-grained states here
	// (like trying to connect, waiting for offer, sent answer,
	// etc.)?
	
	let ct = me.connectivityTo[puid];
	if (ct && ct.c == Connectivity.CONNETED) {
	  // If the peer was in connected state, then we update to
	  // DISCONNECTED and set the awol time.
	  //
	  // (Note this requires updateconnectivity to run at least once
	  // while connected. We could set this explicitly when a connection
	  // is made. Or consider very short-lived connections to not be
	  // connections at all, which is probably fine too)
	  me.connectivityTo[puid] = { c: Connectivity.DISCONNECTED,
				      p: Infinity,
				      a: now };
	} else if (ct && (ct.c == Connectivity.DISCONNECTED ||
			  ct.c == Connectivity.NEVER_CONNECTED)) {

	  // Leave in DISCONNECTED or NEVER_CONNECTED states, and
	  // don't update awol time--player is still awol.
	} else {
	  me.connectivityTo[puid] = { c: Connectivity.NEVER_CONNECTED,
				      p: Infinity,
				      a: now };
	}
      }
    }
  }
}

// Share connectivity with all connected peers. Note this is an n^2
// operation, since we send information about all peers to all peers
// (this is even worse if we have old players that we haven't cleaned
// up). But we use this to keep the set of active players from growing
// without bound, and the per-cell payload is small.
function broadcastConnectivity() {
  if (myUid == '') throw 'precondition';
  let me = players[myUid];
  if (!me) throw 'precondition'

  let now = window.performance.now();
  let msg = {};
  for (puid in me.connectivityTo) {
    let ct = me.connectivityTo[puid];
    // Round ping to integer to make these message smaller... peers
    // don't care about sub-millisecond timing.
    let roundp = isFinite(ct.p) ? Math.round(ct.p) : ct.p;
    // Note that awol time here is stored as absoluve (time since
    // timeOrigin) but sent as relative (how long ago). Different
    // peers of course disagree on timeOrigin, and we avoid using unix
    // epoch so that we don't have to worry about clock skew / NTP /
    // etc.
    let awolSec = Math.round(now - ct.a);
    msg[puid] = { 'c': ct.c,
		  'p': roundp,
		  'a': awolSec };
  }
  
  let json = JSON.stringify({'t': MsgType.CONNECTIVITY, 'row': msg});
  for (let k in peers) {
    let peer = peers[k];
    peer.sendJson(json);
  }
}

// A they-call peer is created from my outstanding listen-connection.
// (Maybe the listen-connection should be wrapped in an object so that
// we can just pass it all here and it can retain any handlers?)
function createTheyCallPeer(puid, conn, channel) {
  if (puid in peers) throw 'precondition';
  if (getPeerType(puid) != PeerType.THEY_CALL) throw 'precondition';
  let peer = new Peer(puid);
  if (conn == null) throw 'precondition';
  if (channel == null) throw 'precondition';
  peer.connection = conn;
  peer.channel = channel;
  peer.channel.onmessage = e => {
    console.log('message on channel');
    console.log(e);
    peer.processMessage(e.data);
  };
  peers[puid] = peer;
  console.log('Created they-call peer ' + puid);
  return peer;
}

// An I-call peer is created from an offer sdp and its uid. We create
// the connection and send an answer to the server.
function createICallPeer(puid, offer, ouid) {
  if (puid in peers) throw 'precondition';
  if (getPeerType(puid) != PeerType.I_CALL) throw 'precondition';
  if (offer === '') throw 'precondition';
  if (ouid === '') throw 'precondition';
  let peer = new Peer(puid);
  peer.connection = new RTCPeerConnection(RTCPEER_ARGS);
  let conn = peer.connection;
  // Should be member function...
  conn.ondatachannel = e => {
    console.log('icall.datachannel'); console.log(e);
    if (e.type === 'datachannel') {
      console.log('got data channel!');
      peer.channel = e.channel;
      peer.channel.onmessage = e => {
	console.log('message on channel');
	console.log(e);
	peer.processMessage(e.data);
      };
    }
  };

  // XXX probably don't need this
  conn.onicecandidate = e => {
    console.log('icall.icecandidate'); console.log(e);
  };

  console.log('Try setting remote description to ' + offer);
  conn.setRemoteDescription({'type': 'offer', 'sdp': offer}).
      then(() => conn.createAnswer()).
      then(answer => conn.setLocalDescription(answer)).
      then(() => {
	// Is it possible for this to be complete already? If so,
	// act on it now.
	if (conn.iceGatheringState == 'complete') {
	  peer.sendAnswerRemotely(puid, ouid);
	} else {
	  conn.onicegatheringstatechange =
	      e => peer.sendAnswerRemotely(puid, ouid);
	}
      });

  console.log('Created I-call peer ' + puid);
  peers[puid] = peer;
  return peer;
}

function getPeerByUid(puid) {
  if (puid in peers) {
    return peers[puid];
  }
  return null;
}

// Initialized upon joining, and then stays the same for the length
// of the session.
let roomUid = '';
let myUid = '';
let mySeq = '';

// TODO: Make this stuff into a class.
// TODO: Add visualization of offer-creation state to UI.
// If non-null, an offer to deliver to the server during the poll
// call.
let offerToSend = null;
// Connection corresponding to the outstanding offer.
let listenConnection = null;
let sendChannel = null;
let makingOffer = false;

// Asynchronously create an offer; initializes offerToSend and
// listenConnection upon success.
//
// TODO: Although it does seem to manage connectivity, this should
// also wait for all the ice candidates to arrive before posting
// the offer.
function makeOffer() {
  if (makingOffer) return;

  makingOffer = true;
  listenConnection = null;
  let lc = new RTCPeerConnection(RTCPEER_ARGS);
  sendChannel = lc.createDataChannel("sendChannel");
  lc.onicecandidate = e => { console.log('icecandidate'); console.log(e); };
  // XXX figure this out -- can we set it up after promoting this
  // connection to a Peer?
  sendChannel.onopen = e => { console.log('channel.onopen'); console.log(e); };
  sendChannel.onclose = e => { console.log('channel.onclose'); console.log(e); };

  return lc.createOffer().
      then(offer => lc.setLocalDescription(offer)).
      then(() => {
	// Is it possible for this to be complete already? If so,
	// act on it now.
	if (lc.iceGatheringState == 'complete') {
	  markOfferReady(lc);
	} else {
	  // Otherwise, wait for the ICE candidates.
	  lc.onicegatheringstatechange = e => markOfferReady(lc);
	}
      }).
      catch(e => {
	listenConnection = null;
	sendChannel = null;
	makingOffer = false;
	todoError(e);
      });
}

function markOfferReady(conn) {
  if (conn.iceGatheringState == 'complete') {
    let desc = conn.localDescription;
    if (desc.type != 'offer')
      throw 'Expected an offer-type description?';
    console.log('Got offer description: ' + desc.sdp);
    let enc = encodeSdp(desc.sdp);
    console.log('Encoded: ' + enc);
    offerToSend = enc;
    listenConnection = conn;
    makingOffer = false;
    // Consider polling immediately?
  }
}

function todoInfo(e) {
  console.log('TODO Info');
  console.log(e);
}

function todoError(e) {
  console.log('TODO error');
  console.log(e);
}

let periodicallyPoll = new Periodically(POLL_MS);
let periodicallyUpdateUi = new Periodically(100);
let periodicallyShareConnectivity = new Periodically(1000);
function uPeriodic() {
  // PERF: We can perhaps avoid long frames by only doing one
  // of these periodic actions per frame (but have to be a little
  // fancy to avoid starvation).
  if (periodicallyPoll.shouldRun()) {
    doPoll();
  }

  // On some delay? Or rename this to uPeriodic?
  for (let k in peers)
    peers[k].periodic();

  if (periodicallyShareConnectivity.shouldRun()) {
    if (myUid !== '') {
      updateMyConnectivity();
      broadcastConnectivity();
    }
  }
  
  if (periodicallyUpdateUi.shouldRun()) {
    // Perhaps only if dirty? 10x a second is ridiculous
    updateUI();
  }

  if (!stop_running)
    window.setTimeout(uPeriodic, 15);
}

function doPoll() {
  // Must have already joined.
  if (myUid === '' ||
      mySeq === '' ||
      roomUid === '')
    return;

  let params = {};
  if (offerToSend != null) {
    params['offer'] = offerToSend;
    // Consume it.
    offerToSend = null;
  }

  // Don't spam the server: Only retry polling once the promise completes.
  periodicallyPoll.pause();
  
  requestJSON(SERVER_URL + '/poll/' + myUid + '/' + mySeq, params).
      then(json => {
	// Process response...
	console.log('parsed poll response');
	console.log(json);
	processPollResponse(json);
	// Allow polling again.
	periodicallyPoll.reset();
      }).
      catch(e => {
	console.log('XXX poll error.');
	console.log(e);
	// XXX restart polling? regen offer?
	// Perhaps increase timeout..?
	periodicallyPoll.reset();
      });
}

function processPollResponse(json) {
  // Process answers first (before creating a new offer).
  // The first one to answer gets to take on the listeningConnection
  // as its connection.
  let answers = json['answers'];
  for (let answer of answers) {
    let puid = answer['uid'];
    let sdp = decodeSdp(answer['s']);

    let peer = getPeerByUid(puid);
    if (peer == null) {
      // Answer from unknown peer. This is normal when a peer
      // connects to us using our offer before we find out
      // about it.

      // (can be forced by a misbehaving peer, but should
      // not normally happen...)
      if (getPeerType(puid) != PeerType.THEY_CALL)
	throw 'peer should not call me';

      if (listenConnection == null ||
	  sendChannel == null) {
	// Already used up our listening connection, like if
	// two peers try to connect to the same offer.
	console.log('peer ' + puid + ' tried to connect but ' +
		    'listening channel is null');
	continue;
      }

      peer = createTheyCallPeer(puid, listenConnection, sendChannel);
      listenConnection = null;
      sendChannel = null;
      offerToSend = null;
    }
    peer.deliverAnswer(sdp);
  }

  let others = json['others'];
  for (let other of others) {
    let puid = other['puid'];
    maybeAddPlayer(puid);
    let peer = getPeerByUid(puid);
    console.log('other ' + puid + ' peer: ' + peer);
    if (peer == null) {
      // Learned about a new player. This is normal when someone new
      // joins, or when joining a room that already has players.
      let peerType = getPeerType(puid);
      switch (peerType) {
      case PeerType.THEY_CALL:
	// If they call, we can actually leave the peer out of our
	// peer set, and it is covered by the "answer from unknown peer"
	// case above.
	// TODO: Is this actually better? Somehow it seems like it
	// would be useful to know about all the peers.
	continue;
	break;

      case PeerType.I_CALL:
	// If I call, and there is an offer available, act on it.
	let encodedOffer = other['s'];
	let ouid = other['ouid'];
	if (encodedOffer !== '' && ouid !== '') {
	  let offer = decodeSdp(encodedOffer);
	  peer = createICallPeer(puid, offer, ouid);
	}
	break;
      }
    }
  }

  // If the server knows of no offer, kick off creation of
  // a new one.
  if (!json['haveoffer']) makeOffer();
}

function doJoin() {
  let elt = document.getElementById('intro');
  elt.style.display = 'none';
 
  requestJSON(SERVER_URL + '/join/' + ROOM_NAME, {}).
      then(json => {
	roomUid = json['room'];
	myUid = json['uid'];
	mySeq = json['seq'];
	console.log('joined!')
	console.log(json);

	addSelfPlayer();
	
	makeOffer();
      });

  // Start loop.
  uPeriodic();
}


// TODO: Can reduce space/bandwidth on server by having a custom
// encoder for SDPs built into the JS code. If we do this we
// probably want some version info in the encoded SDP?
function encodeSdp(sdp) {
  const b64 = btoa(sdp);
  return b64.replace(/[+]/g, '_').replace(/[/]/g, '.');
}

function decodeSdp(enc) {
  let b64 = enc.replace(/[.]/g, '/').replace(/_/g, '+');
  return atob(b64);
}


// UI stuff...

function makeElement(what, cssclass, elt) {
  var e = document.createElement(what);
  if (cssclass) e.setAttribute('class', cssclass);
  if (elt) elt.appendChild(e);
  return e;
}
function IMG(cssclass, elt) { return makeElement('IMG', cssclass, elt); }
function DIV(cssclass, elt) { return makeElement('DIV', cssclass, elt); }
function SPAN(cssclass, elt) { return makeElement('SPAN', cssclass, elt); }
function BR(cssclass, elt) { return makeElement('BR', cssclass, elt); }
function TABLE(cssclass, elt) { return makeElement('TABLE', cssclass, elt); }
function TR(cssclass, elt) { return makeElement('TR', cssclass, elt); }
function TD(cssclass, elt) { return makeElement('TD', cssclass, elt); }
function TEXT(contents, elt) {
  var e = document.createTextNode(contents);
  if (elt) elt.appendChild(e);
  return e;
}

function updateUI() {
  let uelt = document.getElementById('uid');
  uelt.innerHTML = myUid == '' ? '(not yet assigned)' : myUid;

  updatePeersUI();
  updateMatrixUI();
}
  
function updatePeersUI() {
  let elt = document.getElementById('peers');
  elt.innerHTML = '';

  let table = TABLE('peers', elt);
  let hdr = TR('', table);
  TEXT('uid', TD('', hdr));
  TEXT('type', TD('', hdr));
  TEXT('conn state', TD('', hdr));
  TEXT('ice state', TD('', hdr));
  TEXT('ice gathering state', TD('', hdr));
  TEXT('channel', TD('', hdr));
  TEXT('rtt', TD('', hdr));
  for (let k in players) {
    let player = players[k];
    let tr = TR('', table);

    let peer = peers[k];
    TEXT(player.puid, TD(peer ? 'peeruid' : 'nopeeruid', tr));    
    
    if (peer) {
      let s = peer.peerType + ' = ' +
	  ((peer.peerType === PeerType.I_CALL) ? 'I call' : 'They call');
      TEXT(s, TD('', tr));
      TEXT((peer.connection ? peer.connection.connectionState : 'null'),
	   TD('', tr));
      TEXT((peer.connection ? peer.connection.iceConnectionState : 'null'),
	   TD('', tr));
      TEXT((peer.connection ? peer.connection.iceGatheringState : 'null'),
	   TD('', tr));
      TEXT((peer.channel ? peer.channel.readyState : 'null'),
	   TD('', tr));
      TEXT('' + peer.lastPing + ' ms', TD('', tr));
    } else {
      TD('', tr).colSpan = 6;
    }
  }
}

function updateMatrixUI() {
  let elt = document.getElementById('matrix');
  elt.innerHTML = '';
  let mtx = TABLE('matrix', elt);

  let hdr = TR('', mtx);
  // corner
  TEXT('src \\ dest', TD('', hdr));
  for (let k in players) {
    TEXT(k.substr(0, 3), TD('', hdr));
  }

  let now = window.performance.now();
  for (let src in players) {
    let tr = TR('', mtx);
    TEXT(src, TD('', tr));
    let p = players[src];
    for (let dst in players) {
      let ct = p.getConnectivity(dst);
      let cell = TD('cell', tr);
      // NARROW NO-BREAK SPACE
      // let txt = '\u202f';
      let txt =
	  (isFinite(ct.a) && (now - ct.a) > 0.1) ?
	  ((now - ct.a) / 1000.0).toFixed(1) :
	  '\u202f';
      switch (ct.c) {
      case Connectivity.UNKNOWN:
	cell.style.backgroundColor = '#CCC';
	break;
      case Connectivity.SELF:
	cell.style.backgroundColor = '#FFF';
	break;
      case Connectivity.NEVER_CONNECTED:
	cell.style.backgroundColor = '#AA5';
	break;
      case Connectivity.CONNECTED:
	cell.style.backgroundColor = '#5A5';
	// U+221E INFINITY
	txt = isFinite(ct.p) ? (ct.p | 0) : '\u221e';
	break;
      case Connectivity.DISCONNECTED:
	cell.style.backgroundColor = '#A55';
	break;
      }
      TEXT(txt, cell);
      // cell.classList.add();
    }
  }
  
}

let MAX_CHATS = 32;
let chats = [];
function pushChat(uid, msg) {
  if (chats.length == MAX_CHATS) {
    chats.shift();
  }
  chats.push({uid: uid, msg: msg});
}

function drawChats() {
  let elt = document.getElementById('chats');
  elt.innerHTML = '';
  for (chat of chats) {
    let cr = DIV('', elt);
    TEXT('<' + chat.uid + '>', SPAN('chat-uid', elt));
    TEXT(chat.msg, SPAN('chat-msg', elt));
  }
}

function broadcastChat(msg) {
  // Send to self.
  pushChat(myUid, msg);
  let json = JSON.stringify({'t': MsgType.CHAT, 'msg': msg});
  for (let k in peers) {
    let peer = peers[k];
    peer.sendJson(json);
  }
}

// Chat crap
function chatKey(e) {
  if (e.keyCode == 13) {
    let elt = document.getElementById('chatbox');
    let msg = elt.value;
    elt.value = '';

    broadcastChat(msg);
    
    // XXX send it!
    drawChats();
  }
}

function init() {
  let elt = document.getElementById('chatbox');
  elt.onkeyup = chatKey;
}

// Debugging crap.

function stop() {
  // XXX debugging thing
  stop_running = true;
}

function anyPeer() {
  for (let k in peers) return peers[k];
  return null;
}


