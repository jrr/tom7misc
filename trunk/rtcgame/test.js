
const SERVER_URL = 'http://spacebar.org/f/a/rtcgame';
const XSSI_HEADER = ")]}'\n";
// From url, etc?
const ROOM_NAME = 'test';

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
  for (o in params) {
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


// A peer represents my (possibly pending) connection with
// another player. We try to get one for each player in the game.
const PeerState = Object.freeze({
  // Unrecoverable error state
  BROKEN: 0,
  
  // Called CreateOffer. Waiting to 
  WAIT_CREATE_OFFER: 1,


});

// Unfortunately there are two ways we may become connected: We
// initiated the connection, or someone initiated a connection to us.
const PeerType = Object.freeze({
  CALLER: 1,
  RECEIVER: 2,
});

class Peer {
  constructor(peerType) {
    this.peerType = peerType;
    
    switch (peerType) {
    case PeerType.RECEIVER:
      this.connection = new RTCPeerConnection();
      this.sendChannel = connection.createDataChannel("sendChannel");
      this.sendChannel.onopen = e => this.todoInfo(e);
      this.sendChannel.onclose = e => this.todoInfo(e);
      break;
    case PeerType.CALLER:
      this.connection = new RTCPeerConnection();
      // I guess we will receive a data channel from the other side
      this.ondatachannel = e => this.todoInfo(e);
      break;
    default:
      throw 'bad PeerType?';
    }

    // The UID for this player, a websafe string.
    // Note that we may not have one yet, in the case that this is
    // our outstanding offer. (Maybe it would be better to keep that/those
    // separate from the peer list, which could be a map of uid -> peer)?
    this.uid = '';
    
    this.connection.onicecandidate = e => {
      let cand = e.candidate;
      if (cand) {
	console.log('local ice candidate: ' + cand);
	// I think this is a notification that *I* have a new ice
	// candidate, and my job is to deliver it to the remote
	// peer, who then calls addIceCandidate with it.
      }
      // throw 'implement this';
    };

    if (peerType == PeerType.CALLER) {
      this.state = PeerState.WAIT_CREATE_OFFER;
      this.offerPromise =
	  this.connection.createOffer().
	  then(offer => this.connection.setLocalDescription(offer)).
	  then(() => this.offerReady(this.connection.localDescription)).
	  catch(e => this.todoError(e));
    }
  }

  offerReady(desc) {
    if (this.state != PeerState.WAIT_CREATE_OFFER)
      throw 'Got offer when not in WAIT_CREATE_OFFER state';
      
    if (desc.type != 'offer')
      throw 'Expected an offer-type description?';
    let enc = encodeSdp(desc.sdp);
    console.log('Got description: ' + enc);

    // Now here we want to 
    
    this.state = PeerState.POST_OFFER;
  }

  todoInfo(e) {
    console.log('TODO Info: ' + e);
  }
  
  todoError(e) {
    console.log('TODO Error: ' + e);
    this.state = PeerState.BROKEN;
  }

  // Called periodically.../?
  tick() {
    
  }
};
let peers = [];

// If non-null, an offer to deliver to the server during the poll
// call.
let offerToSend = null;
let roomUid = '';
let myUid = '';
let mySeq = '';

function doPoll() {
  // Must have already joined.
  if (myUid === '' ||
      mySeq === '' ||
      roomUid === '')
    throw 'precondition';
  
  let params = {};
  if (offerToSend != null) {
    params['offer'] = offerToSend;
    // Consume it.
    offerToSend = null;
  }

  // Don't spam the server: Only retry polling once the promise completes.
  requestJSON(SERVER_URL + '/poll/' + myUid + '/' + mySeq, params).
      then(json => {
	// Process response...
	console.log('XXX poll response');
	console.log(json);

	// PERF: Reduce timeout in some situations?
	setTimeout(doPoll, 1000);
      }).
      catch(() => {
	console.log('XXX poll error.');

	// XXX restart polling? regen offer?
      });
}

function startPolling() {
  doPoll();
}

function doJoin() {
  requestJSON(SERVER_URL + '/join/' + ROOM_NAME, {}).
      then(json => {
	roomUid = json.room;
	myUid = json.uid;
	mySeq = json.seq;
	console.log('joined!')
	console.log(json);

	startPolling();
      });
}


var cfg = {'iceServers': [{'url': 'stun:23.21.150.121'}]},
  con = { 'optional': [{'DtlsSrtpKeyAgreement': true}] }


function connect() {
  let p = new Peer(PeerType.CALLER);
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
