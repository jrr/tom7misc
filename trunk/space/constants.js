
var DEBUG = false; // true;

var WIDTH = 320;
var HEIGHT = 200;
var PX = 3;
var MINFRAMEMS = 9.0;

var PHASE_TITLE = 1;
var PHASE_CUTSCENE = 2;
var PHASE_GAME = 3;
// and end, ...

var FONTW = 9;
var FONTH = 16;
var FONTOVERLAP = 1;
var FONTCHARS = " ABCDEFGHIJKLMNOPQRSTUVWXYZ" +
    "abcdefghijklmnopqrstuvwxyz0123456789`-=" +
    "[]\\;',./~!@#$%^&*()_+{}|:\"<>?";

var LFACEX = 26;
var FACEX = 31;
var FACEY = 11;

var INVW = 8;
var INVH = 3;

const INVICON = {x:3, y:174, w:52, h:24};

var INVX = 13;
var INVY = 34;
var INVTITLEX = 18;
var INVTITLEY = 35;

var INVCONTENTSX = INVX + 3;
var INVCONTENTSY = 50;
var INVITEMSIZE = 36;

var INVRECT = {x: INVX, y: INVY, w: 293, h: 127};
var INVCLOSE = {x: 293, y:37, w: 10, h:10};

// Height of top deck; ship is drawn in two parts.
const TOPDECKH = 108;

const MAXSTARS = 500;

const GAMEWIDTH = 6 * WIDTH;

const SCROLLMARGIN = 120;

const VERB_LOOK = 1;
const VERB_GRAB = 2;
const VERB_TALK = 3;
const VERB_OVO = 4;
const VERB_USE = 5;
const VERB_DROP = 6;
function VerbString(v) {
  switch(v) {
  case VERB_LOOK: return "LOOK";
  case VERB_GRAB: return "GRAB";
  case VERB_TALK: return "TALK";
  case VERB_OVO: return "OVOPOSIT";
  case VERB_USE: return "USE";
  case VERB_DROP: return "DROP";
  }
}

const ACTY = 180;

function Action(x, text, verb) {
  this.verb = verb;
  this.x = x;
  this.y = ACTY;
  this.text = text;
  this.w = (FONTW - FONTOVERLAP) * text.length;
  this.h = FONTH;
  return this;
}
const GRABACT = new Action(64, "GRAB", VERB_GRAB);
const TALKACT = new Action(110, "TALK", VERB_TALK);
const OVOACT = new Action(158, "OVOPOSIT", VERB_OVO);
const USEACT = new Action(238, "USE", VERB_USE);
// ?
const DROPACT = new Action(277, "DROP", VERB_DROP);
const ACTIONS = [GRABACT, TALKACT, OVOACT, USEACT];

// Could probably be a function of text length...
const MSGTIME = 80;

// Dimensions of all human parts (graphic itself)
const HUMANW = 42;
const HUMANH = 80;
// from feet to top of image
const HUMANTALL = 74;

const MAXFADEFRAMES = 180;
