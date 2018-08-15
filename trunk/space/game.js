
let CHEATS = false;

let counter = 0, skipped = 0;
let start_time = (new Date()).getTime();

let show_areas = false;

// Last place we saw the mouse. Scaled to the 320x200 screen that
// is the canvas.
let mousex = 0;
let mousey = 0;

// no y-scrolling.
let scrollx = 400;

// Number of elapsed frames in the current scene.
let frames = 0;

// If null, no active sentence.
// Otherwise, we have
//  { verb: (one of the VERB enums)
//    obj1: (use OBJ1 with OBJ2)
//    obj2: (if necessary) }
// obj can be Ent or Item, or ...
let sentence = null;

// Script that asynchronously runs
let script = [];
// If true, script blocks player inputs etc.
let synchronous = false;

// To support the inventory management constraints, when you pick
// up an object you have to place it into inventory. If this is
// non-null, we are currently trying to pick up an item.
// The item either came from the world (worldx,y) or inventory (invx,y).
// { item: the item being picked up. it is detached from the world / inv.
//   worldx, worldy: coordinates item will return to if canceled (or null)
//   invx, invy: same, for inventory}
let grabitem = null;

let areas = [];

// Finale
let fadeout = false;
let fadeframes = MAXFADEFRAMES;

const resources = new Resources(
  ['font.png',
   'spacefont.png',
   'title.png',
   'background.png',

   'bug.png',
   
   'ship.png',
   
   'face-right.png',
   'face-right-blink.png',
   'player1.png',
   'player2.png',
   'player3.png',
   'playerreachup.png',
   'oviposit1.png',
   'oviposit2.png',
   'oviposit3.png',
   
   'humanshadow.png',
   'humanback.png',
   'humanlegs.png',
   'humanhead.png',
   'humanfront.png',   

   'humanwalk1.png',
   'humanwalk2.png',
   'humanwalk3.png',
   'humanwalk4.png',

   'fire1.png',
   'fire2.png',
   'fire3.png',
   'fire4.png',
   
   'grate.png',
   'grateopen.png',
   'grateguydeath1.png',
   'grateguydeath2.png',
   'grateguydeath3.png',
   'grateguybody.png',
   
   'inv-icon.png',
   'inventory.png',
   'inv-used.png',
   'inv-conflict.png',
   'inv-ok.png',

   'energy.png',
   'invenergy.png',
   
   'screwdriver.png',
   'invscrewdriver.png',
   
   'id.png',
   'invid.png',

   'bulge.png',
   'bulgeopen.png',

   'airlocktool.png',
   'invairlocktool.png',
  
   'airlockdoorfg.png',
   'airlockdoor.png',
   'airlockdooropen.png',
   
   'release.png',
   'releaseopen.png',
   
   'nervous1.png',
   'nervous2.png',
   'airdying.png',
   'airbody.png',
   'airbody2.png',

   'airlockclosed1.png',
   'airlockclosed2.png',
   'airlockclosed3.png',
   'airlockopen1.png',
   'airlockopen2.png',
   
   'invextinguisher.png',
   'extinguisher.png',
   
   'invegg.png',

   'bridgedoorfg.png',

   'captaindeath1.png',
   'captaindeath2.png',
   'captaindeath3.png',
   'captaindeath4.png',
   'captaindeath5.png',

   'captainbody1.png',
   'captainbody2.png',
   'captainbody3.png',
   
  ],
  [], null);

function XY(x, y) { return '(' + x + ',' + y + ')'; }

const STARCOLORS = [
  // B G R
  0xFFFFFF,
  0x00FFFF,
  0xFFFF00,
  0xFF0000,
  0xFFAAAA,
  0xAAFFFF
];

// Rectangular region of "ground" where the player can walk.
// Connected to adjacent areas, to allow for rudimentary planning
// between them.
// x2,y2 not included in the area.
function Area(x, y, x2, y2) {
  this.x = x;
  this.y = y;
  this.x2 = x2;
  this.y2 = y2;
  this.w = x2 - x;
  this.h = y2 - y;
  this.e = [];
  this.idx = null;
  // enabled by default, but some turn themselves off at start
  this.enabled = true;
  return this;
}

function AreaFor(x, y) {
  // PERF need not be linear.
  for (let a of areas) {
    if (x >= a.x && x < a.x2 &&
	y >= a.y && y < a.y2)
      return a;
  }
  return null;
}

// For an entity at srcx/srcy, plan a route to dstx/dsty.
// The route will consist of a series of waypoints which lie
// on the edges of areas, except for maybe the final point.
// null if no path.
function Route(srcx, srcy, dstx, dsty) {
  let srca = AreaFor(srcx, srcy);
  if (!srca) return null;
  let dsta = AreaFor(dstx, dsty);
  if (!dsta) return null;

  if (!dsta.enabled) return null;
  
  if (srca == dsta) {
    // console.log('same-area route');
    return [{x:dstx, y: dsty}];
  }
 
  // XXX Should do a shortest path thing here.
  // In the presence of cycles, this can do
  // dumb stuff like walk around an obstacle to
  // get to a place I'm right next to.
  let visited = {};

  let RouteRec = (ar) => {
    if (visited[ar.idx]) return null;
    visited[ar.idx] = true;
    if (!ar.enabled) return null;
    if (ar == dsta) return [];
    for (let e of ar.e) {
      let r = RouteRec(e.other);
      if (r != null) {
	r.push({x: e.x, y: e.y});
	return r;
      }
    }
    return null;
  };

  let path = RouteRec(srca);
  if (path == null) return null;
  path.reverse();
  path.push({x: dstx, y: dsty});
  return path;
}

function Init() {
  window.font = new Font(resources.Get('font.png'),
                         FONTW, FONTH, FONTOVERLAP, FONTCHARS);
  window.hifont = new Font(
    EzRecolor(resources.Get('font.png'), 0xFFFFFFFF, 0xFF00FFFF),
    FONTW, FONTH, FONTOVERLAP, FONTCHARS);
  
  window.spacefont = new Font(resources.Get('spacefont.png'),
                              FONTW, FONTH, FONTOVERLAP, FONTCHARS);
  window.hispacefont = new Font(
    EzRecolor(resources.Get('spacefont.png'), 0xFFFFFFFF, 0xFF00FFFF),
    FONTW, FONTH, FONTOVERLAP, FONTCHARS);
  
  window.titleframes = Static('title.png');
  window.background = Static('background.png');
  let ship = resources.Get('ship.png');
  window.topdeckbg = new Frames(EzCropY(ship, 0, TOPDECKH));
  window.botdeckbg = new Frames(EzCropY(ship, TOPDECKH, 200 - TOPDECKH));

  window.starframes = [];
  for (let i = 0; i < 16; i++) {
    window.starframes.push(new EzStar(STARCOLORS[i % STARCOLORS.length]));
  }
  
  // UI elements
  window.inv_icon = EzFrames(['inv-icon', 1]);
  window.inventory = EzFrames(['inventory', 1]);
  window.invused = EzFrames(['inv-used', 1]);
  // TODO: Could strobe
  window.invconflict = EzFrames(['inv-conflict', 1]);
  window.invok = EzFrames(['inv-ok', 1]);
  
  // window.playerr = FlipFramesHoriz(window.playerl);
  // window.playerr_run = FlipFramesHoriz(window.playerl_run);
  
  // Audio tweaks.
  // song_theme.multiply = 1.5;
  // song_power.multiply = 1.35;

  // song_menu[0].volume = 0.65;
}

// Cancel the attempt to deposit an item in inventory. It's returned
// to the world or where it used to be in inventory.
function ResetGrabitem() {
  if (grabitem == null)
    return;

  if (grabitem.worldx != null) {
    grabitem.item.worldx = grabitem.worldx;
    grabitem.item.worldy = grabitem.worldy;
  } else {
    grabitem.item.invx = grabitem.invx;
    grabitem.item.invy = grabitem.invy;
  }
  
  grabitem = null;
}

function GrabitemInv() {
  if (grabitem == null) return null;
  // Cursor goes in the center of the object.
  let x = mousex - (INVITEMSIZE * grabitem.item.cellsw) / 2;
  let y = mousey - (INVITEMSIZE * grabitem.item.cellsh) / 2;

  let invx = Math.round((x - INVCONTENTSX) / INVITEMSIZE);
  let invy = Math.round((y - INVCONTENTSY) / INVITEMSIZE);
  if (invx >= 0 && invy >= 0 &&
      invx < INVW && invy < INVH)
    return {invx: invx, invy: invy};
  return null;
}

// Return list of cells (absolute inventory cells like {x:0, y:0})
// that the grabitem would use if placed at invx,invy.
function GrabitemCells(invx, invy) {
  let item = grabitem.item;
  let cs = [];
  for (var y = 0; y < item.cellsh && y + invy < INVH; y++) {
    for (var x = 0; x < item.cellsw && x + invx < INVW; x++) {
      if (item.MaskAt(x, y)) {
        cs.push({x: x + invx, y: y + invy});
      }
    }
  }
  return cs;
}

// Can we actually place the grabitem at this position?
function GrabitemOK(invx, invy) {
  let item = grabitem.item;
  // Off map
  if (invx < 0 || invy < 0) return false;
  if (invy + item.cellsh > INVH) return false;
  if (invx + item.cellsw > INVW) return false;

  // Conflicts with existing.
  for (var y = 0; y < item.cellsh && y + invy < INVH; y++) {
    for (var x = 0; x < item.cellsw && x + invx < INVW; x++) {
      if (item.MaskAt(x, y) && InvUsed(invx + x, invy + y) != null) {
        console.log('conflict at ', x, y);
        return false;
      }
    }
  }
  return true;
}

// Item that can go in inventory.
// mask is assumed to be minimal (no blank outer rows/colums)
function Item(name, invframes, worldframes, mask) {
  this.name = name;
  // Frames when in inventory
  this.invframes = EzFrames(invframes);
  this.worldframes = EzFrames(worldframes);
  this.openframes = null;
  this.mask = mask;

  // General purpose alternative "open" state.
  this.open = false;
  
  // Position of top-left in inventory, or null if detached
  this.invx = null;
  this.invy = null;

  // Position in world (global coordinates, not screen), or null.
  // (Position is of top-left coordinate)
  // Can't have both world position and inventory position,
  // but an unspawned item could have both be null.
  this.worldx = null;
  this.worldx = null;

  // If non-null, the player needs to go to this exact coordinate
  // before interacting with the item (except LOOK / TALK).
  this.actionx = null;
  this.actiony = null;
  
  // width/height in world. derived from first frame.
  this.worldw = this.worldframes.width;
  this.worldh = this.worldframes.height;
  
  this.lookstring = null;

  this.grabbable = true;

  this.haseggs = false;
  
  // Compute inventory cell bounding box, using mask.
  let maxw = 0;
  for (let row of mask) maxw = Math.max(row.length, maxw);
  this.cellsw = maxw;
  this.cellsh = mask.length;
      
  return this;
}

Item.prototype.LookString = function() {
  if (this.lookstring) return this.lookstring;
  return "SOME ALIEN TECHNOLOGY?";
}

Item.prototype.MaskAt = function(x, y) {
  if (x < 0 || y < 0) return false;
  let row = this.mask[y] || '';
  let cell = row[x] || ' ';
  return cell != ' ';
};

// Do the action and be done immediately.
function ScriptDo(fn) {
  this.fn = fn;
  this.Done = function () {
    this.fn();
    return true;
  };
  return this;
}

// Delay keeps firing? Did you set sentence = null??
// In frames
function ScriptDelay(fr) {
  this.fr = fr;
  this.Done = function () {
    this.fr = this.fr - 1;
    if (this.fr <= 0) return true;
  };
  return this;
}

// Wait until function returns true
function ScriptAny(fn) {
  this.Done = fn;
  return this;
}

function ScriptGoto(ent, x, y) {
  this.ent = ent;
  this.x = x;
  this.y = y;
  this.init = false;
  this.Done = function () {
    // First time we're called, initialize
    if (!this.init) {
      let r = Route(this.ent.worldx, this.ent.worldy, this.x, this.y);
      if (r == null) {
	console.log('emergency warp!');
	this.ent.route = [];
	this.ent.worldx = this.x;
	this.ent.worldy = this.y;
      } else {
	this.ent.route = r;
      }
      this.init = true;
    }

    // Assume done when route is empty
    return this.ent.route.length == 0;
  };
  return this;
}

function ScriptSay(ent, s) {
  this.ent = ent;
  this.s = s;
  this.init = false;
  this.Done = function () {
    if (!this.init) {
      ent.Say(s);
      this.init = true;
    }

    return this.ent.msgq.length == 0;
  }
  return this;
}

function InitGame() {

  scrollx = 5 * WIDTH;

  window.dropscene = false;
  
  window.phase = PHASE_GAME;

  window.fgs = [];
  let fgs = window.fgs;
  fgs.push({f: EzFrames(['airlockdoorfg', 1]),
	    x: 523, y: 15});
  fgs.push({f: EzFrames(['bridgedoorfg', 1]),
	    x : 1685, y: 16});
  
  window.ents = {};
  let ents = window.ents;
  ents.grateguy = Human();
  ents.grateguy.worldx = 1724;
  ents.grateguy.worldy = 90;
  ents.grateguy.grabbable = true;
  ents.grateguy.actionx = 1100;
  ents.grateguy.actiony = 146;
  ents.grateguy.deathanim = EzFrames(['grateguydeath1', 20,
				      'grateguydeath2', 30,
				      'grateguydeath3', 500]);

  ents.airguy = Human();
  ents.airguy.nervousframes = EzRL(['nervous1', 3,
				    'nervous2', 3]);
  ents.airguy.deathanim = EzFrames(['airdying', 2]);
  ents.airguy.worldx = 494;
  ents.airguy.worldy = 89;

  ents.airguy2 = Human();
  ents.airguy2.nervousframes = EzRL(['nervous2', 2,
				     'nervous1', 3]);
  ents.airguy2.deathanim = EzFrames(['airdying', 2]);
  ents.airguy2.worldx = 471;
  ents.airguy2.worldy = 94;

  // ents.airguy.nervous = true;
  
  ents.captain = Human();
  ents.captain.worldx = 1810;
  ents.captain.worldy = 94;
  ents.captain.actionx = 1740;
  ents.captain.actiony = 90;
  ents.captain.deathanim = EzFrames(['captaindeath1', 6,
				     'captaindeath2', 6,
				     'captaindeath3', 6,
				     'captaindeath4', 6,
				     'captaindeath5', 500]);
  
  window.player = Player();
  player.worldx = 1765;
  player.worldy = 160;
  
  script = [
    new ScriptDelay(110),
    new ScriptGoto(ents.captain, 1812, 92),
    new ScriptSay(ents.captain, "What's that on radar?"),
    new ScriptGoto(ents.grateguy, 1741, 96),
    new ScriptSay(ents.grateguy, "A ship...?"),
    new ScriptDo(() => { ents.captain.facingleft = true; }),
    new ScriptDelay(58),
    new ScriptDo(() => { ents.captain.facingleft = false; }),
    new ScriptSay(ents.captain, "It could be an alien race."),
    new ScriptDelay(16),
    new ScriptSay(ents.grateguy, "Should we raise shields?"),
    new ScriptDo(() => { ents.captain.facingleft = true; }),
    new ScriptDelay(58),
    new ScriptDo(() => { ents.captain.facingleft = false; }),
    new ScriptSay(ents.captain, "No, remember Rule Zero:"),
    new ScriptSay(ents.captain, "We must not interfere..."),
    new ScriptDelay(30),
    // go to grate.
    new ScriptGoto(ents.grateguy, 1139, 91),
    // drop card...?
  ];
  
  ents.player = window.player;
                       
  const MASK2x2 = ['**',
                   '**'];
  const MASK1x1 = ['*'];

  window.inventoryopen = false;

  window.items = {};
  let items = window.items;
  items.egg1 = new Item('EGG', ['invegg', 1], ['invegg', 1], MASK2x2);
  items.egg2 = new Item('EGG', ['invegg', 1], ['invegg', 1], MASK2x2);
  items.egg3 = new Item('EGG', ['invegg', 1], ['invegg', 1], MASK2x2);
  items.egg4 = new Item('EGG', ['invegg', 1], ['invegg', 1], MASK2x2);
  for (let egg of [items.egg1,
                   items.egg2,
                   items.egg3,
                   items.egg4]) {
    egg.lookstring = "A PRECIOUS BABY.";
  }
  
  // Start with four eggs in inventory.
  items.egg1.invx = 0;
  items.egg1.invy = 0;
  items.egg2.invx = 2;
  items.egg2.invy = 0;
  items.egg3.invx = 4;
  items.egg3.invy = 0;
  items.egg4.invx = 6;
  items.egg4.invy = 0;
    
  items.id = new Item('CARD',
                      ['invid', 1],
                      ['id', 1],
                      MASK1x1);

  items.id.lookstring = "PLASTIC IN GOLDEN RATIO ASPECT";
  items.id.worldx = 1110;
  items.id.worldy = 136;
  // XXX actionx
  
  items.extinguisher = new Item('CYLINDER',
				['invextinguisher', 1],
				['extinguisher', 1],
				['**']);

  items.extinguisher.lookstring = "PERHAPS A WEAPON";
  items.extinguisher.worldx = 1693;
  items.extinguisher.worldy = 121;
  items.extinguisher.actionx = 1724;
  items.extinguisher.actiony = 148;
  
  items.fire = new Item('FIRE',
			['bug', 1],
			['fire1', 3,
			 'fire2', 3,
			 'fire3', 4,
			 'fire4', 3],
			[]);
  items.fire.worldx = 1604;
  items.fire.worldy = 132;
  items.fire.grabbable = false;
  items.fire.actionx = 1662;
  items.fire.actiony = 158;

  items.grate = new Item('GRATE',
			 ['bug', 1],
			 ['grate', 1],
			 []);
  items.grate.worldx = 1102;
  items.grate.worldy = 86;
  items.grate.grabbable = false;
  items.grate.actionx = 1102;
  items.grate.actiony = 146;
  items.grate.openframes = EzFrames(['grateopen', 1]);
  
  items.grateguybody = new Item('BODY',
				['bug', 1],
				['grateguybody', 1],
				[]);
  items.grateguybody.worldx = null;
  items.grateguybody.worldy = null;
  items.grateguybody.grabbable = false;
  
  items.screwdriver = new Item('SCREWDRIVER',
			       ['invscrewdriver', 1],
			       ['screwdriver', 1],
			       ['***']);

  items.bulge = new Item('BULGE',
			 ['bug', 1],
			 ['bulge', 1],
			 []);
  items.bulge.worldx = 1333;
  items.bulge.worldy = 55;
  items.bulge.actionx = 1336;
  items.bulge.actiony = 82;
  items.bulge.openframes = EzFrames(['bulgeopen', 1]);
  items.bulge.open = false;
  items.bulge.grabbable = false;
  
  items.airlocktool = new Item('TOOL',
                               ['invairlocktool', 1],
                               ['airlocktool', 1],
                               [' **',
                                ' * ',
                                '** ']);
  items.airlocktool.lookstring = "WIGGLY METAL";
  // items.airlocktool.worldx = 1180;
  // items.airlocktool.worldy = 70;

  items.airlock = new Item('AIRLOCK',
			   ['bug', 1],
			   ['airlockclosed1', 3,
			    'airlockclosed2', 3,
			    'airlockclosed3', 5,
			    'airlockclosed2', 3],
			   []);
  items.airlock.worldx = 377;
  items.airlock.worldy = 28;
  items.airlock.open = false;
  items.airlock.openframes = EzFrames(['airlockopen1', 6,
				       'airlockopen2', 6]);
  items.airlock.grabbable = false;
  
  // Outside airlock
  items.release1 = new Item('PORT',
			   ['bug', 1],
			   ['release', 1],
			    []);
  items.release1.openframes = EzFrames(['releaseopen', 1]);
  items.release1.worldx = 562;
  items.release1.worldy = 36;
  items.release1.grabbable = false;
  items.release1.open = false;
  items.release1.actionx = 599;
  items.release1.actiony = 75;

  // Inside airlock
  items.release2 = new Item('PORT',
			   ['bug', 1],
			   ['release', 1],
			   []);
  items.release2.openframes = EzFrames(['releaseopen', 1]);
  items.release2.worldx = 490;
  items.release2.worldy = 36;
  items.release2.grabbable = false;
  items.release2.open = false;
  items.release2.actionx = 482;
  items.release2.actiony = 77;

  items.airbody = new Item('BODY',
			   ['bug', 1],
			   ['airbody', 1],
			   []);
  items.airbody.grabbable = false;
  items.airbody.haseggs = false;
  items.airbody.worldx = null;

  items.airbody2 = new Item('BODY',
			    ['bug', 1],
			    ['airbody2', 1],
			    []);
  items.airbody2.grabbable = false;
  items.airbody2.haseggs = false;
  items.airbody2.worldx = null;
  
  items.airlockdoor = new Item('DOOR',
			       ['bug', 1],
			       ['airlockdoor', 1],
			       []);
  items.airlockdoor.openframes = EzFrames(['airlockdooropen', 1]);
  items.airlockdoor.grabbable = false;
  items.airlockdoor.worldx = 533;
  items.airlockdoor.worldy = 37;
  items.airlockdoor.open = false;
  
  items.energy = new Item('NRG WEAPON',
			  ['invenergy', 1],
			  ['energy', 1],
			  ['****',
			   '  **',
			   ' ***']);
  items.energy.worldx = 529;
  items.energy.worldy = 113;
  items.energy.actionx = 541;
  items.energy.actiony = 147;

  items.captainbody = new Item('BODY',
			       ['bug', 1],
			       ['captainbody1', 6,
				'captainbody2', 6,
				'captainbody3', 6],
			       []);
  items.captainbody.worldx = null;
  items.captainbody.worldy = null;
  items.captainbody.grabbable = false;
  
  console.log('initialized game');
}

// Entity in game that can walk around, speak, etc.
// Includes the player character.
function Ent(name, wd, ht) {
  this.name = name;
  
  this.halfwidth = wd >>> 1;
  this.height = ht;

  this.xspeed = 2;
  this.yspeed = 1;
  
  // If non-null, location in the world.
  this.worldx = null;
  this.worldy = null;

  // If non-empty, we're walking to this series of
  // target spots, from front to back.
  this.route = [];

  // Just the grate guy.
  this.grabbable = false;

  this.actionx = null;
  this.actiony = null;
  
  this.facingleft = false;

  this.lookstring = null;

  this.sayfont = font;
  this.msgq = [];
  this.msgtime = 0;
  
  return this;
}

Ent.prototype.Say = function(s) {
  console.log(s);
  this.msgq.push(s);
  this.msgtime = MSGTIME;
};

Ent.prototype.LookString = function() {
  if (this.lookstring) return this.lookstring;
  return "AN ALIEN?";
};

// Separate because it should be above everything.
// Coordinates are the top center of the entity.
Ent.prototype.DrawText = function(x, y) {
  if (this.msgq.length > 0) {
    let str = this.msgq[0];
    // Need to support multiline messages I think...
    let strw = str.length * (FONTW - FONTOVERLAP);
    let fx = x - (strw >>> 1);
    if (fx < 0) fx = 0;
    this.sayfont.Draw(ctx, fx, y - FONTH - 4, str);
  }
}
  
Ent.prototype.DrawFacing = function(fr, x, y) {
  DrawFrame(this.facingleft ? fr.l : fr.r, x, y);
}
  
function Player() {
  let p = new Ent('ME', 68, 46);
  p.stand = EzRL(['player1', 1]);
  p.move = EzRL(['player1', 9,
                 'player2', 2,
                 'player3', 6,
                 'player2', 2]);
  p.face = EzRL(['face-right', 280,
                 'face-right-blink', 2,
                 'face-right', 68,
                 'face-right-blink', 2]);
  p.reachingupframes = EzRL(['playerreachup', 1]);
  p.oviframes = EzRL(['oviposit1', 6,
		      'oviposit2', 6,
		      'oviposit3', 8,
		      'oviposit2', 6,
		      'oviposit1', 600]);
  p.sayfont = window.spacefont;
  p.lookstring = "IT ME :-)";
  p.reachingup = false;
  p.ovipositing = false;
  p.Draw = function(x, y) {
    const moving = this.route.length > 0;

    let baseframes = moving ? this.move : this.stand;
    if (this.ovipositing) {
      // No face.
      this.DrawFacing(this.oviframes, x, y);
    } else if (this.reachingup) {
      // Just during grate anim.
      this.DrawFacing(this.reachingupframes, x, y);
    } else {
      this.DrawFacing(baseframes, x, y);
    }
    if (!this.ovipositing) {
      DrawFrame(this.facingleft ? this.face.l : this.face.r,
		x + (this.facingleft ? LFACEX : FACEX), y + FACEY);
    }
  };
  return p;
}

// Regular function, not constructor.
function Human() {
  let human = new Ent('ALIEN', HUMANW, HUMANTALL);

  human.shadow = EzRL(['humanshadow', 1]);
  human.backarm = EzRL(['humanback', 1]);
  human.legs = EzRL(['humanlegs', 1]);
  human.walklegs = EzRL(['humanwalk1', 6,
			 'humanwalk2', 6,
			 'humanwalk3', 6,
			 'humanwalk4', 6]);
  human.head = EzRL(['humanhead', 1]);
  human.front = EzRL(['humanfront', 1]);

  human.lookstring = "LOOKS WARM";
  human.dying = false;
  
  human.Draw = function(x, y) {
    if (this.dying) {
      DrawFrame(this.deathanim, x, y);
    } else if (this.nervous) {
      this.DrawFacing(this.nervousframes, x, y);
    } else {
      const moving = this.route.length > 0;

      let stack = [this.shadow, this.backarm,
		   moving ? this.walklegs : this.legs,
		   this.head, this.front];
      for (let f of stack) {
	this.DrawFacing(f, x, y);
      }
    }
  };
  return human;
}

// Returns null if nothing, otherwise, the item.
function InvUsed(x, y) {
  for (let i in items) {
    let item = items[i];
    if (item.invx != null) {
      if (item.MaskAt(x - item.invx, y - item.invy))
        return item;
    }
  }
  return null;
}

let stars = [];

function SpawnStar() {
  return {x: GAMEWIDTH, y : 0 | (Math.random() * (HEIGHT - 1)),
          // Not integral
          dx: -0.1 + (Math.random() * -4.0) };
}

function DrawStars() {
  for (let i = 0; i < stars.length; i++) {
    let star = stars[i];
    if (star) {
      let x = (0 | star.x) - scrollx;
      if (x >= 0 && x < WIDTH) {
        DrawFrame(window.starframes[i % window.starframes.length],
                  x, star.y);
      }
    }
  }
}
function UpdateStars() {
  for (let i = 0; i < stars.length; i++) {
    if (stars[i] == null) {
      stars[i] = SpawnStar();
    } else {
      let star = stars[i];
      star.x += star.dx;
      if (star.x < 0)
        stars[i] = null;
    }
  }
}

function InitStars() {
  for (let i = 0; i < MAXSTARS; i++) {
    let star = SpawnStar();
    star.x = Math.random() * GAMEWIDTH;
    star.dx = -0.1 + (Math.random() * -4.0);
    stars.push(star);
  }
}

function DrawItemsWhen(cond) {
  // Draw items in the world.
  for (let o in items) {
    let item = items[o];
    if (item.worldx != null && cond(item)) {
      if (item.open && item.openframes) {
	DrawFrame(item.openframes,
                  item.worldx - scrollx,
                  item.worldy);
      } else {
	DrawFrame(item.worldframes,
                  item.worldx - scrollx,
                  item.worldy);
      }
    }
  }
}

function DrawEntsWhen(cond) {
  for (let o in ents) {
    let ent = ents[o];
    if (ent.worldx != null && cond(ent)) {
      ent.Draw(ent.worldx - ent.halfwidth - scrollx,
               ent.worldy - ent.height);
    }
  }
}

// Try extending the current sentence with a noun (entity or item)
function ExtendSentence(noun) {
  if (sentence == null) {
    return { verb: VERB_LOOK, obj1 : noun, obj2 : null };
  } else if (sentence.obj1 == null) {
    return { verb: sentence.verb, obj1 : noun, obj2 : null };
  } else {
    if (sentence.verb == VERB_OVI ||
        sentence.verb == VERB_USE && sentence.obj2 == null) {
      return { verb: sentence.verb, obj1 : sentence.obj1, obj2 : noun };
    }
  }
  return null;
}

// Get the sentence we would construct with a click at screen
// coordinates x,y.
function GetSentenceAt(x, y) {
  let globalx = x + scrollx;

  // No way to form sentences while grabbing.
  if (grabitem != null)
    return;
  
  // If we have no sentence and no grabitem, we can always pick a verb
  if (sentence == null && y > ACTY) {
    for (let act of ACTIONS) {
      if (InRect(mousex, mousey, act)) {
        return { verb: act.verb, obj1: null, obj2: null };
        return;
      }
    }
  }

  if (window.inventoryopen) {

    if (sentence != null &&
	(sentence.verb == VERB_USE ||
	 sentence.verb == VERB_OVI ||
	 sentence.verb == VERB_LOOK)) {
      let invx = Math.floor((mousex - INVCONTENTSX) / INVITEMSIZE);
      let invy = Math.floor((mousey - INVCONTENTSY) / INVITEMSIZE);
      if (invx >= 0 && invy >= 0 &&
          invx < INVW && invy < INVH) {
	let item = InvUsed(invx, invy);
	if (item != null)
	  return ExtendSentence(item);
      }
      return null;
    }
    
  } else {
    // In an item?
    for (let o in items) {
      let item = items[o];
      if (item.worldx != null &&
          InCoords(globalx, y,
                   item.worldx, item.worldy,
                   item.worldw, item.worldh)) {
        // see if we can add to sentence
        let sent = ExtendSentence(item);
        if (sent != null) return sent;
      }
    }

    // How about an entity?
    for (let o in ents) {
      let ent = ents[o];
      if (ent.worldx != null &&
          InCoords(globalx, y,
                   ent.worldx - ent.halfwidth,
                   ent.worldy - ent.height,
                   ent.halfwidth * 2,
                   ent.height)) {
        let sent = ExtendSentence(ent);
        if (sent != null) return sent;
      }
    }
  }
    
  return null;
}

function GetSentence(s) {
  // Nice to show this with colors since it's in space language :)
  let ll = [];
  let OneObject = (v) => {
    if (s.obj1 != null)
      return [v, s.obj1.name];
    else
      return [v];
  };

  let TwoObjects = (v, c) => {
    if (s.obj2 != null)
      return [v, s.obj1.name, c, s.obj2.name];
    else if (s.obj1 != null)
      return [v, s.obj1.name, c];
    else
      return [v];
  };

  switch(s.verb) {
  case VERB_LOOK: ll = OneObject("LOOK AT"); break;
  case VERB_GRAB: ll = OneObject("GRAB"); break;
  case VERB_TALK: ll = OneObject("TALK TO"); break;
  case VERB_OVI: ll = TwoObjects("OVIPOSIT", "INTO"); break;
  case VERB_USE: ll = TwoObjects("USE", "WITH"); break;
  case VERB_DROP: ll = OneObject("DROP"); break;
  }
  return ll;
}

// Draw the sentence (argument) at the top of the screen.
// Note this is also used when hovering, which happens before
// the sentence is actually saved.
let lastsentencestring = '';
function DrawSentence(s) {
  if (s == null) return;
  
  let ll = GetSentence(s);
  let x = 1;
  let str = '';
  for (let i = 0; i < ll.length; i++) {
    let ph = ll[i];
    str += ph + ' ';
    ((i % 2 == 1) ? hispacefont : spacefont).Draw(ctx, x, 1, ph);
    x += (ph.length + 1) * (FONTW - FONTOVERLAP);
  }
  if (str != lastsentencestring) {
    console.log(str);
    lastsentencestring = str;
  }
}

function DrawGame() {
  ClearScreen();
  DrawStars();

  DrawFrame(window.botdeckbg, -scrollx, TOPDECKH);

  // draw inventory icon
  // TODO: hover/open state?
  DrawFrame(window.inv_icon, INVICON.x, INVICON.y);

  for (let act of ACTIONS) {
    // XXX conditions for highlighting? are they always clickable?
    let canhighlight = sentence == null && grabitem == null;
    let f = (canhighlight && InRect(mousex, mousey, act)) ?
        hispacefont : spacefont;
    f.Draw(ctx, act.x, act.y, act.text);
  }
    
  // font.Draw(ctx, 1, 1, "USE ID WITH CARD READER");
  let nsent = GetSentenceAt(mousex, mousey);
  // Show the current sentence if executing a synchronous script though.
  if (!synchronous && nsent != null)
    DrawSentence(nsent);
  else
    DrawSentence(sentence);

  // Objects need to be clipped by the top deck, so only
  // draw lower deck items first...
  DrawItemsWhen((item) => item.worldy > TOPDECKH);
  DrawEntsWhen((ent) => ent.worldy > TOPDECKH);

  DrawFrame(window.topdeckbg, -scrollx, 0);
  
  DrawItemsWhen((item) => item.worldy <= TOPDECKH);
  DrawEntsWhen((ent) => ent.worldy <= TOPDECKH);

  for (let f of window.fgs) {
    DrawFrame(f.f, f.x - scrollx, f.y);
  }
  
  if (show_areas) {
    for (let area of areas) {
      ctx.fillStyle =
	  area.enabled ? 'rgba(0,200,0,.25)' : 'rgba(200,0,0,.25)';
      ctx.strokeStyle = '#00F';
      ctx.fillRect(area.x - scrollx, area.y, area.w, area.h);
      ctx.strokeRect(area.x - scrollx, area.y, area.w, area.h);

      for (e of area.e) {
	// console.log('edge to ', e.other, e.x, e.y);
	ctx.fillStyle = '#F00';
	ctx.fillRect(e.x - scrollx, e.y, 1, 1);
      }
    }
  }

 
  
  if (window.inventoryopen) {
    // Above game stuff: Inventory
    let pos = GrabitemInv();
    let grabcells = pos == null ?
        [] : GrabitemCells(pos.invx, pos.invy);
    let IsGrabCell = (x, y) => {
      for (let c of grabcells)
        if (c.x == x && c.y == y)
          return true;
      return false;
    };
    
    DrawFrame(window.inventory, INVX, INVY);
    for (let y = 0; y < INVH; y++) {
      for (let x = 0; x < INVW; x++) {
        let gc = IsGrabCell(x, y);
        if (InvUsed(x, y) != null) {
          let iframe = gc ? invconflict : invused;
          DrawFrame(iframe, 
                    INVCONTENTSX + INVITEMSIZE * x,
                    INVCONTENTSY + INVITEMSIZE * y);
        } else {
          if (gc) {
            DrawFrame(window.invok,
                      INVCONTENTSX + INVITEMSIZE * x,
                      INVCONTENTSY + INVITEMSIZE * y);
          }
        }
      }
    }

    for (let o in items) {
      let item = items[o];
      if (item.invx != null) {
        DrawFrame(item.invframes,
                  INVCONTENTSX + INVITEMSIZE * item.invx,
                  INVCONTENTSY + INVITEMSIZE * item.invy);
      }
    }

    // TODO: could highlight filled inventory slots.

    spacefont.Draw(ctx, INVTITLEX, INVTITLEY, "INVENTORY");
  }

  // (XXX only player text should go above inventory...)
  // (XXX or close inventory when we speak?)
  for (let o in ents) {
    let ent = ents[o];
    if (ent.worldx != null && ent.msgq.length > 0) {
      ent.DrawText(ent.worldx - scrollx,
                   ent.worldy - ent.height);
    }
  }

  // Item currently being grabbed is like part of the mouse cursor,
  // so it's always on top.
  if (grabitem) {
    DrawFrame(grabitem.item.invframes,
              mousex - (INVITEMSIZE * grabitem.item.cellsw) / 2,
              mousey - (INVITEMSIZE * grabitem.item.cellsh) / 2);
  }
    
  // Unmute button?

  if (fadeout) {
    let f = Math.max(fadeframes, 0);
    let a = (1.0 - (f / MAXFADEFRAMES)).toFixed(2);
    ctx.fillStyle = 'rgba(0,0,0,' + a +')';
    ctx.fillRect(0, 0, WIDTH + 1, HEIGHT + 1);

    if (f == 0) {
      spacefont.Draw(ctx, WIDTH * 0.15, HEIGHT * 0.3,
		     "RUNNING OUT OF SPACE");

      spacefont.Draw(ctx, WIDTH * 0.4, HEIGHT * 0.6,
		     "TOM 7   2018");
    }
  }
}

function DrawTitle() {
  DrawFrame(window.titleframes, 0, 0);
  spacefont.Draw(ctx, WIDTH * 0.35, HEIGHT * 0.5,
                 "RUNNING OUT OF SPACE");
}

function Draw() {
  switch (window.phase) {
    case PHASE_TITLE:
    DrawTitle();
    break;
    // case PHASE_CUTSCENE:
    // DrawCutscene();
    break;
    case PHASE_GAME:
    DrawGame();
    break;
  }
}

// Act on the current sentence. It has to be complete.
// Some sentences (LOOK AT ME) can always be executed
// instantly, but many require the player to get close
// to the object in question.
//
// In this case, we keep the sentence around, but set
// a target for the player entity if possible.
function DoSentence() {
  if (sentence == null)
    return;
  // Every verb needs a first object, at least.
  if (sentence.obj1 == null)
    return;

  // Any special casing should go here.

  // Make it easier to just click this thing to see it.
  if (sentence.verb == VERB_LOOK &&
      sentence.obj == items.bulge &&
      !items.bulge.open) {
    sentence.verb = VERB_GRAB;
  }
  
  // Returns true if we're there and can proceed.
  let Goto = (obj) => {
    // Can do from anywhere.
    if (obj.actionx == null)
      return true;
    
    // Are we already at the spot?
    if (player.worldx == obj.actionx &&
	player.worldy == obj.actiony) {
      return true;
    } else {
      let r = Route(player.worldx, player.worldy,
		    obj.actionx, obj.actiony);
      if (r == null) {
	player.Say("I CAN'T GO THERE :-(");
	sentence = null;
	return false;
      }
      player.route = r;
      return false;
    }
  };
  
  switch (sentence.verb) {
  case VERB_GRAB: {
    let obj = sentence.obj1;

    if (!Goto(obj))
      return;
    
    if (obj instanceof Item) {
      if (obj.worldx == null) {
        player.Say("HOW...?");
        sentence = null;
        return;
      }

      if (obj.grabbable) {
	window.inventoryopen = true;
	grabitem = { item: obj,
                     worldx : obj.worldx,
                     worldy : obj.worldy };
	obj.worldx = null;
	obj.worldy = null;
	sentence = null;
      } else if (obj == items.bulge && !items.bulge.open) {
	items.bulge.open = true;
	// spawn tool
	items.airlocktool.worldx = 1346;
	items.airlocktool.worldy = 68;
	items.airlocktool.actionx = 1353;
	items.airlocktool.actiony = 94;

      } else {
	player.Say("I CAN'T PICK IT UP");
	sentence = null;
      }
      return;
    }

    if (obj == ents.grateguy) {
      player.facingleft = false;
      // should arrange for this to be false, but..
      ents.grateguy.facingleft = false;
      // XXX might be possible to do this before he gets
      // to grate..?
      synchronous = true;
      script = [
	new ScriptDo(() => {
	  // Put player in reach-up state.
	  player.reachingup = true;
	}),
	new ScriptDelay(10),
	new ScriptDo(() => {
	  // Wow hax, but otherwise this can start in the middle
	  // of the animation...
	  frames = 0;
	  ents.grateguy.dying = true;
	}),
	new ScriptDelay(60),
	new ScriptDo(() => {
	  player.reachingup = false;
	  // Despawn grate guy.
	  ents.grateguy.worldx = null;
	  // Spawn grate guy body.
	  items.grateguybody.worldx = 1120;
	  items.grateguybody.worldy = 138;
	  items.grateguybody.actionx = 1138;
	  items.grateguybody.actiony = 150;
	  // Spawn screwdriver.
	  items.screwdriver.worldx = 1194;
	  items.screwdriver.worldy = 144;
	  items.screwdriver.actionx = 1194;
	  items.screwdriver.actiony = 144;

	  // Better if this happened when you see him / talk to
	  // him, but, ...
	  ents.airguy.nervous = true;
	  ents.airguy2.nervous = true;
	}),
	new ScriptSay(player, "DIE!"),
      ];
      
      sentence = null;
    }
    break;
  }
  case VERB_LOOK: {
    let desc = sentence.obj1.LookString();
    player.Say(desc || "DOESN'T LOOK LIKE ANYTHING TO ME");
    sentence = null;
    break;
  }
  case VERB_TALK: {

    if (sentence.obj1 == ents.captain) {
      if (!Goto(sentence.obj1))
	return;

      player.facingleft = false;
      ents.captain.facingleft = true;
      script = [
	new ScriptSay(player, "HI"),
	new ScriptSay(ents.captain, "You are welcome aboard our ship!"),
	new ScriptSay(ents.captain, "Please make yourself at home!"),
      ];
      sentence = null;
      return;
    }

    
    player.Say("HI " + sentence.obj1.name + "! :-)");
    sentence = null;
    break;
  }
  case VERB_OVI:
    if (sentence.obj2 == null)
      return;

    // Go to object 2 first
    if (!Goto(sentence.obj2))
      return;

    if (sentence.obj1.name != "EGG") {
      player.Say("CAN ONLY OVIPOSIT AN *EGG*");
      sentence = null;
      return;
    }

    if (sentence.obj2.name != "BODY") {
      player.Say("MUST OVIPOSIT AN EGG INTO A *BODY*");
      sentence = null;
      return;
    }

    if (sentence.obj2.haseggs) {
      player.Say("THIS BODY ALREADY HAS EGGS.");
      sentence = null;
      return;
    }

    synchronous = true;
    let oldegg = sentence.obj1;
    let body = sentence.obj2;
    script = [
      new ScriptDo(() => {
	frames = 0;
	player.ovipositing = true;
      }),
      new ScriptDelay(40),
      new ScriptDo(() => {
	player.ovipositing = false;
	// Despawn egg from inventory
	oldegg.invx = null;
	oldegg.invy = null;
	// Don't let this happen again
	body.haseggs = true;
	// XXX walk away from body

	if (items.egg1.invx == null &&
	    items.egg2.invx == null &&
	    items.egg3.invx == null &&
	    items.egg4.invx == null) {
	  fadeout = true;
	  fadeframes = MAXFADEFRAMES;
	}
      }),
    ];
    sentence = null;
    
    break;
  case VERB_USE:
    if (sentence.obj1.invx == null) {
      player.Say("I NEED TO *GRAB* IT FIRST.");
      sentence = null;
      break;
    }
    
    if (sentence.obj2 == null)
      return;

    // Go to object 2 first
    if (!Goto(sentence.obj2))
      return;
    
    // Handle combinations that make sense here.
    if (sentence.obj1 == items.extinguisher &&
	sentence.obj2 == items.fire) {
      // animate...
      player.Say("SAFETY FIRST!");
      items.fire.worldx = null;
      items.fire.worldy = null;
      window.firearea.enabled = true;
      sentence = null;
      return;
    }

    if (sentence.obj1 == items.screwdriver &&
	sentence.obj2 == items.grate &&
	!items.grate.open) {
      synchronous = true;
      player.reachingup = true;
      player.facingleft = false;
      script = [
	new ScriptDelay(30),
	new ScriptDo(() => {
	  player.reachingup = false;
	  items.grate.open = true;
	  window.gratearea.enabled = true;
	})];
      sentence = null;
      return;
    }

    if (sentence.obj1 == items.energy &&
	sentence.obj2 == ents.captain) {
      synchronous = true;
      player.facingleft = false;
      ents.captain.facingleft = true;
      script = [
	new ScriptDo(() => {
	  frames = 0;
	  ents.captain.dying = true;
	  // XXXX nice to have player firing anim
	}),
	new ScriptDelay(40),
	new ScriptDo(() => {
	  ents.captain.worldx = null;
	  items.captainbody.worldx = 1796;
	  items.captainbody.worldy = 67;
	  items.captainbody.actionx = 1804;
	  items.captainbody.actiony = 92;
	})
      ];
      sentence = null;
      return;
    }
    
    // Both releases operate in tandem.
    if (sentence.obj1 == items.airlocktool &&
	(sentence.obj2 == items.release1 ||
	 sentence.obj2 == items.release2)) {

      // New open state.
      let open = !items.release1.open;
      // Flip together.
      items.release1.open = open;
      items.release2.open = open;

      // Enable airlock door area, and airlock area
      if (open) {
	// Airlock is open
	airlockarea.enabled = true;
	airlockdoorarea.enabled = false;
	items.airlockdoor.open = false;
	items.airlock.open = true;
      } else {
	// Door to ship is open
	airlockarea.enabled = false;
	airlockdoorarea.enabled = true;
	items.airlockdoor.open = true;
	items.airlock.open = false;
      }
	
      // If this is the first time, launch the human out
      if (ents.airguy.worldx != null) {
	synchronous = true;
	player.facingleft = true;
	script = [
	  new ScriptSay(ents.airguy, "Wh..."),
	  new ScriptDo(() => {
	    ents.airguy.dying = true;
	    ents.airguy2.dying = true;
	  }),
	  new ScriptAny(() => {
	    ents.airguy.msgq = ['Noooo...'];
	    ents.airguy.msgtime = 5;
	    ents.airguy.worldx -= 3;
	    ents.airguy2.worldx -= 4;
	    if (ents.airguy.worldx < 250) {
	      // despawn
	      ents.airguy.worldx = null;
	      ents.airguy2.worldx = null;	      
	      // spawn body
	      items.airbody.worldx = 200;
	      items.airbody.worldy = 110;
	      items.airbody.actionx = 213;
	      items.airbody.actiony = 155;

	      items.airbody2.worldx = 120;
	      items.airbody2.worldy = 80;
	      items.airbody2.actionx = 120;
	      items.airbody2.actiony = 110;
	      
	      // item out here?
	      
	      return true;
	    }
	    return false;
	  })
	];
      }
	
      /*
      synchronous = true;
      player.facingleft = true;
      script = [
	new ScriptDo(() => {
	});
	];
      */
      
      sentence = null;
      return;
    }
    
    
    player.Say("I DON'T KNOW HOW TO DO THAT");
    sentence = null;
    break;
  }
}

function DoScript() {
  while (script.length > 0 && script[0].Done()) {
    script.shift(1);
  }

  if (script.length == 0) {
    synchronous = false;
  }
}

let last = 0;
function Step(time) {
  // Throttle to 30 fps or something we
  // should be able to hit on most platforms.
  // Word has it that 'time' may not be supported on Safari, so
  // compute our own.
  var now = (new Date()).getTime();
  var diff = now - last;
  // debug.innerHTML = diff;
  // Don't do more than 30fps.
  // XXX This results in a frame rate of 21 on RIVERCITY, though
  // I can easily get 60, so what gives?
  if (diff < MINFRAMEMS) {
    skipped++;
    window.requestAnimationFrame(Step);
    return;
  }
  last = now;

  frames++;
  if (frames > 1000000) frames = 0;

  if (window.phase == PHASE_GAME) {
    if (fadeout) {
      fadeframes--;
      if (fadeframes == Math.round(MAXFADEFRAMES / 2)) {
	StartSong(song_theme);
      }
    }

    if (dropscene == false && player.worldx < 1200) {
      dropscene = true;
      script = [
	new ScriptSay(ents.grateguy, "Ugh..."),
	new ScriptDelay(60),
	new ScriptSay(ents.grateguy, "I can't believe I dropped my ID card"),
	new ScriptSay(ents.grateguy, "down the vent again..."),
      ];
    }

    UpdateStars();

    DoSentence();

    DoScript();

    // Update entities.
    for (var o in ents) {
      let ent = ents[o];
      // Timeout text
      if (ent.msgq.length > 0) {
	if (ent.msgtime == 0) {
	  ent.msgq.shift(1);
	  if (ent.msgq.length > 0) {
	    ent.msgtime = MSGTIME;
	  }
	} else {
	  ent.msgtime--;
	}
      }

      // Move towards targets
      if (ent.route.length > 0) {
	let targetx = ent.route[0].x;
	let targety = ent.route[0].y;

	const dx = targetx - ent.worldx
	const dy = targety - ent.worldy;
	// At target?
	if (Math.abs(dx) <= ent.xspeed &&
	    Math.abs(dy) <= ent.yspeed) {
	  ent.worldx = targetx;
	  ent.worldy = targety;
	  ent.route.shift(1);
	  continue;
	}

	// XXX use bresenham
	// XXX avoid obstacles if non-convex?
	if (Math.abs(dx) <= ent.xspeed) {
	  ent.worldx = targetx;
	} else {
	  ent.worldx += dx < 0 ? -ent.xspeed : ent.xspeed;
	  if (dx < 0) ent.facingleft = true;
	  else if (dx > 0) ent.facingleft = false;
	}

	if (Math.abs(dy) <= ent.yspeed) {
	  ent.worldy = targety;
	} else {
	  ent.worldy += dy < 0 ? -ent.yspeed : ent.yspeed;
	}
      }
    }


    // XXX also when on bridge
    // Update here or in Draw?
    let smleft = player.worldx < 655 ? 230 : SCROLLMARGIN;
    let smright = player.worldx < 655 ? 80 : SCROLLMARGIN;
    if (player.worldx - smleft < scrollx) {
      let target = player.worldx - smleft;
      scrollx = Math.round(((scrollx * 7) + target) * 0.125);
      // scrollx--;
    } else if (player.worldx + smright > scrollx + WIDTH) {
      let target = player.worldx + smright - WIDTH;
      // scrollx++;
      scrollx = Math.round(((scrollx * 7) + target) * 0.125);
    }
    if (scrollx < 0) scrollx = 0;
  }
    
  UpdateSong();

  Draw();

  // process music in any state
  // UpdateSong();

  // On every frame, flip to 4x canvas
  bigcanvas.Draw4x(ctx);

  if (DEBUG) {
    counter++;
    var sec = ((new Date()).getTime() - start_time) / 1000;
    document.getElementById('counter').innerHTML =
        'skipped ' + skipped + ' drew ' +
        counter + ' (' + (counter / sec).toFixed(2) + ' fps)';
  }

  // And continue the loop...
  window.requestAnimationFrame(Step);
}

function InitAreas() {
  areas = [];

  areas.push(
    // Space (open)
    new Area(19, 41, 297, 180),
    // Space (above engine)
    new Area(297, 41, 347, 102));

  window.airlockarea = new Area(347, 73, 393, 102);
  window.airlockarea.enabled = false;
  
  areas.push(
    airlockarea,
    // Airlock chamber
    new Area(393, 73, 530, 102)
  );
  
  window.airlockdoorarea = new Area(530, 72, 553, 95);
  airlockdoorarea.enabled = false;
  areas.push(airlockdoorarea);

  areas.push(
    // Upper deck near airlock
    new Area(553, 73, 657, 102),
    new Area(657, 79, 689, 102),
    new Area(689, 73, 977, 102),
    new Area(977, 77, 1010, 102),
    // B full
    new Area(1010, 73, 1297, 102),

    // A full (all box depth)
    new Area(1297, 77, 1669, 102));

  // XXX looks terrible when going up here...
  window.gratearea = new Area(1109, 102, 1132, 139);
  gratearea.enabled = false;
  areas.push(gratearea);

  // Door to bridge. This one should
  // require items etc.?
  window.bridgedoorarea =
      new Area(1669, 83, 1688, 96);
  areas.push(bridgedoorarea);

  areas.push(
    // Bridge
    new Area(1688, 78, 1754, 102),
    // under chair
    new Area(1754, 98, 1786, 102),
    new Area(1786, 80, 1830, 102));
  
  // Bottom floor
  areas.push(
    new Area(398, 158, 438, 169),
    new Area(438, 139, 608, 169),
    new Area(608, 139, 744, 169),
    new Area(744, 139, 821, 146),
    new Area(744, 160, 821, 169),
    new Area(821, 139, 1599, 169));

  // Fire. Require items
  window.firearea = 
      new Area(1599, 139, 1657, 169);
  areas.push(firearea);
  firearea.enabled = false;
  
  // start space in cargo hold
  areas.push(
    new Area(1657, 139, 1821, 169));

  let Midpoint = (ap, bp, ap2, bp2) => {
    let ip = Math.max(ap, bp);
    let ip2 = Math.min(ap2, bp2);
    if (ip >= ip2)
      return null;
    return Math.round((ip + ip2) / 2);
  };

  let AddEdge = (a1, a2, x, y) => {
    a1.e.push({other: a2, x: x, y: y});
    a2.e.push({other: a1, x: x, y: y});
  };
  
  
  for (let i = 0; i < areas.length; i++)
    areas[i].idx = i;
  
  // PERF: Obviously, need not be quadratic. Fine for this game.
  for (let i = 0; i < areas.length - 1; i++) {
    let a = areas[i];
    for (let j = i + 1; j < areas.length; j++) {
      let b = areas[j];
      // Check for touching edges. It has to be the case that
      // the left edge touches the right edge of the other (or
      // symmetric cases) -- left edge can't touch left edge.
      if (a.x == b.x2) {
	let y = Midpoint(a.y, b.y, a.y2, b.y2);
	if (y != null) {
	  AddEdge(a, b, a.x, y);
	}
      } else if (a.x2 == b.x) {
	let y = Midpoint(a.y, b.y, a.y2, b.y2);
	if (y != null) {
	  AddEdge(b, a, b.x, y);
	}
      } else if (a.y == b.y2) {
	let x = Midpoint(a.x, b.x, a.x2, b.x2);
	if (x != null) {
	  AddEdge(a, b, x, a.y);
	}
      } else if (a.y2 == b.y) {
	let x = Midpoint(a.x, b.x, a.x2, b.x2);
	if (x != null) {
	  AddEdge(b, a, x, a.y2);
	}
      }
    }
  }
  
}

function Start() {
  Init();
  InitAreas();
  InitGame();
  InitStars();
  
  window.phase = PHASE_TITLE;
  StartSong(song_theme_maj);

  // straight to game to start
  // window.phase = PHASE_GAME;

  // For mouse control.
  bigcanvas.canvas.onmousemove = CanvasMove;
  bigcanvas.canvas.onmousedown = CanvasMousedown;
  bigcanvas.canvas.onmouseup = CanvasMouseup;
  
  start_time = (new Date()).getTime();
  window.requestAnimationFrame(Step);
}

function InRect(x, y, r) {
  return x >= r.x && x < r.x + r.w &&
      y >= r.y && y < r.y + r.h;
}

function InCoords(x, y, x0, y0, w, h) {
  return x >= x0 && x < x0 + w &&
      y >= y0 && y < y0 + h;
}

function CanvasMousedownGame(x, y) {
  let globalx = scrollx + x;
  // console.log('click ', x, y, " = global ", globalx, y);

  if (fadeout && fadeframes < 0) {
    ClearSong();
    Init();
    InitAreas();
    InitGame();
    
    fadeout = false;
    fadeframes = MAXFADEFRAMES;
    window.phase = PHASE_TITLE;
    StartSong(song_theme_maj);
    return;
  }
  
  if (synchronous) {
    // failsafe
    if (script.length == 0) synchronous = false;
    return;
  }
  
  let sent = GetSentenceAt(x, y);
  if (sent != null) {
    sentence = sent;
    // Stop moving, since executing the sentence may give us
    // a new target.
    player.route = [];
    // XXX execute it...

    // Special case for USE...
    // (could also do for OVI but I think this should be a little
    // puzzle?)
    if ((sentence.verb == VERB_USE ||
	 sentence.verb == VERB_OVI) &&
	sentence.obj1 == null) {
      window.inventoryopen = true;
    }

    if (sentence.verb == VERB_USE &&
	sentence.obj1 != null &&
	sentence.obj2 == null) {
      window.inventoryopen = false;
    }
    
    // Help you figure this one out...
    if (sentence.verb == VERB_OVI &&
	sentence.obj1 != null &&
	sentence.obj2 == null &&
	sentence.obj1.name == "EGG") {
      window.inventoryopen = false;
    }
    
    return;
  } 
  
  if (window.inventoryopen) {
    // TODO: First check action clicks, since these are the
    // only thing outside the inventory itself that don't close it
    
    if (InRect(x, y, INVCLOSE) ||
        !InRect(x, y, INVRECT)) {
      window.inventoryopen = false;
      if (grabitem != null)
        ResetGrabitem();
      return;
    }

    if (grabitem != null) {
      let pos = GrabitemInv();
      if (pos == null) {
        // play sound?
        console.log("!pos");
        return;
      }
      if (GrabitemOK(pos.invx, pos.invy)) {
        // Put it in inventory!
        grabitem.item.invx = pos.invx;
        grabitem.item.invy = pos.invy;
        grabitem = null;
        return;
      } else {
        console.log("!GrabitemOK");
        // play sound?
        return;
      }
    }

    if (grabitem == null) {
      let invx = Math.floor((mousex - INVCONTENTSX) / INVITEMSIZE);
      let invy = Math.floor((mousey - INVCONTENTSY) / INVITEMSIZE);
      if (invx >= 0 && invy >= 0 &&
          invx < INVW && invy < INVH) {
        let item = InvUsed(invx, invy);
        if (item != null) {
          grabitem = { item: item, invx: item.invx, invy: item.invy };
          item.invx = null;
          item.invy = null;
          return;
        }
      }
    }
      
    // TODO:
    // (nice to have) drag item?
    // click USE, then ITEM
    // click to close inventory
    
  } else {
    // inventory closed:

    if (InRect(x, y, INVICON)) {
      inventoryopen = true;
      return;
    }
         
    // XXX test that it's in bounds, not item, etc.
    // (double-click to walk?!)
    sentence = null;
    let route = Route(player.worldx, player.worldy,
		      globalx, y);
    if (route == null) {
      // or just be quiet?
      player.Say("I CAN'T GO THERE");
    } else {
      player.route = route;
    }
    
    // TODO:
    // click GET ITEM, open inventory...
  }
}

function CanvasMouseupGame(x, y) {

}

function CanvasMousedown(event) {
  event = event || window.event;
  var bcx = bigcanvas.canvas.offsetLeft;
  var bcy = bigcanvas.canvas.offsetTop;
  var x = Math.floor((event.pageX - bcx) / PX);
  var y = Math.floor((event.pageY - bcy) / PX);

  switch (window.phase) {
    case PHASE_GAME:
    return CanvasMousedownGame(x, y);
    break;
    case PHASE_TITLE:
    ClearSong();
    window.phase = PHASE_GAME;
    break;
  }
}

function CanvasMouseup(event) {
  event = event || window.event;
  var bcx = bigcanvas.canvas.offsetLeft;
  var bcy = bigcanvas.canvas.offsetTop;
  var x = Math.floor((event.pageX - bcx) / PX);
  var y = Math.floor((event.pageY - bcy) / PX);

  switch (window.phase) {
    case PHASE_GAME:
    return CanvasMouseupGame(x, y);
    break;
    case PHASE_TITLE:
    // ignored
    break;
  }
}

function CanvasMove(event) {
  event = event || window.event;
  var bcx = bigcanvas.canvas.offsetLeft;
  var bcy = bigcanvas.canvas.offsetTop;
  var x = Math.floor((event.pageX - bcx) / PX);
  var y = Math.floor((event.pageY - bcy) / PX);
  mousex = x;
  mousey = y;

  // If we use movement anywhere else (unlikely!) then
  // do the switch thing.
  if (window.phase != PHASE_GAME) return;

  // If dragging, etc.
}

// XXX remove "cheat" keys
document.onkeydown = function(event) {
  event = event || window.event;
  if (event.ctrlKey) return true;

  // console.log('key: ' + event.keyCode);

  
  const kc = event.keyCode;
  if (kc == 27) {
    ClearSong();
    document.body.innerHTML =
        '<b style="color:#fff;font-size:40px">(SILENCED. ' +
        'RELOAD TO PLAY)</b>';
    Step = function() { };
    return;
  }

  if (!CHEATS) return;
  
  switch (kc) {
  case 32:  // SPACE
    if (window.phase == PHASE_TITLE) {
      ClearSong();
      window.phase = PHASE_GAME;
    } else if (window.phase == PHASE_GAME) {
      window.inventoryopen = ! window.inventoryopen;
    }
    break;
  case 38:  // UP
  case 40:  // DOWN
  case 88:  // x
    for (var o in items) {
      items[o].invx = null;
    }
    items.energy.invx = 2;
    items.energy.invy = 0;
    items.egg1.invx = 0;
    items.egg1.invy = 0;
    player.worldx = 1617;
    player.worldy = 89;
    break;
  case 90:  // z
    show_areas = !show_areas
    break;
    
  case 49: 
  case 50:
  case 51:
  case 52:
  case 53:
  case 54:
    player.worldy = 153;
    if (kc == 52) {
      ents.grateguy.worldx = 1229;
      ents.grateguy.worldy = 86;
    } else if (kc == 51) {
      player.worldy = 87;
      items.airlocktool.invx = null;
      items.airlocktool.worldx = 737;
      items.airlocktool.worldy = 66;
      items.egg1.invx = null;
      items.egg2.invx = null;
    }
    player.worldx = WIDTH * (kc - 49 + 0.5); break;
    break;
  }
};

resources.WhenReady(Start);
