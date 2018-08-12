
let counter = 0, skipped = 0;
let start_time = (new Date()).getTime();

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

// To support the inventory management constraints, when you pick
// up an object you have to place it into inventory. If this is
// non-null, we are currently trying to pick up an item.
// The item either came from the world (worldx,y) or inventory (invx,y).
// { item: the item being picked up. it is detached from the world / inv.
//   worldx, worldy: coordinates item will return to if canceled (or null)
//   invx, invy: same, for inventory}
let grabitem = null;

const resources = new Resources(
  ['font.png',
   'spacefont.png',
   'title.png',
   'background.png',

   'ship.png',
   
   'face-right.png',
   'face-right-blink.png',
   'player1.png',
   'player2.png',
   'player3.png',

   'grateguy.png',
   
   'inv-icon.png',
   'inventory.png',
   'inv-used.png',
   'inv-conflict.png',
   'inv-ok.png',
   
   'id.png',
   'airlocktool.png',
   
   'invid.png',
   'invairlocktool.png',
   'invegg.png',
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

  // DrawFrame(starframes[0], x, y);
  
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
  this.mask = mask;
  
  // Position of top-left in inventory, or null if detached
  this.invx = null;
  this.invy = null;

  // Position in world (global coordinates, not screen), or null.
  // (Position is of top-left coordinate)
  // Can't have both world position and inventory position,
  // but an unspawned item could have both be null.
  this.worldx = null;
  this.worldx = null;

  // width/height in world. derived from first frame.
  this.worldw = this.worldframes.width;
  this.worldh = this.worldframes.height;
  
  this.lookstring = null;

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

function InitGame() {

  scrollx = 5 * WIDTH;
  
  window.phase = PHASE_GAME;

  window.ents = {};
  let ents = window.ents;
  ents.grateguy = new Ent('ALIEN',
                          ['grateguy', 1],
                          // no moving anim
                          ['grateguy', 1],
                          30, 65);
  ents.grateguy.lookstring = "LOOKS WARM";
  ents.grateguy.worldx = 1140;
  ents.grateguy.worldy = 91;

  window.player = new Ent('ME',
                          ['player1', 1],
                          ['player1', 9,
                           'player2', 2,
                           'player3', 6,
                           'player2', 2],
                          68,
                          46);
  player.sayfont = window.spacefont;
  player.lookstring = "IT ME :-)";
  player.worldx = WIDTH * 5 + 122;
  player.worldy = 160;
  player.facer = EzFrames(['face-right', 280,
                           'face-right-blink', 2,
                           'face-right', 68,
                           'face-right-blink', 2]);
  player.facel = FlipFramesHoriz(player.facer);
  player.Draw = function(x, y) {
    this.SuperDraw(x, y);
    DrawFrame(this.facingleft ? this.facel : this.facer,
              x + (this.facingleft ? LFACEX : FACEX), y + FACEY);
  };
                       
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
  
  items.airlocktool = new Item('TOOL',
                               ['invairlocktool', 1],
                               ['airlocktool', 1],
                               [' **',
                                ' * ',
                                '** ']);
  items.airlocktool.lookstring = "WIGGLY METAL";
  items.airlocktool.worldx = 1780;
  items.airlocktool.worldy = 48;
  
  items.id = new Item('CARD',
                      ['invid', 1],
                      ['id', 1],
                      MASK1x1);

  items.id.lookstring = "PLASTIC IN GOLDEN RATIO ASPECT";
  items.id.worldx = WIDTH * 5 + 30;
  items.id.worldy = 140;
  
  console.log('initialized game');
}

// Entity in game that can walk around, speak, etc.
// Includes the player character.
function Ent(name, stand, move, wd, ht) {
  this.name = name;
  
  this.halfwidth = wd >>> 1;
  this.height = ht;

  this.xspeed = 2;
  this.yspeed = 1;
  
  // If non-null, location in the world.
  this.worldx = null;
  this.worldy = null;

  // If non-null, we're walking to this target spot.
  this.targetx = null;
  this.targety = null;

  // Current frames.
  this.standr = EzFrames(stand);
  this.standl = FlipFramesHoriz(this.standr);

  this.mover = EzFrames(move);
  this.movel = FlipFramesHoriz(this.mover);

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

Ent.prototype.GetFrames = function() {
  const moving = this.targetx != null &&
        (this.worldx != this.targetx ||
         this.worldy != this.targety);
  
  if (this.facingleft)
    return moving ? this.movel : this.standl;
  else
    return moving ? this.mover : this.standr;
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
  
Ent.prototype.SuperDraw = function(x, y) {
  const f = this.GetFrames();
  DrawFrame(f, x, y);
};

// passed top-left corner (y - height).
Ent.prototype.Draw = function(x, y) {
  this.SuperDraw(x, y);
};
  
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
      DrawFrame(item.worldframes,
                item.worldx - scrollx,
                item.worldy);
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
    if (sentence.verb == VERB_OVO ||
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
  
  // If we have no sentence, we can always pick a verb
  // (XXX: NOT if we've grabbed an item and need to put it in
  // inventory, though)
  if (sentence == null && y > ACTY) {
    for (let act of ACTIONS) {
      if (InRect(mousex, mousey, act)) {
        console.log('start sentence ', VerbString(act.verb));
        return { verb: act.verb, obj1: null, obj2: null };
        return;
      }
    }
  }

  if (window.inventoryopen) {
    // XXX Allow extending with inventory item...
    
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

// Draw the sentence (argument) at the top of the screen.
// Note this is also used when hovering, which happens before
// the sentence is actually saved.
function DrawSentence(s) {
  if (s == null) return;
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
  case VERB_OVO: ll = TwoObjects("OVOPOSIT", "INTO"); break;
  case VERB_USE: ll = TwoObjects("USE", "WITH"); break;
  case VERB_DROP: ll = OneObject("DROP"); break;
  }

  let x = 1;
  for (let i = 0; i < ll.length; i++) {
    let ph = ll[i];
    ((i % 0 == 1) ? hifont : font).Draw(ctx, x, 1, ph);
    x += (ph.length + 1) * (FONTW - FONTOVERLAP);
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
  if (nsent != null)
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
  
}

function DrawTitle() {
  DrawFrame(window.titleframes, 0, 0);
  spacefont.Draw(ctx, WIDTH * 0.35, HEIGHT * 0.5,
                 "SPACE GAME TITLE TBD");
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
  
  switch (sentence.verb) {
  case VERB_GRAB: {
    let obj = sentence.obj1;
    // XXX go to obj1 first
    if (obj instanceof Item) {
      // XXX test if it's possible to get this item.
      if (obj.worldx == null) {
        player.Say("HOW...?");
        sentence = null;
        return;
      }
      
      window.inventoryopen = true;
      grabitem = { item: obj,
                   worldx : obj.worldx,
                   worldy : obj.worldy };
      obj.worldx = null;
      obj.worldy = null;
      sentence = null;
      return;
    }

    // XXX handle grabbing grate guy
    
    break;
  }
  case VERB_LOOK: {
    let desc = sentence.obj1.LookString();
    player.Say(desc || "DOESN'T LOOK LIKE ANYTHING TO ME");
    sentence = null;
    break;
  }
  case VERB_TALK: {
    // XXX handle talking to humans
    player.Say("HI " + sentence.obj1.name + "! :-)");
    sentence = null;
    break;
  }
  case VERB_OVO:
    if (sentence.obj2 == null)
      return;

    // XXX go to obj2 first
    
    if (sentence.obj1.name != "EGG") {
      player.Say("CAN ONLY OVOPOSIT AN *EGG*");
      sentence = null;
      return;
    }

    if (sentence.obj2.name != "ALIEN") {
      player.Say("CAN ONLY OVOPOSIT AN EGG INTO AN *ALIEN*");
      sentence = null;
      return;
    }

    // XXX do ovoposit if the human is dead, etc.
    
    break;
  case VERB_USE:
    if (sentence.obj2 == null)
      return;

    // XXX go to obj2 first
    
    // Handle combinations that make sense here.
    
    player.Say("I DON'T KNOW HOW TO DO THAT");
    sentence = null;
    break;
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
    
  UpdateStars();

  DoSentence();

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
    if (ent.targetx != null) {
      const dx = ent.targetx - ent.worldx
      const dy = ent.targety - ent.worldy;
      // At target?
      if (Math.abs(dx) <= ent.xspeed &&
          Math.abs(dy) <= ent.yspeed) {
        ent.worldx = ent.targetx;
        ent.worldy = ent.targety;
        ent.targetx = null;
        ent.targety = null;
        continue;
      }

      // XXX use bresenham
      // XXX avoid obstacles if non-convex?
      if (Math.abs(dx) <= ent.xspeed) {
        ent.worldx = ent.targetx;
      } else {
        ent.worldx += dx < 0 ? -ent.xspeed : ent.xspeed;
        if (dx < 0) ent.facingleft = true;
        else if (dx > 0) ent.facingleft = false;
      }
      
      if (Math.abs(dy) <= ent.yspeed) {
        ent.worldy = ent.targety;
      } else {
        ent.worldy += dy < 0 ? -ent.yspeed : ent.yspeed;
      }
    }
  }


  // Update here or in Draw?
  if (player.worldx - SCROLLMARGIN < scrollx) {
    let target = player.worldx - SCROLLMARGIN;
    scrollx = Math.round(((scrollx * 7) + target) * 0.125);
    // scrollx--;
  } else if (player.worldx + SCROLLMARGIN > scrollx + WIDTH) {
    let target = player.worldx + SCROLLMARGIN - WIDTH;
    // scrollx++;
    scrollx = Math.round(((scrollx * 7) + target) * 0.125);
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

function Start() {
  Init();
  InitGame();
  InitStars();
  
  // window.phase = PHASE_TITLE;
  // StartSong(song_theme_maj);

  // straight to game to start
  window.phase = PHASE_GAME;

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
  console.log('click ', x, y, " = global ", globalx, y);

  let sent = GetSentenceAt(x, y);
  if (sent != null) {
    sentence = sent;
    // Stop moving, since executing the sentence may give us
    // a new target.
    player.targetx = null;
    player.targety = null;
    // XXX execute it...
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
    player.targetx = globalx;
    player.targety = y;
    
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
  case 90:  // z
  case 88:  // x
    break;
    
  case 49: 
  case 50:
  case 51:
  case 52:
  case 53:
  case 54:
    player.worldx = WIDTH * (kc - 49 + 0.5); break;
    /*
  case 55: window.phase = PHASE_PUZZLE; Level7(); break;
    case 9:
    if (window.cutscene) {
      var cuts = window.cutscenes[window.cutscene];
      if (cuts) {
        window.cutscene = null;
        cuts.cont();
      }
    }
    break;
    */
    case 27: // ESC
    if (true || DEBUG) {
      ClearSong();
      document.body.innerHTML =
          '<b style="color:#fff;font-size:40px">(SILENCED. ' +
          'RELOAD TO PLAY)</b>';
      Step = function() { };
      // n.b. javascript keeps running...
    }
    break;
  }
};

resources.WhenReady(Start);
