
// TODO: Clean this out and replace with a simple game.

let counter = 0, skipped = 0;
let start_time = (new Date()).getTime();

// Number of elapsed frames in the current scene.
let frames = 0;

let holding_left = false, holding_right = false;

const resources = new Resources(
  ['font.png',
   'spacefont.png',
   'title.png',
   'background.png',
   'face-right.png',
   'face-right-blink.png',
   'player1.png',
   'player2.png',
   'player3.png',

   'inv-icon.png',
   'inventory.png',
   'inv-used.png',

   'id.png',
   'airlocktool.png',
   
   'invid.png',
   'invairlocktool.png',
   'invegg.png',
  ],
  [], null);

function XY(x, y) { return '(' + x + ',' + y + ')'; }

function Init() {
  window.font = new Font(resources.Get('font.png'),
                         FONTW, FONTH, FONTOVERLAP, FONTCHARS);
  window.spacefont = new Font(resources.Get('spacefont.png'),
                              FONTW, FONTH, FONTOVERLAP, FONTCHARS);
  window.hispacefont = new Font(
    EzRecolor(resources.Get('spacefont.png'), 0xFFFFFFFF, 0xFF00FFFF),
    FONTW, FONTH, FONTOVERLAP, FONTCHARS);
  
  window.titleframes = Static('title.png');
  window.background = Static('background.png');
  window.facer = EzFrames(['face-right', 280,
			   'face-right-blink', 2,
			   'face-right', 68,
			   'face-right-blink', 2]);
  window.facel = FlipFramesHoriz(window.facer);
  window.playerr_run =
	EzFrames(['player1', 9,
		  'player2', 2,
		  'player3', 6,
		  'player2', 2]);
  window.playerr = EzFrames(['player1', 1]);
      
  window.playerl_run = FlipFramesHoriz(window.playerr_run);
  window.playerl = FlipFramesHoriz(window.playerr);

  // UI elements
  window.inv_icon = EzFrames(['inv-icon', 1]);
  window.inventory = EzFrames(['inventory', 1]);
  window.invused = EzFrames(['inv-used', 1]);
  
  // window.playerr = FlipFramesHoriz(window.playerl);
  // window.playerr_run = FlipFramesHoriz(window.playerl_run);
  
  // Audio tweaks.
  // song_theme.multiply = 1.5;
  // song_power.multiply = 1.35;

  // song_menu[0].volume = 0.65;
}

// Item that can go in inventory.
function Item(invframes, worldframes, mask) {
  // Frames when in inventory
  this.invframes = EzFrames(invframes);
  this.worldframes = EzFrames(worldframes);
  this.mask = mask;

  // Position of top-left in inventory, or null if detached
  this.invx = null;
  this.invy = null;

  // Position in world (global coordinates, not screen), or null.
  // Can't have both world position and inventory position,
  // but an unspawned item could have both be null.
  this.worldx = null;
  this.worldx = null;
  
  // TODO: state it goes back to if dropped?
  
  return this;
}

Item.prototype.MaskAt = function(x, y) {
  if (x < 0 || y < 0) return false;
  let row = this.mask[y] || '';
  let cell = row[x] || ' ';
  return cell != ' ';
};

function InitGame() {

  window.playerx = 18;
  window.playery = 34;
  window.playerdx = 0;
  window.playerdy = 0;
  window.facingleft = true;

  window.phase = PHASE_GAME;

  const MASK2x2 = ['**',
		   '**'];
  const MASK1x1 = ['*'];

  window.inventoryopen = false;
  
  window.items = {};
  window.items.egg1 = new Item(['invegg', 1], ['invegg', 1], MASK2x2);
  window.items.egg2 = new Item(['invegg', 1], ['invegg', 1], MASK2x2);
  window.items.egg3 = new Item(['invegg', 1], ['invegg', 1], MASK2x2);
  window.items.egg4 = new Item(['invegg', 1], ['invegg', 1], MASK2x2);

  // XXX?
  window.items.egg1.invx = 1;
  window.items.egg1.invy = 0;
  
  window.items.airlocktool = new Item(['invairlocktool', 1],
				      ['airlocktool', 1],
				      [' **',
				       ' * ',
				       '** ']);

  window.items.airlocktool.worldx = 80;
  window.items.airlocktool.worldy = 48;
  
  window.items.id = new Item(['invid', 1],
			     ['id', 1],
			     MASK1x1);

  window.items.id.worldx = 16;
  window.items.id.worldy = 80;
  
  console.log('initialized game');
}

// Entity in game that can walk around, speak, etc.
// Includes the player character.
function Ent() {
  return this;
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
  return false;
}

function DrawGame() {
  // ClearScreen();
  DrawFrame(window.background, 0, 0);

  // draw inventory icon
  // TODO: hover/open state?
  DrawFrame(window.inv_icon, INVICON.x, INVICON.y);

  // TODO: hover/active states
  spacefont.Draw(ctx, 64, 180, "GRAB");
  hispacefont.Draw(ctx, 110, 180, "TALK");
  hispacefont.Draw(ctx, 158, 180, "OVOPOSIT");
  spacefont.Draw(ctx, 238, 180, "USE");
  spacefont.Draw(ctx, 277, 180, "DROP");

  // Draw items in the world. XXX need scrollin'
  // TODO: may need to handle upper/lower deck
  for (let o in items) {
    let item = items[o];
    if (item.worldx != null) {
      DrawFrame(item.worldframes,
		item.worldx,
		item.worldy);
    }
  }
  
  // Draw the scene. TODO: two layers for upper and lower decks
  
  const running = Math.abs(window.playerdx) > 1;
  if (window.facingleft) {
    DrawFrame(running ? window.playerl_run : window.playerl,
	      window.playerx, window.playery);
    DrawFrame(window.facel,
	      window.playerx + LFACEX, window.playery + FACEY);
  } else {
    DrawFrame(running ? window.playerr_run : window.playerr,
	      window.playerx, window.playery);
    DrawFrame(window.facer,
	      window.playerx + FACEX, window.playery + FACEY);
  }

  // DrawFrame(invegg, playerx + 100, playery);

  if (window.inventoryopen) {
    // Above everything: Inventory
    DrawFrame(window.inventory, INVX, INVY);
    for (let y = 0; y < INVH; y++) {
      for (let x = 0; x < INVW; x++) {
	if (InvUsed(x, y) != null) {
	  DrawFrame(invused, 
		    INVCONTENTSX + INVITEMSIZE * x,
		    INVCONTENTSY + INVITEMSIZE * y);
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

last = 0;
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

  if (holding_right)
    window.playerdx += 0.1;
  else if (holding_left)
    window.playerdx -= 0.1;
  else
    window.playerdx *= 0.9;
  
  if (window.playerdx > 2.0) window.playerdx = 2.0;
  else if (window.playerdx < -2.0) window.playerdx = -2.0;
  
  window.playerx += window.playerdx;
  window.playery += window.playerdy;
  if (window.playerx < 6) window.playerx = 6;
  if (window.playerx > 300) window.playerx = 300;
  if (window.playery < 6) window.playery = 6;
  if (window.playery > 111) {
    window.playery = 111;
    window.playerdy *= -0.5;
  }
  window.playerdy += 0.05;

  if (window.playerdx < -0.5)
    window.facingleft = true;
  else if (window.playerdx > 0.5)
    window.facingleft = false;
  
  
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

/*
function InRect(x, y, x0, y0, w, h) {
  return x >= x0 && x < x + w &&
      y >= y0 && y < y + h;
}
*/

function CanvasMousedownGame(x, y) {
  console.log('click ', x, y);
  if (window.inventoryopen) {
    // TODO: First check action clicks, since these are the
    // only thing outside the inventory itself that don't close it
    
    if (InRect(x, y, INVCLOSE) ||
	!InRect(x, y, INVRECT)) {
      window.inventoryopen = false;
    }

    // TODO:
    // click ITEM to snag it
    // (nice to have) drag item?
    // click USE, then ITEM
    // click to close inventory
    
  } else {
    // inventory closed:

    if (InRect(x, y, INVICON)) {
      inventoryopen = true;
      return;
    }
	
    // TODO:
    // click to walk
    // click to form sentence
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
  window.mousex = x;
  window.mousey = y;

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

  switch (event.keyCode) {
  case 32:  // SPACE
    if (window.phase == PHASE_TITLE) {
      ClearSong();
      window.phase = PHASE_GAME;
    } else if (window.phase == PHASE_GAME) {
      window.inventoryopen = ! window.inventoryopen;
    }
    break;
  case 37:  // LEFT
    holding_left = true;
    break;
  case 39:  // RIGHT
    holding_right = true;
    break;
  case 38:  // UP
  case 40:  // DOWN
  case 90:  // z
  case 88:  // x
    break;
    
    /*
    case 49: window.phase = PHASE_PUZZLE; Level1(); break;
    case 50: window.phase = PHASE_PUZZLE; Level2(); break;
    case 51: window.phase = PHASE_PUZZLE; Level3(); break;
    case 52: window.phase = PHASE_PUZZLE; Level4(); break;
    case 53: window.phase = PHASE_PUZZLE; Level5(); break;
    case 54: window.phase = PHASE_PUZZLE; Level6(); break;
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
