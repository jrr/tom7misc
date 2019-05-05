
let counter = 0, skipped = 0;
let start_time = (new Date()).getTime();

var song_theme = [];

// Number of elapsed frames in the current scene.
let frames = 0;

let controls = {
  holding_left: false,
  holding_right: false,
  holding_jump: false,

  // These functions are called "onkeydown" if non-null.
  impulse_left: null,
  impulse_right: null,
  impulse_jump: null
};

const resources = new Resources(
  ['font.png',
   'title.png',
   'background.png',
  ],
  [], null);

function XY(x, y) { return '(' + x + ',' + y + ')'; }

function Init() {
  window.font = new Font(resources.Get('font.png'),
                         FONTW, FONTH, FONTOVERLAP, FONTCHARS);
  window.titleframes = Static('title.png');
  window.background = Static('background.png');

  // Audio tweaks.
  // song_theme.multiply = 1.5;
  // song_power.multiply = 1.35;

  // song_menu[0].volume = 0.65;
}


function InitGame() {

  window.playerx = 18;
  window.playery = 34;
  window.playerdx = 0;
  window.playerdy = 0;
  window.facingleft = true;

  window.phase = PHASE_GAME;

  controls.impulse_jump = () => {
    if (window.phase == PHASE_TITLE) {
      window.phase = PHASE_GAME;
    } else if (window.phase == PHASE_GAME) {
      // XXX probably the wrong place for "jumping" logic.
      window.playerdy -= 1;
    }
  };

  // Dense grid for the whole screen, representing all the particles.
  // We get constant-time tests for particles by location, and particles cannot
  // overlap by definition, but otherwise rather expensive. Still, the screen
  // is only 320x200,* so 64k is pretty manageable.
  //
  // For each pixel-sized cell, we know if it's occupied with some blood, and
  // its current velocity. 
  // Type of thing present. 0 = empty, 1 = blood
  window.grid_t = [];

  window.grid_dx = [];
  window.grid_dy = [];

  for (let y = 0; y < HEIGHT; y++) {
    let cy = (y - (HEIGHT / 2));
    for (let x = 0; x < WIDTH; x++) {
      let cx = (x - (WIDTH / 2));
      if ((cy * cy + cx * cx) < 20 * 20) {
	grid_t.push(1);
	grid_dx.push(0.1);
	grid_dy.push(0.15);
      } else {
	grid_t.push(0);
	grid_dx.push(0.0);
	grid_dy.push(0.0);
      }
    }
  }
  
  // *I think we want this pinball game to kind of scroll, so this might not
  // be a safe assumption?
  
  console.log('initialized game');
}


function DrawPixel(x, y, c32) {
}

function DrawGame() {
  // ClearScreen();
  DrawFrame(window.background, 0, 0);

  // DrawLine(10, 10, window.playerx, window.playery, '#FF0');
  for (let i = 0; i < boundaries.length; i++) {
    let line = boundaries[i];
    // draw normal?
    DrawLine(line.p0.x, line.p0.y,
	     line.p1.x, line.p1.y,
	     '#FF0');
  }

  // Copy image data for pixel-level work
  let oldid = ctx.getImageData(0, 0, WIDTH, HEIGHT);
  let oldid32 = new Uint32Array(oldid.data.buffer);
  
  for (let y = 0; y < HEIGHT; y++) {
    for (let x = 0; x < WIDTH; x++) {
      let idx = y * WIDTH + x;
      if (window.grid_t[idx] > 0) {
	// BGRA
	oldid32[idx] = 0xFF0000FF;
      }
    }
  }

  // And write it back.
  ctx.putImageData(oldid, 0, 0);
  
  /*
  const running = Math.abs(window.playerdx) > 1;
  if (window.facingleft) {
    DrawFrame(running ? window.playerl_run : window.playerl,
	      window.playerx, window.playery);
  } else {
    DrawFrame(running ? window.playerr_run : window.playerr,
	      window.playerx, window.playery);
  }
*/
}

function Draw() {
  switch (window.phase) {
    case PHASE_TITLE:
    DrawFrame(window.titleframes, 0, 0);
    break;
    // case PHASE_CUTSCENE:
    // DrawCutscene();
    break;
    case PHASE_GAME:
    DrawGame();
    break;
  }
}

function xy(x, y) {
  return {x,y};
}

// Each boundary is just a line with an "inward" normal direction.
// The line is described as p0->p1 (each a pair of coordinates)
// with whatever hand of rule makes the normal point to the left
// if you drive from p0 towards p1.
let boundaries = [
  // interior of test table
  {p0: xy(306, 9), p1: xy(9, 9)},
  {p0: xy(9, 9), p1: xy(19, 146)},
  {p0: xy(19, 146), p1: xy(128, 186)},
  {p0: xy(128, 186), p1: xy(176, 187)},
  {p0: xy(176, 187), p1: xy(261, 169)},
  {p0: xy(261, 169), p1: xy(306, 9)}
];


function DrawLine(x0, y0, x1, y1, color) {
  ctx.lineWidth = 2;
  ctx.strokeStyle = color;
  ctx.beginPath();
  ctx.moveTo(x0, y0);
  ctx.lineTo(x1, y1);
  ctx.stroke();
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

  UpdateGamepadControls();

  if (controls.holding_right)
    window.playerdx += 0.1;
  else if (controls.holding_left)
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
  if (window.playery > 175) {
    window.playery = 175;
    window.playerdy *= -0.5;
  }
  window.playerdy += 0.05;

  if (window.playerdx < -0.5)
    window.facingleft = true;
  else if (window.playerdx > 0.5)
    window.facingleft = false;


  // "Physics"
  for (let y = 0; y < HEIGHT; y++) {
    for (let x = 0; x < WIDTH; x++) {
      let idx = y * WIDTH + x;
      if (window.grid_t[idx] > 0) {
	

	// BGRA
	oldid32[idx] = 0xFF0000FF;
      }
    }
  }



  // UpdateSong();

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

// This gets called when the gamepad is interacted with.
// It often happens that there are multiple gamepads, so
// we take the first one with "standard" mapping. the_gamepad
// will be an index into the getGamepads array, or null (note
// 0 != null) if we haven't detected one.
let the_gamepad = null;
function GamepadConnected(e) {
  console.log('gamepad connected!');
  console.log(e);
  if (the_gamepad != null) {
    console.log('already have one.');
    return;
  }
  if (!e.gamepad) return;
  let gp = navigator.getGamepads()[e.gamepad.index];
  if (!gp) return;
  console.log('gp ' + gp.index + ' = ' + gp.id + ' mapping ' + gp.mapping);
  if (gp.mapping == 'standard') {
    the_gamepad = e.gamepad.index;
  }
}

let old_gp_left = false;
let old_gp_right = false;
let old_gp_a = false;
function UpdateGamepadControls() {
  if (the_gamepad == null) return;
  let gp = navigator.getGamepads()[the_gamepad];

  // D-pad up down left right: 12, 13, 14, 15
  // buttons: 3 0 2 1, aka y a x b

  let new_left = !!gp.buttons[14].pressed;
  if (new_left != old_gp_left) {
    if (new_left && controls.impulse_left) controls.impulse_left();
    controls.holding_left = new_left;
    old_gp_left = new_left;
  }
  let new_right = !!gp.buttons[15].pressed;
  if (new_right != old_gp_right) {
    if (new_right && controls.impulse_right) controls.impulse_right();
    controls.holding_right = new_right;
    old_gp_right = new_right;
  }

  let new_a = !!gp.buttons[0].pressed;
  if (new_a != old_gp_a) {
    if (new_a && controls.impulse_jump) controls.impulse_jump();
    controls.holding_jump = new_a;
    old_gp_a = new_a;
  }
}

function CanvasMousedown(event) {
  event = event || window.event;
  var bcx = bigcanvas.canvas.offsetLeft;
  var bcy = bigcanvas.canvas.offsetTop;
  var x = Math.floor((event.pageX - bcx) / PX);
  var y = Math.floor((event.pageY - bcy) / PX);
  console.log('click at ' + x + ', ' + y);
}

function Start() {
  Init();
  InitGame();

  window.phase = PHASE_TITLE;
  StartSong(song_theme);

  // For mouse control.
  // bigcanvas.canvas.onmousemove = CanvasMove;
  bigcanvas.canvas.onmousedown = CanvasMousedown;
  // bigcanvas.canvas.onmouseup = CanvasMouseup;

  window.addEventListener("gamepadconnected",
			  GamepadConnected);

  start_time = (new Date()).getTime();
  window.requestAnimationFrame(Step);
}

document.onkeyup = function(event) {
  event = event || window.event;
  if (event.ctrlKey) return true;

  // console.log('key: ' + event.keyCode);

  switch (event.keyCode) {
  case 32:  // SPACE
    controls.holding_jump = false;
    break;
  case 37:  // LEFT
    controls.holding_left = false;
    break;
  case 39:  // RIGHT
    controls.holding_right = false;
    break;
  case 38:  // UP
  case 40:  // DOWN
  case 90:  // z
  case 88:  // x
  }
}

document.onkeydown = function(event) {
  event = event || window.event;
  if (event.ctrlKey) return true;

  // console.log('key: ' + event.keyCode);

  switch (event.keyCode) {
  case 32:  // SPACE
    controls.holding_jump = true;
    if (controls.impulse_jump) controls.impulse_jump();
    break;
  case 37:  // LEFT
    controls.holding_left = true;
    if (controls.impulse_left) controls.impulse_left();
    break;
  case 39:  // RIGHT
    controls.holding_right = true;
    if (controls.impulse_right) controls.impulse_right();
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
      // TODO: Would be much better if this just paused (with music
      // silenced)! But that can introduce some race conditions.
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
