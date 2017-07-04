// The following files must be provided externally:
// atom.js provides atom_glyphs.
// letters.js provides letters.

let ctx;

// In canvas screen coordinates.
let mousex = 10;
let mousey = 10;

// If non-null, should lock the interface.
let locked = null;

const MODE_ATOM = 0;
const MODE_LETTER = 1;
const MODE_WORD = 2;
const MODE_ANIMATE = 3;
const NUM_MODES = 4;
let mode = MODE_ANIMATE;

const DEG_TO_RADIANS = 3.141592653589 / 180.0;

// Atoms have a single path (e and o are just self-overlapping). The
// first position is always a "moveto" command. Each successive
// position has an x,y coordinate, and if it has XXX then it is a
// bezier curveTo, otherwise a plain lineto. The path is always closed
// at the end. Coordinates are integer grid coordinates. The center of
// the atom (for rotations, etc.) is defined to be 0,0.

// Can modify the mesh of atomic pieces.
let atoms = "ceors'.?";

///////////////////////////
// For atom editing mode.
// Current node being dragged (same as return type of 'closest')
let dragging = null;
let onion_atom = 'e';
let current_atom = 'c';

///////////////////////////
// For letter editing mode.
let current_letter = 'd';
// Index of current piece within the current_letter. Make sure
// it gets reset to 0 when current_letter changes.
let letter_piece = 0;

//////////////////////////
// For word mode.
let current_word = 'anagram';

//////////////////////////
// For animate mode.
let timeout_id = null;
let animate_pieces = [];
/*
let startword = 'vulnerability';
let endword = 'authenticity';
// For (s)ource and (d)estination (s)lot and (p)iece
// See anaglyph.sml (makeplan).
let plan = [
  {a:"'",ss:0,sp:0,ds:0,dp:0},
  {a:"'",ss:0,sp:1,ds:1,dp:0},
  {a:"r",ss:1,sp:0,ds:1,dp:0},
  {a:"'",ss:1,sp:0,ds:2,dp:0},
  {a:"'",ss:2,sp:0,ds:2,dp:1},
  {a:"'",ss:2,sp:1,ds:2,dp:2},
  {a:"r",ss:3,sp:0,ds:3,dp:0},
  {a:"'",ss:3,sp:0,ds:3,dp:0},
  {a:"e",ss:4,sp:0,ds:4,dp:0},
  {a:"r",ss:5,sp:0,ds:5,dp:0},
  {a:"c",ss:6,sp:0,ds:0,dp:0},
  {a:"'",ss:6,sp:0,ds:3,dp:1},
  {a:"c",ss:7,sp:0,ds:8,dp:0},
  {a:"'",ss:7,sp:0,ds:5,dp:0},
  {a:"'",ss:7,sp:1,ds:6,dp:0},
  {a:"'",ss:8,sp:0,ds:6,dp:1},
  {a:".",ss:8,sp:0,ds:7,dp:0},
  {a:"'",ss:9,sp:0,ds:6,dp:2},
  {a:"'",ss:9,sp:1,ds:7,dp:0},
  {a:"'",ss:10,sp:0,ds:9,dp:0},
  {a:".",ss:10,sp:0,ds:9,dp:0},
  {a:"'",ss:11,sp:0,ds:10,dp:0},
  {a:"'",ss:11,sp:1,ds:10,dp:1},
  {a:"'",ss:11,sp:2,ds:10,dp:2},
  {a:"r",ss:12,sp:0,ds:11,dp:0},
  {a:"'",ss:12,sp:0,ds:11,dp:0},
  {a:"?",ss:12,sp:0,ds:11,dp:0}
];
*/

let startword = 'anxious';
let endword = 'wisdom';
let plan = [
  {a:"c",h:~3,ss:0,sp:0,ds:3,dp:0},
  {a:"'",h:~2,ss:0,sp:0,ds:0,dp:0},
  {a:"r",h:~1,ss:1,sp:0,ds:5,dp:0},
  {a:"'",h:0,ss:1,sp:0,ds:0,dp:1},
  {a:"'",h:1,ss:2,sp:0,ds:0,dp:2},
  {a:"'",h:2,ss:2,sp:1,ds:0,dp:3},
  {a:"'",h:~3,ss:2,sp:2,ds:1,dp:0},
  {a:"'",h:~2,ss:2,sp:3,ds:3,dp:0},
  {a:"'",h:~1,ss:3,sp:0,ds:3,dp:1},
  {a:".",h:0,ss:3,sp:0,ds:1,dp:0},
  {a:"o",h:1,ss:4,sp:0,ds:4,dp:0},
  {a:"r",h:2,ss:5,sp:0,ds:5,dp:1},
  {a:"'",h:~3,ss:5,sp:0,ds:5,dp:0},
  {a:"s",h:~2,ss:6,sp:0,ds:2,dp:0}
];
/*
let startword = 'w';
let endword = 'x';
let plan = [
  {a:"'",ss:0,sp:0,ds:0,dp:0},
  {a:"'",ss:0,sp:1,ds:0,dp:1},
  {a:"'",ss:0,sp:2,ds:0,dp:2},
  {a:"'",ss:0,sp:3,ds:0,dp:3}
];
*/

const CANVASWIDTH = 1920;
const CANVASHEIGHT = 1080;

// Cell size must divide both evenly.
// Common factors of 1920x1080: 2, 2, 2, 3, 5
const CELLSIZES = [2, 3, 4, 5, 6, 8, 10, 12, 15, 20, 24, 30, 40];

let CELLSIZEIDX = 3;
let CELLSIZE = CELLSIZES[CELLSIZEIDX];
let CELLSW = CANVASWIDTH / CELLSIZE;
let CELLSH = CANVASHEIGHT / CELLSIZE;

function SetCellSizeIdx(i) {
  if (i < 0 || i >= CELLSIZES.length) return;
  CELLSIZEIDX = i;
  CELLSIZE = CELLSIZES[i];
  CELLSW = CANVASWIDTH / CELLSIZE;
  CELLSH = CANVASHEIGHT / CELLSIZE;
}

// Returns screen (canvas) coordinates; grid 0,0 is the center of the
// screen.
function GridToScreen(x, y) {
  return {x : (x * CELLSIZE) + CANVASWIDTH / 2,
	  y : (y * CELLSIZE) + CANVASHEIGHT / 2};
}

// Returns the closest integral grid coordinate.
function ScreenToGrid(x, y) {
  return {x : Math.round((x - CANVASWIDTH / 2) / CELLSIZE),
	  y : Math.round((y - CANVASHEIGHT / 2) / CELLSIZE)}
}


// Returns the index of the closest point on the supplied path.
function ClosestPoint(path, sx, sy, mindist, allow_control) {
  let dsquared =
      (CANVASWIDTH + CANVASHEIGHT) * (CANVASWIDTH + CANVASHEIGHT);
  let besti = 0;
  for (let i = 0; i < path.length; i++) {
    let pt = path[i];
    const { x, y } = GridToScreen(pt.x, pt.y);
    const dx = x - sx;
    const dy = y - sy;
    const dsq = dx * dx + dy * dy;
    if (dsq < dsquared) {
      dsquared = dsq;
      besti = i;
    }
  }
  if (mindist != undefined && dsquared < mindist * mindist) {
    return { idx: besti };
  } else {
    return null;
  }
}

function DrawPath(p, fill, stroke) {
  ctx.beginPath();
  if (p.length == 0) throw 'empty path';
  ctx.strokeStyle = stroke;
  ctx.fillStyle = fill;
  const { x: startx, y: starty } = GridToScreen(p[0].x, p[0].y);
  ctx.moveTo(startx, starty);
  for (let i = 1; i < p.length; i++) {
    const pt = p[i];
    const {x, y} = GridToScreen(pt.x, pt.y);
    // XXX bezier.
    ctx.lineTo(x, y);
  }
  // Return to start (XXX if not already there?)
  ctx.lineTo(startx, starty);
  if (fill) ctx.fill();
  if (stroke) {
    ctx.lineWidth = 6;
    ctx.stroke();
  }
}

// Returns screen x,y coordinate that's in the center
// between the two points. For lines, this is basically
// correct. For curves, it may be way off.
function Bisect(p1, p2) {
  // XXX do something for bezier?
  const { x: p1x, y: p1y } = GridToScreen(p1.x, p1.y);
  const { x: p2x, y: p2y } = GridToScreen(p2.x, p2.y);
  // console.log('bisect ', p1x, p1y, p2x, p2y);
  return { x: Math.round((p1x + p2x) / 2),
	   y: Math.round((p1y + p2y) / 2) };
}

function DrawControlPoints(p, highlight) {
  if (p.length == 0) throw 'empty path';

  for (let i = 0; i < p.length; i++) {
    const pt = p[i];
    const {x, y} = GridToScreen(pt.x, pt.y);

    ctx.beginPath();
    // console.log(p);
    let width;
    if (highlight == i) {
      ctx.fillStyle = 'rgba(255,255,128,1.0)';
      ctx.strokeStyle = 'rgba(64,0,0,1.0)';
      width = CELLSIZE * 0.45;
    } else {
      ctx.fillStyle = 'rgba(255,255,255,0.75)';
      ctx.strokeStyle = 'rgba(0,0,0,0.75)';
      width = CELLSIZE * 0.4;
    }
    ctx.ellipse(x, y,
		width, width, width, width,
		360);
    ctx.fill();
    ctx.lineWidth = 3;
    ctx.stroke();
  }

  // Next/prev bisection hints
  if (highlight >= 0 && highlight < p.length) {
    const pt = p[highlight];
    const prev = p[highlight == 0 ? p.length - 1 : highlight - 1];
    const next = p[(highlight + 1) % p.length];
    const { x: px, y: py } = Bisect(prev, pt);
    const { x: nx, y: ny } = Bisect(pt, next);
    // console.log(px, py, nx, ny);
    ctx.font = 'bold 18pt Helvetica,sans-serif';
    ctx.fillStyle = '#f00';
    ctx.fillText("1", px, py);
    ctx.fillText("2", nx, ny);
  }
}

// Draw the grid at the current scale. dx and dy yield a red rule,
// reflected around the axis.
function DrawGrid(dx, dy) {
  // Draw grid
  ctx.lineWidth = 1;
  for (let y = 0; y <= CELLSH; y++) {
    ctx.beginPath();
    const yy = y - CELLSH / 2;
    if (yy == 0) {
      ctx.strokeStyle = '#777';
    } else if (yy == dy || yy == -dy) {
      ctx.strokeStyle = '#d99';
    } else {
      ctx.strokeStyle = '#ddd';
    }
    ctx.moveTo(0, y * CELLSIZE);
    ctx.lineTo(CELLSIZE * CELLSW, y * CELLSIZE);
    ctx.stroke();
  }

  for (let x = 0; x <= CELLSW; x++) {
    ctx.beginPath();
    const xx = x - CELLSW / 2;
    if (xx == 0) {
      ctx.strokeStyle = '#777';
    } else if (xx == dx || xx == -dx) {
      ctx.strokeStyle = '#d99';
    } else {
      ctx.strokeStyle = '#ddd';
    }
    ctx.moveTo(x * CELLSIZE, 0);
    ctx.lineTo(x * CELLSIZE, CELLSH * CELLSIZE);
    ctx.stroke();
  }
}

function DrawModeAtom() {
  const path = atom_glyphs[current_atom];
  
  let dx = null, dy = null;
  if (dragging) {
    // XXX Bezier
    dx = path[dragging.idx].x;
    dy = path[dragging.idx].y;
  }

  DrawGrid(dx, dy);
  
  // XXX onion skin.
  if (onion_atom) {
    const opath = atom_glyphs[onion_atom];
    DrawPath(opath, 'rgba(16,75,16,0.15)', undefined);
    // DrawControlPoints(path, undefined);
  }
  
  let highlight;
  if (dragging) {
    highlight = dragging.idx;
  } else {
    const closest = ClosestPoint(path, mousex, mousey, CELLSIZE, true);
    highlight = closest ? closest.idx : -1;
  }
  DrawPath(path, 'rgba(16,16,16,0.75)', '#55f');
  DrawControlPoints(path, highlight);

  if (locked) {
    ctx.font = 'bold 40pt Helvetica,sans-serif';
    ctx.fillStyle = '#f00';
    ctx.fillText(locked, 10, 50);
  }
}

function TransformPath(p, dx, dy, r) {
  let ret = [];
  for (const pt of p) {
    const sinr = Math.sin(r * DEG_TO_RADIANS);
    const cosr = Math.cos(r * DEG_TO_RADIANS);
    const x = pt.x * cosr - pt.y * sinr;
    const y = pt.y * cosr + pt.x * sinr;
    ret.push({x: x + dx, y: y + dy});
  }
  return ret;
}

function DrawModeLetter() {
  DrawGrid(null, null);

  ctx.font = 'bold 40pt Helvetica,sans-serif';
  ctx.fillStyle = '#cdc';
  ctx.fillText('Letter ' + current_letter + '/' + letter_piece,
	       20, 50);
  
  const letter = letters[current_letter];
  for (let i = 0; i < letter.p.length; i++) {
    let piece = letter.p[i];
    let fill = i == letter_piece ? 'rgba(0,0,0,0.75)' : 'rgba(0,0,0,0.25)';
    let path = TransformPath(atom_glyphs[piece.a], piece.x, piece.y, piece.r);
    DrawPath(path, fill, null);
  }

  // Draw metrics
  ctx.lineWidth = 1;
  ctx.beginPath();
  ctx.strokeStyle = '#7a7';
  const { x: widthx, y: unused } = GridToScreen(letter.m.w, 0);
  ctx.moveTo(widthx, 0);
  ctx.lineTo(widthx, CELLSH * CELLSIZE);
  ctx.stroke();
}

function DrawLetter(letter, dx, dy) {
  for (let i = 0; i < letter.p.length; i++) {
    let piece = letter.p[i];
    let fill = '#000';
    let path = TransformPath(atom_glyphs[piece.a],
			     dx + piece.x, dy + piece.y, piece.r);
    DrawPath(path, fill, null);
  }
}

function WordWidth(w) {
  let total = 0;
  for (let i = 0; i < w.length; i++) {
    total += letters[w[i]].m.w;
  }
  return total;
}

function DrawModeWord() {
  DrawGrid(null, null);
  // Total width from letter metrics, so that
  // we can center it.
  const total_width = WordWidth(current_word);
  let x = total_width * -0.5;
  for (let i = 0; i < current_word.length; i++) {
    let c = current_word[i];
    DrawLetter(letters[c], x, 0);
    x += letters[c].m.w;
  }
}

function Draw() {
  ctx.clearRect(0, 0, CANVASWIDTH, CANVASHEIGHT);

  if (mode == MODE_ATOM) {
    DrawModeAtom();
  } else if (mode == MODE_LETTER) {
    DrawModeLetter();
  } else if (mode == MODE_WORD) {
    DrawModeWord();
  }
}

function Click(e) {
  if (locked)
    return;

  if (mode == MODE_ATOM) {
    // console.log(e);
    const x = e.offsetX;
    const y = e.offsetY;

    const closest = ClosestPoint(atom_glyphs[current_atom],
				 mousex, mousey,
				 CELLSIZE, true);
    dragging = closest;
    Draw();
  } else if (mode == MODE_LETTER) {
    // Any click drags the width metric. Could generalize this.
    dragging = true;
    const { x, y } = ScreenToGrid(e.offsetX, e.offsetY);
    if (x > 0) {
      letters[current_letter].m.w = x;
    }
    Draw();
  }
}

function Unclick(e) {
  dragging = null;
  Draw();
}

function MouseMove(e) {
  mousex = e.offsetX;
  mousey = e.offsetY;
  if (mode == MODE_ATOM) {
    if (dragging) {
      const pt = atom_glyphs[current_atom][dragging.idx];
      const { x: gx, y: gy } = ScreenToGrid(mousex, mousey);
      pt.x = gx;
      pt.y = gy;
    }
    
    Draw();
  } else if (mode == MODE_LETTER) {
    // Currently can only drag the width metric.
    if (dragging) {
      const { x, y } = ScreenToGrid(mousex, mousey);
      if (x > 0) {
	letters[current_letter].m.w = x;
      }
      Draw();
    }
  }
}

function getFrameServerUrl(url, k) {
  const req = new XMLHttpRequest();
  req.onreadystatechange = function() {
    console.log(req.readyState, req.status);
    if (req.readyState == 4 &&
	req.status == 200) {
      if (k) k ();
    }
  };
  req.open("GET", 'http://localhost:8000' + url, true);
  req.send(null);
}

function getSynchronous(url) {
  // XXX queue?
  if (locked)
    throw 'synchronous while locked';

  locked = 'Saving...';
  getFrameServerUrl(url, function () {
    console.log('callback');
    locked = null;
    Draw();
  });
  Draw();
}

function ArrowKey(e) {
  switch (e.key) {
  case 'ArrowUp':
  case 'ArrowDown':
  case 'ArrowLeft':
  case 'ArrowRight': {
    let dx = 0, dy = 0;
    switch (e.key) {
    case 'ArrowUp': dy = -1; break;
    case 'ArrowDown': dy = 1; break;
    case 'ArrowLeft': dx = -1; break;
    case 'ArrowRight': dx = 1; break;
    }
    return {dx, dy};
  }
  }
  return null;
}

function KeyAtom(e) {
  for (const c of atoms) {
    if (e.key == c) {
      onion_atom = current_atom == c ? null : current_atom;
      current_atom = c;
      Draw();
      return;
    }
  }

  if (e.code == 'NumpadEnter') {
    // Save data to frameserver.
    const data = JSON.stringify(atom_glyphs);
    getSynchronous('/save_atoms/' +
		   encodeURIComponent(data));
    return;
  }

  let ak = ArrowKey(e);
  if (ak) {
    let path = atom_glyphs[current_atom];
    for (o of path) {
      // XXX bezier
      o.x += ak.dx;
      o.y += ak.dy;
    }
  }

  switch (e.key) {
  case 'Delete': {
    let path = atom_glyphs[current_atom];
    if (path.length <= 3)
      break;
    const closest = ClosestPoint(path, mousex, mousey, CELLSIZE, false);
    if (!closest) return;
    path.splice(closest.idx, 1);
    break;
  }
  case '1':
  case '2': {
    console.log('bisect');
    if (dragging) return;
    let path = atom_glyphs[current_atom];
    const closest = ClosestPoint(path, mousex, mousey, CELLSIZE, false);
    if (!closest) return;
    const prev = path[closest.idx == 0 ? path.length - 1 : closest.idx - 1];
    const pt = path[closest.idx];
    const next = path[(closest.idx + 1) % path.length];
    if (e.key == '1') {
      const {x, y} = Bisect(prev, pt);
      let newpt = ScreenToGrid(x, y);
      path.splice(closest.idx, 0, newpt);
    } else if (e.key == '2') {
      const {x, y} = Bisect(pt, next);
      let newpt = ScreenToGrid(x, y);
      path.splice(closest.idx + 1, 0, newpt);
    } else {
      throw 'impossible';
    }
    break;
  }
  }
  Draw();
}

function KeyLetter(e) {
  if (e.key in letters) {
    current_letter = e.key;
    letter_piece = 0;
    Draw();
    return;
  }

  // Maybe should just save all data?
  if (e.code == 'NumpadEnter') {
    // Save data to frameserver.
    const data = JSON.stringify(letters);
    getSynchronous('/save_letters/' +
		   encodeURIComponent(data));
    return;
  }

  let ak = ArrowKey(e);
  if (ak) {
    if (e.shiftKey) {
      for (let piece of letters[current_letter].p) {
	piece.x += ak.dx;
	piece.y += ak.dy;
      }
    } else {
      let piece = letters[current_letter].p[letter_piece];
      piece.x += ak.dx;
      piece.y += ak.dy;
    }
  }

  switch (e.key) {
  case '[': {
    let piece = letters[current_letter].p[letter_piece];
    piece.r += 15;
    if (piece.r >= 360) piece.r -= 360;
    break;
  }
  case ']': {
    let piece = letters[current_letter].p[letter_piece];
    piece.r -= 15;
    if (piece.r < 0) piece.r += 360;
    break;
  }
  case ',':
    letter_piece--;
    if (letter_piece < 0)
      letter_piece = letters[current_letter].p.length - 1;
    break;
  case '.':
    letter_piece++;
    if (letter_piece >= letters[current_letter].p.length)
      letter_piece = 0;
    break;
  }
  
  Draw();
}

function KeyWord(e) {
  if (e.key == 'Escape') {
    current_word = '';
  }

  if (e.key == 'Backspace') {
    current_word = current_word.substr(0, current_word.length - 1);
  }
  
  if (e.key in letters) {
    current_word += e.key;
  }
  
  Draw();
}

function LayoutWord(word) {
  let slots = [];
  const total_width = WordWidth(word);
  let x = total_width * -0.5;
  for (let i = 0; i < word.length; i++) {
    let c = word[i];
    let pieces = letters[c].p;
    // The slot contains an object, keyed by atom, with a list of the
    // positions that atom appears in. We do this because the
    // SML-based planner doesn't know anything about the decomposition
    // of a letter except the count of atoms. (It doesn't know where
    // they are, for example.) This allows it to at least consistently
    // refer to an atom within a letter by saying "it's the third
    // tick".
    let slot = {};
    for (let piece of pieces) {
      if (!slot[piece.a]) slot[piece.a] = [];
      slot[piece.a].push({ x: x + piece.x, y: piece.y, r: piece.r });
    }
    slots.push(slot);
    x += letters[c].m.w;
  }
  return slots;
}


function BestSecant(a, b) {
  // We'll blend between the source and dest angles, but this
  // space is not convex or whatever because it is mod 360. So
  // pick a pair of equivalent angles with the shortest distance.
  const strategies = [
    {s: a - 360, d: b},
    {s: a, d: b - 360},
  ];
  let bestdist = Math.abs(b - a);
  let besta = a, bestb = b;
  for (const strat of strategies) {
    let dist = Math.abs(strat.s - strat.d);
    if (dist < bestdist) {
      bestdist = dist;
      besta = strat.s;
      bestb = strat.d;
    }
  }
  return {a: besta, b: bestb};
}

function InitializeAnimation() {
  if (timeout_id) {
    window.clearTimeout(timeout_id);
    timeout_id = null;
  }

  animate_frame = 0;

  // "Draw" the two words.
  let slots1 = LayoutWord(startword);
  let slots2 = LayoutWord(endword);
  console.log(slots1);
  console.log(slots2);

  // Make the array of mobile pieces.
  animate_pieces = [];
  for (let row of plan) {
    let atom = row.a;
    let { x: srcx, y: srcy, r: srcr } = slots1[row.ss][atom][row.sp];
    let { x: dstx, y: dsty, r: dstr } = slots2[row.ds][atom][row.dp];

    let { a, b } = BestSecant(srcr, dstr);
    animate_pieces.push({atom,
			 height: row.h,
			 src: { x: srcx, y: srcy, r: a },
			 dst: { x: dstx, y: dsty, r: b }})
  }

  const TOTAL_FRAMES = 300;
  let AnimateCallback = () => {
    animate_frame++;

    if (animate_frame > TOTAL_FRAMES) {
      // Don't clear canvas.
      
      window.clearTimeout(timeout_id);
      timeout_id = null;
      // swap words
      let t = endword;
      endword = startword;
      startword = t;
      // swap source/dest in plan.
      let newplan = [];
      for (let row of plan) {
	newplan.push({a: row.a, h: row.h,
		      ss: row.ds, sp: row.dp,
		      ds: row.ss, dp: row.sp});
      }
      plan = newplan;

    } else {
      ctx.clearRect(0, 0, CANVASWIDTH, CANVASHEIGHT);
      DrawGrid(null, null);

      // Pure linear interpolation
      let pct = animate_frame / TOTAL_FRAMES;
      // Actually, use cosine easing.
      let f = (1 - Math.cos(pct * Math.PI)) * 0.5;
      let omf = 1.0 - f;

      for (let ap of animate_pieces) {
	let x = f * ap.src.x + omf * ap.dst.x;
	let y = f * ap.src.y + omf * ap.dst.y;
	let r = f * ap.src.r + omf * ap.dst.r;

	const VERT_FRAC = 0.10;
	const CHANNEL_HEIGHT = 20;
	const zenith = ap.height * CHANNEL_HEIGHT;
	// Height displacement.
	if (f < VERT_FRAC) {
	  y -= zenith * (f / VERT_FRAC);
	} else if (f < (1.0 - VERT_FRAC)) {
	  y -= zenith;
	} else {
	  y -= zenith * (omf / VERT_FRAC);
	}
	
	let fill = '#000';
	let path = TransformPath(atom_glyphs[ap.atom], x, y, r);
	DrawPath(path, fill, null);
      }
      timeout_id = window.setTimeout(AnimateCallback, 1000 / 60);
    }
  };
  timeout_id = window.setTimeout(AnimateCallback, 1000 / 60);
}

function KeyAnimate(e) {
  // Restart animation.
  if (e.key == ' ') {
    InitializeAnimation();
  }
}

function Key(e) {
  if (locked)
    return;

  // Common keys.
  switch (e.key) {
  case '`': {
    mode++; mode %= NUM_MODES;
    dragging = null;
    Draw();
    window.focus();
    document.getElementById('canvas').focus();
    return;
  }
  case '+':
  case '-':
  case '=': {
    const dir = e.key == '-' ? -1 : 1;
    SetCellSizeIdx(CELLSIZEIDX + dir);
    Draw();
    return;
  }
  }
    
  console.log(e);
  
  if (mode == MODE_ATOM) {
    KeyAtom(e);
  } else if (mode == MODE_LETTER) {
    KeyLetter(e);
  } else if (mode == MODE_WORD) {
    KeyWord(e);
  } else if (mode == MODE_ANIMATE) {
    KeyAnimate(e);
  }
}

function Init() {
  let c = document.getElementById('canvas');
  ctx = c.getContext('2d');
  Draw();

  c.onmousedown = Click;
  c.onmouseup = Unclick;
  c.onmousemove = MouseMove;
  window.addEventListener("keydown", Key, false);
}
