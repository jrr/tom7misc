// Some files and configuration must be provided externally;
// see for example editor-ceors.html. We need
// config: atoms
// atoms: atom_glyphs
// letters: letters
// plan: startword, endword, plan (see anaglyph.sml)
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
let mode = MODE_ATOM;

// Are we writing frames to the frameserver? Set manually from
// console.
let saving = false;
let grid = true;

const DEG_TO_RADIANS = 3.141592653589 / 180.0;

// Atoms have a single path (e and o are just self-overlapping).
// (This might not work for i and j in the non-decomposing case btw.)
//
// Each point has an x,y position and two optional control points c,d
// (entry) and e,f (exit). We always start by moving to the first
// point and then drawing lines between pairs of successive nodes.
// Between two nodes n1 and n2, we always start at x1,y1 and then are
// in one of the following cases based on the presence of control
// points e1,f1 and c2,d2:
//
//        prev       next
//        x1,y1      x2,y2        lineTo(x2,y2)
//     x1,y1,e1,f1   x2,y2        quadraticCurveTo(e1,f1,x2,y2)
//        x1,y1     c2,d2,x2,y2   quadraticCurveTo(c2,d2,x2,y2)
//     x1,y1,e1,f1  c2,d2,x2,y2   bezierCurveTo(e1,f1,c2,d2,x2,y2)
//
// The path is always closed at the end. Coordinates are integer grid
// coordinates. The center of the atom (for rotations, etc.) is
// defined to be 0,0.

///////////////////////////
// For atom editing mode.
// Current node/control point being dragged (same as return type of 'closest')
let dragging = null;
let onion_atom = atoms[0];
let current_atom = atoms[0];

///////////////////////////
// For letter editing mode.
let current_letter = 'd';
// Index of current piece within the current_letter. Make sure
// it gets reset to 0 when current_letter changes.
let letter_piece = 0;
// Kerning pair (second) for the current letter.
let letter_kern = null;

//////////////////////////
// For word mode.
let current_word = 'anagram';

//////////////////////////
// For animate mode.
let timeout_id = null;
let animate_pieces = [];

const CANVASWIDTH = 1920;
const CANVASHEIGHT = 1080;

// Cell size must divide both evenly.
// Common factors of 1920x1080: 2, 2, 2, 3, 5
const CELLSIZES = [2, 3, 4, 5, 6, 8, 10, 12, 15, 20, 24, 30, 40];

// One for each mode.
let CELLSIZEIDXS = [8, 6, 2, 2];
(() => { if (CELLSIZEIDXS.length != NUM_MODES) throw 'bad init'; })();

let CELLSIZE = CELLSIZES[CELLSIZEIDXS[mode]];
let CELLSW = CANVASWIDTH / CELLSIZE;
let CELLSH = CANVASHEIGHT / CELLSIZE;

function SetCellSizeIdx(i) {
  if (i < 0 || i >= CELLSIZES.length) return;
  CELLSIZEIDXS[mode] = i;
  UpdateCellSizes();
}

function UpdateCellSizes() {
  let i = CELLSIZEIDXS[mode];
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
// If allow_control is true, can select control points (c,d,e,f)
// as well.
function ClosestPoint(path, sx, sy, mindist, allow_control) {
  let Dsq = (gridx, gridy) => {
    const { x, y } = GridToScreen(gridx, gridy);
    const dx = x - sx;
    const dy = y - sy;
    return dx * dx + dy * dy;
  };
  let dsquared =
      (CANVASWIDTH + CANVASHEIGHT) * (CANVASWIDTH + CANVASHEIGHT);
  let best = null;
  for (let i = 0; i < path.length; i++) {
    let pt = path[i];
    const dsq = Dsq(pt.x, pt.y);
    if (dsq < dsquared) {
      dsquared = dsq;
      best = { idx: i, xy: true };
    }

    if (allow_control && pt.c != undefined && pt.d != undefined) {
      const cdsq = Dsq(pt.c, pt.d);
      if (cdsq < dsquared) {
	dsquared = cdsq;
	best = { idx: i, cd: true };
      }
    }

    if (allow_control && pt.e != undefined && pt.f != undefined) {
      const edsq = Dsq(pt.e, pt.f);
      if (edsq < dsquared) {
	dsquared = edsq;
	best = { idx: i, ef: true };
      }
    }
  }
  if (mindist != undefined && dsquared < mindist * mindist) {
    return best;
  } else {
    return null;
  }
}

function DrawPath(p, fill, stroke) {
  ctx.beginPath();
  if (p.length == 0) throw 'empty path';
  ctx.strokeStyle = stroke;
  ctx.fillStyle = fill;
  // Start is special; we just move there.
  const { x: startx, y: starty } = GridToScreen(p[0].x, p[0].y);
  ctx.moveTo(startx, starty);
  // Now work on pairs.
  for (let i = 0; i < p.length; i++) {
    const prev = p[i];
    const next = p[(i + 1) % p.length];

    // We can assume that x,y are present always.
    const {x, y} = GridToScreen(next.x, next.y);

    if ((prev.e == undefined || prev.f == undefined) &&
	(next.c == undefined || next.d == undefined)) {
      // No control points.
      ctx.lineTo(x, y);
    } else if (prev.e != undefined && prev.f != undefined) {
      const {x:e, y:f} = GridToScreen(prev.e, prev.f);
      if (next.c != undefined && next.d != undefined) {
	const {x:c, y:d} = GridToScreen(next.c, next.d);
	// Both control points.
	ctx.bezierCurveTo(e, f, c, d, x, y);
      } else {
	// Just exit control point; use quadratic.
	ctx.quadraticCurveTo(e, f, x, y);
      }
    } else {
      if (!(next.c != undefined && next.d != undefined))
	throw 'impossible';
      const {x:c, y:d} = GridToScreen(next.c, next.d);
      ctx.quadraticCurveTo(c, d, x, y);
    }
  }
  if (fill) ctx.fill();
  if (stroke) {
    ctx.lineWidth = 6;
    ctx.stroke();
  }
}

// Returns integral screen x,y coordinate that's in the center between
// the two points. Honors cubic and quadratic curves.
function Bisect(p1, p2) {
  // XXX do something for quadratic/bezier? It's not actually that
  // hard to compute the midpoint.
  const { x: p1x, y: p1y } = GridToScreen(p1.x, p1.y);
  const { x: p2x, y: p2y } = GridToScreen(p2.x, p2.y);

  let Quadratic = (c, d) => {
    const x = 0.25 * p1x + 2 * 0.5 * 0.5 * c + 0.25 * p2x;
    const y = 0.25 * p1y + 2 * 0.5 * 0.5 * d + 0.25 * p2y;    
    return { x, y };
  }
  
  if (p1.e != undefined && p1.f != undefined) {
    const { x: e, y: f } = GridToScreen(p1.e, p1.f);

    if (p2.c != undefined && p2.d != undefined) {
      // Cubic.
      const { x: c, y: d } = GridToScreen(p2.c, p2.d);
      const x = 0.125 * p1x + 3*0.25*0.5*c + 3*0.5*0.25*e + 0.125 * p2x;
      const y = 0.125 * p1y + 3*0.25*0.5*d + 3*0.5*0.25*f + 0.125 * p2y;
      return { x, y };
    } else {
      return Quadratic(e, f);
    }
  } else if (p2.c != undefined && p2.d != undefined) {
    const { x: c, y: d } = GridToScreen(p2.c, p2.d);
    return Quadratic(c, d);
  }

  // Linear.
  return { x: Math.round((p1x + p2x) / 2),
	   y: Math.round((p1y + p2y) / 2) };
}

function DrawControlPoints(p, highlight) {
  if (p.length == 0) throw 'empty path';

  for (let i = 0; i < p.length; i++) {
    const pt = p[i];

    // True if the node is highlighted (could be one of its control
    // points).
    const highlighted = !!highlight && highlight.idx == i;

    // Draw node's position, which always exists.
    const {x, y} = GridToScreen(pt.x, pt.y);

    const DrawCircle = (x, y, width, lwidth, fill, stroke) => {
      ctx.beginPath();
      ctx.fillStyle = fill;
      ctx.strokeStyle = stroke;
      ctx.ellipse(x, y,
		  width, width, width, width,
		  360);
      ctx.fill();
      ctx.lineWidth = lwidth;
      ctx.stroke();
    };

    // console.log(p);
    let width;
    if (highlighted && highlight.xy) {
      DrawCircle(x, y, CELLSIZE * 0.45, 3,
		 'rgba(255,255,128,1.0)',
		 'rgba(64,0,0,1.0)');
    } else {
      DrawCircle(x, y, CELLSIZE * 0.4, 3,
		 'rgba(255,255,255,0.75)',
		 'rgba(0,0,0,0.75)');
    }

    // Draw entrance control point if it exists.
    if (pt.c != undefined && pt.d != undefined) {
      const {x:c, y:d} = GridToScreen(pt.c, pt.d);
      DrawCircle(c, d,
		 highlighted && highlight.cd ?
		 CELLSIZE * 0.35 :
		 CELLSIZE * 0.25,
		 1,
		 'rgba(128,255,128,0.75)',
		 'rgba(0,75,0,0.5)');
    }

    // Draw exit control point if it exists.
    if (pt.e != undefined && pt.f != undefined) {
      const {x:e, y:f} = GridToScreen(pt.e, pt.f);
      DrawCircle(e, f,
		 highlighted && highlight.cd ?
		 CELLSIZE * 0.35 :
		 CELLSIZE * 0.25,
		 1,
		 'rgba(255,128,128,0.75)',
		 'rgba(75,0,0,0.5)');
    }
  }

  // Next/prev bisection hints
  if (highlight && highlight.xy) {
    const hidx = highlight.idx;
    const pt = p[hidx];
    const prev = p[hidx == 0 ? p.length - 1 : hidx - 1];
    const next = p[(hidx + 1) % p.length];
    const { x: px, y: py } = Bisect(prev, pt);
    const { x: nx, y: ny } = Bisect(pt, next);
    // console.log(px, py, nx, ny);
    ctx.font = 'bold 12pt Helvetica,sans-serif';
    ctx.fillStyle = '#f00';
    ctx.fillText('1,3', px, py);
    ctx.fillText('2,4', nx, ny);
  }
}

// Draws the faint lines from nodes to their control points,
// if any. Highlight is an object { idx, xy, cd, ef }, the
// same as is returned from ClosestPoint.
function DrawControlLines(p, highlight) {
  if (p.length == 0) throw 'empty path';

  for (let i = 0; i < p.length; i++) {
    const pt = p[i];

    // Node's position always exists.
    const {x, y} = GridToScreen(pt.x, pt.y);

    const DrawLine = (x0, y0, x1, y1, stroke) => {
      ctx.beginPath();
      ctx.strokeStyle = stroke;
      ctx.moveTo(x0, y0);
      ctx.lineTo(x1, y1);
      ctx.lineWidth = 1.5;
      ctx.stroke();
    };

    // Draw entrance control point if it exists.
    if (pt.c != undefined && pt.d != undefined) {
      const {x:c, y:d} = GridToScreen(pt.c, pt.d);
      DrawLine(x, y, c, d, 'rgba(0,90,0,0.25)');
    }

    // Draw exit control point if it exists.
    if (pt.e != undefined && pt.f != undefined) {
      const {x:e, y:f} = GridToScreen(pt.e, pt.f);
      DrawLine(x, y, e, f, 'rgba(90,0,0,0.25)');
    }
  }
}


// Draw the grid at the current scale. dx and dy yield a red rule,
// reflected around the axis.
function DrawGrid(dx, dy, novert = false, cellstep = 1) {
  // Draw grid
  ctx.lineWidth = 1;
  for (let y = 0; y <= CELLSH; y += cellstep) {
    ctx.beginPath();
    const yy = y - CELLSH / 2;
    if (yy == 0) {
      ctx.strokeStyle = '#777';
    } else if (yy === dy || -yy === dy) {
      ctx.strokeStyle = '#d99';
    } else {
      ctx.strokeStyle = '#ddd';
    }
    ctx.moveTo(0, y * CELLSIZE);
    ctx.lineTo(CELLSIZE * CELLSW, y * CELLSIZE);
    ctx.stroke();
  }

  for (let x = 0; x <= CELLSW; x += cellstep) {
    ctx.beginPath();
    const xx = x - CELLSW / 2;
    if (xx == 0 && !novert) {
      ctx.strokeStyle = '#777';
    } else if (xx === dx || -xx === dx) {
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
    if (dragging.xy) {
      dx = path[dragging.idx].x;
      dy = path[dragging.idx].y;
    } else if (dragging.cd) {
      dx = path[dragging.idx].c;
      dy = path[dragging.idx].d;
    } else if (dragging.ef) {
      dx = path[dragging.idx].e;
      dy = path[dragging.idx].f;
    }
  }

  DrawGrid(dx, dy);

  ctx.font = 'bold 40pt Helvetica,sans-serif';
  ctx.fillStyle = '#ccd';
  ctx.fillText('Atom ' + current_atom, 20, 50);
  
  if (onion_atom) {
    const opath = atom_glyphs[onion_atom];
    DrawPath(opath, 'rgba(16,75,16,0.15)', undefined);
    // DrawControlPoints(path, undefined);
  }

  let highlight;
  if (dragging) {
    highlight = dragging;
  } else {
    highlight = ClosestPoint(path, mousex, mousey, CELLSIZE, true);
  }
  DrawControlLines(path, highlight);
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
    var o = {x: x + dx, y: y + dy};
    // Control points are in the same coordinate system
    // so can just be transformed like x,y are.
    if (pt.c != undefined && pt.d != undefined) {
      const c = pt.c * cosr - pt.d * sinr;
      const d = pt.d * cosr + pt.c * sinr;
      o.c = c + dx;
      o.d = d + dy;
    }
    if (pt.e != undefined && pt.e != undefined) {
      const e = pt.e * cosr - pt.f * sinr;
      const f = pt.f * cosr + pt.e * sinr;
      o.e = e + dx;
      o.f = f + dy;
    }
    ret.push(o);
  }
  return ret;
}

// This is TransformPath(p, 0, 0, 90), but without using
// sin/cos, in order to ensure that integer inputs remain
// integers.
function Rotate90(p, ccw) {
  let ret = [];
  for (const pt of p) {
    const sinr = ccw ? -1 : 1;
    // cosr is always 0.
    const x = - pt.y * sinr;
    const y =   pt.x * sinr;
    var o = {x, y};
    // Control points are in the same coordinate system
    // so can just be transformed like x,y are.
    if (pt.c != undefined && pt.d != undefined) {
      o.c = - pt.d * sinr;
      o.d =   pt.c * sinr;
    }
    if (pt.e != undefined && pt.e != undefined) {
      o.e = - pt.f * sinr;
      o.f =   pt.e * sinr;
    }
    ret.push(o);
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

  let kern = 0;
  if (letter_kern) {
    let dx = letter.m.w;
    if (letter.k && letter.k[letter_kern] != undefined) {
      kern = letter.k[letter_kern];
    }
    dx += kern;

    const pal = letters[letter_kern];
    for (let i = 0; i < pal.p.length; i++) {
      let piece = pal.p[i];
      let fill = 'rgba(127,127,160,1.0)'
      let path = TransformPath(atom_glyphs[piece.a],
			       piece.x + dx, piece.y, piece.r);
      DrawPath(path, fill, null);
    }
  }
  
  // Draw metrics
  ctx.lineWidth = 1;
  ctx.beginPath();
  ctx.strokeStyle = '#7a7';
  const { x: widthx, y: unused } = GridToScreen(letter.m.w, 0);
  ctx.moveTo(widthx, 0);
  ctx.lineTo(widthx, CELLSH * CELLSIZE);
  ctx.stroke();

  if (kern != 0) {
    ctx.lineWidth = 1;
    ctx.beginPath();
    ctx.strokeStyle = '#a77';
    const { x: widthx, y: unused } = GridToScreen(letter.m.w + kern, 0);
    ctx.moveTo(widthx, 0);
    ctx.lineTo(widthx, CELLSH * CELLSIZE);
    ctx.stroke();
  }
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
    const c = w[i];
    if (!letters[c]) debugger;
    total += letters[c].m.w;
    if (i != w.length - 1) {
      let pal = w[i + 1];
      if (letters[c].k && letters[c].k[pal]) {
	total += letters[c].k[pal];
      }
    }
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
    if (i != current_word.length - 1) {
      let pal = current_word[i + 1];
      if (letters[c].k && letters[c].k[pal]) {
	x += letters[c].k[pal];
      }
    }
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
    console.log(dragging);
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
      if (dragging.xy) {
	pt.x = gx;
	pt.y = gy;
      } else if (dragging.cd) {
	pt.c = gx;
	pt.d = gy;
      } else if (dragging.ef) {
	pt.e = gx;
	pt.f = gy;
      }
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

function GetFrameServerUrl(url, k) {
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

function GetSynchronous(url) {
  // XXX queue?
  if (locked)
    throw 'synchronous while locked';

  locked = 'Saving...';
  GetFrameServerUrl(url, function () {
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
    GetSynchronous('/save_atoms/' + code + '/' +
		   encodeURIComponent(data));
    return;
  }

  let ak = ArrowKey(e);
  if (ak) {
    let path = atom_glyphs[current_atom];
    for (o of path) {
      o.x += ak.dx;
      o.y += ak.dy;
      if (o.c != undefined) o.c += ak.dx;
      if (o.d != undefined) o.d += ak.dy;
      if (o.e != undefined) o.e += ak.dx;
      if (o.f != undefined) o.f += ak.dy;
    }
  }

  switch (e.key) {
  case 'Delete': {
    let path = atom_glyphs[current_atom];
    if (path.length <= 3)
      break;
    // XXX should allow deleting control points
    const closest = ClosestPoint(path, mousex, mousey, CELLSIZE, false);
    if (!closest) return;
    path.splice(closest.idx, 1);
    break;
  }
  case '[': {
    // Can only guarantee integer coordinates if we rotate 90 degrees.
    let path = Rotate90(atom_glyphs[current_atom], true);
    atom_glyphs[current_atom] = path;
    break;
  }
  case ']': {
    let path = Rotate90(atom_glyphs[current_atom], false);
    atom_glyphs[current_atom] = path;
    break;
  }
  case '1':
  case '2':
  case '3':
  case '4': {
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
    } else if (e.key == '3') {
      if (pt.c != undefined || pt.d != undefined) {
	delete pt.c;
	delete pt.d;
      } else {
	const {x, y} = Bisect(prev, pt);
	const ctrl = ScreenToGrid(x, y);
	pt.c = ctrl.x;
	pt.d = ctrl.y;
      }
    } else if (e.key == '4') {
      if (pt.e != undefined || pt.f != undefined) {
	delete pt.e;
	delete pt.f;
      } else {
	const {x, y} = Bisect(pt, next);
	const ctrl = ScreenToGrid(x, y);
	pt.e = ctrl.x;
	pt.f = ctrl.y;
      }
    } else {
      throw 'impossible';
    }
    break;
  }
  }
  Draw();
}

function KeyLetter(e) {
  const lc = e.key.toLocaleLowerCase();
  if (lc in letters) {
    if (e.shiftKey) {
      // Set kerning pair;
      letter_kern = lc;
      console.log(letter_kern);
    } else {
      current_letter = lc;
      letter_piece = 0;
      letter_kern = null;
    }
    Draw();
    return;
  }

  // Maybe should just save all data?
  if (e.code == 'NumpadEnter') {
    // Save data to frameserver.
    const data = JSON.stringify(letters);
    GetSynchronous('/save_letters/' + code + '/' +
		   encodeURIComponent(data));
    return;
  }

  let ak = ArrowKey(e);
  if (ak) {
    if (e.ctrlKey) {
      for (let piece of letters[current_letter].p) {
	piece.x += ak.dx;
	piece.y += ak.dy;
      }
    } if (e.shiftKey && letter_kern) {
      if (!letters[current_letter].k)
	letters[current_letter].k = {};
      let k = letters[current_letter].k[letter_kern] || 0;
      k += ak.dx;
      if (k == 0) {
	// Don't store default kerning.
	delete letters[current_letter].k[letter_kern];
	// Last one? Delete the k object too.
	if (Object.keys(letters[current_letter].k).length == 0) {
	  delete letters[current_letter].k;
	}
      } else {
	letters[current_letter].k[letter_kern] = k;
      }
      // XXX
      console.log(letters[current_letter]);
      
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
    if (i != word.length - 1) {
      let pal = word[i + 1];
      if (letters[c].k && letters[c].k[pal]) {
	x += letters[c].k[pal];
      }
    }
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

  const TOTAL_FRAMES = 200;
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

      // Don't overwrite.
      saving = false;
      
    } else {
      ctx.clearRect(0, 0, CANVASWIDTH, CANVASHEIGHT);
      // Don't draw vertical line (useless). Since we
      // are often zoomed way out, draw a coarser grid
      if (grid)
	DrawGrid(null, null, true, 6);

      const CHANNEL_HEIGHT = 30;

      // Pure linear interpolation
      let pct = animate_frame / TOTAL_FRAMES;
      // Actually, use cosine easing.
      let f = (1 - Math.cos(pct * Math.PI)) * 0.5;
      let omf = 1.0 - f;

      let a = omf * omf * omf;
      let b = 3 * omf * omf * f;
      let c = 3 * omf * f * f;
      let d = f * f * f;

      for (let ap of animate_pieces) {
	// Zenith point "above" start and end.
	let hsy = ap.src.y + ap.height * CHANNEL_HEIGHT;
	let hdy = ap.dst.y + ap.height * CHANNEL_HEIGHT;

	let x = (a + b) * ap.src.x + (c + d) * ap.dst.x;
	let y = a * ap.src.y + b * hsy + c * hdy + d * ap.dst.y;
	let r = (a + b) * ap.src.r + (c + d) * ap.dst.r;

	/*
	let x = f * ap.src.x + omf * ap.dst.x;
	let y = f * ap.src.y + omf * ap.dst.y;
	let r = f * ap.src.r + omf * ap.dst.r;

	const VERT_FRAC = 0.10;
	const zenith = ap.height * CHANNEL_HEIGHT;
	// Height displacement.
	if (f < VERT_FRAC) {
	  y -= zenith * (f / VERT_FRAC);
	} else if (f < (1.0 - VERT_FRAC)) {
	  y -= zenith;
	} else {
	  y -= zenith * (omf / VERT_FRAC);
	}
	*/

	let fill = '#000';
	let path = TransformPath(atom_glyphs[ap.atom], x, y, r);
	DrawPath(path, fill, null);
      }

      if (saving) {
	// Record the contents of the frame, saving it to the
	// frameserver, and continue with the next frame once that
	// has succeeded.
	const url = canvas.toDataURL();
	// console.log(url);
	GetFrameServerUrl('/frame/' + animate_frame + '/' +
			  encodeURIComponent(url),
			  AnimateCallback);
      } else {
	// Just keep drawing frames as the timer elapses.
	timeout_id = window.setTimeout(AnimateCallback, 1000 / 60);
      }
    }
  };
  // XXX maybe makes more sense to immediately invoke it?
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
    UpdateCellSizes();
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
    SetCellSizeIdx(CELLSIZEIDXS[mode] + dir);
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
