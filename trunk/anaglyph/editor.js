
let ctx;
// Current node being dragged (same as return type of 'closest')
let dragging = null;
// In canvas screen coordinates.
let mousex = 10;
let mousey = 10;

// Atoms have a single path (e ok?). The first position is always a
// "moveto" command. Each successive position has an x,y coordinate,
// and if it has XXX then it is a bezier curveTo, otherwise a plain
// lineto. The path is always closed at the end.
// Coordinates are integer grid coordinates. The center of the atom
// is defined to be 0,0.
function defaultbox() {
  return [{x: -10, y: -10},
	  {x: 10, y: -10},
	  {x: 10, y: 10},
	  {x: -10, y: 10}];
};

// Can modify the mesh of atomic pieces.
let atoms = "ceorsy'.?";
let atom_glyphs = {
  "c": defaultbox(),
  "e": defaultbox(),
  "o": defaultbox(),
  "r": defaultbox(),
  "s": defaultbox(),
  "y": defaultbox(),
  "'": defaultbox(),
  ".": defaultbox(),
  "?": defaultbox()
};

let current_atom = 'c';

const CELLSIZE = 20;
const CELLSW = 96;
const CELLSH = 54;

const CANVASWIDTH = 1920;
const CANVASHEIGHT = 1080;

function Clear() {
  Draw();
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


// Returns the index of the closest point.
function ClosestPoint(path, sx, sy, mindist, allow_control) {
  let dsquared =
      (CANVASWIDTH + CANVASHEIGHT) * (CANVASWIDTH + CANVASHEIGHT);
  let besti = 0;
  for (var i = 0; i < path.length; i++) {
    var pt = path[i];
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
    ctx.font = '18pt Helvetica bold';
    ctx.fillStyle = '#f00';
    ctx.fillText("1", px, py);
    ctx.fillText("2", nx, ny);
  }
}

function Draw() {
  ctx.clearRect(0, 0, CANVASWIDTH, CANVASHEIGHT);
  
  // Draw grid
  ctx.lineWidth = 1;
  for (let y = 0; y <= CELLSH; y++) {
    ctx.beginPath();
    if (y == CELLSH / 2) {
      ctx.strokeStyle = '#777';
    } else {
      ctx.strokeStyle = '#ddd';
    }
    ctx.moveTo(0, y * CELLSIZE);
    ctx.lineTo(CELLSIZE * CELLSW, y * CELLSIZE);
    ctx.stroke();
  }

  for (let x = 0; x <= CELLSW; x++) {
    ctx.beginPath();
    if (x == CELLSW / 2) {
      ctx.strokeStyle = '#777';
    } else {
      ctx.strokeStyle = '#ddd';
    }
    ctx.moveTo(x * CELLSIZE, 0);
    ctx.lineTo(x * CELLSIZE, CELLSH * CELLSIZE);
    ctx.stroke();
  }

  // XXX obviously, make it possible to assemble atoms
  // to composite letters too.
  const path = atom_glyphs[current_atom];

  let highlight;
  if (dragging) {
    highlight = dragging.idx;
  } else {
    const closest = ClosestPoint(path, mousex, mousey, CELLSIZE, true);
    highlight = closest ? closest.idx : -1;
  }
  DrawPath(path, 'rgba(16,16,16,0.75)', '#55f');
  DrawControlPoints(path, highlight);
}

function Click(e) {
  console.log(e);
  const x = e.offsetX;
  const y = e.offsetY;

  const closest = ClosestPoint(atom_glyphs[current_atom],
			       mousex, mousey,
			       CELLSIZE, true);
  dragging = closest;
  Draw();
}

function Unclick(e) {
  dragging = null;
  Draw();
}

function MouseMove(e) {
  mousex = e.offsetX;
  mousey = e.offsetY;
  if (dragging) {
    const pt = atom_glyphs[current_atom][dragging.idx];
    const { x: gx, y: gy } = ScreenToGrid(mousex, mousey);
    pt.x = gx;
    pt.y = gy;
  }
    
  Draw();
}

function Key(e) {
  switch (e.key) {
  case '1':
  case '2':
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
  }
  Draw();
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
