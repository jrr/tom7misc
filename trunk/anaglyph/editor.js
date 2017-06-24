

// ctx.moveTo(x, y);
// ctx.lineTo(x1, y1);
// ctx.stroke();

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

function GridToScreen(x, y) {
  return {x : (x * CELLSIZE) + CANVASWIDTH / 2,
	  y : (y * CELLSIZE) + CANVASHEIGHT / 2};
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

function Draw() {
  ctx.clearRect(0, 0, CANVASWIDTH, CANVASHEIGHT);

  // Draw grid
  ctx.strokeStyle = '#ddd';
  ctx.lineWidth = 1;
  for (let y = 0; y <= CELLSH; y++) {
    ctx.beginPath();
    ctx.moveTo(0, y * CELLSIZE);
    ctx.lineTo(CELLSIZE * CELLSW, y * CELLSIZE);
    ctx.stroke();
  }

  for (let x = 0; x <= CELLSW; x++) {
    ctx.beginPath();
    ctx.moveTo(x * CELLSIZE, 0);
    ctx.lineTo(x * CELLSIZE, CELLSH * CELLSIZE);
    ctx.stroke();
  }

  // XXX obviously, make it possible to assemble atoms
  // to composite letters too.
  const path = atom_glyphs[current_atom];
  DrawPath(path, '#111', '#55f');
  
  /*
  // Circles.
  for (let i = 0; i < circles.length; i++) {
    let c = circles[i];
    ctx.beginPath();
    ctx.strokeStyle = '#ccf';
    ctx.lineWidth = 3;
    ctx.ellipse(c.x * CELLSIZE + CELLSIZE / 2, 
		c.y * CELLSIZE + CELLSIZE / 2,
		CELLSIZE * c.r, CELLSIZE * c.r,
		CELLSIZE * c.r, CELLSIZE * c.r,
		360);
    ctx.lineWidth = 3;
    ctx.stroke();
  }
  */
/*
  // All pairs lines.
  for (let i = 0; i < points.length; i++) {
    let p = points[i];
    if (p.next == null) {
      let pc = Circles(p);
      for (let j = i + 1; j < points.length; j++) {
	let q = points[j];
	if (q.next == null && p.color == q.color) {
	  let qc = Circles(q);
	  // XXX hack -- fastest way to compare equality
	  // of circle lists?
	  if ('' + pc == '' + qc) {
	    ctx.beginPath();
	    ctx.lineWidth = 6;
	    ctx.strokeStyle = ['#555', '#BBB'][p.color];
	    ctx.moveTo(p.x * CELLSIZE + CELLSIZE / 2,
		       p.y * CELLSIZE + CELLSIZE / 2);
	    ctx.lineTo(q.x * CELLSIZE + CELLSIZE / 2,
		       q.y * CELLSIZE + CELLSIZE / 2);
	    ctx.stroke();
	  }
	}
      }
    }
  }
  
  // Dots themselves (should be last)
  for (let i = 0; i < points.length; i++) {
    let p = points[i];
    ctx.beginPath();
    // console.log(p);
    ctx.fillStyle = ['#222', '#fff'][p.color];
    ctx.strokeStyle = ['#000', '#222'][p.color];
    ctx.ellipse(p.x * CELLSIZE + CELLSIZE / 2, 
		p.y * CELLSIZE + CELLSIZE / 2,
		CELLSIZE * 0.4, CELLSIZE * 0.4,
		CELLSIZE * 0.4, CELLSIZE * 0.4,
		360);
    ctx.fill();
    ctx.lineWidth = 3;
    ctx.stroke();
  }
*/
}

function Click(e) {
  console.log(e);
  let x = e.offsetX;
  let y = e.offsetY;
  points.push({x: Math.floor(x / CELLSIZE),
	       y: Math.floor(y / CELLSIZE),
	       next: null,
	       color: e.shiftKey ? 1 : 0})
  Draw();
}

let ctx;
function Init() {
  let c = document.getElementById('canvas');
  ctx = c.getContext('2d');
  Draw();

  c.onmousedown = Click;
}
