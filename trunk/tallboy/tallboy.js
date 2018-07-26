
const CANVASWIDTH = 512;
const CANVASHEIGHT = 512;
const CELLSIZE = 16;
let bodyx = 5;
let bodyy = 5;
let leftarm = 0;
let rightarm = 0;
let legs = 0;

const MAPW = 32;
const MAPH = 32;
function MapFromString(s) {
  let m = [];
  for (let c of s) {
    m.push(c == '#');
  }
  return m;
}
function MapToString(m) {
  let s = '';
  for (let i = 0; i < m.length; i++) {
    s += m[i] ? '#' : ' ';
  }
  return s;
}

let map = MapFromString(
			"#################################                              ##                   #          ##                              ##           #                  ##                              ##               ####           ##     #       # #  ###         ##            ##       #        ##            #         #       ##            #          #      ##   #        #    ##     #     ##            #     #     #     ##             #  # # ##   #    ##             ##   #      #    ##   #    #      ####      #    ##                         #    ##        # #             ##    ##              #        #      ##      #   # ###   #   ##      ##     ## #     # ####  #  #    ##    ##   #    # #     #     ####  #       ###       ##        ##  ##         ###  ##          ##     #          ##            ##     ###                      ##                    #####     ##################################                              ##                              ##                              #################################");


function GetMap(x, y) {
  return map[y * MAPW + x];
}

function SetMap(x, y, v) {
  map[y * MAPW + x] = v;
}

function Draw() {
  ctx.clearRect(0, 0, CANVASWIDTH, CANVASHEIGHT);
  // Draw grid
  ctx.strokeStyle = '#ddd';
  ctx.lineWidth = 1;
  for (let y = 0; y <= MAPH; y++) {
    ctx.beginPath();
    ctx.moveTo(0, y * CELLSIZE);
    ctx.lineTo(CELLSIZE * MAPW, y * CELLSIZE);
    ctx.stroke();
  }
  for (let x = 0; x <= MAPW; x++) {
    ctx.beginPath();
    ctx.moveTo(x * CELLSIZE, 0);
    ctx.lineTo(x * CELLSIZE, CELLSIZE * MAPH);
    ctx.stroke();
  }

  // Draw map.
  ctx.fillStyle = '#000';
  for (let y = 0; y < MAPH; y++) {
    for (let x = 0; x < MAPW; x++) {
      if (GetMap(x, y)) {
	ctx.fillRect(x * CELLSIZE, y * CELLSIZE,
		     CELLSIZE, CELLSIZE);
      }
    }
  }
}

let handle = null;
function Click(e) {
  const x = 0 | (e.offsetX / CELLSIZE);
  const y = 0 | (e.offsetY / CELLSIZE);
  console.log({x, y});
  const v = !GetMap(x, y)
  SetMap(x, y, v);
  handle = {x, y, v};
  Draw();
}

function Drag(e) {
  const x = 0 | (e.offsetX / CELLSIZE);
  const y = 0 | (e.offsetY / CELLSIZE);

  if (handle && (handle.x != x || handle.y != y)) {
    SetMap(x, y, handle.v);
    handle.x = x;
    handle.y = y;
    Draw();
  }
}

function Unclick(e) {
  handle = null;
}

let ctx;
function Init() {
  let c = document.getElementById('canvas');
  ctx = c.getContext('2d');
  Draw();
  
  c.onmousedown = Click;
  c.onmouseup = Unclick;
  c.onmousemove = Drag;
}
