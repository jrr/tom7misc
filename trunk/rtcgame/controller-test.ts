

let controller : Controller;

function showHeld(b : Button, id : string) {
  let elt = document.getElementById(id);
  if (!elt) return;

  if (b.impulse) {
    console.log(id + '!');
  }
  
  elt.style.background = b.held ? '#222' : '#F7F7F7';
}

function Frame() {
  let controls = controller.update();

  showHeld(controls.up, 'up');
  showHeld(controls.down, 'down');
  showHeld(controls.left, 'left');
  showHeld(controls.right, 'right');  

  showHeld(controls.select, 'select');
  showHeld(controls.start, 'start');  
  
  showHeld(controls.a, 'a');
  showHeld(controls.b, 'b');
  showHeld(controls.x, 'x');
  showHeld(controls.y, 'y');  
  
  // Continue looping...
  window.requestAnimationFrame(Frame);
}

function init () {
  controller = new Controller(true);
  window.requestAnimationFrame(Frame);
}
