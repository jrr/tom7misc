// Arcade input (discrete directional + some buttons), via htm5
// gamepad api, keyboard, or mobile touch screen. No support
// yet for analog or positional input; maybe that would be a
// different class.

// State of a discrete button.
class Button {
  held : boolean;
  impulse : boolean;

  constructor(h : boolean = false, i : boolean = false) {
    this.held = h;
    this.impulse = i;
  }

  static combine(prev : Button, cur : Button) : Button {
    return new Button(cur.held, cur.held && !prev.held);
  }

  clone() : Button {
    return new Button(this.held, this.impulse);
  }
}

// The state of the game's local controls; returned from update()
// below.
class Controls {
  up : Button;
  down : Button;
  left : Button;
  right : Button;

  // These buttons refer to the xinput positions, which are
  //        Y
  //      X   B
  //        A
  // Beware that these are not the same as other controllers,
  // e.g. the Super Nintendo swaps X/Y and swaps A/B.
  a : Button;
  b : Button;
  x : Button;
  y : Button;
  select : Button;
  start : Button;

  // TODO: Support L/R as well.
  
  constructor(up : Button = new Button,
              down : Button = new Button,
              left : Button = new Button,
              right : Button = new Button,

              a : Button = new Button,              
              b : Button = new Button,
              x : Button = new Button,
              y : Button = new Button,              
              select : Button = new Button,
              start : Button = new Button) {
    this.up = up;
    this.down = down;
    this.left = left;
    this.right = right;

    this.a = a;
    this.b = b;
    this.x = x;
    this.y = y;
    this.select = select;
    this.start = start;
  }

  // Using only the 'held' state from this and prev,
  // set the impulse state.
  static combine(prev : Controls, cur : Controls) : Controls {
    return new Controls(
      Button.combine(prev.up, cur.up),
      Button.combine(prev.down, cur.down),
      Button.combine(prev.left, cur.left),
      Button.combine(prev.right, cur.right),

      Button.combine(prev.a, cur.a),
      Button.combine(prev.b, cur.b),
      Button.combine(prev.x, cur.x),
      Button.combine(prev.y, cur.y),            
      Button.combine(prev.select, cur.select),
      Button.combine(prev.start, cur.start));
  }

  // Or just the 'held' state with some other control object,
  // modifying the current one in place.
  orWith(other : Controls) {
    this.up.held = this.up.held || other.up.held;
    this.down.held = this.down.held || other.down.held;
    this.left.held = this.left.held || other.left.held;
    this.right.held = this.right.held || other.right.held;    

    this.a.held = this.a.held || other.a.held;    
    this.b.held = this.b.held || other.b.held;
    this.x.held = this.x.held || other.x.held;
    this.y.held = this.y.held || other.y.held;    
    this.select.held = this.select.held || other.select.held;
    this.start.held = this.start.held || other.start.held;
  }
  
  clone() : Controls {
    return new Controls(this.up.clone(),
                        this.down.clone(),
                        this.left.clone(),
                        this.right.clone(),

                        this.a.clone(),
                        this.b.clone(),
                        this.x.clone(),
                        this.y.clone(),
                        this.select.clone(),
                        this.start.clone());
  }

  // serialize, deserialize...
}

class Controller {
  // This is somewhat tricky because the different input methods
  // present different interfaces. Keyboard gives us events on
  // edges, so we need to retain state to know if a key is currently
  // held. Gamepad gives us a polling interface. Touch controls
  // TODO. We abstract this for the client; each time they call
  // update they get the effective state of each button.
  // (We allow multiple modes of control to be active at once, but
  // we don't care about corner cases like if the keyboard "right"
  // button is pressed while the gamepad "right" is held. We allow
  // the key to get 'stuck' in such cases, and for the "impulse"
  // to be ignored.)
  
  // TODO: Configure what controller types are allowed?
  // TODO: Configure default keyboard mapping?
  // TODO: Configure buttons for on-screen touch?
  // TODO: Or perhaps better would be to support an enumeration
  // of built-in control schemes? e.g. "4 directions and two impulse".
  constructor(private readonly verbose : boolean = false) {
    window.addEventListener("gamepadconnected",
			    (e) => this.gamepadConnected(e));

    this.prev_controls = new Controls;
    this.kbd_controls = new Controls;
  }

  // Internal control state. Only the 'held' booleans are meaningful.
  // prev_controls are the button states returned to the client in
  // the last call to update. 
  private prev_controls : Controls;
  // Current state of keyboard (only), and only the 'held' booleans
  // are meaningful. We need to track this across frames because
  // the keyboard is event-driven.
  private kbd_controls : Controls;
  
  // Call once per frame. Computes and returns a new Controls state.
  update() : Controls {
    let cur_controls = this.readGamepad();
    cur_controls.orWith(this.kbd_controls);
    
    let ret = Controls.combine(this.prev_controls, cur_controls);
    // Shift so that we are ready for the next frame.
    this.prev_controls = cur_controls;
    return ret;
  }

  // This gets called when the gamepad is interacted with.
  // It often happens that there are multiple gamepads, so
  // we take the first one with "standard" mapping. the_gamepad
  // will be an index into the getGamepads array, or null (note
  // 0 != null) if we haven't detected one.
  private the_gamepad : (number | null) = null;
  private gamepadConnected(e : GamepadEvent) {
    if (this.verbose) {
      console.log('gamepad connected!');
      console.log(e);
    }
    if (this.the_gamepad != null) {
      if (this.verbose) {
        console.log('already have one.');
      }
      return;
    }

    if (!e.gamepad) return;
    let gp = navigator.getGamepads()[e.gamepad.index];
    if (!gp) return;
    if (this.verbose) {
      console.log('gp ' + gp.index + ' = ' + gp.id + ' mapping ' + gp.mapping);
    }
    if (gp.mapping == 'standard') {
      this.the_gamepad = e.gamepad.index;
    }
  }


  private readGamepad() : Controls {
    let controls = new Controls;
    if (this.the_gamepad === null) return controls;
    let gp = navigator.getGamepads()[this.the_gamepad];
    if (gp === null) return controls;
    
    // D-pad up down left right: 12, 13, 14, 15
    // buttons: 3 0 2 1, aka y a x b

    controls.up.held = gp.buttons[12].pressed;
    controls.down.held = gp.buttons[13].pressed;
    controls.left.held = gp.buttons[14].pressed;
    controls.right.held = gp.buttons[15].pressed;    

    /*
    let a = [];
    for (let i = 0 ; i < gp.buttons.length; i++) {
      if (gp.buttons[i].pressed) a.push(i);
    }
    if (a.length > 0) console.log(a);
    */
    controls.select.held = gp.buttons[8].pressed;
    controls.start.held = gp.buttons[9].pressed;
    
    controls.a.held = gp.buttons[0].pressed;
    controls.b.held = gp.buttons[1].pressed;
    controls.x.held = gp.buttons[2].pressed;
    controls.y.held = gp.buttons[3].pressed;    
    return controls;
  }


}
