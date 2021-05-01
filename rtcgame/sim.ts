
// Sketch of simulation

// TODO: Frame index representation! 32 bits is seemingly not enough?
// (810 days at 60fps, but we really do want simulations to be able to
// persist indefinitely and assume the frame counter is monotonic).
// Would be nice to use BigInt but it's not available in Safari yet
// (it's in TP so maybe it's still a reasonable choice, like maybe it
// comes out with the next version of iOS even?) Could use a pair of
// 32-bit ints and implement simple number ops manually, too.

// TODO: Could be useful to have a test harness that checks for
// expected properties (e.g. determinism) of zipStep/step/etc. given
// an actual production implementation of Ops.

// A simulation is parameterized on the following types:
//  - Input, the per-player input for a single frame
//  - State, the "game state" at a particular frame.
// Ops encapsulates all the operations on these, which define what
// we're actually simualating. These two types are left completely
// abstract (no member functions/properties). Everything happens
// through ops.
interface Ops<Input, State> {
  // Input is the inputs (like they might be pressing multiple keys)
  // for one player on one frame.
  emptyInput() : Input;
  cloneInput(i : Input) : Input;
  inputString(i : Input) : string;
  eqInput(a : Input, b : Input) : boolean;

  // State is the complete state of the stimulation. Needs to be
  // serializable because we share them, at least to onboard new
  // players, but also to "save"/"load" simulations.

  // Debugging print.
  stateString(s : State) : string;
  cloneState(s : State) : State;
  serializeState(s : State) : string;
  deserializeState(s : string) : State;
  initialState(N : number) : State;
  eqState(a : State, b: State) : boolean;

  // For complete input_row and stale_row.
  // Assuming that step(stale_src_state, stale_row) results in stale_dst_state,
  // compute step(state, input_row). This can apply various optimizations,
  // for example in the simple case that state = stale_src_state and
  // input_row = stale_row are the same!
  zipStep(src_state : State, input_row : InputRow<Input, State>,
	  stale_src_state : State, stale_row : InputRow<Input, State>,
	  stale_dst_state : State) : State;
  // Step without any previous stale state/inputs, for example when we
  // compute a frame for the first time.
  step(src_state : State, input_row : InputRow<Input, State>) : State;
}

// Set of inputs (possibly incomplete) for a frame.
// (This doesn't actually depend on state.)
class InputRow<Input, State> {
  // readonly N: number;
  // readonly ops: Ops<Input, State>;
  inputs: Array<Input | null>;
  
  constructor(private readonly ops : Ops<Input, State>,
              public N : number) {
    // In a row, null means unknown. It is not a valid input.
    this.inputs = new Array(N).fill(null);
  }

  known(idx : number) : boolean {
    return this.inputs[idx] !== null;
  }

  // Get an input that is known.
  get(idx : number) : Input {
    let i = this.inputs[idx];
    if (i === null) throw 'use known to test this';
    return i;
  }

  set(idx : number, value : Input) : void {
    if (value === null) throw 'null is not a valid input';
    this.inputs[idx] = value;
  }

  toString() : string {
    let a = [];
    for (let i = 0; i < this.N; i++) {
      if (this.known(i)) {
        a.push(this.ops.inputString(this.get(i)));
      } else {
        a.push('?');
      }
    }
    return a.join(',');
  }
  
  complete() : boolean {
    for (let i = 0; i < this.N; i++)
      if (!this.known(i))
	return false;
    return true;
  }

  // Clone, cloning inputs as well.
  clone() : InputRow<Input, State> {
    let ret = new InputRow<Input, State>(this.ops, this.inputs.length);
    for (let i = 0; i < this.inputs.length; i++) {
      let iput = this.inputs[i];
      if (iput === null) {
	ret.inputs[i] = null;
      } else {
	ret.inputs[i] = this.ops.cloneInput(iput);
      }
    }
    return ret;
  }
  
  static eq<Input, State>(ops : Ops<Input, State>,
                           a: InputRow<Input, State>,
                           b: InputRow<Input, State>) : boolean {
    if (a.N != b.N) throw 'different radix';
    for (let i = 0; i < a.N; i++) {
      if (a.known(i) != b.known(i))
        return false;
      if (a.known(i) && !ops.eqInput(a.get(i), b.get(i)))
	return false;
    }
    return true;
  }
  
  // A complete row of empty inputs.
  static completeEmpty<Input, State>(ops : Ops<Input, State>, N : number) {
    let ret = new InputRow<Input, State>(ops, N);
    for (let i = 0; i < N; i++) {
      ret.inputs[i] = ops.emptyInput();
    }
    return ret;
  }
}


// Representation of the uncertainty window's data. First frame in
// the window has index 0, but is index cframe to the Sim.
// Each row is a WindowRow {state, stale, actual}:
//  - state = step(prev_row[state], stale).
//    This is the state speculatively computed for that frame. This state
//    could be inaccurate because stale != actual, or because some
//    earlier state was inaccurate.
//  - The stale inputs, which is what we used to simulate that frame.
//    These are all filled in, by guessing.
//  - The actual inputs, which may not be completely filled in yet.
type WindowRow<Input, State> =
  {state: State,
   stale: InputRow<Input, State>,
   actual: InputRow<Input, State>};
//
// Wrapper around list so that we can replace it with a better data
// structure if needed.
class UWindow<Input, State> {
  private data: Array<WindowRow<Input, State>>;
  
  // PERF Use a circular buffer or deque or something like that. This
  // needs access to both ends, but also random access probably?
  constructor(private readonly ops : Ops<Input, State>) {
    this.data = [];
  }

  empty() : boolean {
    return this.data.length === 0;
  }
  
  // Is this even needed? It should always be nframe - cframe.
  length() {
    return this.data.length;
  }

  getRow(widx : number) : WindowRow<Input, State> {
    if (widx < 0 || widx >= this.data.length) throw 'widx oob';
    return this.data[widx];
  }

  // Append row when we advance nframe.
  pushBack(wrow : WindowRow<Input, State>) {
    this.data.push(wrow);
  }

  removeFront() : WindowRow<Input, State> {
    let elt = this.data.shift();
    if (elt === undefined) throw 'remove on empty window';
    return elt;
  }
};

// Queued inputs are beyond the current uncertainty window; we
// just queue them until they become relevant.
type QueuedInput<Input> = {frame: number, player_idx: number, input: Input};

// A view of the simulation from a specific peer. This is distributed, so
// even though we may talk about "the" simulation, it's realized by a
// collection of these spread around the network. When the inputs are all
// up to date, they compute the same thing.
//
// A simulation has a fixed number of players N with indices 0..N-1.
// I also know which player index I am.
class Sim<Input, State> {
  cframe: number;
  cstate: State;
  cinputs: InputRow<Input, State>;
  mframe: number;
  window: UWindow<Input, State>;
  nframe: number;
  queued_inputs: Array<QueuedInput<Input>>;
  
  constructor(private ops : Ops<Input, State>,
              // Number of participants.
              public readonly N : number,
              public readonly my_id : number,
              start_frame : number,
              start_state : State) {
    if (my_id < 0 || my_id >= N) throw 'my_id out of radix';
    
    // The frame idx before which we believe all players have all inputs.
    // Before this we discard history, so it's impossible to rewind.
    this.cframe = start_frame;

    // The state before cframe. If everything is working correctly,
    // all participants agree that the state has this value entering
    // cframe.
    this.cstate = start_state;
    // Some approximation of the inputs that produced cstate.
    // This can be anything and we will still get the correct result,
    // since it is only used to guess inputs that we haven't yet received.
    // So it is okay to just start it as empty (and this is a good guess
    // at the very beginning of the simulation anyway).
    // (XXX we also use this in getMostRecentState, for advanceframe..
    // is it true that it's only used for guessing?)
    this.cinputs = InputRow.completeEmpty<Input, State>(this.ops, N);
    
    // Frame idx before which we have all inputs (i.e., one for each
    // player in 0..N-1). Must be at least cframe by definition, but
    // we may know more beyond that. We try to get everyone to advance
    // cframe up to this point. Within the window, the states are
    // accurate up to mframe. (XXX: Could be a property of
    // the window?)
    this.mframe = start_frame;
    
    // The next frame that we will speculatively simulate (for its
    // first time). As this local replica makes an input, it's written
    // to this frame.
    this.nframe = start_frame;
    // The state before nframe is available from getMostRecentState().

    // Invariant that cframe <= mframe <= nframe.
    
    // The uncertainty window. Entering the window, the state is cstate,
    // and the first frame has index cframe. It ranges until nframe,
    // which is what we're showing to the user and where this user's
    // inputs would be written.
    this.window = new UWindow<Input, State>(this.ops);


    // If we receive inputs >= nframe, we just queue them. They will
    // be added in advanceFrame when the local player makes an input
    // at that frame.
    // PERF sorted or keyed by frame? we do a linear scan in advanceFrame.
    this.queued_inputs = [];
  }

  // Returns the "current" frame index (=nframe), which is the one
  // that the user's next input will be used to generate.
  getNFrame() : number {
    return this.nframe;
  }

  getMFrame() : number {
    return this.mframe;
  }

  getCFrame() : number {
    return this.cframe;
  }

  // Update local cframe. This should only move forward, so it must be
  // >= every participant's cframe (including this.cframe). It should
  // be <= every participant's mframe (including this.mframe).
  // This modifies the window, cframe, and cinputs.
  setConsensus(frame : number) {
    if (frame < this.cframe)
      throw 'precondition';
    if (frame === this.cframe)
      return;
    if (frame > this.mframe)
      throw 'precondition';

    while (this.cframe < frame) {
      let wrow = this.window.removeFront();
      this.cstate = wrow.state;
      this.cinputs = wrow.actual;
      this.cframe++;
    }
  }
  
  // Used in debugging.
  checkInvariants() {
    if (!(this.cframe <= this.mframe)) throw 'want cframe <= mframe';
    if (!(this.mframe <= this.nframe)) throw 'want mframe <= nframe';

    for (const qi of this.queued_inputs) {
      if (qi.frame < this.nframe) throw 'queued input not future';
      if (qi.player_idx < 0 || qi.player_idx >= this.N ||
          qi.player_idx == this.my_id) throw 'bad queued input player idx';
     }

    if ((this.nframe - this.cframe) != this.window.length())
      throw 'window is the wrong length';

    if (!this.cinputs.complete())
      throw 'input guess should be complete';

    // TODO: More here!
    // can verify that the stale states in the window are correct...
    // I think this is high value since that's the most complex part.
    // - check that objects are not aliased?
  }
  
  // Advance a frame by supplying the local player's input.
  // Updates the window. Updates nframe.
  advanceFrame(input : Input) {
    let nrow = new InputRow<Input, State>(this.ops, this.N);
    nrow.set(this.my_id, input);
    // Process any inputs we already recieved (from other players)
    // for this frame.
    let oq = [];
    for (let qi of this.queued_inputs) {
      if (qi.frame === this.nframe) {
        if (nrow.known(qi.player_idx)) {
          // TODO: Perhaps allow duplicates if they are equal?
          throw 'duplicate inputs for player in queued input';
        } else {
          nrow.set(qi.player_idx, qi.input);
        }
      } else {
        oq.push(qi);
      }
    }
    this.queued_inputs = oq;

    // Compute the new row for the uncertainty window.
    let last = this.getMostRecentState();

    // PERF: It's possible (but rare?) that we had inputs from all
    // other players and an accurate state, and are now creating an
    // accurate frame, but we currently let a later updateWindow
    // handle this. Better than implementing the optimization here
    // would be to refactor to share functionality.
    let guessed_inputs = this.guessInputRow(last.inputs, nrow).inputs;

    let state = this.ops.step(last.state, guessed_inputs);

    // Add to window.
    this.window.pushBack({stale: guessed_inputs, actual: nrow, state: state});

    this.nframe++;
  }

  // Compute complete input row guess given some previous (complete)
  // input row (guess) and a current (possibly incomplete) (actual) input
  // row. Returns true for 'guessed' if we had to guess inputs; otherwise
  // the inputs are accurate and could be used with an accurate state
  // to produce an accurate successor.
  guessInputRow(prev : InputRow<Input, State>,
                actual : InputRow<Input, State>) :
  {inputs : InputRow<Input, State>, guessed : boolean} {
    if (actual.complete()) {
      // we have the actual inputs, so use those.
      return { inputs: actual.clone(), guessed: false };
    } else {
      let inputs = actual.clone();
      for (let i = 0; i < this.N; i++) {
	if (!inputs.known(i)) {
          // don't try to 'set' null
          // TODO: probably the "guess" should be informed by ops.
          // Some kinds of inputs may be inherently "impulse" and so
          // copying them is a bad guess!
          if (prev.known(i)) {
	    inputs.set(i, prev.get(i));
          }
	}
      }
      return { inputs: inputs, guessed: true };
    }
  }
  
  // Get the state at nframe - 1, and the input row (possibly
  // inaccurate) used to compute it.
  getMostRecentState() : {state : State, inputs : InputRow<Input, State>} {
    if (this.window.empty()) {
      return {state: this.cstate, inputs: this.cinputs};
    } else {
      let wlen = this.window.length();
      let {state, stale, actual} = this.window.getRow(wlen - 1);
      return {state: state, inputs: stale};
    }
  }
    
  // Update the input at the given frame, like when we receive a
  // network message from a player with their input.
  // Need to call updateWindow after this to maintain invariants, but
  // can do a batch of setInput and then one updateWindow.
  setInput(frame : number, player_idx : number, input : Input) {
    // Nothing really wrong with this, but we expect the local
    // player's input to be handled by advanceFrame.
    if (this.my_id === player_idx)
      throw 'use advanceFrame to set local player input';

    // We could get inputs from the future (>=nframe). We just
    // queue those up so that we don't need to think about them.
    if (frame >= this.nframe) {
      this.queued_inputs.push({frame, player_idx, input});
      return;
    }

    // Should not happen: cframe represents the frame before which
    // all players agree on all inputs. (In principle we could
    // receive a late replay of a move we already got?)
    if (frame < this.cframe)
      throw 'setInput frame < cframe';

    let row = this.window.getRow(frame - this.cframe);

    // Again, in principle we could receive a replay. We could
    // ignore it if it's equal?
    if (row.actual.known(player_idx))
      throw 'setInput input already known';

    row.actual.set(player_idx, input);
  }
  
  // Update the window. Do this after setting multiple inputs in the
  // window, maybe once per UI frame, right before processing the
  // local player's input. Zips stale inputs/states with actual inputs
  // (still possibly incomplete) to update the states and stale inputs
  // in the window. Moves mframe/mstate forward when rows become complete.
  // Does not advance cframe/cstate.
  updateWindow() {
    // Index of the frame before which everything is accurate. This frame
    // will be in the window, or at its end (in which case the window
    // should be empty).
    let frame_accurate_before = this.mframe;

    // The accurate state preceding frame_accurate_before. This could
    // come from the window or be cframe.
    let src_state = (() => {
      let widx = frame_accurate_before = frame_accurate_before - this.cframe;
      if (widx < 0) throw 'invariant';
      if (widx == 0) return this.cstate;
      let row = this.window.getRow(widx - 1);
      return row.state;
    })();


    // To reuse old computations, we also need to keep track of what state
    // we used to compute each row in the input last time. This is the
    // state field from the previous row.
    let stale_src_state = src_state;

    let accurate = true;

    // FIXME: If we are recomputing from the middle of the window,
    // this needs to come from the window, not before it...
    let prev_inputs = this.cinputs;
    
    // Entering the loop,
    //   frame is the next frame to compute (lies in the window)
    //   stale_src_state is the state used to compute the frame
    //     last time.
    //   src_state is the state for this run.
    //   prev_inputs are the inputs from the previous row, which we
    //     use to guess unknown inputs on this row.
    //   accurate: true if we had all inputs (mframe advances).
    for (let frame = this.mframe; frame < this.nframe; frame++) {
      if (!prev_inputs.complete()) throw 'invariant';

      // Index will lie within the window.
      let widx = frame - this.cframe;
      let row = this.window.getRow(widx);
     
      // Guess inputs for this round, combining the inputs that
      // we know (row.actual) with the previous row.
      let guess = this.guessInputRow(prev_inputs, row.actual);
      
      // No longer known to be accurate because we had to guess inputs.
      if (guess.guessed) accurate = false;

      if (!guess.inputs.complete()) throw 'guessed inputs not complete?!';

      // TODO PERF: zipStep can apply its own optimizations with
      // knowledge of the state, but it might make sense to guarantee
      // certain optimizations by implementing them here or in a
      // wrapper. (This is no longer easy to do by extending some base
      // class due to the use of generics.) One important and easy one
      // is that if we guessed an input correctly, we should not have
      // to recompute.
      let state = this.ops.zipStep(
        src_state,
	// (possibly guessed) inputs for this execution.
	guess.inputs,
	// State from previous row.
	stale_src_state,
	// Stale inputs saved in row.
	row.stale,
	// State we computed last time.
	row.state);

      // Get values for next round.
      // State we computed last time is used for the next round,
      // but we're about to overwrite it.
      stale_src_state = row.state;
      prev_inputs = guess.inputs;
      src_state = state;
      
      // Write the result into the row.
      row.stale = guess.inputs;
      row.state = state;

      // Advance mframe if this computation is accurate; this should
      // only be the case if we are also currently at the mframe.
      if (accurate) {
	if (frame != this.mframe) throw 'bug?';
	this.mframe++;
      }
    }

    // State exiting the window; this is what we'd show to the user or
    // use to extend the window with recent user inputs.
    return src_state;
  }

  // TODO: advancing cframe. We probably do this by publishing our mframe
  // to everyone else, keeping for each other player the maximum such
  // value we've seen, and then updating cframe to be the minimum of these.
  
  
}
