
// Sketch of simulation

// TODO: Some of this code is generic "core" code and some is specific
// to an instantiation on specific states/input/step definitions.
// Annoyingly, there seems to be no clean way to make this separation
// in typescript since "static" cannot be used in either interfaces
// (to constrain a generic type parameter) or with abstract (to enforce
// that e.g. an implementation of Inputs should have a static method
// empty()). We can't use <> in typescript like templates in C++
// (I guess at the place(s) that we instantiate Sim<T> for some particular
// T, it is not making a new instance of Sim specialized to T. Rather
// the class Sim is just erased to create a single instance. So if we
// want to do dispatch based on T, we need to do that explicitly by
// having an instance of T available. T's constructor is often used; this
// is essentially what the class "is", and the constructor apparently
// has the static properties attached to it. So you could also pass T's
// constructor to Sim's constructor and save it. Not awful, but note
// that we have to do this for all static methods of Sim, too (they
// wouldn't have access to members we saved in the constructor).

// (24 Apr 2021) Another option might be to create an explicit level
// distinction here, requiring that stuff like State and Input are
// concretely represented as bytes, which means that the object-level
// stuff has to do its own marshalling/casting. This comes with some
// performance penalty probably, and may also be annoying, but perhaps
// it makes "nesting" simulations more straightforward?
//
// An obvious twist on the previous is to use something like "object"
// or typescript "any" instead of a concrete representation in the
// previous.

// TODO: Frame index representation! 32 bits is seemingly not enough?
// (810 days at 60fps, but we really do want simulations to be able to
// persist indefinitely and assume the frame counter is monotonic).
// Would be nice to use BigInt but it's not available in Safari yet
// (it's in TP so maybe it's still a reasonable choice, like maybe it
// comes out with the next version of iOS even?) Could use a pair of
// 32-bit ints and implement simple number ops manually, too.


// A simulation is parameterized on the following types:
//  - Inputs, the per-player input for a single frame
//  - State, the "game state" at a particular frame.

// Per-player input for a frame.
interface InputsI {
  toString() : string;
  clone() : InputsI;
}

// Static parts of inputs.
interface InputsTypeI<Inputs extends InputsI> {
  // Don't actually need constructor.
  // new () : Inputs;
  empty() : Inputs;
  eq(a : Inputs, b : Inputs) : boolean;
}

// Set of inputs (possibly incomplete) for a frame.
class InputRow<Inputs extends InputsI, InputsType extends InputsTypeI<Inputs>> {
  readonly N: number;
  readonly inputs_type: InputsType;
  inputs: Array<Inputs>;

  // not allowed? :(
  // type Self = InputRow<Inputs extends InputsI, InputsType extends InputsTypeI<Inputs>>;
  
  constructor(N : number, inputs_type : InputsType) {
    this.N = N;
    this.inputs_type = inputs_type;
    // In a row, null means unknown. It is not a valid input.
    this.inputs = new Array(N).fill(null);
  }

  known(idx : number) : boolean {
    return this.inputs[idx] !== null;
  }

  get(idx : number) {
    return this.inputs[idx];
  }

  set(idx : number, value) : void {
    if (value === null) throw 'null is not a valid input';
    this.inputs[idx] = value;
  }

  complete() : boolean {
    for (let i = 0; i < this.N; i++)
      if (!this.known(i))
	return false;
    return true;
  }

  // Clone, cloning inputs as well.
  clone() : InputRow<Inputs, InputsType> {
    let ret = new InputRow<Inputs, InputsType>(this.inputs.length,
                                               this.inputs_type);
    for (let i = 0; i < this.inputs.length; i++) {
      if (this.inputs[i] === null) {
	ret.inputs[i] = null;
      } else {
	ret.inputs[i] = this.inputs[i].clone();
      }
    }
    return ret;
  }
  
  static eq
    <Inputs extends InputsI, InputsType extends InputsTypeI<Inputs>>
    (inputs_type : InputsType,
     a: InputRow<InputsI, InputsType>,
     b: InputRow<InputsI, InputsType>) : boolean {
    if (a.N != b.N) throw 'different radix';
    for (let i = 0; i < a.N; i++)
      if (!inputs_type.eq(a.get(i), b.get(i)))
	return false;
    return true;
  }

  
  // A complete row of empty inputs.
  static completeEmpty
    <Inputs extends InputsI, InputsType extends InputsTypeI<Inputs>>
    (N, inputs_type : InputsType) {
      let ret = new InputRow<Inputs, InputsType>(N, inputs_type);
      for (let i = 0; i < N; i++) {
        ret.inputs[i] = inputs_type.empty();
      }
      return ret;
    }
  
}

/*

// A view of the simulation from a specific peer. This is distributed, so
// even though we may talk about "the" simulation, it's realized by a
// collection of these spread around the network. When the inputs are all
// up to date, they compute the same thing.
//
// A simulation has a fixed number of players N with indices 0..N-1.
// I also know which player index I am.





type QueuedInput = {frame: number, player_idx: number, input: Inputs};
class Sim {
  readonly Inputs inputs_type;
  
  readonly N: number;
  readonly my_id: number;
  cframe: number;
  cstate: State;
  cinputs: InputRow;
  mframe: number;
  window: UWindow;
  nframe: number;
  queued_inputs: Array<QueuedInput>;
  
  constructor(
    inputs_type,

    N, my_id, start_frame, start_state) {
    // Type parameters. Typescript uses an erasure approach so we
    // also need some run-time representation of the type parameters
    // (their constructors).
    this.inputs_type = inputs_type;
    this.state_type = state_type;
    
    // Constants.
    this.N = N;
    this.my_id = my_id;

    // The frame idx before which we believe all players have all inputs.
    // Before this we discard history, so it's impossible to rewind.
    this.cframe = start_frame;

    // The state before cframe. All participants should agree that the
    // state has this value entering cframe.
    this.cstate = start_state;
    // Some approximation of the inputs at cstate.
    // This can be anything and we will still get the correct result,
    // since it is only used to guess inputs that we haven't yet received.
    // So it is okay to just start it as empty (and this is a good guess
    // at the very beginning of the simulation anyway).
    this.cinputs = InputRow.completeEmpty(N);
    
    // Frame idx before which we have all inputs. Must be at least cframe
    // by definition, but we may know more beyond that. We try to get
    // everyone to advance cframe up to this point. Within the window,
    // the states are accurate up to mframe.
    // (XXX NOTE: If inputs are "open world", then we can't know that
    // we are complete wrt newly-joining players. Here we are assuming
    // that we have exactly the N players.)
    this.mframe = start_frame;
    
    // The next frame that we will speculatively simulate (for its
    // first time). An input from this replica is written for this
    // frame.
    this.nframe = start_frame;
    // The state before nframe.
    // XXX do we need this? It's the same as the last state in the
    // window (or cstate if the window is empty).
    // this.nstate = start_state;

    // Invariant that cframe <= mframe <= nframe.
    
    // The uncertainty window. Entering the window, the state is cstate,
    // and the first frame has index cframe. It ranges until nframe,
    // which is what we're showing to the user and where this user's
    // inputs would be written.
    this.window = new UWindow();


    // If we receive inputs >= nframe, we just queue them and replay
    // them once they are in the cframe-nframe window.
    // XXX If we receive input for =nframe, we could be storing those
    // and executing them when we process the local input, which would
    // be slightly better (but more complicated). (Perhaps more simply, we
    // could consult the queued inputs and do this at the moment we
    // take the local input?)
    this.queued_inputs = [];
  }

  // Update the input at the given frame.
  // Need to call updateWindow after this to maintain invariants, but
  // can do a batch of setInput and then one updateWindow.
  setInput(frame, player_idx, input) {
    // We could get inputs from the future (>=nframe). We just
    // queue those up so that we don't need to think about them.
    if (frame >= this.nframe) {
      this.queued_inputs.push({frame, player_idx, input});
      return;
    }

    // Should not happen: cframe represents the frame before which
    // all players agree on all inputs.
    if (frame < this.cframe)
      throw 'setInput frame < cframe';

    let row = this.window.getRow(frame - this.cframe);

    if (row[player_idx].actual.known())
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

      // Input guess for this round. If the
      let inputs;
      if (row.actual.complete()) {
	// we have the actual inputs, so use those.
	inputs = row.actual;
	// accurate can stay true, but note we might already be inaccurate
	// because a previous row's inputs was incomplete.
      } else {
	inputs = row.actual;
	for (let i = 0; i < this.N; i++) {
	  if (!inputs.known(i)) {
	    inputs.set(i, prev_inputs.get(i));
	  }
	}
	// Not known to be accurate because we had to guess inputs.
	accurate = false;
      }

      if (!inputs.complete()) throw 'guessed inputs not complete?!';

      let state = State.zipStep(src_state,
				// (possibly guessed) inputs for this execution.
				inputs,
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
      prev_inputs = inputs;
      src_state = state;
      
      // Write the result into the row.
      row.stale = inputs;
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
}

type WindowRow = {state: State, stale: InputRow, actual: InputRow};

// Representation of the uncertainty window's data. First frame in
// the window has index 0, but is index cframe to the Sim.
// Each row has {state, stale, actual}:
//  - state = step(prev_row[state], stale).
//    This is the state speculatively computed for that frame. This state
//    could be inaccurate because stale != actual, or because some
//    earlier state was inaccurate.
//  - The stale inputs, which is what we used to simulate that frame.
//    These are all filled in, by guessing.
//  - The actual inputs, which may not be completely filled in yet.
//
// Wrapper around list so that we can replace it with a better data
// structure if needed.
class UWindow {
  private data: Array<WindowRow>;
  
  // PERF Use a circular buffer or deque or something like that. This
  // needs access to both ends, but also random access probably?
  constructor() {
    this.data = [];
  }

  // Is this even needed? It should always be nframe - cframe.
  length() {
    return this.data.length;
  }

  getRow(widx) {
    if (widx < 0 || widx >= this.data.length) throw 'widx oob';
    return this.data[widx];
  }
  
  // TODO operations.
  // this should really just be data structure operations, not logical
  // ones..
};
  
// Complete state of the simulation. Needs to be serializable because we
// share them, at least to onboard new players, but also to "save"/"load"
// simulations.
abstract class State {

  static eq(a : State, b : State) : boolean {
    // TODO EZ: Check the serialized representations.
    // Might be smart to maintain a checksum and check it here.
    return false;
  }

  // For complete input_row and stale_row.
  // Assuming that step(stale_src_state, stale_row) results in stale_dst_state,
  // compute step(state, input_row). This can apply various optimizations,
  // for example in the simple case that state = stale_src_state and
  // input_row = stale_row are the same!
  static zipStep(src_state : State, input_row : InputRow,
		 stale_src_state : State, stale_row : InputRow,
		 stale_dst_state : State) : State {
    if (!input_row.complete()) throw 'zipStep input_row not complete';
    if (!stale_row.complete()) throw 'zipStep stale_row not complete';

    // TODO: For debugging, we should compute the step fresh and
    // assert that it is the same as the one recomputed from the 
    // stale data.
    
    // This optimization is always valid because step() must be
    // deterministic.
    if (State.eq(src_state, stale_src_state) &&
	InputRow.eq(input_row, stale_row))
      return stale_dst_state;

    
    // TODO: Further optimizations in the hierarchical case.
    // TODO: Some way to write the state transformation just once, but
    // with optional stale data.
    
    // Always safe (but maybe slower) to just ignore the stale data.
    return State.step(src_state, input_row);
  }
  
  // The simulation's step function. Needs a complete input row.
  // Returns a new state.
  static step(state : State, input_row : InputRow) : State {
    throw 'unimplemented';
  }
  
}


// Set of inputs (possibly incomplete) for a frame.
class InputRow< Inputs extends InputsIface,
                InputOps extends InputOpsIface<Inputs> > {
  readonly N: number;
  readonly input_ops: InputOps;                 
  inputs: Array<Inputs>;
  
  constructor(N : number, input_ops : InputOps) {
    this.N = N;
    this.input_ops = input_ops;
    // In a row, null means unknown. It is not a valid input.
    this.inputs = new Array(N).fill(null);
  }

  known(idx : number) : boolean {
    return this.inputs[idx] !== null;
  }

  get(idx : number) {
    return this.inputs[idx];
  }

  set(idx : number, value) : void {
    if (value === null) throw 'null is not a valid input';
    this.inputs[idx] = value;
  }

  complete() : boolean {
    for (let i = 0; i < this.N; i++)
      if (!this.known(i))
	return false;
    return true;
  }

  // A complete row of empty inputs.
  static completeEmpty(N, input_ops : InputOps) {
    let ret = new InputRow<InputOps>(N, input_ops);
    for (let i = 0; i < N; i++) {
      ret.inputs[i] = input_ops.empty();
    }
    return ret;
  }
  
  // Clone, cloning inputs as well.
  clone() : InputRow {
    let ret = new InputRow(this.inputs.length);
    for (let i = 0; i < this.inputs.length; i++) {
      if (this.inputs[i] === null) {
	ret.inputs[i] = null;
      } else {
	ret.inputs[i] = this.inputs[i].clone();
      }
    }
    return ret;
  }
  
  static eq(input_ops : InputOps,
            a: InputRow, b: InputRow) : boolean {
    if (a.N != b.N) throw 'different radix';
    for (let i = 0; i < a.N; i++)
      if (!input_ops.eq(a.get(i), b.get(i)))
	return false;
    return true;
  }
}


*/
