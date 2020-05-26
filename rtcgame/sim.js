
// Sketch of simulation

// TODO: Frame index representation! 32 bits is seemingly not enough?
// (810 days at 60fps, but we really do want simulations to be able to
// persist indefinitely) Would be nice to use BigInt but it's not
// available in Safari yet (it's in TP so maybe it's still a reasonable
// choice, like maybe it comes out with the next version of iOS even?) 
// Could use a pair of 32-bit ints and implement simple number ops
// manually, too.

// A view of the simulation from a specific peer. This is distributed, so
// even though we may talk about "the" simulation, it's realized by a
// collection of these spread around the network. When the inputs are all
// up to date, they compute the same thing.
//
// A simulation has a fixed number of players N with indices 0..N-1.
// I also know which player index I am.
class Sim {
  constructor(N, my_id, start_frame, start_state) {
    // Constants.
    this.N = N;
    this.my_id = my_id;

    // The frame idx before which we believe all players have all inputs.
    // Before this we discard history, so it's impossible to rewind.
    this.cframe = start_frame;

    // The state before cframe. All participants should agree that the
    // state has this value entering cframe.
    this.cstate = start_state;

    // Frame idx before which we have all inputs. Must be at least cframe
    // by definition, but we may know more beyond that. We try to get
    // everyone to advance cframe up to this point.
    // (XXX NOTE: If inputs are "open world", then we can't know that
    // we are complete wrt newly-joining players. Here we are assuming
    // that we have exactly the N players.)
    this.mframe = start_frame;
    // 
    this.mstate = start_state;
    
    // The next frame that we will speculatively simulate. An input from
    // this replica is written for this frame.
    this.nframe = start_frame;
    this.nstate = start_state;

    // Invariant that cframe <= mframe <= nframe.
    
    // The uncertainty window. Entering the window, the state is cstate,
    // and the first frame has index cframe. It ranges until nframe,
    // which is what we're showing to the user and where this user's
    // inputs would be written.
    this.window = new Window();


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

    if (row[player_idx].known())
      throw 'setInput input already known';

    row.set(player_idx, input);

    // Try advancing mframe. But this is only possible if we may have just
    // completed the current row.
    //
    // XXX No, should really be comparing with old/predicted inputs, see below.
    if (frame == mframe) {
      let state = this.mstate;
      while (mframe <= cframe) {
	let r = this.window.getRow(mframe - this.cframe);
	if (r.complete()) {
	  mframe++;
	  this.mstate = step(state, r);
	  state = this.mstate;
	} else {
	  break;
	}
      }
    }

    // PERF: This should allow zipping the old inputs and old state with
    // the new ones to produce the new state. Two optimizations:
    //   - We may have predicted correct inputs when we simulated before.
    //     In this case there's nothing to do.
    //   - Some state transformations may know that inputs don't matter,
    //     or don't matter for some parts of the state (this would commonly
    //     be the case for hierarchical states where inputs are also
    //     hierarchical), making the simulation cheaper.

    // 
  }

  // TODO: 
  updateWindow() {
  }

  // For complete input_row and stale_row.
  // Assuming that step(state, stale_row) results in stale_state,
  // compute step(state, input_row). This can apply various optimizations,
  // for example in the simple case that input_row and stale_row are
  // the same!
  static zipStep(state, input_row, stale_row, stale_state) {
    if (!input_row.complete()) throw 'zipStep input_row not complete';
    if (!stale_row.complete()) throw 'zipStep stale_row not complete';

    // This optimization is always valid because step() must be
    // deterministic.
    if (InputRow.eq(input_row, stale_row))
      return stale_state;

    // TODO: Further optimizations in the hierarchical case.
    // TODO: Some way to write the state transformation just once, but
    // with optional stale data.
    
    // Always safe (but maybe slower) to just ignore the stale data.
    return step(state, input_row);
  }
  
  // The simulation's step function. Needs a complete input row.
  // Returns a new state.
  static step(state, input_row) {
    throw 'unimplemented';
  }
}


// Representation of the uncertainty window's data (dense series of
// optional inputs).
// Wrapper around list so that we can replace it with a better data
// structure if needed.
class Window {
  // PERF Use a circular buffer or deque, as this needs access to
  // both ends (but also random access?)
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
class State {
  
}

// Set of inputs (possibly incomplete) for a frame.
class InputRow {
  constructor(N) {
    // In a row, null means unknown. It is not a valid input.
    this.inputs = new Array(N).fill(null);
  }

  known(idx) {
    return this.inputs[idx] !== null;
  }

  get(idx) {
    return this.inputs[idx];
  }

  set(idx, value) {
    if (value === null) throw 'null is not a valid input';
    this.input[idx] = value;
  }

  complete() {
    for (let i = 0; i < N; i++)
      if (!known(idx))
	return false;
    return true;
  }

  // Clone, cloning inputs as well.
  clone() {
    let ret = new InputRow(this.inputs.length);
    for (let i = 0; i < this.inputs.length; i++) {
      if (this.inputs[i] === null) {
	ret.inputs[i] = null;
      } else {
	ret.inputs[i] = this.inputs[i].clone();
      }
    }
  }
  
  static eq(a, b) {
    if (a.N != b.N) throw 'different radix';
    for (let i = 0; i < a.N; i++)
      if (a.get(i) !== b.get(i))
	return false;
    return true;
  }
}

// Per-player input for a frame.
class Inputs {
  // TODO

  clone() {
    throw 'unimplemented';
  }
  
}
