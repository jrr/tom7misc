
// Utilities for testing ops (particularly the zipStep/step
// relationship).
class OpsTester<Input, State> {

  constructor(private readonly ops : Ops<Input, State>,
              public N : number) {}

  // TODO: Basic tests, like that serialization works?

  // This is the main test of interest. Runs a specific sequence of
  // inputs in different random orders to make sure that they produce
  // the same result. Errors could be in sim itself, or because
  // step and zipStep don't have the expected properties.
  //
  // We require that *some* state other than the initial state is
  // reached by the inputs (which also implies that we must have at
  // least one row of input). Otherwise the test would be degenerate.
  //
  // Returns the final state.
  testRandomOrder(
    // Rectangular array of inputs (inner width N; outer width is the
    // number of frames to run).
    inputs: Array<Array<Input>>,
    start: State,
    // Number of times to randomly issue the inputs.
    iters: number) : State {

    if (inputs.length === 0)
      throw 'must have at least one row of inputs';
    
    // Compute the reference final state with just step(),
    // as well as some state that is not equal to the final state.
    const {final, other} = (() => {
      let state = start;
      // Some state not equal to the *start* state.
      let other = null;
      for (let frame = 0; frame < inputs.length; frame++) {
        if (inputs[frame].length !== this.N)
          throw 'wrong length';
        let row = new InputRow<Input, State>(this.ops, this.N);
        for (let p = 0; p < this.N; p++) {
          row.set(p, inputs[frame][p]);
        }
        if (!row.complete()) throw '??';
        state = this.ops.step(state, row);
        if (this.ops.eqState(state, start)) {
          if (!this.ops.eqState(start, state))
            throw 'eqState is not symmetric';
          
        } else {
          // Test many examples for symmetry.
          if (this.ops.eqState(start, state))
            throw 'eqState is not symmetric';

          // And collect one example of a different state.
          if (other === null) {
            other = this.ops.cloneState(state);
            if (!this.ops.eqState(other, state))
              throw 'clone did not preserve equality';
            if (this.ops.eqState(other, start))
              throw 'clone did not preserve inequality';
          }
        }
      }

      if (other === null)
        throw ('never reached a state differing from the initial one. ' +
          'this is not "wrong" but the test will be degenerate, and ' +
          'might be due to a problem with eqState etc.');

      if (this.ops.eqState(other, start) || this.ops.eqState(start, other))
        throw 'eqState is not an eq relation or some problem in test?';
        
      // We computed a state not equal to the input state, but want
      // one that is different from the *final* state.
      if (this.ops.eqState(other, state)) {
        // If other == final and other != start, then final != start.
        if (this.ops.eqState(state, start))
          throw 'eqState is not an equivalence relation';
        return {final: state, other: start};
      } else {
        return {final: state, other: other};
      }
    })();

    let rc = new ArcFour('ops-tester');
    
    // Now test using the simulation.
    for (let iter = 0; iter < iters; iter++) {
      const me = rc.randTo32(this.N);
      // TODO: Could start after 0, I guess?
      const start_frame = 0;
      let sim = new Sim<Input, State>(this.ops,
                                      this.N,
                                      me,
                                      start_frame,
                                      start);
      sim.checkInvariants();

      // We want to issue all the inputs in a random order. Rather
      // than keep searching for an input that we haven't issued
      // yet, generate all the indices, then permute.
      // There is one complication, though, which is that we don't
      // allow issuing the local player's inputs out-of-order.
      // So we also keep a sorted list of the player's inputs, and
      // when we encounter a player input in the shuffled array,
      // we just execute the next one from that list.
      //
      // TODO: This totally random schedule seems to make it pretty
      // unlikely that we completely clear the window; we should
      // probably try to make this happen more often to tickle
      // those cases in the code.
      let indices : Array<{frame: number, player: number}> = [];
      for (let frame = 0; frame < inputs.length; frame++) {
        for (let player = 0; player < this.N; player++) {
          indices.push({frame, player});
        }
      }

      // Shuffle.
      rc.shuffleArray(indices);

      let next_player_index = 0;
      for (let {frame, player} of indices) {
        if (player === me) {
          // Instead of getting this input from the shuffled
          // indices, we'll use the next one from inputs.
          let input = inputs[next_player_index][player];
          sim.advanceFrame(input);
          next_player_index++;
          if (sim.getNFrame() != next_player_index + start_frame)
            throw 'wrong frame after advanceFrame';
        } else {
          let input = inputs[frame][player];
          sim.setInput(frame, player, input);
        }
        sim.checkInvariants();

        if (rc.byte() < 100) {
          sim.updateWindow();
          sim.checkInvariants();
        }

        if (rc.byte() < 100) {
          let cf = sim.getCFrame();
          // Treat the local maximum as being the network's max; we
          // aren't simulating it, but since this only happens (well,
          // if Sim is correct) when we've issued every input before
          // mframe, we won't be contradicted by advancing cframe this
          // far.
          let mf = sim.getMFrame();
          // how many frames can we advance, max?
          let adv = mf - cf;
          if (adv > 0) {
            let ncf = cf + rc.randTo32(adv + 1);
            sim.setConsensus(ncf);
            // console.log(cf + ' < ' + mf + '(' + adv + ') so ' + ncf);
          }
        }
      }

      // After all inputs are executed, update window one more
      // time. This should result in the correct final state.
      sim.updateWindow();
      sim.checkInvariants();
      
      // TODO: test mframe

      const sim_final1 = sim.getMostRecentState().state;
      if (!this.ops.eqState(sim_final1, final))
        throw ('didn\'t get the right final state on iter ' + iter);

      sim.setConsensus(sim.getMFrame());
      sim.checkInvariants();
      if (sim.getCFrame() != sim.getNFrame())
        throw 'should have been able to complete the window';
      const sim_final2 = sim.getMostRecentState().state;
      
      if (!this.ops.eqState(sim_final2, final))
        throw 'didn\'t get the right final2 state';
    }
      
    return final;
  }
  
}
