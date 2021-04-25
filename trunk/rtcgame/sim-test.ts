
// Test instantiations of sim stuff.

class ActualInputs {
  constructor(public data : number) { }
}

class ActualState {
  positions: Array<{x: number, y: number}>;
  constructor(public N : number) {
    this.positions = [];
    for (let i = 0; i < this.N; i++) {
      this.positions.push({x: i, y: 0});
    }
  }
}

type ActualRow = InputRow<ActualInputs, ActualState>;

class ActualOps implements Ops<ActualInputs, ActualState> {
  inputsString(i : ActualInputs) { return '' + i.data; }
  cloneInputs(i : ActualInputs) : ActualInputs {
    return new ActualInputs(i.data);
  }
  eqInputs(a : ActualInputs, b : ActualInputs) : boolean {
    return a.data === b.data;
  }
  emptyInputs() {
    return new ActualInputs(0);
  }


  initialState(N : number) : ActualState {
    return new ActualState(N);
  }

  serializeState(a : ActualState) : string {
    throw 'unimplemented';
  }
  
  eqState(a : ActualState, b : ActualState) : boolean {
    // Might be smart to maintain a checksum and check it here.
    // return false;
    // PERF: Could be faster to compare the state directly,
    // especially when returning false for zipStep optimizations.
    return this.serializeState(a) === this.serializeState(b);
  }

  zipStep(src_state : ActualState, input_row : ActualRow,
	  stale_src_state : ActualState, stale_row : ActualRow,
	  stale_dst_state : ActualState) : ActualState {
    if (!input_row.complete()) throw 'zipStep input_row not complete';
    if (!stale_row.complete()) throw 'zipStep stale_row not complete';

    // TODO: For debugging, we should compute the step fresh and
    // assert that it is the same as the one recomputed from the 
    // stale data.
    
    // This optimization is always valid because step() must be
    // deterministic.
    if (this.eqState(src_state, stale_src_state) &&
      InputRow.eq<ActualInputs, ActualState>(this, input_row, stale_row)) {
      return stale_dst_state;
    }

    
    // TODO: Further optimizations in the hierarchical case.
    // TODO: Some way to write the state transformation just once, but
    // with optional stale data.
    
    // Always safe (but maybe slower) to just ignore the stale data.
    return this.step(src_state, input_row);
  }

  // The simulation's step function. Needs a complete input row.
  // Returns a new state.
  step(state : ActualState, input_row : ActualRow) : ActualState {
    if (!input_row.complete()) throw 'step input_row not complete';
    throw 'unimplemented';
  }
}


let ir : ActualRow = new InputRow(new ActualOps, 5);
let ire1 : ActualRow = InputRow.completeEmpty(new ActualOps, 5);
let ire2 : ActualRow = InputRow.completeEmpty(new ActualOps, 5);


ir.set(1, new ActualInputs(7));
ire1.set(0, new ActualInputs(2));
ire2.set(0, new ActualInputs(2));

console.log((new ActualOps).inputsString(ir.get(1)));
console.log(ire1.toString());
console.log(InputRow.eq<ActualInputs, ActualState>(new ActualOps, ire1, ire2));

