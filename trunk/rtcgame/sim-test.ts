
// Test instantiations of sim stuff.

// player input is an arbitrary int.
class TestInput {
  constructor(public data : number) { }
}

// test state is the recent history of inputs
// for each player, plus a weak hash of them.
const LAST = 10;
class TestState {
  // Outer: N players.
  // Inner: LAST inputs.
  hist: Array<Array<number>>;
  hash: number;
  constructor(public N : number) {
    this.hist = [];
    for (let p = 0; p < this.N; p++) {
      let h = [];
      for (let i = 0; i < LAST; i++) {
        h.push(0);
      }
      this.hist.push(h);
    }
    this.hash = 0xCAFE;
  }

  toString() : string {
    let s = 'hash: 0x' + this.hash.toString(16) + '\n';
    for (let p = 0; p < this.N; p++) {
      s += p.toString() + ': ' + this.hist[p].join(',') + '\n';
    }
    return s;
  }

  clone() : TestState {
    let ret = new TestState(this.N);
    ret.hist = [];
    // Deep copy. Probably should have some utility for
    // this!
    for (let p = 0; p < this.N; p++) {
      ret.hist.push([...this.hist[p]]);
    }
    ret.hash = this.hash;
    return ret;
  }
}

type TestRow = InputRow<TestInput, TestState>;

class TestOps implements Ops<TestInput, TestState> {
  inputString(i : TestInput) { return '' + i.data; }
  cloneInput(i : TestInput) : TestInput {
    return new TestInput(i.data);
  }
  eqInput(a : TestInput, b : TestInput) : boolean {
    return a.data === b.data;
  }
  emptyInput() {
    return new TestInput(0);
  }


  initialState(N : number) : TestState {
    return new TestState(N);
  }

  serializeState(a : TestState) : string {
    // XXX use json etc.
    return a.toString();
    // throw 'unimplemented';
  }

  cloneState(a : TestState) : TestState {
    return a.clone();
  }

  stateString(a : TestState) : string {
    return a.toString();
  }
  
  eqState(a : TestState, b : TestState) : boolean {
    // Might be smart to maintain a checksum and check it here.
    // return false;
    // PERF: Could be faster to compare the state directly,
    // especially when returning false for zipStep optimizations.
    return this.serializeState(a) === this.serializeState(b);
  }

  // TODO: Some of this should actually be utility code for anyone
  // who has to implement a state, right?
  zipStep(src_state : TestState, input_row : TestRow,
	  stale_src_state : TestState, stale_row : TestRow,
	  stale_dst_state : TestState) : TestState {
    if (!input_row.complete()) throw 'zipStep input_row not complete';
    if (!stale_row.complete()) throw 'zipStep stale_row not complete';

    // TODO: For debugging, we should compute the step fresh and
    // assert that it is the same as the one recomputed from the 
    // stale data.
    
    // This optimization is always valid because step() must be
    // deterministic.
    if (this.eqState(src_state, stale_src_state) &&
      InputRow.eq<TestInput, TestState>(this, input_row, stale_row)) {
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
  step(state : TestState, input_row : TestRow) : TestState {
    if (!input_row.complete()) throw 'step input_row not complete';

    // For the test simulation, we just pop the oldest inputs,
    // push the new row, and update the hash.
    if (input_row.N !== state.N) throw 'N should be the same??';

    let ret = this.cloneState(state);
    for (let p = 0; p < ret.N; p++) {
      ret.hist[p].shift();
      ret.hist[p].push(input_row.get(p).data);
    }

    // Hash is intentionally weak so that we can cause state collisions.
    // (This is not to test like "hash collision resistance" -- it's
    // common in practice to have multiple routes to the same state in
    // some game, like for example in the simple case that inputs are
    // sometimes ignored. Here we use hashing to catch accidental bugs
    // but want an easy way to also test confluent states.)

    // In particular, if more than half of players have the same
    // input, we set the hash to that value (and no other inputs matter).

    // Map numbers to their counts.
    let m = {};
    // And compute new hash as we go.
    let new_hash = (ret.hash + 1) | 0;
    for (let p = 0; p < input_row.N; p++) {
      let input = input_row.get(p);
      let v = input.data;
      m[v] = (m[v] || 0) + 1;

      new_hash = (new_hash * 31337 + p) | 0;
      new_hash ^= v;
    }

    // Was there a majority vote?
    const half = Math.floor(input_row.N / 2);
    for (const k in m) {
      if (m[k] > half) {
        ret.hash = parseInt(k);
        return ret;
      }
    }

    // Otherwise, just some arbitrary function.
    ret.hash = new_hash;
    
    return ret;
  }
}


(() => {
  let ir : TestRow = new InputRow(new TestOps, 3);
  let ire1 : TestRow = InputRow.completeEmpty(new TestOps, 3);
  let ire2 : TestRow = InputRow.completeEmpty(new TestOps, 3);


  ir.set(1, new TestInput(7));
  ire1.set(0, new TestInput(2));
  ire2.set(0, new TestInput(2));

  console.log((new TestOps).inputString(ir.get(1)));
  console.log(ire1.toString());
  console.log(InputRow.eq<TestInput, TestState>(new TestOps, ire1, ire2));
})();
  
console.log('------');

(() => {
  const ops = new TestOps;

  let s = ops.initialState(3);
  console.log(ops.stateString(s));

  let ire = InputRow.completeEmpty(new TestOps, 3);
  ire.set(0, new TestInput(5));
  ire.set(1, new TestInput(7));
  ire.set(2, new TestInput(1));
  console.log('row: ' + ire.toString());
  let s2 = ops.step(s, ire);
  console.log(ops.stateString(s2));

  let ire2 = InputRow.completeEmpty(new TestOps, 3);
  ire2.set(0, new TestInput(3));
  ire2.set(1, new TestInput(2));
  ire2.set(2, new TestInput(3));
  console.log('row: ' + ire2.toString());
  let s3 = ops.step(s2, ire2);
  console.log(ops.stateString(s3));
  if (s3.hash != 3) throw '3 should win vote';
})();
