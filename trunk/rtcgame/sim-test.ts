
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
    // zero-fill right shift by zero to produce an unsigned result
    let s = 'hash: 0x' + (this.hash >>> 0).toString(16) + '\n';
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

  // Unlike the internal representation (and perhaps unwisely),
  // the outer here is the rounds (in reverse order) and inner
  // is N players. A number in the input array can be 'null',
  // which means accept anything.
  checkEndsWith(a : Array<Array<number | null>>) {
    for (let i = 0; i < a.length; i++) {
      let r = a[i];
      if (r.length != this.N)
        throw 'bad endsWith row';
      for (let j = 0; j < this.N; j++) {
        if (r[j] !== null) {
          if (this.hist[j][this.hist[j].length - 1 - i] !== r[j])
            throw ('state does not end correctly; offset ' +
              i + ' from end, player ' + j + ' wanted ' +
              r[j] + '. but got ' +
              this.hist[j][this.hist[j].length - 1 - i] +
              '\nstate is: ' + this.toString());
        }
      }
    }
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
    return JSON.stringify(a);
  }

  deserializeState(s : string) : TestState {
    // XXX no validation...
    let { N, hist, hash } = JSON.parse(s);
    let ts = new TestState(N);
    ts.hist = hist;
    ts.hash = hash;
    return ts;
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
    let m : {[key: string]: number} = {};
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
        ret.hash = (parseInt(k) | 0) >>> 0;
        return ret;
      }
    }

    // Otherwise, just some arbitrary function.
    ret.hash = new_hash >>> 0;
    
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

// XXX: Be careful about testing members of sim directly;
// typescript has unsound assumptions (ugh) that properties
// are not modified (!?) by functions. So if you have
//
// if (sim.nframe !== 1) throw '';
// sim.advanceFrame();
// if (sim.nframe !== 2) throw '';
//
// it will complain that the second condition is always
// satisfied because nframe is known to be 1. This is incorrect!
// See typescript issue #9998. Bleh. The "solution" seems to be
// to use accessor methods to read the property.

// Test really basic simulation scenario.
const testSimpleSim = () => {
  let ops = new TestOps;
  let N = 3;
  let sim = new Sim<TestInput, TestState>(ops,
                                          N,
                                          1,
                                          0,
                                          ops.initialState(N));
  sim.checkInvariants();
  if (sim.getNFrame() !== 0) throw 'didn\'t start on frame 0???';
  
  // TODO issue some inputs...
  sim.setInput(0, 0, new TestInput(555));
  sim.checkInvariants();
  if (sim.window.length() !== 0)
    throw 'uncertainty window should be empty';
  // (Shouldn't do anything yet; all inputs are in the future)
  sim.updateWindow();
  sim.checkInvariants();

  sim.advanceFrame(new TestInput(777));
  if (sim.getNFrame() !== 1)
    throw 'should be on frame 1 now';
  if (sim.window.length() !== 1)
    throw 'should have one frame in uncertainty window';
  sim.checkInvariants();
  if (sim.queued_inputs.length > 0)
    throw 'should have consumed queued input';
  sim.setInput(0, 2, new TestInput(999));
  sim.checkInvariants();
  // first frame's inputs are now complete.
  sim.updateWindow();
  sim.checkInvariants();
  if (sim.getMFrame() !== 1)
    throw 'frame 0 should be accurate now';
  
  {
    let last = sim.getMostRecentState().state;
    last.checkEndsWith([[555, 777, 999],
                        [0, 0, 0]]);
    if (last.hash != 0xde6eafd1)
      throw 'unexpected state';
    console.log('test state now: ' + last.toString());
  }

  sim.advanceFrame(new TestInput(22));
  if (sim.getNFrame() !== 2)
    throw 'wrong nframe';
  sim.advanceFrame(new TestInput(33));
  if (sim.getNFrame() !== 3)
    throw 'wrong nframe';
  if (sim.getMFrame() !== 1)
    throw 'not enough inputs yet to advance past 1';
  sim.checkInvariants();
  sim.updateWindow();
  sim.checkInvariants();
  if (sim.getMFrame() !== 1)
    throw 'still not enough inputs to advance past 1';

  // state could be anything now since we are allowed to
  // make guesses when the inputs aren't yet known. But
  // we know that the history will contain the player's
  // inputs.
  {
    let last = sim.getMostRecentState().state;
    last.checkEndsWith([/* 2 */ [null, 33, null],
                        /* 1 */ [null, 22, null],
                        /* 0 */ [555, 777, 999],
                        [0, 0, 0]]);
  }

  // Now fill in some inputs, out of order.
  sim.setInput(2, 2, new TestInput(33));
  sim.updateWindow();
  {
    let last = sim.getMostRecentState().state;
    last.checkEndsWith([/* 2 */ [null, 33,  33],
                        /* 1 */ [null, 22,  null],
                        /* 0 */ [555,  777, 999],
                        [0, 0, 0]]);
    // We actually know the hash is 33 because it
    // wins the vote, no matter what we computed for player 0.
    if (last.hash != 33) throw '33 should win the hash vote';
  }
  
  sim.setInput(2, 0, new TestInput(55));
  sim.updateWindow();
  sim.checkInvariants();
  
  {
    let last = sim.getMostRecentState().state;
    last.checkEndsWith([/* 2 */ [55,   33,  33],
                        /* 1 */ [null, 22,  null],
                        /* 0 */ [555,  777, 999],
                        [0, 0, 0]]);
  }

  sim.setInput(1, 2, new TestInput(88));
  sim.updateWindow();
  sim.checkInvariants();
  
  {
    let last = sim.getMostRecentState().state;
    last.checkEndsWith([/* 2 */ [55,   33,  33],
                        /* 1 */ [null, 22,  88],
                        /* 0 */ [555,  777, 999],
                        [0, 0, 0]]);
  }

  sim.setInput(1, 0, new TestInput(11));
  sim.updateWindow();
  sim.checkInvariants();
  {
    let last = sim.getMostRecentState().state;
    last.checkEndsWith([/* 2 */ [55,   33,  33],
                        /* 1 */ [11,   22,  88],
                        /* 0 */ [555,  777, 999],
                        [0, 0, 0]]);

    // Hash is actually known now because the inputs
    // are complete.
    if (last.hash != 0x21) throw 'wrong hash';
  }

  if (sim.getNFrame() !== 3)
    throw 'next frame is 3';
  
  if (sim.getMFrame() !== 3)
    throw 'with full inputs we should be able to advance mframe';
  
  console.log('testSimpleSim OK');
};
testSimpleSim();


const testWithOpsTester = () => {
  let test_cases =
    [
      [[0, 1, 0]],

      [[1, 0, 1],
       [0, 0, 0]],

      [[0, 0, 0],
       [0, 0, 1],
       [0, 0, 0],
       [1, 2, 3]],
      
      [[11, 22, 33],
       [11, 22, 11],
       [11, 22, 33],
       [11, 222, 333],
       [111, 222, 333],
       [33, 22, 11],
       [3, 2, 1],
       [3, 2, 1],
       [3, 2, 2],
       [1, 2, 3],
       [555, 777, 999],
       [555, 999, 777],
       [555, 555, 555],
       [555, 111, 555],
       [111, 555, 111],
       [333, 222, 111]]
    ];

  for (const testcase of test_cases)  {
    let inputs =
      testcase.map(r => r.map(x => new TestInput(x)));

    let ops = new TestOps;
    let ot = new OpsTester<TestInput, TestState>(ops, 3);
    let final = ot.testRandomOrder(inputs, ops.initialState(3), 1000);

    // reverse to get expected end encoded in state.
    let expected : Array<Array<number>> = [];
    for (let i = 0; i < Math.min(3, testcase.length); i++) {
      expected.push(testcase[testcase.length - 1 - i]);
    }

    final.checkEndsWith(expected);
  }
  console.log('random tests ok :)');
};
testWithOpsTester();
