
// Test instantiations of sim stuff.


class ActualInputs implements InputsI {
  data: number;
  // constructor() { this.data = 0; }
  constructor(n) { this.data = n; }
  toString() { return '' + this.data; }
  clone() : ActualInputs { return new ActualInputs(this.data); }
  static eq(a : ActualInputs, b : ActualInputs) {
    return a.data == b.data;
  }
  static empty() {
    return new ActualInputs(0);
  }
}

type ActualRow = InputRow<ActualInputs, InputsTypeI<ActualInputs>>;

let ir : ActualRow = new InputRow(5, ActualInputs);

let ire : ActualRow = InputRow.completeEmpty(5, ActualInputs);


console.log(ir.inputs_type.empty().toString());
console.log(ire.inputs[0].toString());
console.log(ire.inputs_type.eq(ire.inputs[0], ire.inputs[1]));



/*
old standalone tests, delete me
// Per-player input for a frame.
interface Inputs {
  toString() : string;
  clone() : Inputs;
}

// type of "the constructor"
// (this is like, the static parts.)
interface InputsType<I extends Inputs> {
  // new () : I;
  empty() : I;
  eq(a : I, b : I) : boolean;
}

class ActualInputs implements Inputs {
  data: number;
  // constructor() { this.data = 0; }
  constructor(n) { this.data = n; }
  toString() { return '' + this.data; }
  clone() { return new ActualInputs(this.data); }
  static eq(a : ActualInputs, b : ActualInputs) {
    return a.data == b.data;
  }
  static empty() {
    return new ActualInputs(0);
  }
}

// Set of inputs (possibly incomplete) for a frame.
class InputRow<I extends Inputs, IT extends InputsType<I>> {
  readonly N: number;
  readonly input_type: IT;
  inputs: Array<I>;
  
  constructor(N : number, input_type : IT) {
    this.N = N;
    this.input_type = input_type;
    // In a row, null means unknown. It is not a valid input.
    this.inputs = new Array(N).fill(null);
  }

  // A complete row of empty inputs.
  static completeEmpty<I extends Inputs, IT extends InputsType<I>>(
    N, input_type : IT) {
    let ret = new InputRow<I, IT>(N, input_type);
    for (let i = 0; i < N; i++) {
      ret.inputs[i] = input_type.empty();
    }
    return ret;
  }
}

type ActualRow = InputRow<ActualInputs, InputsType<ActualInputs>>;

let ir : ActualRow = new InputRow(5, ActualInputs);

let ire : ActualRow = InputRow.completeEmpty(5, ActualInputs);


console.log(ir.input_type.empty().toString());
console.log(ire.inputs[0].toString());
console.log(ire.input_type.eq(ire.inputs[0], ire.inputs[1]));
*/
