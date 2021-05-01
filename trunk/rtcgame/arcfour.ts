
/* Alleged RC4 algorithm. Based on cc-lib implementation, which is
   based on the algorithm published in Applied Cryptography.

   This algorithm is adorably simple, but should probably not be used
   for cryptographic applications any more. Note also that like many
   pseudorandom number generators, there are some small biases in its
   output statistics.

   TODO: Test, especially wrt signedness which is weird in js.
   TODO: to ts-lib, js-lib, etc.
*/

class ArcFour {
  private ss : Uint8Array;
  // These are also uint8.
  private ii : number;
  private jj : number;

  // Seed from an arbitrary (non-empty) string.
  constructor(init : string) {
    let kk = new Uint8Array(256);
    for (let i = 0; i < 256; i++) {
      kk[i] = init.charCodeAt(i % init.length) & 0xFF;
    }

    this.ii = 0;
    this.jj = 0;
    this.ss = new Uint8Array(256);

    this.initialize(kk);
    // Discard beginning of stream as a standard measure to reduce the
    // (weak) correlation with the key. Maybe could be configurable
    // but basically everyone should do this.
    for (let i = 0; i < 1000; i++) this.byte();
  }

  // Returns a byte in 0..255, advancing the state.
  byte() : number {
    this.ii = (this.ii + 1) & 0xFF;
    this.jj = (this.jj + this.ss[this.ii]) & 0xFF;
    const ti = this.ss[this.ii];
    const tj = this.ss[this.jj];

    this.ss[this.ii] = tj;
    this.ss[this.jj] = ti;

    return this.ss[(ti + tj) & 0xFF];
  }

  // Generate a random unsigned 32-bit number in [0, n).
  randTo32(n : number) : number {
    let mask : number = (n - 1) | 0;
    mask |= mask >>> 1;
    mask |= mask >>> 2;
    mask |= mask >>> 4;
    mask |= mask >>> 8;
    mask |= mask >>> 16;

    // Now, repeatedly generate random numbers, modulo that
    // power of two.

    // PERF: If the number is small, we only need Rand16, etc.
    for (;;) {
      let x = (this.rand32() & mask) >>> 0;
      if (x < n) return x;
    }
  }

  rand32() : number {
    let uu : number = 0;
    uu = this.byte() | (uu << 8);
    uu = this.byte() | (uu << 8);
    uu = this.byte() | (uu << 8);
    uu = this.byte() | (uu << 8);
    return uu >>> 0;
  }
  
  // Uniformly shuffle an array with fewer than 2^32 elements,
  // in place.
  shuffleArray<T>(a : Array<T>) {
    const len = a.length;
    if (len <= 1) return;

    for (let i = len - 1; i >= 1; i--) {
      const j = this.randTo32(i + 1);
      if (i != j) {
        let t : T = a[i];
        a[i] = a[j];
        a[j] = t;
      }
    }
  }

  // Initializes members from a 256-byte key array.
  private initialize(kk : Uint8Array) {
    if (kk.length != 256)
      throw 'key buffer must be 256 bytes';

    for (let n = 0; n < 256; n++) {
      this.ss[n] = n;
    }

    let j : number = 0;
    for (let n = 0; n < 256; n++) {
      j = (j + this.ss[n] + kk[n]) & 0xFF;
      let t = this.ss[n];
      this.ss[n] = this.ss[j];
      this.ss[j] = t;
    }
  }
}
  
