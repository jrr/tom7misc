// TODO: Can probably avoid 'let' for local variables.
// TODO: Just use { (which is 'z'+1) for |, right?
// Size of input history.
H=32;
// Radix.
R=27;
// Symbol from Char.
function S(c){return c=='\n'?26:c.charCodeAt(0)-97}
// Char from Symbol.
function C(s){return s>25?'\n':String.fromCharCode(97+s)}
// Array of zeroes.
function Z(n){let a=[];while(n--)a.push(0);return a}
// Tabulate an array, like ML.
function T(n,f){let a=[];for(let i=0;i<n;i++)a.push(f(i));return a}

// Predict the next character. Returns the entire list,
// sorted from most to least likely.
function P(h){
  // Prepare input layer.
  let p=Z(H*R);
  // One-hot.
  for(let i=0;i<H;i++)p[i*R+S(h[i])]=1;
  // Now, fill layers. note n is num_layers+1 length
  for(let l=0;l<N.n.length-1;l++){
    // Number of nodes on pRevious layer
    let r=p.length;
    // Number of nodeZ on this layer
    let z=N.n[l+1];
    if (!z) throw ['z', l];
    // LaYer itself
    let y=N.l[l];
    if (!y) throw ['y', l];
    // Tabulate Destination layer from previous p.
    let d=T(z,(i)=>{
      // Node i.
      // The bias (which will be the accumulated potential):
      let b=y[i];
      for(let j=0;j<r;j++){
	// j is index of node on previous layer
	// w is its Weight. skip z biases, 
	let w=y[z+i*r+j];
	b+=w*p[j];
      }
      // Apply transfer function to get value.
      return b>0?b:.01*b;
    });
    // Now shift destination to be previous.
    p=d;
  }
  // After the loop, the layer (of size R) is the
  // output. We want to find the indices with the highest
  // scores, descending.
  // Start with identity array
  let e=T(R,(i)=>i);
  e.sort((a,b)=>p[b]-p[a]);
  // And make into characters.
  return [p,e.map(C)];
}
