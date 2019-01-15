// Depends on netcode.js, also expects dict, net...

let s = dict.substr(0,H);
// console.log(s)

let q=dict.length-H;
let exact=0;
let depth_loss=0;
let u='';
let depths=[];
for(let i = 0; i < R; i++) depths.push(0);
for(let i = H; i < dict.length; i++) {
  // Expected..
  let e=dict[i];
  let r=P(s);
  let d=0;
  for (let j = 0; j < r.length; j++) {
    if (r[j] == e) break;
    d++;
  }

  if (d == 0) exact++;
  depth_loss += d;
  if (i % 10000 == 0) console.log(
    '' + ((100 * i) / dict.length).toFixed(2) + '%');

  depths[d]++;
  u += C(d);
  
  // Shift *correct* char onto history.
  s = s.substr(1, H - 1) + e;
}

// console.log(u);
console.log(depths);

console.log('exact ', exact, '  depth loss ', depth_loss);
