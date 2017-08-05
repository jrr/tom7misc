// needs atoms, indices, decom, tree from tree.js

let word = 'radioisotope';
let banned = {};

// Decompose into a sorted string of atoms.
function Decompose(s) {
  let outid = [];
  for (const c of s) {
    let a = decomp[c];
    for (const id of a) {
      outid.push(id);
    }
  }
  outid.sort();
  
  let out = '';
  for (const id of outid)
    out += atoms[id];
  return out;
}

function InputKey(e) {
  if (e.code == 'Enter') {
    Restart();
  }
}

// Read from text input, make unexpanded word list
function Restart() {
  const elt = document.getElementById('wordinput');
  // strip whitespace etc.
  word = elt.value;
  Draw();
}

function Draw() {
  const out = document.getElementById('output');
  out.innerHTML = '';
  // Get chars left through decomposition.
  const r = Decompose(word);
  console.log(r);
  if (!Make(r, out)) {
    out.innerHTML = 'NO anaglyphs!';
  }
}

// Now we'll recursively create ouptut trees. We don't
// expand nodes eagerly (document would be too big), but
// we do want to know whether the node is inhabited. So
// this function returns false if there are no anaglyphs
// formable, and allows its argument to be null.
//
// r is the set of atoms left (as a string, sorted)
// 
function Make(r, elt) {
  return false;
}

function Init() {
  document.getElementById('wordinput').onkeydown = InputKey;
}
