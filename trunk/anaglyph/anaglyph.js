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

// Take the atoms in 't' from 'src'. Returns the remaining atoms
// (sorted), or null if t is not a subset of src. Both must be sorted.
function Take(src, t) {
  let r = '';
  let si = 0, ti = 0;
  for (;;) {
    if (ti == t.length) {
      // Exhausted take letters, so succeed.
      return r + src.substr(si);
    }
    if (si == src.length) {
      // Exhausted src letters, so we fail.
      return null;
    }
    const tc = t[ti];
    const sc = src[si];
    const tx = indices[tc];
    const sx = indices[sx];
    if (tc == sc) {
      // Mutual match.
      ti++;
      si++;
    } else if (tx < sx) {
      // If the char from t should come first, but it's not at the
      // head of src, then we won't find it so fail.
      return null;
    } else {
      if (!(sx < tx)) throw 'impossible';
      // Advance past the src char 
      r += sc;
      si++;
    }
  }
}

// Now we'll recursively create ouptut trees. We don't expand output
// nodes eagerly (document would be too big), but we do want to know
// whether the node is inhabited. So this function returns false if
// there are no anaglyphs formable, and allows its argument to be
// null.
//
// r is the set of atoms left (as a string, sorted)
function Make(r, elt) {
  let Walk = (n) => {
    
  };
  
}

function Init() {
  document.getElementById('wordinput').onkeydown = InputKey;
}
