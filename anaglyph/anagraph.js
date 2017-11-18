// needs atoms, indices, decom, tree from tree.js

let word = 'radio';
let banned = {};
const MINLEN = 0;

function makeElement(what, cssclass, elt) {
  var e = document.createElement(what);
  if (cssclass) e.setAttribute('class', cssclass);
  if (elt) elt.appendChild(e);
  return e;
}
function IMG(cssclass, elt) { return makeElement('IMG', cssclass, elt); }
function DIV(cssclass, elt) { return makeElement('DIV', cssclass, elt); }
function SPAN(cssclass, elt) { return makeElement('SPAN', cssclass, elt); }
function BR(cssclass, elt) { return makeElement('BR', cssclass, elt); }
function TEXT(contents, elt) {
  var e = document.createTextNode(contents);
  if (elt) elt.appendChild(e);
  return e;
}

// Decompose into a sorted string of atoms.
function Decompose(s) {
  let outid = [];
  for (const c of s) {
    let a = decomp[c];
    for (const id of a) {
      outid.push(id);
    }
  }
  outid.sort(function(a, b) { return a - b; });
  console.log(outid);
  
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
  const belt = document.getElementById('bannedinput');
  const bwords = belt.value.split(',');
  banned = {};
  for (const w of bwords) {
    const ws = w.replace(/^\s+|\s+$/g,'');
    console.log('banned: [' + ws + ']');
    banned[ws] = true;
  }
  
  Draw();
}

function Draw() {
  const out = document.getElementById('output');
  out.innerHTML = '';
  // Get chars left through decomposition.
  const r = Decompose(word);
  // console.log(r);
  if (!Make(r, out, true)) {
    out.innerHTML = 'NO anagraphs!';
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
      // Exhausted src letters (but not take), so we fail.
      return null;
    }
    const tc = t[ti];
    const sc = src[si];
    const tx = indices[tc];
    const sx = indices[sc];
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

// Get the words at the current node, taking into account the banned
// words.
function NodeWords(n) {
  if (!n.w) return null;
  for (let i = 0; i < n.w.length; i++) {
    if (banned[n.w[i]] || n.w[i].length < MINLEN) {
      // Since this case is rare, only allocate a new array once
      // we see a single banned word.
      let ret = n.w.slice(0, i);
      for (let j = i + 1; j < n.w.length; j++) {
	if (!banned[n.w[j]] && n.w[j].length >= MINLEN) {
	  ret.push(n.w[j]);
	}
      }
      return ret;
    }
  }
  return n.w;
}

// Populate one level of the output tree. We don't generate all the
// anagrams eagerly (document would be too big), but we do want to
// know whether the anagram can be completed; we consult the HasAny
// function (which works the same way) for that.
//
// r - the set of atoms left (as a string, sorted). We have to use
//     all of them to complete the anagraph.
// elt - the output element.
// top - true if this is the top-level loop.
function Make(r, elt, top) {
  let any = false;
  // Walk the tree with the current atoms. Whenever we find a word
  // that can be made, we check to see if an anagraph can be completed
  // (using HasAny) and if so, we add it to the element.
  let Walk = (n, a) => {
    if (!n) return false;
    // Can only enter the node if we have all its atoms, and those
    // get removed from the available ones for this subtree.
    const left = Take(a, n.a);
    // console.log('walk [' + a + '] (' + n.a + ') = ' + (left ? left : '-'));
    // Can't enter subtree because we don't have the atoms.
    if (left === null) return false;
    
    // Always visit children. Do this first to generate longer
    // words first.
    if (n.c) {
      for (c of n.c) {
	Walk(c, left);
      }
    }

    // Any words at this node are possible.
    const nw = NodeWords(n);
    if (nw && nw.length > 0) {
      // Is it completable?
      const MAX = 5;
      const card = Cardinality(left, MAX + 1);
      const comp = (left === '') || card > 0;
      
      if (comp) {
	var s = nw.join(',');
	const d = DIV(top ? 'top an' : 'an', elt);
	TEXT(s, d);
	if (left !== '') {
	  const ct = DIV('cont', d);
	  // Incomplete. If there's a small number of completions,
	  // just do it.
	  if (card <= MAX) {
	    Make (left, ct, false);
	  } else {
	    // Otherwise make a ... link.
	    const m = SPAN('more', ct);
	    TEXT("...", m);
	    m.onclick = () => {
	      // Get rid of 'more' link
	      ct.removeChild(m);
	      Make (left, ct, true);
	    };
	  }
	}
	any = true;
      }
    }
  };

  Walk(tree, r);
  return any;
}

// Compute the number of full anagraphs of the atoms r, but stop and
// return max (or some higher number) if we know we have at least max.
// This allows us to efficiently expand cases where there are a small
// number of completions, without counting the exact number.
function Cardinality(r, max) {
  // Once this returns true, we can stop and return true.
  let Walk = (n, a, m) => {
    if (m <= 0) return 0;
    if (!n) return 0;
    // Can only enter the node if we have all its atoms, and those
    // get removed from the available ones for this subtree.
    let left = Take(a, n.a);
    // Can't enter subtree because we don't have the atoms.
    if (left === null) return 0;

    // Check children to find longer anagraphs first.
    let found = 0;

    // Any words at this node are possible.
    // PERF: Could just count rather than copying.
    const nw = NodeWords(n);
    const num = nw ? nw.length : 0;
    if (num > 0) {
      // ... completing the anagraph.
      if (left === '') {
	found += num;
	if (found >= m) return found;
      }

      // ... with some more atoms to be anagraphed.
      // Actually could be like (m - found) div num?
      const rec = Walk(tree, left, m - found);
      // Also, we could define this to count chunks of
      // words as just 1, which might be more what we need
      // for the UI.
      found += num * rec;
      if (found >= m) return found;
    }

    if (n.c) {
      for (c of n.c) {
	found += Walk(c, left, m - found);
	if (found >= m) return found;
      }
    }
    return found;
  };

  return Walk(tree, r, max);
}

// Returns true if there are any full anagraph of the atoms r.
function HasAny(r) {
  return Cardinality(r, 1) > 0;
}

function Init() {
  const input = document.getElementById('wordinput');
  input.onkeydown = InputKey;

  // XXX
  input.value = word;

  const binput = document.getElementById('bannedinput');
  binput.onkeydown = InputKey;

  Restart();
}
