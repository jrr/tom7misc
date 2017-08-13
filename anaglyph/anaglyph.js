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

// Populate one level of the output tree. We don't generate all the
// anagrams eagerly (document would be too big), but we do want to
// know whether the anagram can be completed; we consult the HasAny
// function (which works the same way) for that.
//
// r is the set of atoms left (as a string, sorted)
// elt is the output element.
function Make(r, elt) {
  // The current anagraph being generated. Each element is a list of
  // words (aliases tree) that use the same atoms.
  let cur = [];
  // Walk the tree with the current atoms. Output any 
  let Walk = (n, a) => {    
    if (!n) return false;
    // Can only enter the node if we have all its atoms, and those
    // get removed from the available ones for this subtree.
    let left = Take(a, n.a);
    // Can't enter subtree because we don't have the atoms.
    if (left === null) return false;

    // Always visit children. Do this first to generate longer
    // words first.
    if (n.c) {
      for (c of n.c) {
	Walk (c, left);
      }
    }

    // Any words at this node are possible.
    if (n.w) {
      cur.push(n.w);
      
      if (left === '') {
	// If we exactly used the input string and have any words,
	// then this is a complete anagraph. Note that we can't easily
	// do this at the head of Walk, since it is also used (with
	// reduced atoms) to walk subtrees.

	var s = '';
	for (wl of cur) {
	  s += wl.join(',');
	  s += "  ";
	}
	console.log(s);

	// return true;
      } else {
	// Start from root.
	Walk (tree, left);
      }
      cur.pop();
    }
  };

  return Walk(tree, r);
}

// Returns true if there are any full anagrams of the atoms r.
function HasAny(r) {
  // Once this returns true, we can stop and return true.
  let Walk = (n, a) => {    
    if (!n) return false;
    // Can only enter the node if we have all its atoms, and those
    // get removed from the available ones for this subtree.
    let left = Take(a, n.a);
    // Can't enter subtree because we don't have the atoms.
    if (left === null) return false;

    if (n.c)
      for (c of n.c)
	if (Walk (c, left))
	  return true;

    // Any words at this node are possible.
    if (n.w) {
      // ... completing the anagraph.
      if (left === '')
	return true;

      // ... with some more atoms to be anagraphed.
      if (Walk (tree, left))
	return true;
    }
  };

  return Walk(tree, r);
}


function Init() {
  document.getElementById('wordinput').onkeydown = InputKey;
}
