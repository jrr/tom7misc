// needs atoms, indices, decom, tree from tree.js

let word = 'radioisotope';
let banned = {};

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
// r - the set of atoms left (as a string, sorted). We have to use
//     all of them to complete the anagraph.
// elt - the output element.
function Make(r, elt) {
  let any = false;
  // Walk the tree with the current atoms. Whenever we find a word
  // that can be made, we check to see if an anaglyph can be completed
  // (using HasAny) and if so, we add it to the element.
  let Walk = (n, a) => {
    if (!n) return false;
    // Can only enter the node if we have all its atoms, and those
    // get removed from the available ones for this subtree.
    const left = Take(a, n.a);
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
      // Is it completable?
      const comp = (left === '') || HasAny(left);
      // console.log(n.w.join(',') + ' ' + comp);
      
      if (comp) {
	var s = n.w.join(',');
	const d = DIV('', elt);
	TEXT(s, d);
	if (left !== '') {
	  const ct = DIV('cont', d);
	  const m = SPAN('more', ct);
	  TEXT("...", m);
	  m.onclick = () => {
	    // Get rid of 'more' link
	    ct.removeChild(m);
	    Make (left, ct);
	  };
	}
	any = true;
      }
    }
  };

  Walk(tree, r);
  return any;
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
  const input = document.getElementById('wordinput');
  input.onkeydown = InputKey;

  // XXX
  input.value = 'digital';
  Restart();
}
