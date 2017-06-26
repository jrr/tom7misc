
#include "stdafx.h"
#include "Tabsynth.h"
#include "mapper.h"

#include <stdlib.h>

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

mapper::mapper() : maphead(0) {
	insert(0,0);
	insert(65535,65535);
}

mapper::~mapper() { delete maphead; }

uint mapper::map(uint x) {
  /* tricky: do a binary search to find the range that
     includes x: */
  uint lowx = 0, lowy = 0,
    hix = 65535, hiy = 65535;

  /*
    char msg[128];
    sprintf(msg,"Map %d", x);

    ((CDialog*)0)->MessageBox(msg, "Called map", MB_OK);
  */

  mapnode * mom = maphead;

  while (mom) {
    if (mom->x == x) return mom->height;
    if (x > mom->x) {
      if (mom->x > lowx) { lowx = mom->x; lowy = mom->height; }
      mom = mom -> r;
    } else {
      if (mom->x < hix) { hix = mom->x; hiy = mom->height; }
      mom = mom -> l;		
    }
  }

  /* didn't find it. Interpolate: */

  /*
    sprintf(msg,"End of map: lowx(%d), hix(%d), lowy(%d), hiy(%d), (x-lowx)[%d], (hiy-lowy)/(hix-hiy)[%f]",
    lowx, hix, lowy, hiy, (x-lowx), ((hiy-lowy)/float(hix-lowx)));

    ((CDialog*)0)->MessageBox(msg, "LEAVING MAP", MB_OK);
  */

  if (hix == lowx) return lowy;        // HACK FIXME!-
  if (lowy < hiy)  
    return lowy + (((hiy-lowy)/float(hix-lowx)) * (x - lowx));
  else
    return lowy - ((abs(hiy-lowy)/float(hix-lowx)) * (x - lowx));
}

mapnode * mapper::insert(uint xx,uint ht) {
  if (xx> 65535) xx=65535;

  if (ht > 65535) ht = 65535;

  bool dir = false;
  mapnode ** mom = &maphead;

  while (*mom) {
    if ((*mom)->x == xx) {
      /* redef?? */
      return 0;
      if (dir) mom = & ( (*mom)->l);
      else     mom = & ( (*mom)->r);
      //      (*mom)->height = ht;
      //      return (*mom);
    } else if ((*mom)->x < xx) {
      dir = true;
      mom = & ( (*mom)->r );
    } else {
      dir = false;
      mom = & ( (*mom)->l );
    }
  }

  return ((*mom) = new mapnode(xx,ht,0,0));
}

//mapnode * mapper::inshelp (uint xx, uint ht, bool dir){
//}

mapnode * mapper::getnode (uint xx, uint tolerance) {
  if (xx> 65535) xx=65535;

  mapnode * mom = maphead;

  while (mom) {
    if (abs(mom->x - xx) < tolerance) {
      // found node within tolerance:
      return mom;
    } else if (mom->x < xx) {
      mom = mom->r;			
    } else 
      mom = mom->l;
  }

  return 0; /* not found */
}

mapnode * mapper::move  (mapnode* z,uint xx, uint yy) {

  /* FIXME: Only delete, replace if it crosses another point! */

  int ox = z->x, oy = z->height;
  remove(z);
  //	return 0;
  mapnode * r = insert(xx,yy);
  return  r?r:insert(ox,oy); /* if we can't move it there, then replace
				the old one */
}

void mapper::remove  (mapnode* z) {
	
  /* first delete the node: */

  /* FIXME? This is retarded, I have a handle to the mapnode
     but I have to search through the tree to find it anyway,
     just so I have a pointer to its address... */

  mapnode ** mom = &maphead;

  while (*mom != z) {
    if (! *mom) {
      /* PANIC: node is not found! */
      ((CDialog*)0)->MessageBox("Can't find node in mapper::move!",
				"ERROR", MB_OK);
      exit(1);
    }
    else if ((*mom)->x < z->x) {
      mom = & ( (*mom)->r );			
    } else 
      mom = & ( (*mom)->l );
  }

  /* now mom is a ** to the node to delete */

  /* is this an easy case? */

  if (!(*mom)->l) {
    mapnode * delme = *mom;
    *mom = (*mom)->r; /* maybe null */
    delme -> l = delme -> r = 0;
    delete delme; 
    return;
  } else if (!(*mom)->r) {
    mapnode * delme = *mom;
    *mom = (*mom)->l; /* maybe null */
    delme -> l = delme -> r = 0;
    delete delme;
    return;
  } else {
    /* has two children; not easy! */

    /* find rightmost child on left. */

    mapnode ** savior = & (*mom)->l;

    while ((*savior)->r) savior = & ((*savior)->r);

    mapnode * replacement = *savior;

    /* now we have to get rid of left children of this node. They
       can replace the rightmost node. */

    *savior = (*savior)->l; /* maybe null, doesn't matter */

    /* now replace the deleted node with replacement. Preserves BST
       property */

    mapnode * temp = *mom;

    *mom = replacement;
    (replacement)->l = temp->l;
    (replacement)->r = temp->r;

    temp->l = temp->r = 0; 
    delete temp;
    return;
  }
  
}
