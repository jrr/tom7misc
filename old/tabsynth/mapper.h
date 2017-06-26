// mapper.h: interface for the mapper class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_MAPPER_H__02786C25_B560_11D2_A014_0080C8443AA1__INCLUDED_)
#define AFX_MAPPER_H__02786C25_B560_11D2_A014_0080C8443AA1__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif

/* mapper class provides a filter for integers, mapping
   integers in the entire unsigned range to integers in
   the entire unsigned range.
*/

typedef unsigned int uint;

struct mapnode {
	mapnode () : l(0), r(0) {};
	mapnode (uint xx, uint hh, mapnode* ll, mapnode* rr) 
												: height(hh),
												  x(xx),
												  l(ll),
												  r(rr) {}
	uint height;
	uint x;
	mapnode *l, *r;
};

class mapper {
public:
	mapper();
	~mapper();

	uint map(uint);	
	/* for win32 */
	void setup(); /* open modal? setup dlg */

private:

	mapnode * maphead;

	mapnode * getnode(uint,uint);

	mapnode * insert(uint,uint);
	mapnode * move  (mapnode*,uint,uint);
	void remove(mapnode*);
	
	/* for win32 */
	friend class mapdlg;
};

#endif // !defined(AFX_MAPPER_H__02786C25_B560_11D2_A014_0080C8443AA1__INCLUDED_)
