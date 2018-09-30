
#ifndef __LIST_UTIL_H
#define __LIST_UTIL_H

#include <list>

// Move the iterator (list cell) to the end of the given list.
// This works even if the list cell comes from a different
// list, unless they use different allocators. No iterators
// are invalidated.
template<class T>
void ListMoveToBack(std::list<T> *l,
		    typename std::list<T>::const_iterator it) {
  l->splice(l->end(), *l, it);
}

#endif
