#include <iostream.h>
#include <string>

int main () {
  string c;
  while (cin >> c) {

    if (c.length() <= 7 
	&& (c.find('a')<100)
	&& (c.find('e')<100)
	&& (c.find('i')<100)
	&& (c.find('o')<100)
	&& (c.find('u')<100)) {
      cout << c << endl;
    }
    
  }
  return 0;
}
