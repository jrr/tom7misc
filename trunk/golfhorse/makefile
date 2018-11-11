default: list.js

CXX=g++
CXXFLAGS=--std=c++17 -O2 -Wall -I ../cc-lib/ -Wno-deprecated -Wno-sign-compare

CXX=x86_64-w64-mingw32-g++
CC=x86_64-w64-mingw32-gcc
LFLAGS=-static

list.js : makedata.exe wordlist.asc
	./makedata.exe wordlist.asc list.js

list10.js : makedata.exe ten-hundred
	./makedata.exe ten-hundred list10.js

verify : list.js
	node verifier.js wordlist.asc list.js && ls -al list.js >> bench.txt

CCLIB_OBJECTS=../cc-lib/util.o ../cc-lib/base/stringprintf.o ../cc-lib/base/logging.o ../cc-lib/arcfour.o

%.o : %.cc *.h
	@$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c -o $@ $<
	@bash -c "echo -n '.'"

makedata.exe : makedata.o $(CCLIB_OBJECTS)
	${CXX} $^ -o $@ ${LFLAGS}

clean:
	rm -f *.exe *.o $(CCLIB_OBJECTS)

