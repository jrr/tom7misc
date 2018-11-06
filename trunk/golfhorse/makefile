default: list.js

CXX=g++
CXXFLAGS=-O2 -Wall -I ../cc-lib/ -Wno-deprecated -Wno-sign-compare


list.js : list-data.js list-process.js
	cat list-data.js list-process.js > list.js

list10.js : list10-data.js list-process.js
	cat list10-data.js list-process.js > list10.js

list-data.js : makedata.exe wordlist.asc
	./makedata.exe wordlist.asc > list-data.js

list10-data.js : makedata.exe ten-hundred
	./makedata.exe ten-hundred > list10-data.js

verify : list.js
	node verifier.js wordlist.asc list.js && ls -al list.js >> bench.txt

CCLIB_OBJECTS=../cc-lib/util.o

%.o : %.cc *.h
	@$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c -o $@ $<
	@bash -c "echo -n '.'"

makedata.exe : makedata.o $(CCLIB_OBJECTS)
	g++ $^ -o $@
