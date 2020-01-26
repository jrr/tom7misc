all : tempo.exe

CXX=g++
CXXFLAGS=-Wall -Wno-sign-compare -Wno-deprecated-declarations -O --std=c++17 -I ../cc-lib `mysql_config --cflags` -I/usr/include/mysql++ -Wno-psabi
LFLAGS=`mysql_config --libs` -lmysqlpp -lpthread

%.o : %.cc *.h
	@$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c -o $@ $<
	@bash -c "echo -n '.'"

CCLIB_OBJECTS=../cc-lib/util.o ../cc-lib/web.o ../cc-lib/base/logging.o ../cc-lib/base/stringprintf.o

tempo.exe : tempo.o onewire.o $(CCLIB_OBJECTS)
	$(CXX) $^ -o $@ $(LFLAGS)

clean :
	rm -f *.o *.exe core $(CCLIB_OBJECTS)

veryclean : clean
	rm -f *~
