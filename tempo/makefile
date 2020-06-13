all : tempo.exe am2315_test.exe

CXX=g++
CXXFLAGS=-Wall -Wno-sign-compare -Wno-deprecated-declarations -O --std=c++17 -I ../cc-lib `mysql_config --cflags` -I/usr/include/mysql++ -Wno-psabi -ffunction-sections
LFLAGS=`mysql_config --libs` -lmysqlpp -lpthread

%.o : %.cc *.h
	@$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c -o $@ $<
	@bash -c "echo -n '.'"

CCLIB_OBJECTS=../cc-lib/util.o ../cc-lib/webserver.o ../cc-lib/base/logging.o ../cc-lib/base/stringprintf.o ../cc-lib/stb_image_write.o ../cc-lib/stb_image.o ../cc-lib/image.o ../cc-lib/pi/netutil.o ../cc-lib/pi/bcm2835.o ../cc-lib/arcfour.o

tempo.exe : tempo.o database.o onewire.o am2315.o logic.o $(CCLIB_OBJECTS)
	$(CXX) $^ -o $@ $(LFLAGS)

am2315_test.exe : am2315_test.o am2315.o $(CCLIB_OBJECTS)
	$(CXX) $^ -o $@ $(LFLAGS)

clean :
	rm -f *.o *.exe core $(CCLIB_OBJECTS)

veryclean : clean
	rm -f *~
