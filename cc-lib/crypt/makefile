
default : aes_test.exe sha256_test.exe cryptrand_test.exe md5_test.exe

BASE=../base/logging.o ../base/stringprintf.o

# for 64 bits on windows
CXX=x86_64-w64-mingw32-g++
CC=x86_64-w64-mingw32-g++
CXXFLAGS=-I. -I.. --std=c++17 -O2 -static

# For linux, others...
# CXX=g++
# CC=gcc
# CXXFLAGS=-I. -I.. --std=c++17 -O2 -DNDEBUG=1

aes.o : aes.cc aes.h
	$(CXX) $(CXXFLAGS) $< -o $@ -c

aes_test.o : aes_test.cc aes.h
	$(CXX) $(CXXFLAGS) $< -o $@ -c

aes_test.exe : aes.o aes_test.o $(BASE)
	$(CXX) $(CXXFLAGS) $^ -o $@

sha256.o : sha256.cc sha256.h
	$(CXX) $(CXXFLAGS) $< -o $@ -c

sha256_test.o : sha256_test.cc sha256.h
	$(CXX) $(CXXFLAGS) $< -o $@ -c

sha256_test.exe : sha256.o sha256_test.o $(BASE)
	$(CXX) $(CXXFLAGS) $^ -o $@

cryptrand.o : cryptrand.cc cryptrand.h
	$(CXX) $(CXXFLAGS) $< -o $@ -c

cryptrand_test.o : cryptrand_test.cc cryptrand.h
	$(CXX) $(CXXFLAGS) $< -o $@ -c

cryptrand_test.exe : cryptrand.o cryptrand_test.o $(BASE)
	$(CXX) $(CXXFLAGS) $^ -o $@

md5_test.exe : md5_test.o md5.o $(BASE)
	$(CXX) $(CXXFLAGS) $^ -o $@

clean :
	rm -f *.o *.exe
