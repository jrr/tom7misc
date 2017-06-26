TARG = mid
default : $(TARG).exe 
$(TARG).exe : $(TARG).o /djgpp/lib/liballeg.a
	gcc -Wall -O2 -o $(TARG).exe $(TARG).o -lalleg
$(TARG).o : $(TARG).c 
	gcc -Wall -O2 -o $(TARG).o $(TARG).c -c 
