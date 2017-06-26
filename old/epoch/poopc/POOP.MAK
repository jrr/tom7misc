# ---------------------------------------------------------------------------
VERSION = BCB.01
# ---------------------------------------------------------------------------
!ifndef BCB
BCB = $(MAKEDIR)\..
!endif
# ---------------------------------------------------------------------------
PROJECT = poop.exe
OBJFILES = poop.obj PClient.obj squares.obj gamelist.obj options.obj \
   aboutbox.obj gamerq.obj
RESFILES = poop.res
RESDEPEN = $(RESFILES) PClient.dfm gamelist.dfm options.dfm aboutbox.dfm \
   gamerq.dfm
LIBFILES = 
DEFFILE = 
# ---------------------------------------------------------------------------
CFLAG1 = -Od -Hc -w -k -r- -y -v -vi- -c -a4 -b- -w-par -w-inl -Vx -Ve -x
CFLAG2 = -Ic:\vb16\icons\misc;$(BCB)\projects;$(BCB)\include;$(BCB)\include\vcl \
   -H=$(BCB)\lib\vcld.csm 
PFLAGS = -Uc:\vb16\icons\misc;$(BCB)\projects;$(BCB)\lib\obj;$(BCB)\lib \
   -Ic:\vb16\icons\misc;$(BCB)\projects;$(BCB)\include;$(BCB)\include\vcl \
   -AWinTypes=Windows;WinProcs=Windows;DbiTypes=BDE;DbiProcs=BDE;DbiErrs=BDE -v \
   -$Y -$W -$O- -JPHNV -M  
RFLAGS = -ic:\vb16\icons\misc;$(BCB)\projects;$(BCB)\include;$(BCB)\include\vcl 
LFLAGS = -Lc:\vb16\icons\misc;$(BCB)\projects;$(BCB)\lib\obj;$(BCB)\lib -aa \
   -Tpe -x -v -V4.0 
IFLAGS = 
LINKER = ilink32
# ---------------------------------------------------------------------------
ALLOBJ = c0w32.obj $(OBJFILES)
ALLRES = $(RESFILES)
ALLLIB = $(LIBFILES) vcl.lib import32.lib cp32mt.lib 
# ---------------------------------------------------------------------------
.autodepend

$(PROJECT): $(OBJFILES) $(RESDEPEN) $(DEFFILE)
    $(BCB)\BIN\$(LINKER) @&&!
    $(LFLAGS) +
    $(ALLOBJ), +
    $(PROJECT),, +
    $(ALLLIB), +
    $(DEFFILE), +
    $(ALLRES) 
!

.pas.hpp:
    $(BCB)\BIN\dcc32 $(PFLAGS) { $** }

.pas.obj:
    $(BCB)\BIN\dcc32 $(PFLAGS) { $** }

.cpp.obj:
    $(BCB)\BIN\bcc32 $(CFLAG1) $(CFLAG2) -o$* $* 

.c.obj:
    $(BCB)\BIN\bcc32 $(CFLAG1) $(CFLAG2) -o$* $**

.rc.res:
    $(BCB)\BIN\brcc32 $(RFLAGS) $<
#-----------------------------------------------------------------------------
