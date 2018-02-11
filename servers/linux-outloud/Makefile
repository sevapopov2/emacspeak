#$Id: Makefile 7541 2012-03-04 22:31:26Z tv.raman.tv $

INSTALL = install
PREFIX = /usr
LIBPARENTDIR = ${PREFIX}/share/emacs/site-lisp
LIBDIR =$(LIBPARENTDIR)/emacspeak/servers/linux-outloud

TCL_INCLUDE=/usr/include/tcl
CFLAGS+= -g    -O2 -fPIC  -DPIC  -pedantic -ansi -Wall  -Wno-long-long -I$(TCL_INCLUDE)
CXXFLAGS+= $(CFLAGS)
LIBS= -ltcl -lasound -ldl    

all:   atcleci.so

atcleci.so: langswitch.o atcleci.o
	$(CXX) $(LDFLAGS) -shared -o $@ langswitch.o atcleci.o $(LIBS)

alsaparams: alsaparams.c
	gcc -o alsaparams alsaparams.c -lasound

clean:
	rm -f *.so *.o 

install: atcleci.so
	$(INSTALL) -d $(DESTDIR)$(LIBDIR)
	$(INSTALL) $< $(DESTDIR)$(LIBDIR)

