#$Id: Makefile,v 17.0 2002/11/23 01:29:10 raman Exp $
#
CFLAGS=-g -fpic
CC=gcc
LIBS=-ltts -ltts_us -ltcl

tcldtk.so: tcldtk.o
	$(CC) $< $(LIBS)  -shared -o $@

clean:
	rm tcldtk.so tcldtk.o
