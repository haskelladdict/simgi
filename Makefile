# Copyright 2009 Markus Dittrich <haskelladdict@users.sourceforge.net>
# Distributed under the terms of the GNU General Public License v3

VERSION=0.2
DESTDIR=
prefix=/usr
mandir=$(DESTDIR)$(prefix)/share/man/man1
docdir=$(DESTDIR)$(prefix)/share/doc/simgi-$(VERSION)
htmldir=$(docdir)/html
bindir=$(DESTDIR)$(prefix)/bin

GHC_FLAGS_DEVEL = -O -Wall -fwarn-simple-patterns -fwarn-tabs -fwarn-incomplete-record-updates -fwarn-monomorphism-restriction -fwarn-implicit-prelude -fno-warn-orphans
GHC_FLAGS_RELEASE = -O2

OBJECTS = src/simgi.hs src/CommandLine.hs src/Engine.hs  \
	  src/GenericModel.hs src/InputCheck.hs src/InputParser.hs \
	  src/PrettyPrint.hs src/RpnParser.hs src/RpnCalc.hs \
	  src/RpnData.hs src/TokenParser.hs 

all: debug

simgi: $(OBJECTS) 
	ghc -i./Models -i./src $(GHC_FLAGS_RELEASE) --make src/simgi.hs



debug: $(OBJECTS) 
	ghc -i./Models -i./src $(GHC_FLAGS_DEVEL) --make src/simgi.hs



check: $(OBJECTS) all
	make -C test


install: simgi
	install -d $(docdir)
	install -d $(bindir)
	install -d $(htmldir)
	install -m 0755 src/simgi $(bindir)/
	install -m 0644 ChangeLog COPYING AUTHORS $(docdir)/
	install -m 0644 doc/*.pdf doc/*.rst $(docdir)/
	install -m 0644 doc/*.html $(htmldir)/


.PHONY: clean doc

doc:
	make -C doc

clean:
	rm -f src/*.o src/*.hi src/simgi 
	make -C test clean	
	make -C doc clean
