# This specifies where to install. You can override it ("make install
# PREFIX=/opt/piffle"). Note that if the boilerplates are installed in
# a nonstandard location, the compiler will *not* be able to find them
# unless you specify the PIFFLEBOILER environment variable!
PREFIX=/usr/local

# This is the release number, used for building the tarball. It must be
# overridden on the command line ("make dist RELEASE=1.0")
RELEASE=

######################################################################
# NO USER-SERVICEABLE PARTS BEYOND THIS LINE

# By default, when "make" is run, compile the source, but not the
# documentation or the test suite.
all: src 

######################################################################
# BUILD THINGS

src: .force
	( cd src; $(MAKE) )

# Build documentation with make -k, so that if a user does not have
# all the tools, they will still get *some* documentation.
doc: .force
	echo $(RELEASE) > doc/release.tex
	( cd doc; $(MAKE) -k)

test: src .force
	( cd test; $(MAKE) )

######################################################################
# CLEAN UP

# This cleans up everything to the point where we can make a
# distribution tarball.

clean:
	-( cd src; $(MAKE) clean )
	-( cd doc; $(MAKE) clean )
	-( cd test; $(MAKE) clean )
	-rm -f *~ \#*

######################################################################
# INSTALL

# I have tried to install everything into the standard UNIXy
# locations. If you disagree with my interpretation of the partially
# unwritten conventions of what-goes-where on UNIX, please let me
# know.

install: all doc
	install -d $(PREFIX)/share/doc/piffle
	install -t $(PREFIX)/share/doc/piffle doc/piffle.pdf || true
	install -t $(PREFIX)/share/doc/piffle doc/pfc.txt
	install -t $(PREFIX)/share/doc/piffle doc/pfc.pcap.txt
	install -t $(PREFIX)/share/doc/piffle doc/pfc.test.txt
	install -d $(PREFIX)/share/man/man1
	install -t $(PREFIX)/share/man/man1 doc/pfc.1
	install -t $(PREFIX)/share/man/man1 doc/pfc.pcap.1
	install -t $(PREFIX)/share/man/man1 doc/pfc.test.1
	install -d $(PREFIX)/share/piffle
	install -t $(PREFIX)/share/piffle src/pcap.c
	install -t $(PREFIX)/share/piffle src/test.c
	install -d $(PREFIX)/bin
	install -t $(PREFIX)/bin src/pfc

######################################################################
# DISTRIBUTION

# Produce a dated copy of the source for distribution

dist: clean
	if [ unspecified$(RELEASE) = unspecified ]; then exit 1; fi
	CURRENT=`pwd | xargs basename` ;\
	NUMBERED=$$CURRENT-$(RELEASE) ;\
	cd ..;\
	cp -r $$CURRENT $$NUMBERED ;\
	tar --exclude=".svn*" -czv -f $$NUMBERED.tar.gz $$NUMBERED;\
	googlecode-upload.py -s "Piffle $(RELEASE)" \
	 -p piffle -u jaapweel $$NUMBERED.tar.gz || true

# Produce a dated copy of the manual for distribution

distman: doc
	if [ unspecified$(RELEASE) = unspecified ]; then exit 1; fi
	MANUAL=`pwd | xargs basename`/doc/piffle.pdf ;\
	cd ..;\
	cp $$MANUAL piffle-$(RELEASE).pdf ;\
	googlecode-upload.py -s "Piffle Manual $(RELEASE)" -l "Documentation" \
	 -p piffle -u jaapweel piffle-$(RELEASE).pdf || true

# Produce a dated copy of the HTML manual

disthtmlman: doc
	if [ unspecified$(RELEASE) = unspecified ]; then exit 1; fi
	FROM=`pwd | xargs basename`/doc/html ;\
	TO=web/piffle-html-$(RELEASE) ;\
	cd ..; 	( mkdir $$TO || true ) ;\
	cp -R $$FROM/* $$FROM/.htaccess $$TO/ ;\
	svn add $$TO/ ;\
	svn propset svn:mime-type text/html $$TO/*.html ;\
	svn propset svn:mime-type text/css $$TO/*.css ;\
	svn log -v > web/changes.txt ;\
	cd web ;\
	sed "s/@@RELEASE@@/$(RELEASE)/" < latest.html.in > latest.html;\

.force:

