	MANTARGETS=pfc.txt pfc.pcap.txt pfc.test.txt
TARGETS=html/piffle.html piffle.pdf $(MANTARGETS)
TEXSOURCES= piffle.tex release.tex $(MANTARGETS) pcap_tcp.pfl pcap_tcp.c

all: $(TARGETS)

######################################################################
# THE MANUAL

# The TeX manual includes the text versions of manpages, as well as
# the example program, using \VerbInclude, so those need to be
# generated first. 

piffle.dvi: $(TEXSOURCES)
	latex piffle
	latex piffle
	$(MAKE) cleanup_tex_tempfiles

%.pdf: %.dvi
	dvipdfm $<
.SUFFIXES: .pdf .dvi

# Compared to the alternatives, tex4ht provides a remarkably good
# conversion from LaTeX to HTML.

html/piffle.html: $(TEXSOURCES)
	-htlatex piffle.tex "" "" "-dhtml/"
	$(MAKE) cleanup_tex_tempfiles
	echo "DirectoryIndex piffle.html" > html/.htaccess
	-rm -f piffle.dvi	# htlatex overwrites the dvi!


######################################################################
# CODE EXAMPLES FOR IN THE MANUAL

# We copy a .pfl file from the test suite, and compile it to .c, for
# inclusion in the manual. This requires that the pfc compiler be
# already available. The resulting .c is included in the release, so
# users can compile the manual without having a working pfc compiler,
# as long as they don't update the example file. Fancy, eh?

pcap_tcp.c: pcap_tcp.pfl
	../src/pfc pcap_tcp.pfl

pcap_tcp.pfl: ../test/pcap_tcp.pfl
	cp $< $@

######################################################################
# CONVERT MAN PAGE TO TEXT FILE

# Produce a text file version of the man page, which is then included
# in the LaTeX document. colcrt inserts weird multibyte hyphens, which
# are converted to regular hyphens lest TeX get confused. This is all
# a little clumsy, but it's the easiest way of making sure that the
# information in the man page and the manual are in sync.

%.txt: %.1
	nroff -man $< | colcrt - | sed 's/\xe2\x80\x90/-/g' > $@

.SUFFIXES: .txt .1

######################################################################
# CLEANUP

# latex and htlatex leave behind an enormous clutter of temporary
# files.  I remove these immediately after building the documentation,
# and not just upon make clean, because otherwise I wouldn't expect a
# user to actually be able to find the document they just compiled.

cleanup_tex_tempfiles:
	-rm -f *.aux *.log  *.toc *.log *.blg *.bbl *.tmp
	-rm -f *.html *.aux *.css *.idv *.lg *.log *.tm *.toc *.xref
	-rm -f *.4ct *.4tc 

clean: cleanup_tex_tempfiles
	-rm -f *~ \#*
	-rm -f *.dvi *.pdf
	-rm -f html/* html/.htaccess

