GHCFLAGS+= -fallow-overlapping-instances
GHCFLAGS+= -fglasgow-exts
GHCFLAGS+= -fno-monomorphism-restriction
GHCFLAGS+= -Wall
GHCFLAGS+= -cpp
GHCFLAGS+= -fno-warn-name-shadowing
GHCFLAGS+= -fno-warn-missing-signatures
GHCFLAGS+= -fno-warn-type-defaults
GHCFLAGS+= -fno-warn-orphans
GHCFLAGS+= -fno-warn-unused-imports
CFLAGS=-O3 -Wall
LDFLAGS=-lpcap
TARGETS=pfc

all: $(TARGETS) 

pfc: *.hs
	ghc $(GHCFLAGS) -o $@ --make Main.hs
	strip $@		# Cuts down binary size by half!

interactive: .force
	ghci $(GHCFLAGS)

clean:
	-rm -f $(TARGETS)
	-rm -f *~ */*~  \#* */\#* 
	-rm -f *.o */*.o  *.hi */*.hi

.force:
