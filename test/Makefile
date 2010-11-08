PFCPATH=../src/
PFC=$(PFCPATH)pfc
PIFFLEBOILER=$(PFCPATH)
INPUTS=$(wildcard *.pfl)
OBJECTS=$(INPUTS:%.pfl=%)

all: test

# Test the pfc in ../src
test: .force
	PFC=$(PFC) PIFFLEBOILER=$(PIFFLEBOILER)	/bin/bash test.sh

# Test the INSTALLED version of pfc
installedtest:
	$(MAKE) PFCPATH=


clean:
	-rm *.c *.dat *.bad *.output *~ \#* $(OBJECTS)

.force:
