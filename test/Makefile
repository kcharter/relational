.PHONY: clean runtests proftests covtests

GHC_OPTS=-O2 -funbox-strict-fields -i.. -outputdir build -fwarn-unused-imports
GHC_PROF_OPTS=-prof -auto-all -caf-all
GHC_COV_OPTS=-fhpc

PROGS=runtests

TMP=tmp

runtests:
	ghc --make $(GHC_OPTS) runtests.hs	
	./runtests

proftests:
	ghc --make $(GHC_OPTS) $(GHC_PROF_OPTS) runtests.hs
	./runtests +RTS -p -RTS

covtests:
	ghc --make $(GHC_OPTS) $(GHC_COV_OPTS) runtests.hs
	./runtests

runtests.tix: covtests

covmarkup: runtests.tix
	hpc markup --destdir $(TMP) runtests.tix

clean:
	rm -f $(PROGS) *.o *.hi *.tix *.prof *~
	rm -rf build .hpc $(TMP)
