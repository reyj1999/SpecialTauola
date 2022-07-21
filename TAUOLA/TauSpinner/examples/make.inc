HEPMCLOCATION=    /home/atlas/jreynolds/work/another_one/external/TAUOLA/TAUOLA/TauSpinner/examples/../../../../HEPMC/hepmc2.06.09
MCTESTERLOCATION= /home/atlas/jreynolds/work/another_one/external/TAUOLA/TAUOLA/TauSpinner/examples/../../../../MC-TESTER/mc-tester

ifneq ($(MCTESTERLOCATION), )

  ROOTLIB       :=  $(shell root-config --glibs)
  ROOTINC       :=  $(shell root-config --cflags)
  ROOTLIBPATH   :=  $(shell root-config --libdir)
  MC_TESTER_LIBS = -L$(MCTESTERLOCATION)/lib -lHEPEvent -lHepMCEvent -lMCTester

  override CFLAGS += -D_USE_MCTESTER_
endif

