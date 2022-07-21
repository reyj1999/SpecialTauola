include make.inc
include $(TAUOLALOCATION)/make.inc
include $(TAUOLALOCATION)/TauSpinner/examples/make.inc

HEPMC_LIBS     = -L$(HEPMCLOCATION)/lib -lHepMC
TAUOLA_LIBS    = -L$(TAUOLALOCATION)/lib -L../lib -lTauolaTauSpinner -lTauolaFortran -lTauolaCxxInterface
LHAPDF_LIBS    = -L$(LHAPDFLOCATION)/lib -lLHAPDF
PYTHIA_LIBS    = -L$(PYTHIALOCATION)/lib/archive -lpythia8 -lpythia8tohepmc
GENERATOR      = generate-datafiles/generate.cxx

TAU_SPINNER_HEPMC_INTERFACE = $(TAUOLALOCATION)/TauSpinner/src/read_particles_from_TAUOLA.o

all: CP-test.exe
all: 3ProngWizardry.exe
all: Eva1Prong.exe
all: EvaActual1Prong.exe

generate:
	$(CC) -o $(GENERATOR:.cxx=.exe) $(GENERATOR) $(HEPMC_LIBS) $(TAUOLA_LIBS) $(LHAPDF_LIBS) $(PYTHIA_LIBS) \
	-I../include \
	-I$(HEPMCLOCATION)/include   \
	-I$(PYTHIALOCATION)/include  \
	-I$(TAUOLALOCATION)/include  \
	-I$(LHAPDFLOCATION)/include  \
	-Wl,-rpath,$(HEPMCLOCATION)/lib    \
	-Wl,-rpath,$(TAUOLALOCATION)/lib   \
	-Wl,-rpath,$(ROOTLIBPATH)          \
	-Wl,-rpath,$(MCTESTERLOCATION)/lib \
	-Wl,-rpath,$(LHAPDFLOCATION)/lib \
	-lgfortran

%.o: %.cxx
	$(CC) $(CFLAGS) -c $< -o $@ -I../include $(ROOTINC) \
	-I$(HEPMCLOCATION)/include    \
	-I$(TAUOLALOCATION)/include   \
	-I$(MCTESTERLOCATION)/include \
	-I$(LHAPDFLOCATION)/include

%.exe: %.o $(TAU_SPINNER_HEPMC_INTERFACE)
	$(CC) -o $@ $^ $(HEPMC_LIBS) $(TAUOLA_LIBS) $(ROOTLIB) $(MC_TESTER_LIBS) $(LHAPDF_LIBS) $(TAUSPINNER_LIBS) \
	-Wl,-rpath,$(HEPMCLOCATION)/lib    \
	-Wl,-rpath,$(TAUOLALOCATION)/lib   \
	-Wl,-rpath,$(ROOTLIBPATH)          \
	-Wl,-rpath,$(MCTESTERLOCATION)/lib \
	-Wl,-rpath,$(LHAPDFLOCATION)/lib \
	-lgfortran

clean:
	rm -f *.exe */*_C.so */*_C.d */*.exe

Clean: clean
	rm -rf booklet* tester* mc-results* MC-TESTER.DAT
	rm */mc-tester.root -f
