exampledir = $(top_srcdir)/TauSpinner/examples

### Standalone example ###
example_PROGRAMS = tau-reweight-test.exe tau-reweight-CLEO-to-RCHL.exe

if HAS_HEPMC

tau_reweight_test_exe_SOURCES = tau-reweight-test.cxx \
                                distr.f               \
                                ../src/read_particles_from_TAUOLA.cxx

tau_reweight_CLEO_to_RCHL_exe_SOURCES = tau-reweight-CLEO-to-RCHL.cxx \
                                        ../src/read_particles_from_TAUOLA.cxx

INCLUDES = -I$(prefix)/include        \
           -I$(prefix)/include/Tauola \
           -I$(top_srcdir)/TauSpinner/include/

AM_LDFLAGS = -R $(prefix)/lib -R $(with_lhapdf)/lib

LDADD =                          \
$(FLIBS)                         \
$(prefix)/lib/libTauolaTauSpinner.so \
$(prefix)/lib/libTauolaCxxInterface.so \
$(prefix)/lib/libTauolaFortran.so      \
-L$(with_lhapdf)/lib -lLHAPDF

AM_LDFLAGS += -R $(HEPMC_DIR)/lib

INCLUDES += -I$(HEPMC_DIR)/include
LDADD    += -L$(HEPMC_DIR)/lib -lHepMC

endif

