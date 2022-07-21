includedir=$(prefix)/include/TauSpinner

lib_LTLIBRARIES = libTauolaTauSpinner.la

libTauolaTauSpinner_la_SOURCES =            \
src/tau_reweight_lib.cxx                    \
src/nonSM.cxx                               \
src/disth.f

libTauolaTauSpinner_la_LIBADD = ../tauola-fortran/libTauolaFortran.la ../src/libTauolaCxxInterface.la

include_HEADERS =                     \
include/TauSpinner/Particle.h         \
include/TauSpinner/SimpleParticle.h   \
include/TauSpinner/Tauola_wrapper.h   \
include/TauSpinner/tau_reweight_lib.h \
include/TauSpinner/nonSM.h

INCLUDES = -I$(top_srcdir)/include  \
           -I$(LHAPDF_DIR)/include  \
           -Iinclude

