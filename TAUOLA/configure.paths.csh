#!/bin/csh

setenv TAUOLALOCATION "/home/atlas/jreynolds/work/another_one/external/TAUOLA/TAUOLA"
setenv HEPMCLOCATION "/home/atlas/jreynolds/work/another_one/external/TAUOLA/TAUOLA/../../HEPMC/hepmc2.06.09"
setenv PYTHIALOCATION "/home/atlas/jreynolds/work/another_one/external/TAUOLA/TAUOLA/../../PYTHIA/pythia8306"
setenv MCTESTERLOCATION "/home/atlas/jreynolds/work/another_one/external/TAUOLA/TAUOLA/../../MC-TESTER/mc-tester"
setenv PYTHIA8DATA ""

set ROOTLIB=`root-config --libdir`

# Examples have these paths hardcoded during compilation
# Nonetheless, this is line might be useful for any other programs
# that user might want to compile
if (! $?LD_LIBRARY_PATH) setenv LD_LIBRARY_PATH ""

setenv LD_LIBRARY_PATH "${TAUOLALOCATION}/lib:${PREFIX}/lib:${HEPMCLOCATION}/lib:${PYTHIALOCATION}/lib/archive:${MCTESTERLOCATION}/lib:${ROOTLIB}:${LD_LIBRARY_PATH}"
