#!/bin/sh

export TAUOLALOCATION=/home/atlas/jreynolds/work/another_one/external/TAUOLA/TAUOLA
export HEPMCLOCATION=/home/atlas/jreynolds/work/another_one/external/TAUOLA/TAUOLA/../../HEPMC/hepmc2.06.09
export PYTHIALOCATION=/home/atlas/jreynolds/work/another_one/external/TAUOLA/TAUOLA/../../PYTHIA/pythia8306
export MCTESTERLOCATION=/home/atlas/jreynolds/work/another_one/external/TAUOLA/TAUOLA/../../MC-TESTER/mc-tester
export PYTHIA8DATA=

ROOTLIB=`root-config --libdir`

# Examples have these paths hardcoded during compilation
# Nonetheless, this is line might be useful for any other programs
# that user might want to compile
export LD_LIBRARY_PATH=${TAUOLALOCATION}/lib:${PREFIX}/lib:${HEPMCLOCATION}/lib:${PYTHIALOCATION}/lib/archive:${MCTESTERLOCATION}/lib:${ROOTLIB}:${LD_LIBRARY_PATH}
