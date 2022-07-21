/**
 * @brief MC_Initialize, MC_Analyze and MC_Finalize declaration
 *
 * Main routines used inside C++ program to invoke MC-TESTER analysis
 * MC_Initialize and MC_Finalize routines must be invoked exactly
 * once during the analysis. MC_Analyze is invoked for each analyzed event
 *
 * Supporting routines:
 * MC_FillUserHistogram - used to add histogram to User Histograms section of
 *                        documentation
 * PrintAnalysedEvent   - prints out informations regarding the analysed event
 */
#include "HEPEvent.H"

#ifndef __GENERATE_H_
#define __GENERATE_H_

void MC_Initialize();
void MC_Analyze();
void MC_Analyze(HEPEvent * event, double weight=1.0);
void MC_Analyze(int particle, double weight=1.0, HEPEvent * event=0); // PDG code will be got from Setup::decay_particle_name
void MC_Finalize();
void PrintAnalysedEvent();
void MC_FillUserHistogram(char *name, double value, double weight=1.0);

/** MC_FinalizeAtExit

  Executes 'MC_Finalize()' when program exits without error, when program
  is interrupted, aborted, terminated or when segmentation fault occurs.
  This allows to save partial results in case of crash.
  Useful in very long runs which quit due to error.
  
  WARNING: This might be harmful in some cases!!
           Only some of the UNIX oparting systems support this feature. */
void MC_FinalizeAtExit();

#endif
