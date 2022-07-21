/**
 * @class Setup
 * @brief Used by MC-TESTER to store configuration information
 *
 * Stores parameters for both generation and analysis step
 */
#ifndef SETUP_H
#define SETUP_H

#include "TObject.h"

#define MAX_DECAY_MULTIPLICITY 20

class HEPEvent;
class TH1D;
class TObjArray;
class UserEventAnalysis;
class HEPParticle;
class HEPParticleList;
class Setup: public TObject {

public:

    static int decay_particle; // PDG code of decaying particle

    static HEPEvent* EVENT; // event to be analyzed (HEPEVT, LUJETS, PYJETS...)

    static double bin_min [MAX_DECAY_MULTIPLICITY] [MAX_DECAY_MULTIPLICITY]; // lower bin for i-body decay
    static double bin_max [MAX_DECAY_MULTIPLICITY] [MAX_DECAY_MULTIPLICITY]; // upper bin for i-body decay 
    static int    nbins   [MAX_DECAY_MULTIPLICITY] [MAX_DECAY_MULTIPLICITY]; // number of bins for i-body decay

    static char*  gen1_desc_1; // description of generator 1 - 1st line
    static char*  gen1_desc_2; // description of generator 1 - 2nd line
    static char*  gen1_desc_3; // description of generator 1 - 3rd line

    static char*  gen2_desc_1; // description of generator 2 - 1st line
    static char*  gen2_desc_2; // description of generator 2 - 2nd line
    static char*  gen2_desc_3; // description of generator 2 - 3rd line

    static char*   gen1_path;
    static char*   gen2_path;
    
    static char*   result1_path;
    static char*   result2_path;

    static double (*user_analysis)(TH1D*,TH1D*);// pointer to hist.analysis function (i.e. KolmogorovAnalysis)

    static UserEventAnalysis *user_event_analysis;// pointer to EVENT analysis object (i.e. LC_EventAnalysis
						    // used by Linear Collider Workshop

//    static int (*UserTreeAnalysis)(HEPParticle *,HEPParticleList *); //! pointer to function that pre-processes
                                                                    // generated tree: i.e. applies energy cuts.

    static char *UserTreeAnalysis;
    static HEPParticle* UTA_particle;
    static HEPParticleList* UTA_partlist;
    static int    UTA_nparams;
    static double UTA_params[16];


    static int order_matters; // if set, order of particles in event record matters when searching for decay mode
			       // i.e. [pi- pi0 pi+] differs from [pi- pi+ pi0] . By default: 0
			       

    static int stage; // 1,2 - generation from generators, 0 - analysis.

    static int max_cascade_depth;

    static int debug_mode; // default=0(off) 1-prints some info about why certain particles haven't been analyzed...
    static long int events_cnt; // counts processed events (updated by MC-TESTER itself)
    static bool use_log_y;
    static long int mass_power;
    static long int rebin_factor;
    static bool mass_scale_on;

    static bool scale_histograms;
    
    static double event_weight;

    static TObjArray *user_histograms; // list of user-defined histogrammes.

private:
    static int suppress_decay[100];// PDGs of particles which decay we want to suppress
    static int nsuppressed_decays;// number of suppressed particles

public:    
    Setup();
    static char* ResolvePath(char *path, char *resolved_path);

    static void SetHistogramDefaults(int nbins, double min_bin, double max_bin);

    static void SuppressDecay(int pdg);
    static int  IsSuppressed(int pdg);
public:
    static Setup setup; // to cause constructor to be called...

    ClassDef(Setup,1) // settings for MC-TESTER
};

#endif //SETUP_H
