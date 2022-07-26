// MC-TESTER headers
#include "Generate.h"
#include "HepMCEvent.H"
#include "Setup.H"

// TAUOLA header
#include "Tauola/Tauola.h"

#include "TauSpinner/SimpleParticle.h"
#include "TauSpinner/tau_reweight_lib.h"
#include "TauSpinner/read_particles_from_TAUOLA.h"

// LHAPDF header
#include "LHAPDF/LHAPDF.h"
using namespace std;
using namespace Tauolapp;
using namespace TauSpinner;

int main(int argc, char **argv) {

  char *input_filename = "events.dat";

  if(argc<2)
  {
    cout<<endl<<"Processing all available events from default input file: "<<input_filename<<endl;
    cout<<      "You can change this option by using:   "<<argv[0]<<" <filename> [<events_limit>]"<<endl<<endl;
  }
  else input_filename = argv[1];

  //---------------------------------------------------------------------------
  //- Initialization ----------------------------------------------------------
  //---------------------------------------------------------------------------

  // Limit number of processed events
  int events_limit = 0;

  if(argc>2) events_limit = atoi(argv[2]);

  // Initialize Tauola
  Tauola::initialize();

  string name="cteq6ll.LHpdf";
  LHAPDF::initPDFSetByName(name);

  // Next 3 lines are used to initialize TauSpinner
  //  CMSENE should be adjusted, Ipp    = true should be kept and
  // examples are set to work with unpolarized sammples only, Ipol is not
  // functioning yet.

  double CMSENE = 13000.; // center of mass system energy used in PDF calculation
  bool   Ipp    = true;   // for pp collisions
  int    Ipol   = 1;      // are input samples polarized?

  // Initialize TauSpinner (flags nonSM and nonSMN are 0 for these tests)
  initialize_spinner(Ipp, Ipol, 0, 0, CMSENE);

  // Next line is used to initialize  H-rho, H-pi tests:
  setHiggsParametersTR(-1.0, 1.0, 0.0, 0.0); // for scalar H
  //setHiggsParametersTR( 1.0,-1.0, 0.0, 0.0); // for pseudo-scalar H
  //double theta=0.2;
  //setHiggsParametersTR(-cos(2*theta),cos(2*theta) ,-sin(2*theta),-sin(2*theta)); // for mixed parity case


  // Next line is used by  CP-test-Z-pi, CP-test-Z-rho
  // Multipliers for  components of transverse density matrix of DY
  //                  (Rxx,Ryy,Rxy,Ryx)
  setZgamMultipliersTR(1., 1., 1., 1. );

  // Open I/O files
  HepMC::IO_GenEvent input_file(input_filename,std::ios::in);

  // Initialize MC-Tester
  MC_Initialize();

  int    events_count = 0;
  double wt_average   = 0.0;

  //---------------------------------------------------------------------------
  //- Event loop --------------------------------------------------------------
  //---------------------------------------------------------------------------
  while(true) {
    double WT    = 1.0;
    int    pdgid = 0;

    SimpleParticle         X, tau, tau2; // SimpleParticle consist of 4 momentum and PDGid.
    vector<SimpleParticle> tau_daughters, tau_daughters2;
    int nV = 0; // Count the number of vertices

    cout << "baaaa" << nV << endl;

    // Read event from input_file.
    int status = readParticlesFromTAUOLA_HepMC_wizardry(input_file, X, tau, tau2, tau_daughters, tau_daughters2, nV);

    cout << "quack" << nV << endl;

    cout << tau.vx() <<endl;

    // Go to next one if there is nothing more to do with this event
    if( status==1 ) continue;

    // Finish if there is nothing more to read from the file
    if( status==0 ) break;

    pdgid = X.pdgid();

    // Sets polarization state of the input sample
    //setSpinOfSample(0);

    // Calculate weight
    if( abs(X.pdgid())==24 ||  abs(X.pdgid())==37 )
    {
      WT = calculateWeightFromParticlesWorHpn(X, tau, tau2, tau_daughters); // note that tau2 is tau neutrino
    }
    else if( X.pdgid()==25 || X.pdgid()==36 || X.pdgid()==22 || X.pdgid()==23 )
    {
      // NOTE: if any component of transverse density matrix for DY has been turned on
      //       using setZgamMultipliersTR, weight WT will contain transverse part
      //       of the spin amplitude (calculated from tables table1-1.txt table2-2.txt
      //       which must be stored in the same directory as the program)
      WT = calculateWeightFromParticlesH(X, tau, tau2, tau_daughters,tau_daughters2);
    }
    else
    {
      cout<<"WARNING: Unexpected PDG for tau mother: "<<X.pdgid()<<endl;
    }

    wt_average += WT;
    events_count++;

    // For MC-TESTER analysis we use event defined in TauSpinner/src/readParticlesFromTAUOLA_HepMC
    HepMC::GenEvent *mc_tester_event = readParticlesFromTAUOLA_HepMC_getEvent();
    HepMCEvent *temp_event = new HepMCEvent(*mc_tester_event,false);

    // next two lines are needed for all CP-tests
    Setup::UTA_nparams   = 1;
    Setup::UTA_params[0] = WT;

    MC_Analyze(temp_event,WT);

    // Cleanup
    delete temp_event;

    if(events_count%10000==0) cout<<"EVT: "<<events_count<<endl;
    if(events_limit && events_count>=events_limit) break;
  }

  MC_Finalize();

  cout<<endl<<"No of events processed for spin weight: "<<events_count<<endl;
  cout<<      "WT average for these events: "<<wt_average/events_count<<endl;
}
