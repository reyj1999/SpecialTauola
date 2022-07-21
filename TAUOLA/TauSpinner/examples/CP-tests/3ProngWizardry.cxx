// MC-TESTER headers
#include "Generate.h"
#include "HepMCEvent.H"
#include "Setup.H"

// TAUOLA header
#include "Tauola/Tauola.h"
#include "TauSpinner/SimpleParticle.h"
#include "TauSpinner/tau_reweight_lib.h"
#include "TauSpinner/read_particles_from_TAUOLA.h"

// File header 
#include <iostream>
#include <fstream>

// LHAPDF header
#include "LHAPDF/LHAPDF.h"
using namespace std;
using namespace Tauolapp;
using namespace TauSpinner;


// Write data to a file 
/*void writeParticles(const char* const name, SimpleParticle &particle)
{
  string file("tauspunout.dat");
  ofstream file_out;
  file_out.open(file, std::ios_base::app);
  double WT
  if (particle == X){
    file_out << "Weight " << WT << endl;
    file_out << particle.px() << " " << particle.py() << " " << particle.pz() << " " << particle.e() << " " \
	     << particle.pdgid() <<endl;
  }
  else{
    file_out << particle.px() << " " << particle.py() << " " << particle.pz() << " " << particle.e() << " " \
	   << particle.pdgid() <<endl;
  }
 return;
}
*/

/*
void WriteParticles(SimpleParticle &particle)
{
  string file("3prongtauspunout.dat");
  ofstream file_out;
  file_out.open(file, std::ios_base::app);
  file_out << particle.px() << " " << particle.py() << " " << particle.pz() << " " << particle.e() << " " << particle.pdgid() << endl;
  return;
}
*/

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
  //setHiggsParametersTR(-1.0, 1.0, 0.0, 0.0); // for scalar H
  setHiggsParametersTR( 1.0,-1.0, 0.0, 0.0); // for pseudo-scalar H
  //double theta = 3.141592654;
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

  // Open file to write in
  string file("output_for_higgs_team.dat");
  ofstream file_out;
  file_out.open(file, std::ios_base::app);

  // Open file to write in
  string file2("IP_tau_vert.dat");
  ofstream another_file_out;
  another_file_out.open(file2, std::ios_base::app);

  // Open file to write in
  string file3("IP_pion+_3_momentum.dat");
  ofstream super_file_out;
  super_file_out.open(file3, std::ios_base::app);

  // Open file to write in
  string file4("IP_pion-_3_momentum.dat");
  ofstream fantastic_file_out;
  fantastic_file_out.open(file4, std::ios_base::app);

  // Open file to write in 
  string file5("IP_tau+_neutrino_3_momentum.dat");
  ofstream nuplus_file_out;
  nuplus_file_out.open(file5, std::ios_base::app);

  // Open file to write in
  string file6("IP_tau-_neutrino_3_momentum.dat");
  ofstream numinus_file_out;
  numinus_file_out.open(file6, std::ios_base::app);

  //---------------------------------------------------------------------------
  //- Event loop --------------------------------------------------------------
  //---------------------------------------------------------------------------
  while(true) {
    double WT    = 1.0;
    int    pdgid = 0;

    SimpleParticle         X, tau, tau2; // SimpleParticle consist of 4 momentum and PDGid.
    vector<SimpleParticle> tau_daughters, tau_daughters2;
    int nV = 0; // Count the number of vertices

    // Read event from input_file.
    int status = readParticlesFromTAUOLA_HepMC_wizardry(input_file, X, tau, tau2, tau_daughters, tau_daughters2, nV);

    //    cout << "quack" << nV << endl;

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
      //cout << X.pdgid() << "\t" << X.px() << endl;
      //cout << tau.pdgid() << "\t" << tau.px() << endl;
      //cout << tau_daughters[0].pdgid() << "\t" << tau_daughters[0].px() << endl;
      //cout << tau_daughters[1].pdgid() << "\t" << tau_daughters[1].px() << endl;
      //cout << tau_daughters[2].pdgid() << "\t" << tau_daughters[2].px() << endl;
      //cout << tau_daughters[3].pdgid() << "\t" << tau_daughters[3].px() << endl;
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

    //cout << X.pdgid() << "\t" << X.px() << endl;
    //cout << tau.pdgid() << "\t" << tau.px() << endl;
    //cout << tau_daughters[0].pdgid() << "\t" << tau_daughters[0].px() << endl;
    //cout << tau_daughters[1].pdgid() << "\t" << tau_daughters[1].py() << endl;
    //cout << tau_daughters[2].pdgid() << "\t" << tau_daughters[2].pz() << endl;

    auto print_simple_particle = [](const char* const name, SimpleParticle &particle)
      {
	//	cout << "SimpleParticle " << name << "(" << particle.px() << " , " << particle.py() << " , " << particle.pz() << " , " << particle.e() << " , " << particle.pdgid() << ")" << endl;
	cout << particle.px() << " " << particle.py() << " " << particle.pz() << " " << particle.e() << " " << particle.pdgid() << endl;
      };

    auto WriteParticles = [&file_out](SimpleParticle &particle)
    {
      //      string file("3prongtauspunout.dat");
      //      ofstream file_out;
      //      file_out.open(file, std::ios_base::app);
      file_out << " " << particle.px() << "  " << particle.py() << "  " << particle.pz() << "  " << particle.e() << "  " << particle.pdgid() << endl;
      return;
    };

    auto WriteTauVert = [&another_file_out](SimpleParticle &particle)
    {
      another_file_out << particle.vx() << "  " << particle.vy() << "  " << particle.vz() << "  " << particle.t() << endl;
    };

    auto WritePion3Momenta = [&super_file_out, &fantastic_file_out](SimpleParticle &particle)
    {
      if(particle.pdgid()==211){
	super_file_out << particle.px() << "  " << particle.py() << "  " << particle.pz() << "  " << particle.pdgid() << endl;
      }
      else{
	fantastic_file_out << particle.px() << "  " << particle.py() << "  " << particle.pz() << "  " << particle.pdgid() << endl;
      }
    };
    
    auto WriteTauNeutrino3Momenta = [&nuplus_file_out, &numinus_file_out](SimpleParticle &particle)
    {
      if(particle.pdgid()==16){
	nuplus_file_out << particle.px() << "  " << particle.py() << "  " << particle.pz() << "  " << particle.pdgid() << endl;
      }
      else{
	numinus_file_out << particle.px() << "  " << particle.py() << "  " << particle.pz() << "  " << particle.pdgid() << endl;
      };
    };

    /*
    print_simple_particle("X", X);
    print_simple_particle(tau);
    print_simple_particle(tau_daughters[0]);
    print_simple_particle(tau_daughters[1]);
    print_simple_particle(tau_daughters[2]);
    print_simple_particle(tau_daughters[3]);

    print_simple_particle(tau2);
    print_simple_particle(tau_daughters2[0]);
    print_simple_particle(tau_daughters2[1]);
    print_simple_particle(tau_daughters2[2]);
    print_simple_particle(tau_daughters2[3]);
    */

    /*    string file("3prongtauspunout.dat");
    ofstream file_out;
    file_out.open(file, std::ios_base::app);
    */
    file_out << "TUPLE  " << WT << endl;

    WriteParticles(X);

    WriteParticles(tau);

    for(auto daughter : tau_daughters)
      {
	WriteParticles(daughter);
      }
    
    WritePion3Momenta(tau_daughters[2]);

    WriteTauNeutrino3Momenta(tau_daughters[0]);

    WriteTauVert(tau);

    //    WriteParticles(tau_daughters[0]);
    //    WriteParticles(tau_daughters[2]);
    //    WriteParticles(tau_daughters[1]);
    //WriteParticles(tau_daughters[3]);

    WriteParticles(tau2);


   
    for(auto daughter : tau_daughters2)
      {
	WriteParticles(daughter);
      }
    
    WritePion3Momenta(tau_daughters2[2]);
    
    WriteTauNeutrino3Momenta(tau_daughters2[0]);

    WriteTauVert(tau2);

    //   WriteParticles(tau_daughters2[0]);
    //   WriteParticles(tau_daughters2[2]);
    //   WriteParticles(tau_daughters2[1]);
    //   WriteParticles(tau_daughters2[3]);
    
    cout << "Weight " << WT << endl;

    if(events_count%10000==0) cout<<"EVT: "<<events_count<<endl;
    if(events_limit && events_count>=events_limit) break;
    
  }
  MC_Finalize();

  cout<<endl<<"No of events processed for spin weight: "<<events_count<<endl;
  cout<<      "WT average for these events: "<<wt_average/events_count<<endl;
}
