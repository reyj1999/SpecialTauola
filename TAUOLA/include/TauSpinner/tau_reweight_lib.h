#ifndef _TAU_REWEIGHT_LIB_H_
#define _TAU_REWEIGHT_LIB_H_

// Debug mode
#ifdef DEBUG_MODE
#define DEBUG(arg) arg;
#else
#define DEBUG(arg)
#endif

// TAUOLA header
#include "Tauola/Tauola.h"

#include "TauSpinner/Tauola_wrapper.h"
#include "TauSpinner/SimpleParticle.h"
#include "TauSpinner/Particle.h"

// LHAPDF header
#include "LHAPDF/LHAPDF.h"

#include <vector>
#include <iostream>
using std::vector;
using std::cout;
using std::endl;

namespace TauSpinner {

/** Definition of REAL*8 FUNCTION DISTH(S,T,H1,H2) from disth.f */
extern "C" double disth_(double *SVAR, double *COSTHE, int *TA, int *TB);

/** Initialize TauSpinner

    Print info and set global variables */
void initialize_spinner(bool _Ipp, int _Ipol, int _nonSM2, int _nonSMN, double _CMSENE);

/** Set flag for calculating relative(NONSM-SM)/absolute  weight for X-section
  calculated as by product in longitudinal polarization method. */
void setRelWTnonSM(int _relWTnonSM);

/** activates simple formula and set Higgs mass, width and normalization for Higgs born (BW) function */
 void setHiggsParameters(int jak, double mass, double width, double normalization);

/** Get Higgs mass, width and normalization of Higgs born function */
void getHiggsParameters(double *mass, double *width, double *normalization);

/**  Set spin of sample */
void setSpinOfSample(bool _Ipol);

/**  Turn calculation of nonSM on/off */
void setNonSMkey(int key);

void setHiggsParametersTR(double Rxx, double Ryy, double Rxy, double Ryx);

void setZgamMultipliersTR(double Rxx, double Ryy, double Rxy, double Ryx);

void getZgamParametersTR(double &Rxx, double &Ryy, double &Rxy, double &Ryx);

/** Get nonSM weight */
double getWtNonSM();

/** Get tau+ amplitude weight */
double getWtamplitP();

/** Get tau- amplitude weight */
double getWtamplitM();

/** Get tau spin
  
  Used after sample is reweighted to obtain information about tau spin */
double getTauSpin();

/** Calculate weights.

  Determine decay channel and call polarization calculation function.
  Function for W+/- and H+/-  */
double calculateWeightFromParticlesWorHpn(SimpleParticle &W, SimpleParticle &tau, SimpleParticle &nu_tau, vector<SimpleParticle> &tau_daughters);

/** Calculate weights.

  Determine decay channel and call polarization calculation function.
  Function for H */
double calculateWeightFromParticlesH(SimpleParticle &sp_X, SimpleParticle &sp_tau1, SimpleParticle &sp_tau2, vector<SimpleParticle> &sp_tau1_daughters, vector<SimpleParticle> &sp_tau2_daughters);

/** Prepare kinematics for HH calculation
  
  Boost particles to effective bozon rest frame, and rotate them so that tau is on Z axis.
  Then rotate again with theta2 phi2 so neutrino from tau decay is along Z. */
void prepareKinematicForHH(Particle &tau, Particle &nu_tau, vector<Particle> &tau_daughters, double *phi2, double *theta2);

/** Calculate polarization vector.

  We use FORTRAN metdods to calculate HH.
  First decide what is the channel. After that, 4-vectors
  are moved to tau rest frame of tau.
  Polarimetric vector HH is then rotated using angles phi and theta.
  
  Order of the particles does not matter. */
double* calculateHH(int tau_pdgid, vector<Particle> &tau_daughters, double phi, double theta);

/**
 Get Longitudinal polarization
 
 Returns longitudinal polarization in Z/gamma* -> tau+ tau-
 S: invariant mass of the bozon */
double getLongitudinalPolarization(double S, SimpleParticle &sp_tau, SimpleParticle &sp_nu_tau);

/** Check if the list of particles match the list of pdgid

  Returns true if 'particles' contain all of the listed pdgid-s.
  If it does - 'particles' will be rearranged so thath they have
  the same order as listed pdgid-s.

  It is done so the order of particles is the same as the order used by
  TAUOLA Fortran routines. */
bool channelMatch(vector<Particle> &particles, int p1, int p2=0, int p3=0, int p4=0, int p5=0, int p6=0);

//------------------------------------------------------------------------------
//-- Useful debug methods ------------------------------------------------------
//------------------------------------------------------------------------------

/** Prints out two vertices:
      W   -> tau, nu_tau
      tau -> tau_daughters */
void print(Particle &W, Particle &nu_tau, Particle &tau, vector<Particle> &tau_daughters);

/** Sums all 4-vectors of the particles on the list */
Particle *vector_sum(vector<Particle> &x);

} // namespace TauSpinner
#endif
