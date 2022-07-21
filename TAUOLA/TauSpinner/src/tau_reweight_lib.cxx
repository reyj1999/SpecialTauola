#include "TauSpinner/tau_reweight_lib.h"
#include "TauSpinner/nonSM.h"

#include "Tauola/TauolaParticlePair.h"
using namespace Tauolapp;

namespace TauSpinner {

/*****   GLOBAL VARIABLES   *****/

// Center of mass system energy
double CMSENE = 7000.0;

// pp collisions
bool   Ipp    = true;
int    Ipol   = 1;     // Is original sample ( relevant for  Z/gamma case only) polarized?
int    nonSM2 = 0;     // Turn nonSM calculations on/off
int    nonSMN = 0;     // Turn on if calculating nonSM weight for shapes only
int    relWTnonSM = 1; // 1: relWTnonSM is relative to SM or for 0: absolute
double WTnonSM=1.0;    // nonSM weight
double Polari =0.0;    // Helicity-equivalent  for the taus in Z/gamma* production only.
                       // Polari is always attributed for polarized states.
                       // may be should be available with getter only.
bool IfHiggs=false;    // variable for sigborn
double WTamplit=1;     // AMPLIT
double WTamplitP=1;     // AMPLIT for tau+
double WTamplitM=1;     // AMPLIT for tau-
// Higgs parameters
  int    IfHsimple=0;  // switch for simple Higgs
double XMH   = 125.0;  // mass
double XGH   = 1.0;    // width
double Xnorm = 0.15;   // normalization of higgs born

double RXX = 0.0; //-1.0;
double RYY = 0.0; // 1.0;
double RXY = 0.0;
double RYX = 0.0;

double RzXX = 0.0; //-1.0;
double RzYY = 0.0; // 1.0;
double RzXY = 0.0;
double RzYX = 0.0;

// Values calculated inside getLongitudinalPolarization
double R11 = 0.0;
double R22 = 0.0;
double R12 = 0.0;  // for special use may be
double R21 = 0.0; // for special use may be
/********************************/

/**** To be replaced ****/
double f(double x, int ID, double SS, double cmsene)
// this represent set of PDFS; to be replaced.
// x - fraction of parton momenta
// ID flavour of incoming quark
// SS scale of hard process
// cmsene center of mass for pp collision.
{
  // LHAPDF manual: http://projects.hepforge.org/lhapdf/manual
  //  double xfx(const double &x;, const double &Q;, int fl);
  // returns xf(x, Q) for flavour fl - this time the flavour encoding
  // is as in the LHAPDF manual...
  // -6..-1 = tbar,...,ubar, dbar
  // 1..6 = duscbt
  // 0 = g
  // xfx is the C++ wrapper for fortran evolvePDF(x,Q,f)
  // returns the PDF momentum densities f (i.e. x times PDF number density)

  // we return PDF number density so we divide by x
  return LHAPDF::xfx(x, SS, ID)/x;

  //return x*(1-x);

}

double sigborn(int ID, double SS, double costhe)
{
  //  cout << "ID : " << ID << " HgsWTnonSM = " << HgsWTnonSM << " IfHsimple = " << IfHsimple << endl;
  // BORN x-section.
  // WARNING: overall sign of costheta must be fixed
  int tauID = 15;

  // WARNING: THIS WILL BE SEPARATED FUNCTION
    if (IfHiggs) {
      double SIGggHiggs=0.;
      if(ID==0){
        int IPOne =  1;
        int IMOne = -1;
        SIGggHiggs=disth_(&SS, &costhe, &IPOne, &IPOne)+disth_(&SS, &costhe, &IPOne, &IMOne)+
  	           disth_(&SS, &costhe, &IMOne, &IPOne)+disth_(&SS, &costhe, &IMOne, &IMOne);


        double PI=3.14159265358979324;
	SIGggHiggs *=  XMH * XMH * XMH *XGH /PI/ ((SS-XMH*XMH)*(SS-XMH*XMH) + XGH*XGH*XMH*XMH);   
	//	cout << "JK: SIGggHiggs = " << SS << " " << XMH << " " << XGH << " " <<  XMH * XMH * XMH * XGH /PI/ ((SS-XMH*XMH)*(SS-XMH*XMH) + XGH*XGH*XMH*XMH) << " " << SIGggHiggs << endl;

        if(IfHsimple==1) SIGggHiggs =  Xnorm * XMH * XGH /PI/ ((SS-XMH*XMH)*(SS-XMH*XMH) +XGH*XGH*XMH*XMH);
	//	cout << "ZW: SIGggHiggs = " << SS << " " << costhe << " " << SIGggHiggs << endl;
      }
    return SIGggHiggs;
    }
  // WARNING: END OF FUTURE  SEPARATED FUNCTION


  if (ID==0) return 0.0 ;   // for the time being it is zero.
  if (ID>0) initwk_( &ID, &tauID, &SS);
  else
  {
    ID = -ID;
    initwk_( &ID, &tauID, &SS);
  }

  int    iZero = 0;
  double dOne  =  1.0;
  double dMOne = -1.0;
  // sum over all helicity configuration:
  return ( t_born_(&iZero, &SS, &costhe, &dOne , &dOne) + t_born_(&iZero, &SS, &costhe, &dOne , &dMOne)
	   + t_born_(&iZero, &SS, &costhe, &dMOne, &dOne) + t_born_(&iZero, &SS, &costhe, &dMOne, &dMOne))/SS/123231.; 
// overall norm. factor .../SS/123231  most probably it is alpha_QED**2/pi/2/SS is from comparison between Born we use and Born used in Warsaw group. 
}

/*******************************************************************************
  Initialize TauSpinner

  Print info and set global variables
*******************************************************************************/
void initialize_spinner(bool _Ipp, int _Ipol, int _nonSM2, int _nonSMN, double _CMSENE)
{
  Ipp    = _Ipp;
  Ipol   = _Ipol;
  nonSM2 = _nonSM2;
  nonSMN = _nonSMN;

  CMSENE = _CMSENE;

  cout<<" ------------------------------------------------------"<<endl;
  cout<<" TauSpinner v1.4.0"<<endl;
  cout<<" -----------------"<<endl;
  cout<<"   20.Apr.2013  "<<endl;
  cout<<" by Z. Czyczula, T. Przedzinski and Z. Was"<<endl;
  cout<<" for extra coupling contribution "<<endl;
  cout<<" by J. Kalinowski and Wojciech Kotlarski"<<endl;
  cout<<" ------------------------------------------------------"<<endl;
  cout<<" Ipp - true for pp collision; otherwise polarization"<<endl;
  cout<<"       of individual taus from Z/gamma* is set to 0.0"<<endl;
  cout<<" Ipp    = "<<Ipp<<endl;
  cout<<" CMSENE - used in PDF calculations; only if Ipp = true"<<endl;
  cout<<"          and only for Z/gamma*"<<endl;
  cout<<" CMSENE = "<<CMSENE<<endl;
  cout<<" Ipol - relevant for Z/gamma* decays "<<endl;
  cout<<" 0 - events generated without spin effects                "<<endl;
  cout<<" 1 - events generated with all spin effects               "<<endl;
  cout<<" 2 - events generated with spin correlations and <pol>=0  "<<endl;
  cout<<" 3 - events generated with spin correlations and"<<endl;
  cout<<"     polarization but missing angular dependence of <pol>"<<endl;
  cout<<" Ipol    = "<<Ipol<<endl;
  cout<<" Ipol - relevant for Z/gamma* decays "<<endl;
  cout<<" NOTE: For Ipol=0,1 algorithm is identical.               "<<endl;
  cout<<"       However in user program role of wt need change.    "<<endl;
  cout<<" nonSM2  = "<<nonSM2<<endl;
  cout<<" 1/0 extra term in cross section, density matrix on/off   "<<endl;
  cout<<" nonSMN  = "<<nonSMN<<endl;
  cout<<" 1/0 extra term in cross section, for shapes only? on/off "<<endl;
  cout<<" ------------------------------------------------------   "<<endl;
}

/*******************************************************************************
  Set flag for calculating relative(NONSM-SM)/absolute weight for X-section
  calculated as by product in longitudinal polarization method.
  1: relWTnonSM is relative to SM (default)
  0: absolute
*******************************************************************************/
void setRelWTnonSM(int _relWTnonSM)
{
  relWTnonSM = _relWTnonSM;
}

/*******************************************************************************
  Set Higgs mass, width and normalization of Higgs born function
  Default is mass = 125, width = 1.0, normalization = 0.15
*******************************************************************************/
  void setHiggsParameters(int jak, double mass, double width, double normalization)
{
  IfHsimple=jak;
  XMH   = mass;
  XGH   = width;
  Xnorm = normalization;
}

void setHiggsParametersTR(double Rxx, double Ryy, double Rxy, double Ryx)
{
  
  RXX = Rxx;
  RYY = Ryy;
  RXY = Rxy;
  RYX = Ryx;
}

void setZgamMultipliersTR(double Rxx, double Ryy, double Rxy, double Ryx)
{
  RzXX = Rxx;
  RzYY = Ryy;
  RzXY = Rxy;
  RzYX = Ryx;
}

void getZgamParametersTR(double &Rxx, double &Ryy, double &Rxy, double &Ryz)
{
  Rxx = R11;
  Ryy = R22;
  Rxy = 0.0;
  Ryz = 0.0;
}
/*******************************************************************************
  Get Higgs mass, width and normalization of Higgs born function
*******************************************************************************/
void getHiggsParameters(double *mass, double *width, double *normalization)
{
  *mass          = XMH;
  *width         = XGH;
  *normalization = Xnorm;
}

/*******************************************************************************
  Set spin of sample
*******************************************************************************/
void setSpinOfSample(int _Ipol)
{
  Ipol = _Ipol;
}

/*******************************************************************************
  Turn nonSM calculation on/off
*******************************************************************************/
void setNonSMkey(int _key)
{
  nonSM2 = _key;
}

/*******************************************************************************
  Get nonSM weight
*******************************************************************************/
double getWtNonSM()
{
  return WTnonSM;
}
/*******************************************************************************
  Get amplitude weight
*******************************************************************************/
double getWtamplitP(){return WTamplitP;}
double getWtamplitM(){return WTamplitM;}
/*******************************************************************************
  Get tau spin
  
  Used after sample is reweighted to obtain information about tau spin
*******************************************************************************/
double getTauSpin()
{
  return Polari;
}

/*******************************************************************************
  Calculate weights.

  Determine decay channel and call polarization calculation function.
  Function for W+/- and H+/-
*******************************************************************************/
double calculateWeightFromParticlesWorHpn(SimpleParticle &sp_X, SimpleParticle &sp_tau, SimpleParticle &sp_nu_tau, vector<SimpleParticle> &sp_tau_daughters)
{
  // Create Particles from SimpleParticles

  Particle X     (     sp_X.px(),      sp_X.py(),      sp_X.pz(),      sp_X.e(),      sp_X.pdgid() );
  Particle tau   (   sp_tau.px(),    sp_tau.py(),    sp_tau.pz(),    sp_tau.e(),    sp_tau.pdgid() );
  Particle nu_tau(sp_nu_tau.px(), sp_nu_tau.py(), sp_nu_tau.pz(), sp_nu_tau.e(), sp_nu_tau.pdgid() );

  vector<Particle> tau_daughters;

  // tau pdgid
  int tau_pdgid = sp_tau.pdgid();

  // Create list of tau daughters
  for(unsigned int i=0; i<sp_tau_daughters.size(); i++)
  {
    Particle pp(sp_tau_daughters[i].px(),
                sp_tau_daughters[i].py(),
                sp_tau_daughters[i].pz(),
                sp_tau_daughters[i].e(),
                sp_tau_daughters[i].pdgid() );

    tau_daughters.push_back(pp);
  }

  double phi2 = 0.0, theta2 = 0.0;


  //  Move decay kinematics first to tau rest frame  with z axis pointing along nu_tau direction
  //  later rotate again to have neutrino from tau decay along z axis: angles phi2, theta2
  prepareKinematicForHH   (tau, nu_tau, tau_daughters, &phi2, &theta2);


  //  Determine decay channel and then calculate polarimetric vector HH
  double *HH = calculateHH(tau_pdgid, tau_daughters, phi2, theta2);
 
  double sign = 1.0;  // tau from W is 100 % polarized, also from charged Higgs (but with opposite sign)
  if     ( abs(sp_X.pdgid()) == 24 ) { sign= 1.0; } 
  else if( abs(sp_X.pdgid()) == 37 ) { sign=-1.0; } 
  else
  {
    cout<<"wrong sp_W/H.pdgid()="<<sp_X.pdgid()<<endl;
    exit(-1);
  }
  if     (sp_X.pdgid() > 0 )   
    {WTamplitM = WTamplit;}   // tau-
  else
    {WTamplitP = WTamplit;}   // tau+

  double WT   = 1.0+sign*HH[2];     // [2] means 'pz' component

  // Print out some info about the channel
  DEBUG
  (
    cout<<tau_pdgid<<" -> ";
    for(unsigned int i=0;i<tau_daughters.size();i++) cout<<tau_daughters[i].pdgid()<<" ";
    cout<<" (HH: "<<HH[0]<<" "<<HH[1]<<" "<<HH[2]<<" "<<HH[3]<<") WT: "<<WT<<endl;
  )

  // TP:31Nov2013
  if (WT<0.0) {
    printf("WT is: %13.10f. Setting WT = 0.0\n",WT);
    WT = 0.0;
   }

  if (WT>2.0) {
    printf("WT is: %13.10f. Setting WT = 2.0\n",WT);
    WT = 2.0;
  }

  delete HH;
  
  return WT;
}

/*******************************************************************************
  Calculate weights.

  Determine decay channel and call polarization calculation function.
  Function for H
*******************************************************************************/
double calculateWeightFromParticlesH(SimpleParticle &sp_X, SimpleParticle &sp_tau1, SimpleParticle &sp_tau2, vector<SimpleParticle> &sp_tau1_daughters, vector<SimpleParticle> &sp_tau2_daughters)
{
  //  cout << "sp_tau1_daughters = " << sp_tau1_daughters.size() << endl;
  //  cout << "sp_tau2_daughters = " << sp_tau2_daughters.size() << endl;
  SimpleParticle         sp_tau;
  SimpleParticle         sp_nu_tau;
  vector<SimpleParticle> sp_tau_daughters;
  
  // First iteration is for tau plus, so the 'nu_tau' is tau minus
  if (sp_tau1.pdgid() == -15 )
  {
    sp_tau           = sp_tau1;
    sp_nu_tau        = sp_tau2;
    sp_tau_daughters = sp_tau1_daughters;
  }
  else
  {
    sp_tau           = sp_tau2;
    sp_nu_tau        = sp_tau1;
    sp_tau_daughters = sp_tau2_daughters;
  }

  double *HHp, *HHm;
  
  // We use this to separate namespace for tau+ and tau-
  if(true)
  {
    // Create Particles from SimpleParticles
    Particle X     (      sp_X.px(),      sp_X.py(),      sp_X.pz(),      sp_X.e(),      sp_X.pdgid() );
    Particle tau   (    sp_tau.px(),    sp_tau.py(),    sp_tau.pz(),    sp_tau.e(),    sp_tau.pdgid() );
    Particle nu_tau( sp_nu_tau.px(), sp_nu_tau.py(), sp_nu_tau.pz(), sp_nu_tau.e(), sp_nu_tau.pdgid() );

    vector<Particle> tau_daughters;

    // tau pdgid
    int tau_pdgid = sp_tau.pdgid();

    // Create list of tau daughters
    for(unsigned int i=0; i<sp_tau_daughters.size(); i++)
    {
      Particle pp(sp_tau_daughters[i].px(),
                  sp_tau_daughters[i].py(),
                  sp_tau_daughters[i].pz(),
                  sp_tau_daughters[i].e(),
                  sp_tau_daughters[i].pdgid() );

      tau_daughters.push_back(pp);
    }

    double phi2 = 0.0, theta2 = 0.0;


    //  Move decay kinematics first to tau rest frame  with z axis pointing along nu_tau direction
    //  later rotate again to have neutrino from tau decay along z axis: angles phi2, theta2
    prepareKinematicForHH   (tau, nu_tau, tau_daughters, &phi2, &theta2);


    //  Determine decay channel and then calculate polarimetric vector HH
    HHp = calculateHH(tau_pdgid, tau_daughters, phi2, theta2);

    DEBUG
    (
      cout<<tau_pdgid<<" -> ";
      for(unsigned int i=0;i<tau_daughters.size();i++) cout<<tau_daughters[i].pdgid()<<" ";
      cout<<" (HHp: "<<HHp[0]<<" "<<HHp[1]<<" "<<HHp[2]<<" "<<HHp[3]<<") ";
      cout<<endl;
    )

    WTamplitP = WTamplit;
  } // end of tau+

  // Second iteration is for tau minus, so the 'nu_tau' is tau minus
  if(sp_tau1.pdgid() == 15 )
  {
    sp_tau           = sp_tau1;
    sp_nu_tau        = sp_tau2;
    sp_tau_daughters = sp_tau1_daughters;
  }
  else
  {
    sp_tau           = sp_tau2;
    sp_nu_tau        = sp_tau1;
    sp_tau_daughters = sp_tau2_daughters;
  }
  
  // We use this to separate namespace for tau+ and tau-
  if(true)
  {
    // Create Particles from SimpleParticles
    Particle X     (      sp_X.px(),      sp_X.py(),      sp_X.pz(),      sp_X.e(),      sp_X.pdgid() );
    Particle tau   (    sp_tau.px(),    sp_tau.py(),    sp_tau.pz(),    sp_tau.e(),    sp_tau.pdgid() );
    Particle nu_tau( sp_nu_tau.px(), sp_nu_tau.py(), sp_nu_tau.pz(), sp_nu_tau.e(), sp_nu_tau.pdgid() );

    vector<Particle> tau_daughters;

    // tau pdgid
    int tau_pdgid = sp_tau.pdgid();

    // Create list of tau daughters
    for(unsigned int i=0; i<sp_tau_daughters.size(); i++)
    {
      Particle pp(sp_tau_daughters[i].px(),
                  sp_tau_daughters[i].py(),
                  sp_tau_daughters[i].pz(),
                  sp_tau_daughters[i].e(),
                  sp_tau_daughters[i].pdgid() );

      tau_daughters.push_back(pp);
    }

    double phi2 = 0.0, theta2 = 0.0;


    //  Move decay kinematics first to tau rest frame  with z axis pointing along nu_tau direction
    //  later rotate again to have neutrino from tau decay along z axis: angles phi2, theta2
    prepareKinematicForHH   (tau, nu_tau, tau_daughters, &phi2, &theta2);


    //  Determine decay channel and then calculate polarimetric vector HH
    HHm = calculateHH(tau_pdgid, tau_daughters, phi2, theta2);

    DEBUG
    (
      cout<<tau_pdgid<<" -> ";
      for(unsigned int i=0;i<tau_daughters.size();i++) cout<<tau_daughters[i].pdgid()<<" ";
      cout<<" (HHm: "<<HHm[0]<<" "<<HHm[1]<<" "<<HHm[2]<<" "<<HHm[3]<<") ";
      cout<<endl;
    )

    WTamplitM = WTamplit; 
  } // end of tau-

  double sign = 1.0; // longitudinal spin correlation for gamma* or even Z/gamma* (just that this is vector)
  if(sp_X.pdgid() == 25) { sign=-1.0; } 
  if(sp_X.pdgid() == 36) { sign=-1.0; }
  if(sp_X.pdgid() ==553) { sign=-1.0; }  // upsilon(1s) can be treated like Higgs
  double WT = 0.0;
  Polari = 0.0;
  
  if(sign == -1.0) // Case of Higgs 
  {
    double S = sp_X.e()*sp_X.e() - sp_X.px()*sp_X.px() - sp_X.py()*sp_X.py() - sp_X.pz()*sp_X.pz();
    IfHiggs=true;
    double pol = getLongitudinalPolarization(S, sp_tau, sp_nu_tau);

    if(nonSM2==1)
      {
     
      double corrX2;
      double polX2;

      // NOTE: in this case, sp_nu_tau is the 2nd tau
      //      nonSMHcorrPol(S, sp_tau, sp_nu_tau, &corrX2, &polX2); // for future use
      //                                                          WARNING: may be for polX2*HHm[2] we need to fix sign!
      polX2=pol;
      corrX2=-sign; // if X2 is of spin-2, spin correlation like for Z
      
      WT = 1.0+corrX2*HHp[2]*HHm[2]+polX2*HHp[2]+polX2*HHm[2] + RzXX*HHp[0]*HHm[0] + RzYY*HHp[1]*HHm[1] + RzXY*HHp[0]*HHm[1] + RzYX*HHp[1]*HHm[0];
      // we separate cross section into helicity +- and -+ parts
      double RRR = Tauola::randomDouble();  
      Polari=1.0;
      if (RRR<(1.0+polX2)*(1.0+corrX2*HHp[2]*HHm[2]+HHp[2]+HHm[2])/(2.0+2.0*corrX2*HHp[2]*HHm[2]+2.0*polX2*HHp[2]+2.0*polX2*HHm[2])) Polari=-1.0;    }
    else
    {
      //WT = 1.0+sign*HHp[2]*HHm[2];     // [2] means 'pz' component
      WT = 1.0 + sign*HHp[2]*HHm[2] + RXX*HHp[0]*HHm[0] + RYY*HHp[1]*HHm[1] + RXY*HHp[0]*HHm[1] + RYX*HHp[1]*HHm[0];
      // we separate cross section into helicity +- and -+ parts
      double RRR = Tauola::randomDouble();  
      Polari=1.0;
      if (RRR<(1.0+sign*HHp[2]*HHm[2]+HHp[2]-HHm[2])/(2.0+2.0*sign*HHp[2]*HHm[2])) Polari=-1.0;
    }
  }
  else   // Case of Z/gamma*
  { 

    double S = sp_X.e()*sp_X.e() - sp_X.px()*sp_X.px() - sp_X.py()*sp_X.py() - sp_X.pz()*sp_X.pz();

    // Get Z polarization
    // ( Variable names are misleading! sp_tau is tau+ and sp_nu_tau is tau- )

    IfHiggs=false;
    double pol = getLongitudinalPolarization(S, sp_tau, sp_nu_tau);

    // we separate cross section into helicity ++ and -- parts
    double RRR = Tauola::randomDouble();  
    WT = 1.0+sign*HHp[2]*HHm[2]+pol*HHp[2]+pol*HHm[2] + RzXX*R11*HHp[0]*HHm[0] - RzYY*R22*HHp[1]*HHm[1] + RzXY*R12*HHp[0]*HHm[1] + RzYX*R21*HHp[1]*HHm[0]; 
    // we need extra factor for wt which is
    //     F=PLWEIGHT(IDE,IDF,SVAR,COSTHE,1)


    if(Ipol==2) WT = WT/(1.0+sign*HHp[2]*HHm[2]); // extra term in wt
    // to correct sample when only corr. are in, but no pol.
    if(Ipol==3)
    {
      double polp1 = plzap2(11,sp_tau.pdgid(),S,0.0);
      double pol1 =(2*(1-polp1)-1) ;
      WT = WT/(1.0+sign*HHp[2]*HHm[2]+pol1*HHp[2]+pol1*HHm[2]); // extra term in wt
      // to correct sample when  corr. are in,  pol. is in, but angular
      // dependence of pol is missing.
    } 

      Polari=1.0;
      if (RRR<(1.0+pol)*(1.0+sign*HHp[2]*HHm[2]+HHp[2]+HHm[2])/(2.0+2.0*sign*HHp[2]*HHm[2]+2.0*pol*HHp[2]+2.0*pol*HHm[2])) Polari=-1.0; 
  }

  // Print out some info about the channel
  DEBUG( cout<<" WT: "<<WT<<endl; )

  if (WT<0.0) {
    printf("WT is: %13.10f. Setting WT = 0.0\n",WT);
    WT = 0.0; // SwB:23Feb2013
   }

  if (WT>4.0) {
    printf("WT is: %13.10f. Setting WT = 4.0\n",WT);
    WT = 4.0; // SwB:23Feb2013
  }

  if( WT>4.0 || WT<0.0)
  {
    cout<<"ERROR: Z/gamma* or H, and WT not in range [0,4]."<<endl;
    exit(-1);
  }

  if (sign==-1.0) {
    if (WT>2.0) {
      WT = 2.0; // SwB:26Feb2013
      cout << "Setting WT to be 2.0" << endl;
    }
  }

  if( sign==-1.0 && (WT>2.0 || WT<0.0) )
  {
    cout<<"ERROR: H and WT not in range [0,2]."<<endl;
    exit(-1);
  }

  delete[] HHp;
  delete[] HHm;
  
  return WT;
}

/*******************************************************************************
  Prepare kinematics for HH calculation
  
  Boost particles to effective bozon rest frame, and rotate them so that tau is on Z axis.
  Then rotate again with theta2 phi2 so neutrino from tau decay is along Z.
*******************************************************************************/
void prepareKinematicForHH(Particle &tau, Particle &nu_tau, vector<Particle> &tau_daughters, double *phi2, double *theta2)
{
  Particle P_QQ( tau.px()+nu_tau.px(), tau.py()+nu_tau.py(), tau.pz()+nu_tau.pz(), tau.e()+nu_tau.e(), 0 );

  //cout<<endl<<"START: "<<endl;
  //print(P_QQ,nu_tau,tau,tau_daughters);
  
  // 1) boost tau, nu_tau and tau daughters to rest frame of P_QQ

  tau.boostToRestFrame(P_QQ);
  nu_tau.boostToRestFrame(P_QQ);

  for(unsigned int i=0; i<tau_daughters.size();i++)
    tau_daughters[i].boostToRestFrame(P_QQ);

  //cout<<endl<<"AFTER 1: "<<endl;
  //print(P_QQ,nu_tau,tau,tau_daughters);

  // 2) Rotate tau, nu_tau~, tau daughters to frame where tau is along Z
  //    We set accompanying neutino in direction of Z+

  double phi = tau.getAnglePhi();

  tau.rotateXY(-phi);

  double theta   = tau.getAngleTheta();

  tau.rotateXZ(M_PI-theta);

  nu_tau.rotateXY(-phi  );
  nu_tau.rotateXZ(M_PI-theta);

  for(unsigned int i=0; i<tau_daughters.size();i++)
  {
    tau_daughters[i].rotateXY(-phi  );
    tau_daughters[i].rotateXZ(M_PI-theta);
  }

  //cout<<endl<<"AFTER 2: "<<endl;
  //print(P_QQ,nu_tau,tau,tau_daughters);

  // 3) boost tau_daughters along Z to rest frame of tau

  for(unsigned int i=0; i<tau_daughters.size();i++)
    tau_daughters[i].boostAlongZ(-tau.pz(),tau.e());

  //cout<<endl<<"AFTER 3: "<<endl;
  //print(P_QQ,nu_tau,tau,tau_daughters);

  // 4) Now rotate tau daughters second time
  //    so that nu_tau (from tau daughters list) is on Z axis

  *phi2     = tau_daughters[0].getAnglePhi();

  tau_daughters[0].rotateXY( -(*phi2)   );

  *theta2   = tau_daughters[0].getAngleTheta();

  tau_daughters[0].rotateXZ( -(*theta2) );

  for(unsigned int i=1; i<tau_daughters.size();i++)
  {
    tau_daughters[i].rotateXY( -(*phi2)   );
    tau_daughters[i].rotateXZ( -(*theta2) );
  }

  //cout<<endl<<"AFTER 4: "<<endl;
  //print(P_QQ,nu_tau,tau,tau_daughters);
}

/*******************************************************************************
  Calculate polarization vector.

  We use FORTRAN metdods to calculate HH.
  First decide what is the channel. After that, 4-vectors
  are moved to tau rest frame of tau.
  Polarimetric vector HH is then rotated using angles phi and theta.

  Order of the particles does not matter. 
*******************************************************************************/
double* calculateHH(int tau_pdgid, vector<Particle> &tau_daughters, double phi, double theta)
{
  int    channel = 0;
  double *HH     = new double[4];

  HH[0]=HH[1]=HH[2]=HH[3]=0.0;

  vector<int>  pdgid;

  // Create list of tau daughters
  for(unsigned int i=0; i<tau_daughters.size(); i++)
    pdgid.push_back( tau_daughters[i].pdgid() );

  // 17.04.2014: If Tauola++ is used for generation, 
  // KTOM  may be changed to 11 at the time of storing decay to event record.
  // For Tauola++ KTOM is later of no use.
  // For TauSpinner processing, it should always be 1
  // This was the problem if Tauola++ generation and TauSpinner were used simultaneously.
  jaki_.ktom = 1;

  // tau^- --> pi^- nu_tau
  // tau^+ --> pi^+ anti_nu_tau
  // tau^- --> K^-  nu_tau
  // tau^+ --> K^+  anti_nu_tau
  if( pdgid.size()==2 &&
      (
        ( tau_pdgid== 15 && channelMatch(tau_daughters, 16,-211) ) ||
        ( tau_pdgid==-15 && channelMatch(tau_daughters,-16, 211) ) ||
        ( tau_pdgid== 15 && channelMatch(tau_daughters, 16,-321) ) ||
        ( tau_pdgid==-15 && channelMatch(tau_daughters,-16, 321) )
      )
    ) {
    channel = 3;
    if(abs(pdgid[1])==321) channel = 6;
    DEBUG( cout<<"Channel "<<channel<<"  : "; )
    //        PXQ=AMTAU*EPI
    //        PXN=AMTAU*ENU
    //        QXN=PPI(4)*PNU(4)-PPI(1)*PNU(1)-PPI(2)*PNU(2)-PPI(3)*PNU(3)

    //        BRAK=(GV**2+GA**2)*(2*PXQ*QXN-AMPI**2*PXN)
    //        HV(I)=-ISGN*2*GA*GV*AMTAU*(2*PPI(I)*QXN-PNU(I)*AMPI**2)/BRAK

    const double AMTAU = 1.777;
    double AMPI  = sqrt(tau_daughters[1].e() *tau_daughters[1].e()
                       -tau_daughters[1].px()*tau_daughters[1].px()
                       -tau_daughters[1].py()*tau_daughters[1].py()
                       -tau_daughters[1].pz()*tau_daughters[1].pz());

    double PXQ=AMTAU*tau_daughters[1].e();
    double PXN=AMTAU*tau_daughters[0].e();
    double QXN=tau_daughters[1].e()*tau_daughters[0].e()-tau_daughters[1].px()*tau_daughters[0].px()-tau_daughters[1].py()*tau_daughters[0].py()-tau_daughters[1].pz()*tau_daughters[0].pz();
    double BRAK=(2*PXQ*QXN-AMPI*AMPI*PXN);

    WTamplit = (1.16637E-5)*(1.16637E-5)*BRAK/2.;//AMPLIT=(GFERMI)**2*BRAK/2.
    HH[0] = AMTAU*(2*tau_daughters[1].px()*QXN-tau_daughters[0].px()*AMPI*AMPI)/BRAK;
    HH[1] = AMTAU*(2*tau_daughters[1].py()*QXN-tau_daughters[0].py()*AMPI*AMPI)/BRAK;
    HH[2] = AMTAU*(2*tau_daughters[1].pz()*QXN-tau_daughters[0].pz()*AMPI*AMPI)/BRAK;
    HH[3] = 1.0;
  }

  // tau^- --> pi^- pi^0 nu_tau
  // tau^+ --> pi^+ pi^0 anti_nu_tau
  else if( pdgid.size()==3 &&
           (
             ( tau_pdgid== 15 && channelMatch(tau_daughters, 16,-211, 111) ) ||
             ( tau_pdgid==-15 && channelMatch(tau_daughters,-16, 211, 111) )
           )
         ) {

    channel = 4;
    DEBUG( cout<<"Channel "<<channel<<"  : "; )
    //      PRODPQ=PT(4)*QQ(4)
    //      PRODNQ=PN(4)*QQ(4)-PN(1)*QQ(1)-PN(2)*QQ(2)-PN(3)*QQ(3)
    //      PRODPN=PT(4)*PN(4)
    //      BRAK=(GV**2+GA**2)*(2*PRODPQ*PRODNQ-PRODPN*QQ2)
    //      HV(I)=2*GV*GA*AMTAU*(2*PRODNQ*QQ(I)-QQ2*PN(I))/BRAK

    const double AMTAU = 1.777;

    int   MNUM = 0;
    float PT[4]   = { 0.0, 0.0, 0.0, (float)AMTAU };
    float PN[4]   = { (float)tau_daughters[0].px(), (float)tau_daughters[0].py(), (float)tau_daughters[0].pz(), (float)tau_daughters[0].e() };
    float PIM1[4] = { (float)tau_daughters[1].px(), (float)tau_daughters[1].py(), (float)tau_daughters[1].pz(), (float)tau_daughters[1].e() };
    float PIM2[4] = { (float)tau_daughters[2].px(), (float)tau_daughters[2].py(), (float)tau_daughters[2].pz(), (float)tau_daughters[2].e() };
    float AMPLIT = 0.0;
    float HV[4]  = { 0.0 };

    dam2pi_( &MNUM, PT, PN, PIM1, PIM2, &AMPLIT, HV );

    WTamplit = AMPLIT;
    HH[0] = -HV[0];
    HH[1] = -HV[1];
    HH[2] = -HV[2];
    HH[3] =  HV[3];
  }

  // tau^- --> K^-  pi^0 nu_tau
  // tau^+ --> K^+  pi^0 anti_nu_tau
  // tau^- --> pi^- K_S0 nu_tau
  // tau^+ --> pi^+ K_S0 anti_nu_tau
  // tau^- --> pi^- K_L0 nu_tau
  // tau^+ --> pi^+ K_L0 anti_nu_tau
  else if( pdgid.size()==3 &&
           (
             ( tau_pdgid== 15 && channelMatch(tau_daughters, 16,-211, 130) ) ||
             ( tau_pdgid==-15 && channelMatch(tau_daughters,-16, 211, 130) ) ||
             ( tau_pdgid== 15 && channelMatch(tau_daughters, 16,-211, 310) ) ||
             ( tau_pdgid==-15 && channelMatch(tau_daughters,-16, 211, 310) ) ||
             ( tau_pdgid== 15 && channelMatch(tau_daughters, 16,-321, 111) ) ||
             ( tau_pdgid==-15 && channelMatch(tau_daughters,-16, 321, 111) )
           )
         ) {

    channel = 7;
    DEBUG( cout<<"Channel "<<channel<<"  : "; )
    //      PRODPQ=PT(4)*QQ(4)
    //      PRODNQ=PN(4)*QQ(4)-PN(1)*QQ(1)-PN(2)*QQ(2)-PN(3)*QQ(3)
    //      PRODPN=PT(4)*PN(4)
    //      BRAK=(GV**2+GA**2)*(2*PRODPQ*PRODNQ-PRODPN*QQ2)
    //      HV(I)=2*GV*GA*AMTAU*(2*PRODNQ*QQ(I)-QQ2*PN(I))/BRAK

    const double AMTAU = 1.777;

    double QQ[4];
    QQ[0]=tau_daughters[1].e() -tau_daughters[2].e() ;
      QQ[1]=tau_daughters[1].px()-tau_daughters[2].px();
      QQ[2]=tau_daughters[1].py()-tau_daughters[2].py();
      QQ[3]=tau_daughters[1].pz()-tau_daughters[2].pz();

    double PKS[4];
    PKS[0]=tau_daughters[1].e() +tau_daughters[2].e() ;
      PKS[1]=tau_daughters[1].px()+tau_daughters[2].px();
      PKS[2]=tau_daughters[1].py()+tau_daughters[2].py();
      PKS[3]=tau_daughters[1].pz()+tau_daughters[2].pz();

       // orthogonalization of QQ wr. to PKS
    double PKSD=PKS[0]*PKS[0]-PKS[1]*PKS[1]-PKS[2]*PKS[2]-PKS[3]*PKS[3];
    double QQPKS=QQ[0]*PKS[0]-QQ[1]*PKS[1]-QQ[2]*PKS[2]-QQ[3]*PKS[3];

    QQ[0]=QQ[0]-PKS[0]*QQPKS/PKSD;
      QQ[1]=QQ[1]-PKS[1]*QQPKS/PKSD;
      QQ[2]=QQ[2]-PKS[2]*QQPKS/PKSD;
      QQ[3]=QQ[3]-PKS[3]*QQPKS/PKSD;

    double PRODPQ=AMTAU*QQ[0];
    double PRODNQ=tau_daughters[0].e() *QQ[0]
                 -tau_daughters[0].px()*QQ[1]
                 -tau_daughters[0].py()*QQ[2]
                 -tau_daughters[0].pz()*QQ[3];
    double PRODPN=AMTAU*tau_daughters[0].e();
    double QQ2   =QQ[0]*QQ[0]-QQ[1]*QQ[1]-QQ[2]*QQ[2]-QQ[3]*QQ[3];

    double BRAK=(2*PRODPQ*PRODNQ-PRODPN*QQ2);

    WTamplit = (1.16637E-5)*(1.16637E-5)*BRAK/2.;//AMPLIT=(GFERMI)**2*BRAK/2.
    HH[0]=AMTAU*(2*PRODNQ*QQ[1]-QQ2*tau_daughters[0].px())/BRAK;
    HH[1]=AMTAU*(2*PRODNQ*QQ[2]-QQ2*tau_daughters[0].py())/BRAK;
    HH[2]=AMTAU*(2*PRODNQ*QQ[3]-QQ2*tau_daughters[0].pz())/BRAK;
    HH[3]=1.0;
  }

  // tau^- --> e^- anti_nu_e      nu_tau
  // tau^+ --> e^+      nu_e anti_nu_tau
  else if( pdgid.size()==3 &&
           (
             ( tau_pdgid== 15 && channelMatch(tau_daughters, 16, 11,-12) ) ||
             ( tau_pdgid==-15 && channelMatch(tau_daughters,-16,-11, 12) )
           )
         ) {
    DEBUG( cout<<"Channel 1  : "; )
    channel = 1;
    //  ITDKRC=0,XK0DEC=0.01 XK[4]={0},XA[4] nu_e, QP[4] e, XN[4] neutrino tauowe, AMPLIT, HH[4]
    //      SUBROUTINE DAMPRY(ITDKRC,XK0DEC,XK,XA,QP,XN,AMPLIT,HV)

    int    ITDKRC = 0;
    double XK0DEC = 0.01;
    double XK[4] = { 0.0 };
    double XA[4] = { tau_daughters[2].px(), tau_daughters[2].py(), tau_daughters[2].pz(), tau_daughters[2].e() };
    double QP[4] = { tau_daughters[1].px(), tau_daughters[1].py(), tau_daughters[1].pz(), tau_daughters[1].e() };
    double XN[4] = { tau_daughters[0].px(), tau_daughters[0].py(), tau_daughters[0].pz(), tau_daughters[0].e() };
    double AMPLIT = 0.0;
    double HV[4] = { 0.0 };

    // Fix 4-momenta of electron and electron neutrino
    // Since electrons have small mass, they are prone to rounding errors
    QP[3] = sqrt( QP[0]*QP[0] + QP[1]*QP[1] + QP[2]*QP[2] + 0.511e-3*0.511e-3);
    XA[3] = sqrt( XA[0]*XA[0] + XA[1]*XA[1] + XA[2]*XA[2] );

    dampry_( &ITDKRC, &XK0DEC, XK, XA, QP, XN, &AMPLIT, HV );

    WTamplit = AMPLIT;
    HH[0] = -HV[0];
    HH[1] = -HV[1];
    HH[2] = -HV[2];
    HH[3] =  HV[3];
  }

  // tau^- --> e^- anti_nu_e      nu_tau + gamma
  // tau^+ --> e^+      nu_e anti_nu_tau + gamma
  else if( pdgid.size()==4 &&
           (
             ( tau_pdgid== 15 && channelMatch(tau_daughters, 16, 11,-12, 22) ) ||
             ( tau_pdgid==-15 && channelMatch(tau_daughters,-16,-11, 12, 22) )
           )
         ) {
    DEBUG( cout<<"Channel 1b : "; )
    channel = 1;
    //  ITDKRC=0,XK0DEC=0.01 XK[4]  gamma, XA[4] nu_e, QP[4] e, XN[4] neutrino tau , AMPLIT, HH[4]
    //      SUBROUTINE DAMPRY(ITDKRC,XK0DEC,XK,XA,QP,XN,AMPLIT,HV)

    int    ITDKRC = 1;
    double XK0DEC = 0.01;
    double XK[4] = { tau_daughters[3].px(), tau_daughters[3].py(), tau_daughters[3].pz(), tau_daughters[3].e() };
    double XA[4] = { tau_daughters[2].px(), tau_daughters[2].py(), tau_daughters[2].pz(), tau_daughters[2].e() };
    double QP[4] = { tau_daughters[1].px(), tau_daughters[1].py(), tau_daughters[1].pz(), tau_daughters[1].e() };
    double XN[4] = { tau_daughters[0].px(), tau_daughters[0].py(), tau_daughters[0].pz(), tau_daughters[0].e() };
    double AMPLIT = 0.0;
    double HV[4] = { 0.0 };
    
    // Fix 4-momenta of electron and electron neutrino and photon
    // Since electrons have small mass, they are prone to rounding errors
    QP[3] = sqrt( QP[0]*QP[0] + QP[1]*QP[1] + QP[2]*QP[2] + 0.511e-3*0.511e-3);
    XA[3] = sqrt( XA[0]*XA[0] + XA[1]*XA[1] + XA[2]*XA[2] );
    XK[3] = sqrt( XK[0]*XK[0] + XK[1]*XK[1] + XK[2]*XK[2] );
    // XK0DEC must be smaller in TauSpinner  than what was used in generation. We do not use virt. corr anyway.
    if(XK0DEC > XK[3]/(XK[3]+XA[3]+QP[3]+XN[3]))  XK0DEC=0.5*XK[3]/(XK[3]+XA[3]+QP[3]+XN[3]);
    
    dampry_( &ITDKRC, &XK0DEC, XK, XA, QP, XN, &AMPLIT, HV );

    WTamplit = AMPLIT;
    HH[0] = -HV[0];
    HH[1] = -HV[1];
    HH[2] = -HV[2];
    HH[3] =  HV[3];
  }

  // tau^- --> mu^- antui_nu_mu      nu_tau
  // tau^+ --> mu^+       nu_mu anti_nu_tau
  else if( pdgid.size()==3 &&
           (
             ( tau_pdgid== 15 && channelMatch(tau_daughters, 16, 13,-14) ) ||
             ( tau_pdgid==-15 && channelMatch(tau_daughters,-16,-13, 14) )
           )
         ) {

    DEBUG( cout<<"Channel 2  : "; )
    channel = 2;
    //  ITDKRC=0,XK0DEC=0.01 XK[4]={0},XA[4] nu_mu, QP[4] mu, XN[4] neutrino tauowe, AMPLIT, HH[4]
    //      SUBROUTINE DAMPRY(ITDKRC,XK0DEC,XK,XA,QP,XN,AMPLIT,HV)

    int    ITDKRC = 0;
    double XK0DEC = 0.01;
    double XK[4] = { 0.0 };
    double XA[4] = { tau_daughters[2].px(), tau_daughters[2].py(), tau_daughters[2].pz(), tau_daughters[2].e() };
    double QP[4] = { tau_daughters[1].px(), tau_daughters[1].py(), tau_daughters[1].pz(), tau_daughters[1].e() };
    double XN[4] = { tau_daughters[0].px(), tau_daughters[0].py(), tau_daughters[0].pz(), tau_daughters[0].e() };
    double AMPLIT = 0.0;
    double HV[4] = { 0.0 };

    // Fix 4-momenta of muon and muon neutrino
    // Since muon have small mass, they are prone to rounding errors
    QP[3] = sqrt( QP[0]*QP[0] + QP[1]*QP[1] + QP[2]*QP[2] + 0.105659*0.105659);
    XA[3] = sqrt( XA[0]*XA[0] + XA[1]*XA[1] + XA[2]*XA[2] );
    
    dampry_( &ITDKRC, &XK0DEC, XK, XA, QP, XN, &AMPLIT, HV );

    WTamplit = AMPLIT;
    HH[0] = -HV[0];
    HH[1] = -HV[1];
    HH[2] = -HV[2];
    HH[3] =  HV[3];
  }

  // tau^- --> mu^- antui_nu_mu      nu_tau + gamma
  // tau^+ --> mu^+       nu_mu anti_nu_tau + gamma
  else if( pdgid.size()==4 &&
           (
             ( tau_pdgid== 15 && channelMatch(tau_daughters, 16, 13,-14, 22) ) ||
             ( tau_pdgid==-15 && channelMatch(tau_daughters,-16,-13, 14, 22) )
           )
         ) {

    DEBUG( cout<<"Channel 2b : "; )
    channel = 2;
    //  ITDKRC=0,XK0DEC=0.01 XK[4]  gamma, XA[4] nu_mu, QP[4] mu, XN[4] neutrino tau, AMPLIT, HH[4]
    //      SUBROUTINE DAMPRY(ITDKRC,XK0DEC,XK,XA,QP,XN,AMPLIT,HV)

    int    ITDKRC = 1;
    double XK0DEC = 0.01;
    double XK[4] = { tau_daughters[3].px(), tau_daughters[3].py(), tau_daughters[3].pz(), tau_daughters[3].e() };
    double XA[4] = { tau_daughters[2].px(), tau_daughters[2].py(), tau_daughters[2].pz(), tau_daughters[2].e() };
    double QP[4] = { tau_daughters[1].px(), tau_daughters[1].py(), tau_daughters[1].pz(), tau_daughters[1].e() };
    double XN[4] = { tau_daughters[0].px(), tau_daughters[0].py(), tau_daughters[0].pz(), tau_daughters[0].e() };
    double AMPLIT = 0.0;
    double HV[4] = { 0.0 };

    // Fix 4-momenta of muon and muon neutrino and photon
    // Since muons have small mass, they are prone to rounding errors
    QP[3] = sqrt( QP[0]*QP[0] + QP[1]*QP[1] + QP[2]*QP[2] + 0.105659*0.105659);
    XA[3] = sqrt( XA[0]*XA[0] + XA[1]*XA[1] + XA[2]*XA[2] );
    XK[3] = sqrt( XK[0]*XK[0] + XK[1]*XK[1] + XK[2]*XK[2] );
    // XK0DEC must be smaller in TauSpinner  than what was used in generation. We do not use virt. corr anyway.
    if(XK0DEC > XK[3]/(XK[3]+XA[3]+QP[3]+XN[3]))  XK0DEC=0.5*XK[3]/(XK[3]+XA[3]+QP[3]+XN[3]);


    dampry_( &ITDKRC, &XK0DEC, XK, XA, QP, XN, &AMPLIT, HV );

    WTamplit = AMPLIT;
    HH[0] = -HV[0];
    HH[1] = -HV[1];
    HH[2] = -HV[2];
    HH[3] =  HV[3];
  }

  // tau^- --> pi^- pi^0 pi^0 nu_tau
  // tau^+ --> pi^+ pi^0 pi^0 anti_nu_tau
  else if( pdgid.size()==4 &&
           (
             ( tau_pdgid== 15 && channelMatch(tau_daughters, 16, 111, 111,-211) ) ||
             ( tau_pdgid==-15 && channelMatch(tau_daughters,-16, 111, 111, 211) )
           )
         ) {
    DEBUG( cout<<"Channel 5  : "; )
    channel = 5;
    //  MNUM=0, PT[4] tau, PN[4] neutrino, pi0[4], pi0[4], pi[4], AMPLIT, HH[4]
    //        CALL DAMPPK(MNUM,PT,PN,PIM1,PIM2,PIPL,AMPLIT,HH)

    const double AMTAU = 1.777;
    int   MNUM = 0;
    float PT[4]   = { 0.0, 0.0, 0.0, (float)AMTAU };
    float PN[4]   = { (float)tau_daughters[0].px(), (float)tau_daughters[0].py(), (float)tau_daughters[0].pz(), (float)tau_daughters[0].e() };
    float PIM1[4] = { (float)tau_daughters[1].px(), (float)tau_daughters[1].py(), (float)tau_daughters[1].pz(), (float)tau_daughters[1].e() };
    float PIM2[4] = { (float)tau_daughters[2].px(), (float)tau_daughters[2].py(), (float)tau_daughters[2].pz(), (float)tau_daughters[2].e() };
    float PIPL[4] = { (float)tau_daughters[3].px(), (float)tau_daughters[3].py(), (float)tau_daughters[3].pz(), (float)tau_daughters[3].e() };
    float AMPLIT = 0.0;
    float HV[4]  = { 0.0 };

    // For RChL currents one needs to define 3-pi sub-channel used
    chanopt_.JJ=2;

    damppk_( &MNUM, PT, PN, PIM1, PIM2, PIPL, &AMPLIT, HV );

    WTamplit = AMPLIT;
    HH[0] = -HV[0];
    HH[1] = -HV[1];
    HH[2] = -HV[2];
    HH[3] =  HV[3];
  }

  // tau^- --> pi^+ pi^- pi^- nu_tau
  // tau^+ --> pi^- pi^+ pi^+ anti_nu_tau
  else if( pdgid.size()==4 &&
           (
             ( tau_pdgid== 15 && channelMatch(tau_daughters, 16,-211,-211, 211) ) ||
             ( tau_pdgid==-15 && channelMatch(tau_daughters,-16, 211, 211,-211) )
           )
         ) {
    DEBUG( cout<<"Channel 5  : "; )
    channel = 5;
    //  MNUM=0, PT[4] tau, PN[4] neutrino, pi[4], pi[4], pi[4], AMPLIT, HH[4]
    //        CALL DAMPPK(MNUM,PT,PN,PIM1,PIM2,PIPL,AMPLIT,HH)

    const double AMTAU = 1.777;
    int   MNUM = 0;
    float PT[4]   = { 0.0, 0.0, 0.0, (float)AMTAU };
    float PN[4]   = { (float)tau_daughters[0].px(), (float)tau_daughters[0].py(), (float)tau_daughters[0].pz(), (float)tau_daughters[0].e() };
    float PIM1[4] = { (float)tau_daughters[1].px(), (float)tau_daughters[1].py(), (float)tau_daughters[1].pz(), (float)tau_daughters[1].e() };
    float PIM2[4] = { (float)tau_daughters[2].px(), (float)tau_daughters[2].py(), (float)tau_daughters[2].pz(), (float)tau_daughters[2].e() };
    float PIPL[4] = { (float)tau_daughters[3].px(), (float)tau_daughters[3].py(), (float)tau_daughters[3].pz(), (float)tau_daughters[3].e() };
    float AMPLIT = 0.0;
    float HV[4] = { 0.0 };

    // For RChL currents one needs to define 3-pi sub-channel used
    chanopt_.JJ=1;

    damppk_( &MNUM, PT, PN, PIM1, PIM2, PIPL, &AMPLIT, HV );

    WTamplit = AMPLIT;
    HH[0] = -HV[0];
    HH[1] = -HV[1];
    HH[2] = -HV[2];
    HH[3] =  HV[3];
  }
  // tau^- --> pi^+ pi^+ pi^0 pi^- nu_tau
  // tau^+ --> pi^- pi^- pi^0 pi^+ anti_nu_tau
  else if( pdgid.size()==5 &&
           (
             ( tau_pdgid== 15 && channelMatch(tau_daughters, 16,-211,-211, 211, 111) ) ||
             ( tau_pdgid==-15 && channelMatch(tau_daughters,-16, 211, 211,-211, 111) )
           )
         ) {
    DEBUG( cout<<"Channel 8  : "; )
    channel = 8;

    const double AMTAU = 1.777;
    int   MNUM = 1;
    float PT[4]   = { 0.0, 0.0, 0.0, (float)AMTAU };
    float PN[4]   = { (float)tau_daughters[0].px(), (float)tau_daughters[0].py(), (float)tau_daughters[0].pz(), (float)tau_daughters[0].e() };
    float PIM1[4] = { (float)tau_daughters[1].px(), (float)tau_daughters[1].py(), (float)tau_daughters[1].pz(), (float)tau_daughters[1].e() };
    float PIM2[4] = { (float)tau_daughters[2].px(), (float)tau_daughters[2].py(), (float)tau_daughters[2].pz(), (float)tau_daughters[2].e() };
    float PIZ [4] = { (float)tau_daughters[4].px(), (float)tau_daughters[4].py(), (float)tau_daughters[4].pz(), (float)tau_daughters[4].e() };
    float PIPL[4] = { (float)tau_daughters[3].px(), (float)tau_daughters[3].py(), (float)tau_daughters[3].pz(), (float)tau_daughters[3].e() };
    float AMPLIT = 0.0;
    float HV[4] = { 0.0 };

    dam4pi_( &MNUM, PT, PN, PIM1, PIM2, PIZ, PIPL, &AMPLIT, HV );

    WTamplit = AMPLIT;
    HH[0] = -HV[0];
    HH[1] = -HV[1];
    HH[2] = -HV[2];
    HH[3] =  HV[3];
  }
  // tau^- --> pi^0 pi^0 pi^0 pi^- nu_tau
  // tau^+ --> pi^0 pi^0 pi^0 pi^+ anti_nu_tau
  else if( pdgid.size()==5 &&
           (
             ( tau_pdgid== 15 && channelMatch(tau_daughters, 16, 111, 111, 111,-211) ) ||
             ( tau_pdgid==-15 && channelMatch(tau_daughters,-16, 111, 111, 111, 211) )
           )
         ) {
    DEBUG( cout<<"Channel 9  : "; )
    channel = 9;

    const double AMTAU = 1.777;
    int   MNUM = 2;
    float PT[4]   = { 0.0, 0.0, 0.0, (float)AMTAU };
    float PN[4]   = { (float)tau_daughters[0].px(), (float)tau_daughters[0].py(), (float)tau_daughters[0].pz(), (float)tau_daughters[0].e() };
    float PIM1[4] = { (float)tau_daughters[1].px(), (float)tau_daughters[1].py(), (float)tau_daughters[1].pz(), (float)tau_daughters[1].e() };
    float PIM2[4] = { (float)tau_daughters[2].px(), (float)tau_daughters[2].py(), (float)tau_daughters[2].pz(), (float)tau_daughters[2].e() };
    float PIZ [4] = { (float)tau_daughters[3].px(), (float)tau_daughters[3].py(), (float)tau_daughters[3].pz(), (float)tau_daughters[3].e() };
    float PIPL[4] = { (float)tau_daughters[4].px(), (float)tau_daughters[4].py(), (float)tau_daughters[4].pz(), (float)tau_daughters[4].e() };
    float AMPLIT = 0.0;
    float HV[4] = { 0.0 };

    dam4pi_( &MNUM, PT, PN, PIM1, PIM2, PIZ, PIPL, &AMPLIT, HV );

    WTamplit = AMPLIT;
    HH[0] = -HV[0];
    HH[1] = -HV[1];
    HH[2] = -HV[2];
    HH[3] =  HV[3];
  }
  else {

    DEBUG( cout<<tau_daughters.size()<<"-part  ???: "; )

  }

  // Now rotate vector HH using angles phi and theta
  Particle HHbuf(HH[0], HH[1], HH[2], HH[3], 0);
  
  HHbuf.rotateXZ(theta);
  HHbuf.rotateXY(phi);
  
  HH[0] = HHbuf.px();
  HH[1] = HHbuf.py();
  HH[2] = HHbuf.pz();
  HH[3] = HHbuf.e ();

  return HH;
}

/*******************************************************************************
 Get Z polarization
 
 Returns longitudinal polarization in Z/gamma* -> tau+ tau- averaged over
 incoming configurations
 S: invariant mass^2 of the bozon
*******************************************************************************/
double getLongitudinalPolarization(double S, SimpleParticle &sp_tau, SimpleParticle &sp_nu_tau)
{
  // tau+ and tau- in lab frame
  Particle tau_plus (    sp_tau.px(),    sp_tau.py(),    sp_tau.pz(),    sp_tau.e(),    sp_tau.pdgid() );
  Particle tau_minus( sp_nu_tau.px(), sp_nu_tau.py(), sp_nu_tau.pz(), sp_nu_tau.e(), sp_nu_tau.pdgid() );

  // P_QQ = sum of tau+ and tau- in lab frame
  Particle P_QQ( tau_plus.px()+tau_minus.px(), tau_plus.py()+tau_minus.py(), tau_plus.pz()+tau_minus.pz(), tau_plus.e()+tau_minus.e(), 0 );
  
  Particle P_B1(0, 0, 1, 1, 0);
  Particle P_B2(0, 0,-1, 1, 0);

  tau_plus. boostToRestFrame(P_QQ);
  tau_minus.boostToRestFrame(P_QQ);
  P_B1.     boostToRestFrame(P_QQ);
  P_B2.     boostToRestFrame(P_QQ);
  
  double costheta1 = (tau_plus.px()*P_B1.px()    +tau_plus.py()*P_B1.py()    +tau_plus.pz()*P_B1.pz()    ) /
                 sqrt(tau_plus.px()*tau_plus.px()+tau_plus.py()*tau_plus.py()+tau_plus.pz()*tau_plus.pz()) /
                 sqrt(P_B1.px()    *P_B1.px()    +P_B1.py()    *P_B1.py()    +P_B1.pz()    *P_B1.pz()    );

  double costheta2 = (tau_minus.px()*P_B2.px()    +tau_minus.py()*P_B2.py()    +tau_minus.pz()*P_B2.pz()    ) /
                 sqrt(tau_minus.px()*tau_minus.px()+tau_minus.py()*tau_minus.py()+tau_minus.pz()*tau_minus.pz()) /
                 sqrt(P_B2.px()    *P_B2.px()    +P_B2.py()    *P_B2.py()    +P_B2.pz()    *P_B2.pz()    );
               
  double sintheta1 = sqrt(1-costheta1*costheta1);
  double sintheta2 = sqrt(1-costheta2*costheta2);
  
  // Cosine of hard scattering
  double costhe = (costheta1*sintheta2 + costheta2*sintheta1) / (sintheta1 + sintheta2);
  
  // Invariant mass of tau+tau- pair
  double SS     = S; // other option is for tests: P_QQ.recalculated_mass()*P_QQ.recalculated_mass();
  
  /*
    We need to fix sign of costhe and attribute ID. 
    calculate x1,x2;  // x1*x2 = SS/CMSENE/CMSENE; // (x1-x2)/(x1+x2)=P_QQ/CMSENE*2 in lab; 
    calculate weight WID[]=sig(ID,SS,+/-costhe)* f(x1,+/-ID,SS) * f(x2,-/+ID,SS) ; respectively for u d c s b 
    f(x,ID,SS,CMSENE)=x*(1-x) // for the start it will invoke library
    on the basis of this generate ID and set sign for costhe. 
    then we calculate polarization averaging over incoming states.
  */
  
  double x1x2  = SS/CMSENE/CMSENE;
  double x1Mx2 = P_QQ.pz()/CMSENE*2;
  
  double x1 = (  x1Mx2 + sqrt(x1Mx2*x1Mx2 + 4*x1x2) )/2;
  double x2 = ( -x1Mx2 + sqrt(x1Mx2*x1Mx2 + 4*x1x2) )/2;
  
  double WID[11];
  WID[0] = f(x1, 0,SS,CMSENE)*f(x2, 0,SS,CMSENE) * sigborn(0,SS, costhe);
  WID[1] = f(x1, 1,SS,CMSENE)*f(x2,-1,SS,CMSENE) * sigborn(1,SS, costhe);
  WID[2] = f(x1,-1,SS,CMSENE)*f(x2, 1,SS,CMSENE) * sigborn(1,SS,-costhe);
  WID[3] = f(x1, 2,SS,CMSENE)*f(x2,-2,SS,CMSENE) * sigborn(2,SS, costhe);
  WID[4] = f(x1,-2,SS,CMSENE)*f(x2, 2,SS,CMSENE) * sigborn(2,SS,-costhe);
  WID[5] = f(x1, 3,SS,CMSENE)*f(x2,-3,SS,CMSENE) * sigborn(1,SS, costhe);
  WID[6] = f(x1,-3,SS,CMSENE)*f(x2, 3,SS,CMSENE) * sigborn(1,SS,-costhe);
  WID[7] = f(x1, 4,SS,CMSENE)*f(x2,-4,SS,CMSENE) * sigborn(2,SS, costhe);
  WID[8] = f(x1,-4,SS,CMSENE)*f(x2, 4,SS,CMSENE) * sigborn(2,SS,-costhe);
  WID[9] = f(x1, 5,SS,CMSENE)*f(x2,-5,SS,CMSENE) * sigborn(1,SS, costhe);
  WID[10]= f(x1,-5,SS,CMSENE)*f(x2, 5,SS,CMSENE) * sigborn(1,SS,-costhe);
  
  double sum = 0.0;  // normalize
  for(int i=0;i<=10;i++) sum+=WID[i];
  
  WTnonSM=1.0;
  if(relWTnonSM==0)  WTnonSM=sum;
  if(nonSM2==1)
  {
    double WID2[11];
    WID2[0] = f(x1, 0,SS,CMSENE)*f(x2, 0,SS,CMSENE) * sigborn(0,SS, costhe) * plweight(0,SS, costhe);
    WID2[1] = f(x1, 1,SS,CMSENE)*f(x2,-1,SS,CMSENE) * sigborn(1,SS, costhe) * plweight(1,SS, costhe);
    WID2[2] = f(x1,-1,SS,CMSENE)*f(x2, 1,SS,CMSENE) * sigborn(1,SS,-costhe) * plweight(1,SS,-costhe);
    WID2[3] = f(x1, 2,SS,CMSENE)*f(x2,-2,SS,CMSENE) * sigborn(2,SS, costhe) * plweight(2,SS, costhe);
    WID2[4] = f(x1,-2,SS,CMSENE)*f(x2, 2,SS,CMSENE) * sigborn(2,SS,-costhe) * plweight(2,SS,-costhe);
    WID2[5] = f(x1, 3,SS,CMSENE)*f(x2,-3,SS,CMSENE) * sigborn(1,SS, costhe) * plweight(1,SS, costhe);
    WID2[6] = f(x1,-3,SS,CMSENE)*f(x2, 3,SS,CMSENE) * sigborn(1,SS,-costhe) * plweight(1,SS,-costhe);
    WID2[7] = f(x1, 4,SS,CMSENE)*f(x2,-4,SS,CMSENE) * sigborn(2,SS, costhe) * plweight(2,SS, costhe);
    WID2[8] = f(x1,-4,SS,CMSENE)*f(x2, 4,SS,CMSENE) * sigborn(2,SS,-costhe) * plweight(2,SS,-costhe);
    WID2[9] = f(x1, 5,SS,CMSENE)*f(x2,-5,SS,CMSENE) * sigborn(1,SS, costhe) * plweight(1,SS, costhe);
    WID2[10]= f(x1,-5,SS,CMSENE)*f(x2, 5,SS,CMSENE) * sigborn(1,SS,-costhe) * plweight(1,SS,-costhe);
    
    double sum2 = 0.0;  // normalize
    for(int i=0;i<=10;i++) sum2+=WID2[i];

    WTnonSM=sum2/sum ; 
    if(relWTnonSM==0)  WTnonSM=sum2;
    }
  
  double pol = 0.0;
  //  double Rxx = 0.0;
  //  double Ryy = 0.0;
  
  if(IfHiggs && nonSM2==1) {   // we ssume that only glue glue process contributes for Higgs
    double polp = plzap2(0,15,S,costhe);
    pol += (2*(1-polp)-1);
     return pol;
  }
  if(IfHiggs) return NAN;

  // caze of Z/gamma 
  for(int i=0;i<=10;i++) WID[i]/=sum;


  R11 = 0.0;
  R22 = 0.0;

  int ICC = -1;
  
  for(int i=1;i<=10;i++)
  {

    ICC = i;
    double cost = costhe;
    // first beam quark or antiquark

    if( ICC==2 || ICC==4 || ICC==6 || ICC==8 || ICC==10 )  cost = -cost;

    // ID of incoming quark (up or down type)
    int ID = 2;          

    if( ICC==1 || ICC==2 || ICC==5 || ICC==6 || ICC==9 || ICC==10 ) ID = 1;

    int tau_pdgid = 15;

    double polp = plzap2(ID,tau_pdgid,S,cost);
    pol += (2*(1-polp)-1)*WID[i];

    Tauolapp::TauolaParticlePair pp;

    // Set them to 0 in case no tables are loaded by Tauola++
    pp.m_R[1][1] = pp.m_R[2][2] = 0.0;

    pp.recalculateRij(ID,tau_pdgid,S,cost);

    R11 += WID[i]*pp.m_R[1][1];
    R22 += WID[i]*pp.m_R[2][2];
  }
  
  // Calculations are prepared only for pp collision.
  // Otherwise pol = 0.0
  if(!Ipp) pol=0.0;

  return pol;
}

/*******************************************************************************
  Check if the list of particles match the list of pdgid

  Returns true if 'particles' contain all of the listed pdgid-s.
  If it does - 'particles' will be rearranged so thath they have
  the same order as listed pdgid-s.

  It is done so the order of particles is the same as the order used by
  TAUOLA Fortran routines.
*******************************************************************************/
bool channelMatch(vector<Particle> &particles, int p1, int p2, int p3, int p4, int p5, int p6)
{
  // Copy pdgid-s of all particles
  vector<int> list;
  
  for(unsigned int i=0;i<particles.size();i++) list.push_back(particles[i].pdgid());
  
  // Create array out of pdgid-s
  int p[6] = { p1, p2, p3, p4, p5, p6 };

  // 1) Check if 'particles' contain all pdgid-s on the list 'p'
  
  for(int i=0;i<6;i++)
  {
    // if the pdgid is zero - finish
    if(p[i]==0) break;
    
    bool found = false;
    
    for(unsigned int j=0;j<list.size(); j++)
    {
      // if pdgid is found - erese it from the list and search for the next one
      if(list[j]==p[i])
      {
        found = true;
        list.erase(list.begin()+j);
        break;
      }
    }
    
    if(!found) return false;
  }
  
  // if there are more particles on the list - there is no match
  if(list.size()!=0) return false;

  
  // 2) Rearrange particles to match the order of pdgid-s listed in array 'p'

  vector<Particle> newList;
  
  for(int i=0;i<6;i++)
  {
    // if the pdgid is zero - finish
    if(p[i]==0) break;
    
    for(unsigned int j=0;j<particles.size(); j++)
    {
      // if pdgid is found - copy it to new list and erese from the old one
      if(particles[j].pdgid()==p[i])
      {
        newList.push_back(particles[j]);
        particles.erase(particles.begin()+j);
        break;
      }
    }
  }
  
  particles = newList;

  return true;
}

/*******************************************************************************
 Prints out two vertices:
   W   -> tau, nu_tau
   tau -> tau_daughters
*******************************************************************************/
void print(Particle &W, Particle &nu_tau, Particle &tau, vector<Particle> &tau_daughters) {

  nu_tau.print();
  tau   .print();

  double px=nu_tau.px()+tau.px();
  double py=nu_tau.py()+tau.py();
  double pz=nu_tau.pz()+tau.pz();
  double e =nu_tau.e ()+tau.e ();

  // Print out sum and W for comparison
  cout<<"--------------------------------------------------------------------------------------------------------"<<endl;
  Particle sum1(px,py,pz,e,0);
  sum1.print();
  W   .print();

  cout<<endl;

  // Print out tau daughters
  for(unsigned int i=0; i<tau_daughters.size();i++) tau_daughters[i].print();

  // Print out sum and tau for comparison
  cout<<"--------------------------------------------------------------------------------------------------------"<<endl;
  Particle *sum2 = vector_sum(tau_daughters);
  sum2->print();
  tau.print();
  cout<<endl;
  
  delete sum2;
}

/*******************************************************************************
 Sums all 4-vectors of the particles on the list
*******************************************************************************/
Particle *vector_sum(vector<Particle> &x) {

  double px=0.0,py=0.0,pz=0.0,e=0.0;

  for(unsigned int i=0; i<x.size();i++)
  {
    px+=x[i].px();
    py+=x[i].py();
    pz+=x[i].pz();
    e +=x[i].e();
  }

  Particle *sum = new Particle(px,py,pz,e,0);
  return sum;
}

} // namespace TauSpinner

