#include "TauSpinner/tau_reweight_lib.h"
#include "TauSpinner/Particle.h"
#include "TauSpinner/vbfdistr.h"
#include <cstdlib>
using namespace Tauolapp;

namespace TauSpinner {

extern double CMSENE;
extern int    relWTnonSM;
extern double WTnonSM;   
extern double Polari;
extern double WTamplit;
extern double WTamplitP;
extern double WTamplitM;
extern double CMSENE;
extern double f(double x, int ID, double SS, double cmsene);


/** Wrapper to VBDISTR */
double vbfdistr(int I1, int I2, int I3, int I4, int H1, int H2, double P[6][4], int KEY)
{  
  return vbfdistr_(&I1, &I2, &I3, &I4, &H1, &H2,  P, &KEY);
}

/** Get VBF spins
    Returns array W[2][2] */
void getVBFspins(SimpleParticle &p3, SimpleParticle &p4, SimpleParticle &sp_X,SimpleParticle &tau1, SimpleParticle &tau2, double (&W)[2][2], int KEY)
{
  /*
  // this may be necessary because of matrix element calculation may require absolute energy-momentum conservation!
  // FSR photons may need to be treated explicitely or with interpolation procedures.
  Particle P_QQ( p3.px()+p4.px()+tau1.px()+tau2.px(),
                 p3.py()+p4.py()+tau1.py()+tau2.py(),
                 p3.pz()+p4.pz()+tau1.pz()+tau2.pz(),
                 p3.e() +p4.e() +tau1.e() +tau2.e(), 0 );
  */
  Particle P_QQ( p3.px()+p4.px()+sp_X.px(),
                 p3.py()+p4.py()+sp_X.py(),
                 p3.pz()+p4.pz()+sp_X.pz(),
                 p3.e() +p4.e() +sp_X.e() , 0 );


  double SS = P_QQ.recalculated_mass()*P_QQ.recalculated_mass(); 
  
  double x1x2  = SS/CMSENE/CMSENE;
  double x1Mx2 = P_QQ.pz()/CMSENE*2;
  
  double x1 = (  x1Mx2 + sqrt(x1Mx2*x1Mx2 + 4*x1x2) )/2;
  double x2 = ( -x1Mx2 + sqrt(x1Mx2*x1Mx2 + 4*x1x2) )/2;
  
  //---------------------------------------------------------------------------
  // Construct the matrix for FORTRAN function
  // NOTE: different order of indices than in FORTRAN!
  double P[6][4] = { { CMSENE/2*x1, 0.0,       0.0,          CMSENE/2*x1  },
                     { CMSENE/2*x2, 0.0,       0.0,         -CMSENE/2*x2  }, 
                     { p3.e(),      p3.px(),   p3.py(),      p3.pz()      }, 
                     { p4.e(),      p4.px(),   p4.py(),      p4.pz()      },
                     { tau1.e(),    tau1.px(), tau1.py(),    tau1.pz()    },
                     { tau2.e(),    tau2.px(), tau2.py(),    tau2.pz()    } };
 

  W[0][0]=0.0;
  W[0][1]=0.0;
  W[1][0]=0.0;
  W[1][1]=0.0;
  // these loops need to be cleaned from zero contributions! 
  for(int I1=-5;I1<=5;I1++){
    for(int I2=-5;I2<=5;I2++){
      for(int I3=-5;I3<=5;I3++){
        for(int I4=-5;I4<=5;I4++){
          W[0][0] += f(x1, I1,SS,CMSENE)*f(x2, I2,SS,CMSENE)*vbfdistr(I1,I2,I3,I4, 1,  1, P, KEY);
          W[0][1] += f(x1, I1,SS,CMSENE)*f(x2, I2,SS,CMSENE)*vbfdistr(I1,I2,I3,I4, 1, -1, P, KEY);
          W[1][0] += f(x1, I1,SS,CMSENE)*f(x2, I2,SS,CMSENE)*vbfdistr(I1,I2,I3,I4,-1,  1, P, KEY);
          W[1][1] += f(x1, I1,SS,CMSENE)*f(x2, I2,SS,CMSENE)*vbfdistr(I1,I2,I3,I4,-1, -1, P, KEY);
        }
      }
    }
  }
}

double calculateWeightFromParticlesVBF(SimpleParticle &p3, SimpleParticle &p4,SimpleParticle &sp_X, SimpleParticle &sp_tau1, SimpleParticle &sp_tau2, vector<SimpleParticle> &sp_tau1_daughters, vector<SimpleParticle> &sp_tau2_daughters, int KEY)
{
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

  double W[2][2] = { { 0.25, 0.25 },
                     { 0.25, 0.25 } };  // these are defalults: no spin effects
  
  getVBFspins(p3, p4, sp_X, sp_tau1, sp_tau2, W, KEY);
  
  double WT = W[0][0]*(1+HHp[2])*(1+HHm[2])+W[0][1]*(1+HHp[2])*(1-HHm[2])+ W[1][0]*(1-HHp[2])*(1+HHm[2])+W[1][1]*(1-HHp[2])*(1-HHm[2]);
  WT = WT/(W[0][0]+W[0][1]+ W[1][0]+W[1][1]);


  double sum = 0.0; // must be remembered from KEY=0 to KEY=1 case

  if(KEY==0) { 
    sum=(W[0][0]+W[0][1]+ W[1][0]+W[1][1]);
    WTnonSM=1.0;
    if(relWTnonSM==0)  WTnonSM=sum;
  }
  if(KEY==2) { 
    double sum2=(W[0][0]+W[0][1]+ W[1][0]+W[1][1]);
    WTnonSM=sum2/sum;
    if(relWTnonSM==0)  WTnonSM=sum2;
  }
  // we separate cross section into helicity +- and -+ parts
  double RRR = Tauola::randomDouble();  
  Polari=1.0;
  
  // something must be understood (may be fixed too) especially for dependence on KEY
  if (RRR<(W[0][0]*(1+HHp[2])*(1+HHm[2])+W[0][1]*(1+HHp[2])*(1-HHm[2])+ W[1][0]*(1-HHp[2])*(1+HHm[2])+W[1][1]*(1-HHp[2])*(1-HHm[2]))/2.0/(W[0][0]+W[0][1]+ W[1][0]+W[1][1])) Polari=-1.0;

  // Print out some info about the channel
  DEBUG( cout<<" WT: "<<WT<<endl; )

  if (WT<0.0) {
    printf("WT is: %13.10f. Setting WT = 0.0\n",WT);
    WT = 0.0;
   }

  if (WT>4.0) {
    printf("WT is: %13.10f. Setting WT = 4.0\n",WT);
    WT = 4.0;
  }

  delete[] HHp;
  delete[] HHm;
  
  return WT; 
}

} // namespace TauSpinner
