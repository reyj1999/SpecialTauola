#include "TauSpinner/SimpleParticle.h"
#include <vector>
using std::vector;

namespace TauSpinner {

/** Definition of REAL*8 FUNCTION VBDISTR(I1,I2,I3,I4,H1,H2,P,KEY) from VBF_UD.f */
extern "C" double vbfdistr_(int *I1, int *I2, int *I3, int *I4, int *H1, int *H2, double P[6][4], int *KEY);

/** Wrapper to VBDISTR */
double vbfdistr(int I1, int I2, int I3, int I4, int H1, int H2, SimpleParticle &p1, SimpleParticle &p2, SimpleParticle &tau1, SimpleParticle &tau2, int KEY);

/** Get VBF spins
    Returns array W[2][2] */
void getVBFspins(SimpleParticle &p3, SimpleParticle &p4, SimpleParticle &sp_X,SimpleParticle &tau1, SimpleParticle &tau2, double (&W)[2][2], int KEY);

double calculateWeightFromParticlesVBF(SimpleParticle &p3, SimpleParticle &p4,SimpleParticle &sp_X, SimpleParticle &sp_tau1, SimpleParticle &sp_tau2, vector<SimpleParticle> &sp_tau1_daughters, vector<SimpleParticle> &sp_tau2_daughters, int KEY);

} // namespace TauSpinner
