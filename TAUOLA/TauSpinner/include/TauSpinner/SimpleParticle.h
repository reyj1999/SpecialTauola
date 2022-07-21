#ifndef _SIMPLE_PARTICLE_H_
#define _SIMPLE_PARTICLE_H_
/**
  Single particle for user convenience.
  Just 4-momentum and pdgid.
*/

namespace TauSpinner {

class SimpleParticle
{
public:
  SimpleParticle() { _px=_py=_pz=_e=0; _vx=_vy=_vz=_t=0; _pdgid=0; }
  SimpleParticle(double x, double y, double z, double e, int id) { _px=x; _py=y; _pz=z; _e=e; _vx=_vy=_vz=_t=0; _pdgid=id; }

  double px()    { return _px;    }
  double py()    { return _py;    }
  double pz()    { return _pz;    }
  double e ()    { return _e;     }
  double vx()    { return _vx;    }
  double vy()    { return _vy;    }
  double vz()    { return _vz;    }
  double t ()    { return _t;     }
  int    pdgid() { return _pdgid; }

  void setPx(double x ) { _px = x;     }
  void setPy(double y ) { _py = y;     }
  void setPz(double z ) { _pz = z;     }
  void setE (double e ) { _e  = e;     }
  void setVx(double x ) { _vx = x;     }
  void setVy(double y ) { _vy = y;     }
  void setVz(double z ) { _vz = z;     }
  void setT (double t ) { _t  = t;     }
  void setPdgid(int id) { _pdgid = id; }

private:
  double _px,_py,_pz,_e;
  double _vx,_vy,_vz,_t;
  int    _pdgid;
};

} // namespace TauSpinner
#endif
