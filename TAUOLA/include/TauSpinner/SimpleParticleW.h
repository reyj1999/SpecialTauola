#ifndef _SIMPLE_PARTICLE_H_
#define _SIMPLE_PARTICLE_H_
/**
  Single particle for user convenience.
  Just 4-momentum and pdgid.
  with the addition of lorentz vector decay position - notated by xx, yy, zz, t
*/

namespace TauSpinner {

class SimpleParticle
{
public:
  SimpleParticle() { _px=_py=_pz=_e=0_xx=0_yy=0_zz=0_t=0; _pdgid=0; }
  SimpleParticle(double x, double y, double z, double e, double xx, double yy, double zz, double t, int id) { _px=x; _py=y; _pz=z; _e=e; _xx=xx; _yy=yy; _zz=zz; _t=t; _pdgid=id; }

  double px()    { return _px;    }
  double py()    { return _py;    }
  double pz()    { return _pz;    }
  double e ()    { return _e;     }
  double xx()    { return _xx;    }
  double yy()    { return _yy;    }
  double zz()    { return _zz;    }
  double t ()    { return _t;     }
  int    pdgid() { return _pdgid; }

  void setPx(double x ) { _px = x;     }
  void setPy(double y ) { _py = y;     }
  void setPz(double z ) { _pz = z;     }
  void setE (double e ) { _e  = e;     }
  void setXX(double xx) { _xx = xx;    }
  void setYY(double yy) { _yy = yy;    }
  void setZZ(double zz) { _zz = zz;    }
  void setT (double t ) { _t  = t;     }
  void setPdgid(int id) { _pdgid = id; }

private:
  double _px,_py,_pz,_e,_xx,_yy,_zz,_t;
  int    _pdgid;
};

} // namespace TauSpinner
#endif
