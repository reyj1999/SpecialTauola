/**
 * @class MC4Vector
 * @brief Simple 4-vector
 *
 * Simple 4-vector with helpfull routines and operators
 *
 * @author Piotr Golonka
 * @date 2003-10-26
 *
 */

#ifndef _MC4Vector_H
#define _MC4Vector_H


class MC3Vector;

#if defined _USE_ROOT_
#include <TObject.h>
#endif

class MC4Vector
{  

 private:
  double v_X0; // 0th component of 4-vector ( t, E   )
  double v_X1; // 1st component of 4-vector ( x, P_x )
  double v_X2; // 2nd component of 4-vector ( y, P_y )
  double v_X3; // 3rd component of 4-vector ( z, P_z )

  double v_M;  // additional component      (     m  )

public:
  MC4Vector();
  virtual ~MC4Vector();

  MC4Vector(double x0, double x1, double x2, double x3);
  MC4Vector(double x0, double x1, double x2, double x3, double m);

  MC4Vector( MC4Vector const &v);
  MC4Vector( MC4Vector const *v);


  // operators:

  MC4Vector     operator+ ();
  MC4Vector     operator- ();

  MC4Vector     operator+ ( const MC4Vector v  );
  MC4Vector     operator- ( const MC4Vector v  );

  MC4Vector&    operator+=( const MC4Vector v1 );
  MC4Vector&    operator-=( const MC4Vector v1 );

  double       operator* ( const MC4Vector v  );  

  MC4Vector&    operator= ( const MC4Vector &v );

  const bool   operator==( const MC4Vector &v);
  const bool   operator!=( const MC4Vector &v) {return ! operator==(v);}


  virtual double  GetX0();
  virtual double  GetX1();
  virtual double  GetX2();
  virtual double  GetX3();
  virtual double  GetM ();
  virtual void         Get  ( double &x0,
			      double &x1,
			      double &x2,
			      double &x3 );  

  MC3Vector Get3Vector();

  virtual void         Set  ( double  x0,
			      double  x1,
			      double  x2,
			      double  x3,
			      double  m = -1e30  );  

  virtual void         Set  (MC4Vector *v);

  virtual void         SetX0( double x0 ) { v_X0 = x0;              }
  virtual void         SetX1( double x1 ) { v_X1 = x1;              }
  virtual void         SetX2( double x2 ) { v_X2 = x2;              }
  virtual void         SetX3( double x3 ) { v_X3 = x3;              }
  virtual void         SetM ( double m  ) { v_M  = m ;              }



  virtual double       Square();
  virtual double       Length();
  virtual double       Length2();
  virtual double       Xt(); // transverse component, i.e. p_t
  virtual void         AdjustM();
  
  static double        Angle1(double x, double y);
  static double        Angle2(double x, double y);

  virtual double       Phi();
  virtual double       Theta();

  virtual void         Boost(MC4Vector &v);
  virtual void         BoostP(MC4Vector &p);

  virtual void         Boost(double gamma, 
			     double beta1, double beta2, double beta3);

  virtual void         Boost(double beta1, double beta2 , double beta3);

  virtual void         Boost(double p1, double p2, double p3,
			     double E, double m);

  virtual void         Boost3(double expEta);

  virtual void         Rotate1( double angle );
  virtual void         Rotate2( double angle );
  virtual void         Rotate3( double angle );

  virtual void         ls(char *option=0);

#ifdef _USE_ROOT_
  ClassDef(MC4Vector,1) //  4-Vector
#endif

};


#endif // _MC4Vector_H
