/**
 * @class MC3Vector
 * @brief Simple 3-vector
 *
 * Simple 3-vector with helpful routines and operators
 *
 * @author Piotr Golonka
 * @date 2004-03-27
 *
 */

#ifndef _MC3Vector_H
#define _MC3Vector_H

#include <stdio.h>

#if defined _USE_ROOT_
#include <TObject.h>
#endif


class MC3Vector
#ifdef _USE_ROOT_
  : public TObject
#endif
{

 private:
    double v_X0;
    double v_X1;
    double v_X2;
 public:
   MC3Vector();
   virtual ~MC3Vector();
   MC3Vector(double x0, double x1, double x2);
   MC3Vector( MC3Vector const &v);

   MC3Vector     operator+ ();
   MC3Vector     operator- ();

   MC3Vector     operator+ ( const MC3Vector v  ) const;
   MC3Vector     operator- ( const MC3Vector v  ) const;

   MC3Vector&    operator+=( const MC3Vector v1 );
   MC3Vector&    operator-=( const MC3Vector v1 );

   double        operator* ( const MC3Vector &v  ) const;  
   MC3Vector     operator% ( const MC3Vector &v  ) const;
   
   MC3Vector&    operator= ( const MC3Vector &v );

   const bool    operator==( const MC3Vector &v);
   const bool    operator!=( const MC3Vector &v) {return ! operator==(v);}

   double const GetX0()            { return v_X0;            }
   double const GetX1()            { return v_X1;            }
   double const GetX2()            { return v_X2;            }
   void         SetX0( double x0 ) { v_X0 = x0;              }
   void         SetX1( double x1 ) { v_X1 = x1;              }
   void         SetX2( double x2 ) { v_X2 = x2;              }

   double       Length() const;
   double       Angle(const MC3Vector &w) const;
   double       operator^(const MC3Vector &w) {return Angle(w);}
   
   operator double() {return Length();}
   
   virtual void         ls(char *option=0);

#ifdef _USE_ROOT_
   ClassDef(MC3Vector,1) //  3-Vector
#endif
};


#endif // _MC3Vector_H
