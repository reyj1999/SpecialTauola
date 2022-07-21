/**
 * @class LUJETSParticle
 * @brief Interface to COMMON LUJETS - particle information
 *
 * This class provides a set of methods that allows to access a data
 * stored in COMMON/LUJETS/
 *
 * @author Piotr Golonka
 * @date 2000-01-24
 *
 * (C) Faculty of Nuclear Physics & Techniques, UMM Cracow.
 */

#ifndef _LUJETSParticle_H
#define _LUJETSParticle_H

#ifdef _USE_ROOT_
#include <TObject.h>
#endif

#include "HEPParticle.H"


class LUJETSEvent;

class LUJETSParticle : public HEPParticle
{

 private:

  LUJETSEvent*    event ;
  int             id    ;

public:
          LUJETSParticle()       ;

  virtual HEPEvent* GetEvent()   ;

  virtual int    const  GetId()           ;
  virtual int    const  GetMother()       ;
  virtual int    const  GetMother2()      ;
  virtual int    const  GetFirstDaughter();
  virtual int    const  GetLastDaughter() ;

  virtual double const  GetE ()  ;
  virtual double const  GetPx()  ;
  virtual double const  GetPy()  ;
  virtual double const  GetPz()  ;
  virtual double const  GetM ()  ;

  virtual int    const  GetPDGId ();
  virtual int    const  GetStatus();
  virtual int    const  IsStable() { return (GetStatus() == 1); }
  virtual int    const  Decays()   { return (GetStatus() == 11); }
  virtual int    const  IsHistoryEntry()   { return (GetStatus() == 21); }

  virtual double const  GetVx  ();
  virtual double const  GetVy  ();
  virtual double const  GetVz  ();
  virtual double const  GetTau ();

  virtual void   SetEvent        ( HEPEvent  *event ) ;

  virtual void   SetId           ( int id       ) ;
  virtual void   SetMother       ( int mother   ) ;
  virtual void   SetMother2      ( int mother   ) ;
  virtual void   SetFirstDaughter( int daughter ) ;
  virtual void   SetLastDaughter ( int daughter ) ;

  virtual void   SetE  ( double E  ) ;
  virtual void   SetPx ( double px ) ;
  virtual void   SetPy ( double py ) ;
  virtual void   SetPz ( double pz ) ;
  virtual void   SetM  ( double m  ) ;

  virtual void   SetPDGId ( int pdg );
  virtual void   SetStatus( int st ) ;

  virtual void   SetVx ( double vx  );
  virtual void   SetVy ( double vy  );
  virtual void   SetVz ( double vz  );
  virtual void   SetTau( double tau );

  virtual void   ls (char* option =0 ) ;

  // additional in LUJETS - lifetime of particle:

  virtual double const  GetLifetime();
  virtual void   SetLifetime( double lifetime );

  #ifdef _USE_ROOT_
  ClassDef(LUJETSParticle,0) // LUJETS Particle class
  #endif

};

#endif // _LUJETSParticle_H





