/**
 * @class HerwigParticle
 * @brief Herwig HEPEVT interface - particle information
 *
 * Interface to Herwig version of HEPEVT common
 *
 * @author Piotr Golonka
 * @date 2000-01-17
 *
 * (C) Faculty of Nuclear Physics & Techniques, UMM Cracow.
 */

#ifndef _HerwigParticle_H
#define _HerwigParticle_H

#ifdef _USE_ROOT_
#include <TObject.h>
#endif

#include "HEPEVTParticle.H"


class HerwigEvent;

class HerwigParticle : public HEPEVTParticle
{

 private:

  HerwigEvent*    event ;
  int             id    ;

public:
          HerwigParticle()       ;

virtual ~HerwigParticle(){}
  const HerwigParticle operator= (HEPParticle &p);

  virtual int    const GetLastDaughter() ;

  virtual int    const  IsStable();
  virtual int    const  Decays();
  virtual int    const  IsHistoryEntry();



  virtual void   ls (char* option =0 ) ;

  #ifdef _USE_ROOT_
  ClassDef(HerwigParticle,0) // Herwig Particle class
  #endif

};

#endif // _HerwigParticle_H





