/**
 * @class HerwigEvent
 * @brief Herwig HEPEVT interface - event information
 *
 * Interface to Herwig version of HEPEVT common
 *
 * @author Piotr Golonka
 * @date 2003-04-28
 *
 * (C) Faculty of Nuclear Physics & Techniques, UMM Cracow.
 */

#ifndef _HerwigEvent_H
#define _HerwigEvent_H

#ifdef _USE_ROOT_
#include <TObject.h>
#endif



#include "HerwigParticle.H"
#include "HEPEVTEvent.H"


class HerwigEvent: public HEPEVTEvent
{
  
protected:
  HerwigParticle *hparticles;

public:
  HerwigEvent(int size=nmxhep);
  
  HerwigEvent(void *dataptr, int size=nmxhep);
  
  virtual HEPParticle* GetParticle( int idx) { return &(hparticles[idx-1]); }
    
  virtual void  ls( char *option=0);

  #ifdef _USE_ROOT_
  ClassDef(HerwigEvent,1)  //Interface to Herwig version of HEPEVT common
  #endif  
    
};

extern HerwigEvent HerwigEVT;

#endif


