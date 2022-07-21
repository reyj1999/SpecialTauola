/**
 * @class HEPEvent
 * @brief Abstract class. Event record interface - event information
 *
 * For each event record, HEPEvent and HEPParticle classes must be implemented
 *
 * @author Piotr Golonka
 * @date 2000-01-17
 *
 * (C) Faculty of Nuclear Physics & Techniques, UMM Cracow.
 */

#ifndef _HEPEventImp_H
#define _HEPEventImp_H

#ifdef _USE_ROOT_
#include <TObject.h>
#endif

#include "HEPParticle.H"
#include <cstring>

class HEPEvent
{
protected:
  HEPEvent();

 public:
  virtual ~HEPEvent();
  virtual int   GetNumOfParticles()        = 0;
  virtual void  SetNumOfParticles(int num) = 0;
  
  virtual int   GetEventNumber()           = 0;
  virtual void  SetEventNumber(int num)    = 0;

  // abstract functions.
  virtual HEPParticle*      GetParticle(int idx)                        = 0;
  virtual void              AddParticle( HEPParticle *p)                = 0;
  virtual void              SetParticle(int idx,HEPParticle *p)         = 0;
  virtual void              InsertParticle(int at_idx,HEPParticle *p)   = 0;
  virtual void              Clear(int fromIdx=1)                        = 0;
  virtual void              AddParticle( int id, 
					 int pdgid,
					 int status,
					 int mother,
					 int mother2,
					 int firstdaughter,
					 int lastdaughter,
					 double E,
					 double px,
					 double py,
					 double pz,
					 double m,
					 double vx,
					 double vy,
					 double vz,
					 double tau)                   = 0;
					 
  virtual HEPParticleList*  FindParticle(int pdg, HEPParticleList *list=0);
  virtual void              ls (char* option=0 )  ;

  #ifdef _USE_ROOT_
  ClassDef(HEPEvent,0) // HEP Event API abstract class
  #endif
};
#endif // _HEPEventImp_H
