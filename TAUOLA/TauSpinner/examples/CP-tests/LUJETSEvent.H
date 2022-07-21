/**
 * @class LUJETSEvent
 * @brief Interface to COMMON LUJETS - event information
 *
 * This class provides a set of methods that allows to access a data
 * stored in COMMON/LUJETS/
 *
 * @author Piotr Golonka
 * @date 2000-01-24
 *
 * (C) Faculty of Nuclear Physics & Techniques, UMM Cracow.
 */

#ifndef _LUJETSEvent_H
#define _LUJETSEvent_H

#if defined _USE_ROOT_
#include <TObject.h>
#endif




/*========================================================*/
/* COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)          */
/*--------------------------------------------------------*/
typedef struct {
   int    n;
   int    k[5][4000];
   float  p[5][4000];
   float  v[5][4000];
} LujetsCommon;
 
/***********************************************************/
/*           D E S C R I P T I O N :                       */
/*---------------------------------------------------------*/
/*  N      - number of lines in K,P,V matrices occupied by */
/*           current event                                 */
/*  K[0][I] - Status Code KS (look at documentation)       */
/*  K[1][I] - Parton/Particle KF code                      */
/*  K[2][I] - Line number of Parrent Particle              */
/*  K[3][I] - Line number of first daugher[or internal use]*/
/*  K[4][I] - Line number of last daughter[or internal use]*/
/*                                                         */
/*  P[0][I] - Px - momentum in the x direction [GeV/c]     */
/*  P[1][I] - Py - momentum in the y direction [GeV/c]     */
/*  P[2][I] - Pz - momentum in the z direction [GeV/c]     */
/*  P[3][I] - E  - energy [GeV]                            */
/*  P[4][I] - m  - mass [Gev/c^2]                          */
/*                                                         */
/*  V[0][I] - x position of production vertex [mm]         */
/*  V[1][I] - y position of production vertex [mm]         */
/*  V[2][I] - z position of production vertex [mm]         */
/*  V[3][I] - time of production [mm/c]=[3.33E-12 s]       */
/*  V[4][I] - proper lifetime of particle [mm/c]           */ 
/*========================================================*/



#include "LUJETSParticle.H"
#include "HEPEvent.H"


class LUJETSEvent: public HEPEvent
{

private:
  LUJETSParticle *particles;
  
  LujetsCommon *data;
  
public:
  LUJETSEvent();     
  
  virtual int  GetNumOfParticles() { return GetN() ;  } ;
  virtual int  GetEventNumber()    { return 0; } ;
  
  virtual void SetNumOfParticles(int n) { SetN(n)   ; } ;
  virtual void SetEventNumber(int ev)   {             } ;
  
  virtual HEPParticle* GetParticle( int idx) { return &(particles[idx-1]); }
  
  virtual void  SetParticle( int idx, HEPParticle *particle) ;
  virtual void  AddParticle( HEPParticle *particle);
  virtual void  Clear (int fromIdx);
  virtual void  ls( char *option=0);
  
  virtual void  InsertParticle(int at_idx,HEPParticle *p);
  virtual void  AddParticle( int id, int pdgid, int status,
			     int mother, int mother2,
			     int firstdaughter, int lastdaughter,
			     double E,double px, double py, double pz, double m,
			     double vx, double vy, double vz, double tau);
  
  
  // common LUJETS access routines:
  
  virtual int         GetN();
  virtual void        SetN(int n);
  
  virtual int         GetK(int part, int coord);
  virtual void        SetK(int part, int coord, int val);

  virtual float       GetP(int part, int coord);
  virtual void        SetP(int part, int coord, float  value);

  virtual float       GetV(int part, int coord);
  virtual void        SetV(int part, int coord, float  value);




  //"fast" , inline version - no range checking...
  virtual int         GetK_(int part, int k);
  virtual void        SetK_(int part, int h, int val);

  virtual float       GetP_(int part, int coord);
  virtual void        SetP_(int part, int coord, float  value);

  virtual float       GetV_(int part, int coord);
  virtual void        SetV_(int part, int coord, float  value);

  #ifdef _USE_ROOT_
  ClassDef(LUJETSEvent,1)  //Interface to common LUJETS
  #endif

};

extern LUJETSEvent LUJETS;

#endif


