/**
 * @class HEPEVTEvent
 * @brief Interface to COMMON HEPEVT - event information
 *
 * This class provides a set of methods that allows to access a data
 * stored in COMMON/HEPEVT/. This data structure is used by most
 * of FORTRAN 77 HEP programs as a storage for an event record.
 * In particular: majority of event generators used in HEP Monte-Carlo
 * simulation is capable of exporting their results to COMMON/HEPEVT/
 *
 * @author Piotr Golonka
 * @date 2000-10-11
 *
 * (C) Faculty of Nuclear Physics & Techniques, UMM Cracow.
 */

#ifndef _HEPEVTEvent_H
#define _HEPEVTEvent_H

#if defined _USE_ROOT_
#include <TObject.h>
#endif







/*========================================================*/
/* COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),*/
/*               JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),       */
/*               PHEP(5,NMXHEP),VHEP(4,NMXHEP)            */
/*--------------------------------------------------------*/

const int nmxhep=4000;

typedef struct {
  int    nevhep;
  int    nhep;
  int    isthep[nmxhep];
  int    idhep [nmxhep];
  int    jmohep[nmxhep][2];
  int    jdahep[nmxhep][2];
  double phep  [nmxhep][5];
  double vhep  [nmxhep][4];
} HepevtCommon;


/**********************************************************/
/*           D E S C R I P T I O N :                      */
/*--------------------------------------------------------*/
/* This common block specifies list of particles as       */
/* defined by PDG. To use it, one has to convert LUJETS   */
/* to HEPEVT using subroutine LUHEPC.                     */
/*                                                        */
/* NMXHEP          - maximum numbers of entries in HEPEVT.*/
/*                    fixed to default = 4000.            */
/* NEVHEP          - event number (or some special meaning*/
/*                    (see documentation for details)     */
/* NHEP            - actual number of entries in current  */
/*                    event.                              */
/* ISTHEP[IHEP]    - status code for IHEP'th entry - see  */
/*                    documentation for details           */
/* IDHEP [IHEP]    - IHEP'th particle identifier according*/
/*                    to PDG.                             */
/* JMOHEP[IHEP][0] - pointer to position of 1st mother    */
/* JMOHEP[IHEP][1] - pointer to position of 2nd mother    */
/* JDAHEP[IHEP][0] - pointer to position of 1st daughter  */
/* JDAHEP[IHEP][1] - pointer to position of 2nd daughter  */
/* PHEP  [IHEP][0] - X momentum [Gev/c]                   */
/* PHEP  [IHEP][1] - Y momentum [Gev/c]                   */
/* PHEP  [IHEP][2] - Z momentum [Gev/c]                   */
/* PHEP  [IHEP][3] - Energy [Gev]                         */
/* PHEP  [IHEP][4] - Mass[Gev/c^2]                        */
/* VHEP  [IHEP][0] - X vertex [mm]                        */
/* VHEP  [IHEP][1] - Y vertex [mm]                        */
/* VHEP  [IHEP][2] - Z vertex [mm]                        */
/* VHEP  [IHEP][3] - production time [mm/c]               */
/*========================================================*/












#include "HEPEVTParticle.H"
#include "HEPEvent.H"

class HEPEVTParticle;


class HEPEVTEvent: public HEPEvent
{
  
protected:
  HEPEVTParticle *particles;
  
  void *data;
  int           _size;
public:
  HEPEVTEvent(int size=nmxhep);     
  
  HEPEVTEvent(void *dataptr, int size=nmxhep);
  
  virtual int  GetNumOfParticles() { return GetNHEP() ;  }
  virtual int  GetEventNumber()    { return GetNEVHEP(); }
  
  virtual void SetNumOfParticles(int n) { SetNHEP(n)   ; }
  virtual void SetEventNumber(int ev)   { SetNEVHEP(ev); }
  
  virtual HEPParticle* GetParticle( int idx) { return &(particles[idx-1]); }
  
  virtual void  SetParticle( int idx, HEPParticle *particle);
  virtual void  AddParticle( HEPParticle *particle);
  virtual void  Clear (int fromIdx);
  virtual void  ls( char *option=0);
  
  virtual void  InsertParticle(int at_idx,HEPParticle *p);
  virtual void  AddParticle( int id, int pdgid, int status,
			     int mother, int mother2,
			     int firstdaughter, int lastdaughter,
			     double E,double px, double py, double pz, double m,
			     double vx, double vy, double vz, double tau);
  
  
  // common HEPEVT access routines:
  
  virtual int         GetNEVHEP();
  virtual void        SetNEVHEP(int nevhep);
  
  virtual int         GetNHEP();
  virtual void        SetNHEP(int nhep);
  
  virtual int         GetISTHEP(int ihep);
  virtual void        SetISTHEP(int ihep, int value);
  
  virtual int         GetIDHEP(int ihep);
  virtual void        SetIDHEP(int ihep, int value);
  
  virtual int         GetJMOHEP(int mother, int ihep);
  virtual void        SetJMOHEP(int mother, int ihep, int value);
  
  virtual int         GetJDAHEP(int daughter, int ihep);
  virtual void        SetJDAHEP(int daughter, int ihep, int value);
  
  virtual double      GetPHEP(int coord, int ihep);
  virtual void        SetPHEP(int coord, int ihep, double value);
  
  virtual double      GetVHEP(int coord, int ihep);
  virtual void        SetVHEP(int coord, int ihep, double value);
  
  
  

  //"fast" , inline version - no range checking...
  virtual int         GetISTHEP_(int ihep);
  virtual void        SetISTHEP_(int ihep, int value);
  
  virtual int         GetIDHEP_(int ihep);
  virtual void        SetIDHEP_(int ihep, int value);
  
  virtual int         GetJMOHEP_(int mother, int ihep);
  virtual void        SetJMOHEP_(int mother, int ihep, int value);

  virtual int         GetJDAHEP_(int daughter, int ihep);
  virtual void        SetJDAHEP_(int daughter, int ihep, int value);
  
  virtual double      GetPHEP_(int coord, int ihep);
  virtual void        SetPHEP_(int coord, int ihep, double value);
  
  virtual double      GetVHEP_(int coord, int ihep);
  virtual void        SetVHEP_(int coord, int ihep, double value);
  
  
  static HEPEVTEvent* CreateEvent(int size=4000);
  static void         DestroyEvent(HEPEVTEvent *event);
  
  #ifdef _USE_ROOT_
  ClassDef(HEPEVTEvent,1)  //Interface to common HEPEVT
  #endif  
    
};

extern HEPEVTEvent HEPEVT;

#endif


