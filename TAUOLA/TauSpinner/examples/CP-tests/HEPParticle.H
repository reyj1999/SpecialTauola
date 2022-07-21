/**
 * @class HEPParticle
 * @brief Abstract class. Event record interface - particle information
 *
 * For each event record, HEPEvent and HEPParticle classes must be implemented
 *
 * @author Piotr Golonka
 * @date 2000-01-17
 *
 * (C) Faculty of Nuclear Physics & Techniques, UMM Cracow.
 */

#ifndef _HEPParticle_H
#define _HEPParticle_H


#ifdef _USE_ROOT_
#include <TObject.h>
#endif

#include <stdio.h>

#include <list>
class HEPEvent;

class HEPParticleList;
class HEPParticleListIterator;

class MC4Vector;
class MC3Vector;

class HEPParticle
{
 private:
  static double Charge(int id);
  static double Spin  (int id);

protected:
           HEPParticle()                                 ;

public:
  virtual ~HEPParticle()                                 ;
  virtual HEPEvent* GetEvent()                        = 0;

  virtual int    const GetId()                        = 0;
  virtual int    const GetMother()                    = 0;
  virtual int    const GetMother2()                   = 0;
  virtual int    const GetFirstDaughter()             = 0;
  virtual int    const GetLastDaughter()              = 0;
  virtual HEPParticleList* GetDaughterList(HEPParticleList *list);
  virtual HEPParticleList* GetMotherList(HEPParticleList *list);

  virtual double const     GetE ()                    = 0;
  virtual double const     GetPx()                    = 0;
  virtual double const     GetPy()                    = 0;
  virtual double const     GetPz()                    = 0; 
  virtual double const     GetM ()                    = 0;

  virtual int    const     GetPDGId ()                = 0;
  virtual int    const     GetStatus()                = 0;
  virtual int    const     IsStable()                 = 0;
  virtual int    const     Decays()                   = 0;
  virtual int    const     IsHistoryEntry()           = 0;

  virtual double const     GetVx  ()                  = 0;
  virtual double const     GetVy  ()                  = 0;
  virtual double const     GetVz  ()                  = 0;
  virtual double const     GetTau ()                  = 0;

  virtual MC4Vector GetP4();
  virtual MC3Vector GetP3();
  virtual MC3Vector GetV3();
  virtual void SetP4(MC4Vector &v);
  virtual void SetP3(MC3Vector &v);
  virtual void SetV3(MC3Vector &v);

  virtual void   SetEvent        ( HEPEvent  *event ) = 0;

  virtual void   SetId           ( int id       )     = 0;
  virtual void   SetMother       ( int mother   )     = 0;
  virtual void   SetMother2      ( int mother   )     = 0;
  virtual void   SetFirstDaughter( int daughter )     = 0;
  virtual void   SetLastDaughter ( int daughter )     = 0;


  virtual void   SetE  ( double E  )                  = 0;
  virtual void   SetPx ( double px )                  = 0;
  virtual void   SetPy ( double py )                  = 0;
  virtual void   SetPz ( double pz )                  = 0; 
  virtual void   SetM  ( double m  )                  = 0;

  virtual void   SetPDGId ( int pdg )                 = 0;
  virtual void   SetStatus( int st  )                 = 0;

  virtual void   SetVx ( double vx  )                 = 0;
  virtual void   SetVy ( double vy  )                 = 0;
  virtual void   SetVz ( double vz  )                 = 0;
  virtual void   SetTau( double tau )                 = 0;

  virtual void   ls (char* option =0 )             ;

  virtual bool   Compare_WithoutId( HEPParticle &p);

  virtual double GetSpin();
  virtual double GetCharge();
  virtual char*  GetParticleName();
  virtual char*  GetLaTeXName();

  virtual void   Diff(HEPParticle &p);

    // generic,portable assignment operator
  virtual HEPParticle*   Assign( HEPParticle &p);


  static double  GetSpin(int pdgId);
  static double  GetCharge(int pdgId);
  static char*   GetParticleName(int PDGCode);
  static char*   GetLaTeXName(int PDGCode);

  #ifdef _USE_ROOT_
  ClassDef(HEPParticle,0) // HEP Particle API abstract class
  #endif

};

class HEPParticleListIterator {

public:
    HEPParticleList *list;
    HEPParticle *particle;
    void *ptr;
public:
    
    HEPParticleListIterator(HEPParticleList &l);
    virtual ~HEPParticleListIterator();
    
    virtual HEPParticle* me();
    virtual HEPParticle* first();
    virtual HEPParticle* last();
    virtual HEPParticle* next();
    virtual HEPParticle* prev();

    #ifdef _USE_ROOT_
    ClassDef(HEPParticleListIterator,0) // iterator of list of HEP Particles;
    #endif
};


class HEPParticleList
{
public:
    
    HEPParticle *firstParticle;
    HEPParticle *lastParticle;
    
    virtual void push_back(HEPParticle* p);
    virtual void pop_front();

    virtual int empty();
    virtual int size();
    virtual void clear();

    virtual void remove(HEPParticle* p);
    virtual void unique();
    virtual void insert(HEPParticleListIterator *itr, int n,HEPParticle *p);
    virtual bool contains(int Id);

    virtual void ls (char *option=0);    

    virtual void* GetList() { return data;}// if one wants to use data directly,
					    // just cast it on list<HEPParticle*>*    
    virtual long int itr_begin();
    virtual long int itr_end();
    
    HEPParticleList();
    virtual ~HEPParticleList();
private:
    void* data;
    
    friend class HEPParticleListIterator;

    #ifdef _USE_ROOT_
    ClassDef(HEPParticleList,0) // List of HEP Particles;
    #endif
};

#endif // _HEPParticle_H





