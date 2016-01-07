// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//-----------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMCI_Comp_H
#define ESMCI_Comp_H

//-----------------------------------------------------------------------------
//BOPI
// !CLASS:  ESMCI::Comp - C++ interface to the ESMF Comp class
//
// !DESCRIPTION:
//
// The code in this file defines the internal ESMCI::Comp class and declares
// global variables.
//
//EOPI
//-----------------------------------------------------------------------------

#include "ESMCI_State.h"
#include "ESMCI_Clock.h"
#include "ESMCI_Grid.h"
#include "ESMCI_F90Interface.h"

namespace ESMCI {

class CompTunnel; // forward reference here, breaking circular dependency issue
class FTable;     // forward reference here, breaking circular dependency issue

// constants and enums

enum CompType { COMPTYPE_GRID=1, COMPTYPE_CPL, COMPTYPE_UNKNOWN };
enum method { METHOD_NONE=0, 
  METHOD_INITIALIZE, METHOD_RUN, METHOD_FINALIZE, 
  METHOD_WRITERESTART, METHOD_READRESTART, METHOD_SERVICELOOP,
  METHOD_INITIALIZEIC, METHOD_RUNIC, METHOD_FINALIZEIC, 
  METHOD_WRITERESTARTIC, METHOD_READRESTARTIC, METHOD_SERVICELOOPIC,
  METHOD_SETVM, METHOD_SETSERVICES, METHOD_WAIT};


// class definition
class Comp{
  private:
    F90ClassHolder fortranclass;
  public:
    int setServices(void (*func)(Comp *, int *), int *userRc);
    int setEntryPoint(enum method method,
      void (*functionPtr)(Comp *, State *, State *, Clock **, int *),
      int phase=1);
    void *getInternalState(int *rc)const;
    int setInternalState(void *data);
    int execute(enum method method, ESMCI::State *importState, 
      ESMCI::State *exportState, ESMCI::Clock *clock, 
      ESMC_BlockingFlag blockingFlag, int phase, int timeout, 
      int *userRc) const;
    
    int getCurrentPhase(int *currentPhase) const;
    int getTimeout(int *timeout) const;
    int getVmInfo(void **vm_info) const;
    int getVm(VM **vm) const;
    int getVmParent(VM **vmparent) const;
    int getVmPlan(VMPlan **vmplan) const;
    int getFTable(FTable **ftable) const;
    int getTunnel(CompTunnel **tunnel) const;
    static int getComplianceCheckerDepth(int *depth);
};

class GridComp:public Comp{
  public:
    static GridComp *create(char const *name, 
      char const *configFile, ESMCI::Clock *clock, int *rc=NULL);
    static int destroy(GridComp *comp);
    int print() const;
    int initialize(ESMCI::State *importState, ESMCI::State *exportState,
      ESMCI::Clock *clock, int phase, int *userRc) const;
    int run(ESMCI::State *importState, ESMCI::State *exportState,
      ESMCI::Clock *clock, int phase, int *userRc) const;
    int finalize(ESMCI::State *importState, ESMCI::State *exportState,
      ESMCI::Clock *clock, int phase, int *userRc) const;
};

class CplComp:public Comp{
  public:
    static CplComp *create(char const *name, char const *configFile,
      ESMCI::Clock *clock, int *rc=NULL);
    static int destroy(CplComp *comp);
    int print() const;
    int initialize(ESMCI::State *importState, ESMCI::State *exportState,
      ESMCI::Clock *clock, int phase, int *userRc) const;
    int run(ESMCI::State *importState, ESMCI::State *exportState,
      ESMCI::Clock *clock, int phase, int *userRc) const;
    int finalize(ESMCI::State *importState, ESMCI::State *exportState,
      ESMCI::Clock *clock, int phase, int *userRc) const;
};

class SciComp:public Comp{
  public:
    static SciComp *create(char const *name, int *rc=NULL);
    static int destroy(SciComp *comp);
    int print() const;
};

} // namespace ESMCI

#endif  // ESMCI_Comp_H
