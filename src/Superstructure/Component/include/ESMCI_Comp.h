// $Id: ESMCI_Comp.h,v 1.16.2.1 2010/02/05 20:03:51 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
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


#include "ESMCI_LogErr.h"
#include "ESMF_LogMacros.inc"             // for LogErr

#include "ESMCI_State.h"
#include "ESMCI_Clock.h"
#include "ESMCI_Grid.h"
#include "ESMCI_F90Interface.h"


namespace ESMCI {

// constants and enums

enum CompType { COMPTYPE_GRID=1, COMPTYPE_CPL, COMPTYPE_UNKNOWN };
enum GridCompType { ATM=2, LAND, OCEAN, SEAICE, RIVER, UNKNOWN };
enum method { SETINIT=1, SETRUN, SETFINAL, SETWRITERESTART, SETREADRESTART,
  SETREGISTER };


// class definition
class Comp{
  private:
    F90ClassHolder fortranclass;
  public:
    int setServices(void (*func)(Comp *, int *), int *userRc);
    int setEntryPoint(enum method method,
      void (*functionPtr)(Comp *, State *, State *, Clock **, int *),
      int phase);
    void *getInternalState(int *rc)const;
    int setInternalState(void *data);
};

class GridComp:public Comp{
  public:
    static GridComp *create(char const *name, enum GridCompType mtype,
      char const *configFile, ESMCI::Clock *clock, int *rc=NULL);
    static int destroy(GridComp *comp);
    int print(char const *options) const;
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
    int print(char const *options) const;
    int initialize(ESMCI::State *importState, ESMCI::State *exportState,
      ESMCI::Clock *clock, int phase, int *userRc) const;
    int run(ESMCI::State *importState, ESMCI::State *exportState,
      ESMCI::Clock *clock, int phase, int *userRc) const;
    int finalize(ESMCI::State *importState, ESMCI::State *exportState,
      ESMCI::Clock *clock, int phase, int *userRc) const;
};

} // namespace ESMCI

#endif  // ESMCI_Comp_H
