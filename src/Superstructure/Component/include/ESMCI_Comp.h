// $Id: ESMCI_Comp.h,v 1.5 2008/08/26 20:46:48 theurich Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2008, University Corporation for Atmospheric Research, 
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


#include "ESMC_F90Interface.h"
#include "ESMCI_State.h"
#include "ESMCI_Clock.h"
#include "ESMCI_Grid.h"


namespace ESMCI {

// constants and enums

enum CompType { COMPTYPE_GRID=1, COMPTYPE_CPL, COMPTYPE_UNKNOWN };
enum GridCompType { ATM=1, LAND, OCEAN, SEAICE, RIVER, UNKNOWN };


// class definition
class Comp{
  private:
    ESMC_F90ClassHolder fortranclass;
  public:
    int setServices(void (*func)(Comp *, int *));
    int setEntryPoint(const char *functionType,
      void (*functionPtr)(Comp *, State *, State *, Clock *), int phase);
};

class GridComp:public Comp{
  public:
    static GridComp *create(char *name, enum GridCompType mtype,
      char *configFile, ESMCI::Clock *clock, int *rc=NULL);
    static int destroy(GridComp *comp);
    int print(const char *options) const;
};

} // namespace ESMCI

#endif  // ESMCI_Comp_H
