// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//-------------------------------------------------------------------------
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMCI_State_H
#define ESMCI_State_H

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_State - one line general Statement about this class
//
// !DESCRIPTION:
//
// The code in this file defines the ESMC State class prototypes for the
// Fortran interface routines. The companion file ESMC\_State_C.F90  contains
// the definitions (full code bodies) for the interface routines.
//
// 
//

//-----------------------------------------------------------------------------
// 
// !USES:
#include "ESMCI_Array.h"
#include "ESMCI_Field.h"
#include "ESMCI_F90Interface.h"

#include <string>

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
// C++ State class declaration
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
namespace ESMCI{

  typedef enum  ESMC_StateItemType
  {
    ESMC_STATEITEM_FIELD = 101,
    ESMC_STATEITEM_FIELDBUNDLE,
    ESMC_STATEITEM_ARRAY,
    ESMC_STATEITEM_ARRAYBUNDLE,
    ESMC_STATEITEM_ROUTEHANDLE,
    ESMC_STATEITEM_STATE,
    ESMC_STATEITEM_NAME,
    ESMC_STATEITEM_INDIRECT,
    ESMC_STATEITEM_UNKNOWN,
    ESMC_STATEITEM_NOTFOUND
  } ESMC_StateItemType;


  class State{
    // pointer to fortran derived type
    F90ClassHolder fortranclass;
    // methods
    public:
    static State* create(const char* name, int *rc);
    int addArray(Array *array);
    int addField(Field *field);
    int print();
    int getArray(const char* name, Array **array);
    int getField(const char* name, Field **field);
    static int destroy(State *state);

    int getNumItems(int* numItems);
    std::vector<std::string>  getItemNames();

    int getNumItems(int* numItems, ESMC_StateItemType  itemType);
    std::vector<std::string>  getItemNames(ESMC_StateItemType  itemType);

    int getNumArrays(int* numArrays);
    std::vector<std::string>  getArrayNames();

    int read(ESMC_Base* base, const std::string &fileName);
    int write(ESMC_Base* base, const std::string &fileName);

  }; // class State
};// namespace ESMCI


#endif  // ESMCI_State_H
