// $Id: ESMCI_IO.h,v 1.8 2011/05/25 23:58:23 samsoncheung Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2011, University Corporation for Atmospheric Research, 
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

#ifndef ESMCI_IO_H
#define ESMCI_IO_H

//-------------------------------------------------------------------------
//BOPI
// !CLASS: ESMCI::IOClass - IOClass
//
// !DESCRIPTION:
//
// The code in this file defines the C++ {\tt IOClass} members and method
// signatures (prototypes).  The companion file {\tt ESMCI\_IOClass.C}
// contains the full code (bodies) for the {\tt IOClass} methods.
//
//EOPI
//-------------------------------------------------------------------------

#include <ESMC_Util.h>

#include "ESMCI_Base.h"       // Base is superclass to ArrayBundle
#include "ESMCI_VM.h"
#include "ESMCI_ArrayBundle.h"
#include "ESMCI_Container.h"

#include <vector>

//-------------------------------------------------------------------------

namespace ESMCI {

// classes and structs


class IO;

// class definition
class IO : public ESMC_Base {    // inherits from ESMC_Base class
  
  private:
    ESMCI::ArrayBundle *dataContainer;
    bool dataCreator;
  
  public:
    // constructor and destructor
    IO(){
      dataContainer;
      dataCreator = false;
    }
    IO(int baseID):ESMC_Base(baseID){// prevent baseID counter incr.
      dataContainer;
      dataCreator = false;
    }

  private:
    IO(ArrayBundle **dataList, int dataCount, int *rc);
  public:
    ~IO(){destruct(false);}
  private:
    int destruct(bool followCreator=true);
  public:
    // create() and destroy()
    static IO *create(ArrayBundle **dataList, int dataCount, int *rc);
    static int destroy(IO **ioclass);

    // 
    static int read(Array *array, char *file, char *variableName,
             int *timeslice, ESMC_IOFmtFlag *iofmt);
    static int write(Array *array, char *file, char *variableName,
               bool *append, int *timeslice, ESMC_IOFmtFlag *iofmt);

};  // class IO

} // namespace ESMCI

#endif  // ESMCI_IOClass_H
