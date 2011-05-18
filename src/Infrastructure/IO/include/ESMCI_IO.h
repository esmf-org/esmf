// $Id: ESMCI_IO.h,v 1.7 2011/05/18 15:51:26 samsoncheung Exp $
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

#include "ESMCI_Base.h"       // Base is superclass to ArrayBundle
#include "ESMCI_VM.h"
#include "ESMCI_Array.h"
#include "ESMCI_Container.h"

#include <vector>

//-------------------------------------------------------------------------

namespace ESMCI {

// classes and structs


class IO;

// class definition
class IO : public ESMC_Base {    // inherits from ESMC_Base class
  
  private:
    Container<std::string, Array *> dataContainer;
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
    IO(Array **dataList, int dataCount, int *rc);
  public:
    ~IO(){destruct(false);}
  private:
    int destruct(bool followCreator=true);
  public:
    // create() and destroy()
    static IO *create(Array **dataList, int dataCount, int *rc);
    static int destroy(IO **ioclass);

    // 
    void getVector(std::vector<Array *> &dataVector)const{
      dataContainer.getVector(dataVector);
    }
    int getCount()        const {return dataContainer.size();}
    int read(Array *array, char *file, char *variableName,
             int *timeslice, ESMC_IOFmtFlag *iofmt);
    int write(Array *array, char *file, char *variableName,
              bool *append, int *timeslice, ESMC_IOFmtFlag *iofmt);

};  // class IO

} // namespace ESMCI

#endif  // ESMCI_IOClass_H
