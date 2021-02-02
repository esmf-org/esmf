// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research,
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


#ifndef ESMCI_WebServDataMgr_H
#define ESMCI_WebServDataMgr_H

#include <string>
#include <list>
#include "ESMCI_WebServDataDesc.h"
#include "ESMCI_WebServDataContent.h"

using namespace std;

//-------------------------------------------------------------------------
//BOPI
// !CLASS: ESMCI::ESMCI_WebServClientInfo
//
// !DESCRIPTION:
//
// The code in this file defines the C++ ClientInfo members and method
// signatures (prototypes).  The companion file ESMCI\_WebServClientInfo.C
// contains the full code (bodies) for the ClientInfo methods.
//
// This class provides access to a CAM output file.  It reads the relevant
// data variables when constructed and provides access methods to retrieve
// the values based on time, latitude, and longitude.
//
// (KDS: This code is very specific to the CCSM/CAM prototype that only
//       had to fetch a few specific values.  I don't think this is a
//       class we're going to want to keep in the ESMF baseline.)
//
//EOPI
//-------------------------------------------------------------------------

namespace ESMCI
{

  class ESMCI_WebServDataMgr
  {
  public:

     // constructor and desructor
          ESMCI_WebServDataMgr(int      numVars,
                          string*  varNames,
                          int      numLatValues,
                          double*  latValues,
                          int      numLonValues,
                          double*  lonValues);
          ESMCI_WebServDataMgr(ESMCI_WebServDataDesc*  desc);
          ~ESMCI_WebServDataMgr();

     // methods to fetch data values
     int                        getNumVars();
     string*    getVarNames();
     int                        getNumLatValues();
     double*    getLatValues();
     int                        getNumLonValues();
     double*    getLonValues();
                
          ESMCI_WebServDataContent*     getDataValues(double  timeStamp);

     // methods to lookup index values for specific data arrays
          int  getVarIndex(string  varName);
          int  getLatIndex(double  latValue);
          int  getLonIndex(double  lonValue);

     // print method for debug purposes
          void  printSourceValues();

  private:

          ESMCI_WebServDataDesc*        theDataDesc;            // description of the data

          list<ESMCI_WebServDataContent*>       theDataValues;  
                                                                                        // the output data values... one
                                      // entry in the queue for each timestep
  };

} // end namespace

#endif          // ESMCI_WebServDataMgr_H
