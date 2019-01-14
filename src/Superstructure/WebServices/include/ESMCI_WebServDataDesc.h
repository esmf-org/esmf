// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research,
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


#ifndef ESMCI_WebServDataDesc_H
#define ESMCI_WebServDataDesc_H

#include <string>
#include <map>

using namespace std;

//-------------------------------------------------------------------------
//BOPI
// !CLASS: ESMCI::ESMCI_WebServDataDesc
//
// !DESCRIPTION:
//
//
//EOPI
//-------------------------------------------------------------------------

namespace ESMCI
{

  class ESMCI_WebServDataDesc
  {
  public:

     // constructor and desructor
          ESMCI_WebServDataDesc(int      numVars,
                           string*  varNames,
                           int      numLatValues,
                           double*  latValues,
                           int      numLonValues,
                           double*  lonValues);
          ~ESMCI_WebServDataDesc();

     // methods to fetch data values
          int       getNumVars()                        { return theNumVars; }
     string*    getVarNames()                   { return theVarNames; }
          int       getNumLatValues()           { return theNumLatValues; }
          double*   getLatValues()                      { return theLatValues; }
          int       getNumLonValues()           { return theNumLonValues; }
          double*   getLonValues()                      { return theLonValues; }

     // methods to lookup index values for specific data arrays
          int  getVarIndex(string  varName);
          int  getLatIndex(double  latValue);
          int  getLonIndex(double  lonValue);

     // print method for debug purposes
          void  print();

  private:

          int                   theNumVars;                     // the number of data variables
          string*       theVarNames;            // array of data variable names
          int                   theNumLatValues;        // the number of latitude values
          double*       theLatValues;           // array of latitude values
          int                   theNumLonValues;        // the number longitude values
          double*       theLonValues;           // array of longitude values
  };

} // end namespace

#endif          // ESMCI_WebServDataDesc_H
