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
//-------------------------------------------------------------------------
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times


#ifndef ESMCI_WebServDataContent_H
#define ESMCI_WebServDataContent_H

#include <string>
#include <map>

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

  class ESMCI_WebServDataContent 
  {
  public:

     // constructor and desructor
	  ESMCI_WebServDataContent(int  numLatValues, 
                              int  numLonValues);
	  ~ESMCI_WebServDataContent();

     // methods to set data values
	  void   setTimeStamp(double  timestamp);
     void	addDataValues(string   varName,
                          double*  dataValues);

     // methods to fetch data values
	  double   getTimeStamp()	{ return theTimeStamp; }
	  double*  getDataValues(string  varName);
	  double   getDataValue(string  varName,
                           int     latValueIdx, 
                           int     lonValueIdx);
  
     // print method for debug purposes
	  void  print();

  private:

     int							theNumLatValues;
     int							theNumLonValues;
     double						theTimeStamp;
	  map<string, double*>	theDataValues;	// the output data values... one 
	                                       // entry in the map for each variable
  };

} // end namespace

#endif 	// ESMCI_WebServDataContent_H
