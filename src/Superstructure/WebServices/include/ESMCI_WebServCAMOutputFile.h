// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2013, University Corporation for Atmospheric Research,
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


#ifndef ESMCI_WebServCAMOutputFile_H
#define ESMCI_WebServCAMOutputFile_H

#include <string>

#ifdef ESMF_NETCDF
#include <netcdfcpp.h>
#endif

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

  class ESMCI_WebServCAMOutputFile 
  {
  public:

     // constructor and desructor
	  ESMCI_WebServCAMOutputFile(string  filename);
	  ~ESMCI_WebServCAMOutputFile();

     // methods to fetch data values
	  double  getDataValue(string  varName,
	                       double  timeValue, 
                          double  latValue, 
                          double  lonValue);

	  double  getAirTemp(double  timeValue, 
                        double  latValue, 
                        double  lonValue);

	  double  getWindSpeed(double  timeValue, 
                          double  latValue, double  lonValue);

	  double  getPrecip(double  timeValue, 
                       double  latValue, 
                       double  lonValue);

	  double  getSolarRad(double  timeValue, 
                         double  latValue, 
                         double  lonValue);

	  double  getRelHumid(double  timeValue, 
                         double  latValue, 
                         double  lonValue);

  private:

     // methods to lookup index values for specific data arrays
	  int  getTimeIndex(double  timeValue);
	  int  getLatIndex(double  latValue);
	  int  getLonIndex(double  lonValue);
  
     // print method for debug purposes
	  void  printSourceValues();

#ifdef ESMF_NETCDF
	  NcFile*	theNetCdfFile;		// the CAM output file NetCDF pointer
#endif

	  int			theNumLatValues;	// the number of latitude values
	  double*	theLatValues;		// array of latitude values
	  int			theNumLonValues;	// the number longitude values
	  double*	theLonValues;		// array of longitude values
	  int			theNumTimeValues;	// the number time values
	  double*	theTimeValues;		// array of time values

	  double*	thePRECTValues;	// the precipitations values
	  double*	theTSValues;		// the temperature values
	  double*	theSOLINValues;	// the solar radiation values
	  double*	theRELHUMValues;	// the relative humidity values
	  double*	theUBOTValues;		// the UBOT values (used for wind speed)
	  double*	theVBOTValues;		// the VBOT values (used for wind speed)
  };

} // end namespace

#endif 	// ESMCI_WebServCAMOutputFile_H
