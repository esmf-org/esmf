// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2014, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMCI_WebServCAMOutputFile.C"
//==============================================================================
//
// ESMC WebServCAMOutputFile method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ CAMOutputFile methods declared
// in the companion file ESMCI_WebServCAMOutputFile.h.  This code
// provides access methods to the CAM output file.
//
// (KDS: This code is very specific to the CCSM/CAM prototype that only
//       had to fetch a few specific values.  I don't think this is a
//       class we're going to want to keep in the ESMF baseline.)
//
//-----------------------------------------------------------------------------
#include "ESMCI_WebServCAMOutputFile.h"

#include <stdlib.h>
#include <math.h>
#include <iostream>

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#define VERBOSITY             (1)       // 0: off, 10: max
//-----------------------------------------------------------------------------


namespace ESMCI
{

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServCAMOutputFile::ESMCI_WebServCAMOutputFile()"
//BOPI
// !ROUTINE:  ESMCI_WebServCAMOutputFile::ESMCI_WebServCAMOutputFile()
//
// !INTERFACE:
ESMCI_WebServCAMOutputFile::ESMCI_WebServCAMOutputFile(
//
//
// !ARGUMENTS:
//
  string  filename   // the CAM NetCDF output filename
  )
//
// !DESCRIPTION:
//    Opens up the CAM NetCDF ouptut file and reads in the relevant data.
//
//EOPI
//-----------------------------------------------------------------------------
{
#ifdef ESMF_NETCDF
	theNetCdfFile = new NcFile(filename.c_str(), NcFile::ReadOnly);

	NcVar*	latVar = theNetCdfFile->get_var("lat");
	NcDim*	latDim = latVar->get_dim(0);
	theNumLatValues = latDim->size();
	theLatValues = new double[theNumLatValues];
	latVar->get(theLatValues, theNumLatValues);
	//cout << "Lat dim size: " << theNumLatValues << endl;

	NcVar*	lonVar = theNetCdfFile->get_var("lon");
	NcDim*	lonDim = lonVar->get_dim(0);
	theNumLonValues = lonDim->size();
	theLonValues = new double[theNumLonValues];
	lonVar->get(theLonValues, theNumLonValues, 0, 0, 0, 0);
	//cout << "Lon dim size: " << theNumLonValues << endl;

	NcVar*	timeVar = theNetCdfFile->get_var("time");
	NcDim*	timeDim = timeVar->get_dim(0);
	theNumTimeValues = timeDim->size();
	theTimeValues = new double[theNumTimeValues];
	timeVar->get(theTimeValues, theNumTimeValues, 0, 0, 0, 0);
	//cout << "Time dim size: " << theNumTimeValues << endl;

	NcVar*	prectVar = theNetCdfFile->get_var("PRECT");
	thePRECTValues = new double[theNumLatValues * theNumLonValues];
	prectVar->get(thePRECTValues, theNumLatValues, theNumLonValues);

	NcVar*	tsVar = theNetCdfFile->get_var("TS");
	theTSValues = new double[theNumLatValues * theNumLonValues];
	tsVar->get(theTSValues, theNumLatValues, theNumLonValues);

	NcVar*	solinVar = theNetCdfFile->get_var("SOLIN");
	theSOLINValues = new double[theNumLatValues * theNumLonValues];
	solinVar->get(theSOLINValues, theNumLatValues, theNumLonValues);

	//***
	// Right now, I'm just getting the first level of relhum... eventually,
	// I'll need to make sure I'm getting the correct level
	//***
	NcVar*	relhumVar = theNetCdfFile->get_var("RELHUM");
	theRELHUMValues = new double[theNumLatValues * theNumLonValues];
	relhumVar->get(theRELHUMValues, 1, 1, theNumLatValues, theNumLonValues);

	NcVar*	ubotVar = theNetCdfFile->get_var("UBOT");
	theUBOTValues = new double[theNumLatValues * theNumLonValues];
	ubotVar->get(theUBOTValues, theNumLatValues, theNumLonValues);

	NcVar*	vbotVar = theNetCdfFile->get_var("VBOT");
	theVBOTValues = new double[theNumLatValues * theNumLonValues];
	vbotVar->get(theVBOTValues, theNumLatValues, theNumLonValues);
#else
   theNumLatValues = 0;
	theLatValues = NULL;
	theNumLonValues = 0;
	theLonValues = NULL;
	theNumTimeValues = 0;
	theTimeValues = NULL;

	thePRECTValues = NULL;
	theTSValues = NULL;
	theSOLINValues = NULL;
	theRELHUMValues = NULL;
	theUBOTValues = NULL;
	theVBOTValues = NULL;
#endif
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServCAMOutputFile::~ESMCI_WebServCAMOutputFile()"
//BOPI
// !ROUTINE:  ESMCI_WebServCAMOutputFile::~ESMCI_WebServCAMOutputFile()
//
// !INTERFACE:
ESMCI_WebServCAMOutputFile::~ESMCI_WebServCAMOutputFile(
//
//
// !ARGUMENTS:
//
  )
//
// !DESCRIPTION:
//    Cleans up memory allocated for CAM output file.
//
//EOPI
//-----------------------------------------------------------------------------
{
	// need to free up memory here
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServCAMOutputFile::getTimeIndex()"
//BOPI
// !ROUTINE:  ESMCI_WebServCAMOutputFile::getTimeIndex()
//
// !INTERFACE:
int  ESMCI_WebServCAMOutputFile::getTimeIndex(
//
// !RETURN VALUE:
//   int  index of specified time value in array of time values
//
// !ARGUMENTS:
//
  double  timeValue	// the time value to lookup in the array of time values
  )
//
// !DESCRIPTION:
//    Looks up the index of the specified time value in the array of time
//    values read from the CAM output file.
//
//EOPI
//-----------------------------------------------------------------------------
{
	//***
	// Right now, the model values I'm working with deal with just 1 day in 
	// CAM, so I can assume this value will always be zero.
	// NOTE: This needs to be done properly asap
	//***
	return 0;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServCAMOutputFile::getLatIndex()"
//BOPI
// !ROUTINE:  ESMCI_WebServCAMOutputFile::getLatIndex()
//
// !INTERFACE:
int  ESMCI_WebServCAMOutputFile::getLatIndex(
//
// !RETURN VALUE:
//   int  index of specified lat value in array of lat values; -1 if there's
//        a problem.
//
// !ARGUMENTS:
//
  double  latValue	// the lat value to lookup in the array of lat values
  )
//
// !DESCRIPTION:
//    Looks up the index of the specified lat value in the array of latitude
//    values read from the CAM output file.
//
//EOPI
//-----------------------------------------------------------------------------
{
	//***
	// Make sure that the lat values were loaded
	//***
	if (theNumLatValues == 0)
	{
		return -1;
	}

	//***
	// Now, make sure the value to look for is valid
	//***
	int	latIndex = 0;

	if ((latValue < -90)  ||  (latValue > 90))
	{
		// This is not a valid latitude
		return -1;
	}

	//***
	// Right now, I can assume that the lat values are in ascending order, so
	// I just need to find the 2 lats that surround this lat value, and then
	// pick the one that's closer.
	//***
	double	lowValue = theLatValues[0];
	int		valueIdx = 1;

	while ((valueIdx < theNumLatValues)  &&  (theLatValues[valueIdx] < latValue))
	{
		lowValue = theLatValues[valueIdx];
		++valueIdx;
	}

	double	highValue = theLatValues[valueIdx];

	if (abs((long int)(latValue - lowValue)) > 
       abs((long int)(highValue - latValue)))
	{
		latIndex = valueIdx;
	}
	else
	{
		latIndex = valueIdx - 1;
	}

	return latIndex;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServCAMOutputFile::getLonIndex()"
//BOPI
// !ROUTINE:  ESMCI_WebServCAMOutputFile::getLonIndex()
//
// !INTERFACE:
int  ESMCI_WebServCAMOutputFile::getLonIndex(
//
// !RETURN VALUE:
//   int  index of specified lon value in array of lon values; -1 if there's
//        a problem.
//
// !ARGUMENTS:
//
  double  lonValue	// the lon value to lookup in the array of lon values
  )
//
// !DESCRIPTION:
//    Looks up the index of the specified lon value in the array of longitude
//    values read from the CAM output file.
//
//EOPI
//-----------------------------------------------------------------------------
{
	//***
	// Make sure that the lon values were loaded
	//***
	if (theNumLonValues == 0)
	{
		return -1;
	}

	//***
	// Now, make sure the value to look for is valid
	//***
	int	lonIndex = 0;

	if ((lonValue < 0)  ||  (lonValue > 360))
	{
		// This is not a valid latitude
		return -1;
	}

	//***
	// Right now, I can assume that the lon values are in ascending order, so
	// I just need to find the 2 lons that surround this lon value, and then
	// pick the one that's closer.
	//***
	double	lowValue = theLonValues[0];
	int		valueIdx = 1;

	while ((valueIdx < theNumLonValues)  &&  (theLonValues[valueIdx] < lonValue))
	{
		lowValue = theLonValues[valueIdx];
		++valueIdx;
	}

	//***
	// Because the CAM Output does not use the upper value for longitudes, 
	// which is the same as zero, we have to adjust the algorithm a bit
	//***
	double	highValue = 0;
	if (valueIdx == theNumLonValues)
	{
		highValue = 360;
	}
	else
	{
		highValue = theLonValues[valueIdx];
	}

	if (abs((long int)(lonValue - lowValue)) > 
       abs((long int)(highValue - lonValue)))
	{
		if (valueIdx == theNumLonValues)
		{
			lonIndex = 0;
		}
		else
		{
			lonIndex = valueIdx;
		}
	}
	else
	{
		lonIndex = valueIdx - 1;
	}

	return lonIndex;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServCAMOutputFile::getDataValue()"
//BOPI
// !ROUTINE:  ESMCI_WebServCAMOutputFile::getDataValue()
//
// !INTERFACE:
double  ESMCI_WebServCAMOutputFile::getDataValue(
//
// !RETURN VALUE:
//   double  data value for the specified variable, time, lat and lon
//
// !ARGUMENTS:
//
  string  varName,		// variable name of data value to lookup
  double  timeValue,		// time value of data value to lookup
  double  latValue,		// latitude value name of data value to lookup
  double  lonValue		// longitude value name of data value to lookup
  )
//
// !DESCRIPTION:
//    Looks up the data value for the specified variable at the specified
//    time, latitude and longitude.
//
//EOPI
//-----------------------------------------------------------------------------
{
	double	retValue = 0.0;

	if (varName.compare("temp") == 0)
	{
		retValue = getAirTemp(timeValue, latValue, lonValue);
	}
	else if (varName.compare("precip") == 0)
	{
		retValue = getPrecip(timeValue, latValue, lonValue);
	}
	else if (varName.compare("windspeed") == 0)
	{
		retValue = getWindSpeed(timeValue, latValue, lonValue);
	}
	else if (varName.compare("solarrad") == 0)
	{
		retValue = getSolarRad(timeValue, latValue, lonValue);
	}
	else if (varName.compare("relhumid") == 0)
	{
		retValue = getRelHumid(timeValue, latValue, lonValue);
	}

	return retValue;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServCAMOutputFile::getAirTemp()"
//BOPI
// !ROUTINE:  ESMCI_WebServCAMOutputFile::getAirTemp()
//
// !INTERFACE:
double  ESMCI_WebServCAMOutputFile::getAirTemp(
//
// !RETURN VALUE:
//   double  air temperature value for the specified time, lat and lon
//
// !ARGUMENTS:
//
  double  timeValue,		// time value of data value to lookup
  double  latValue,		// latitude value name of data value to lookup
  double  lonValue		// longitude value name of data value to lookup
  )
//
// !DESCRIPTION:
//    Looks up the air temperature value for the specified time, latitude
//    and longitude.
//
//EOPI
//-----------------------------------------------------------------------------
{
	double	retValue = 0.0;

	//***
	// Make sure the TS Values were loaded
	//***
	if (theTSValues == NULL)
	{
		return retValue;
	}

	int	latIdx = getLatIndex(latValue);
	int	lonIdx = getLonIndex(lonValue);

	if ((latIdx >= 0)  &&  (lonIdx >= 0))
	{
		double	tsValue = theTSValues[(latIdx * theNumLonValues) + lonIdx];
		retValue = tsValue - 273.15;
	}

	return retValue;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServCAMOutputFile::getWindSpeed()"
//BOPI
// !ROUTINE:  ESMCI_WebServCAMOutputFile::getWindSpeed()
//
// !INTERFACE:
double  ESMCI_WebServCAMOutputFile::getWindSpeed(
//
// !RETURN VALUE:
//   double  wind speed value for the specified time, lat and lon
//
// !ARGUMENTS:
//
  double  timeValue,		// time value of data value to lookup
  double  latValue,		// latitude value name of data value to lookup
  double  lonValue		// longitude value name of data value to lookup
  )
//
// !DESCRIPTION:
//    Looks up the wind speed value for the specified time, latitude
//    and longitude.
//
//EOPI
//-----------------------------------------------------------------------------
{
	double	retValue = 0.0;

	//***
	// Make sure the UBOT and VBOT Values were loaded
	//***
	if ((theUBOTValues == NULL)  ||  (theVBOTValues == NULL))
	{
		return retValue;
	}

	int	latIdx = getLatIndex(latValue);
	int	lonIdx = getLonIndex(lonValue);

	if ((latIdx >= 0)  &&  (lonIdx >= 0))
	{
		double	ubotValue = theUBOTValues[(latIdx * theNumLonValues) + lonIdx];
		double	vbotValue = theVBOTValues[(latIdx * theNumLonValues) + lonIdx];
		retValue = sqrt(pow(ubotValue, 2) + pow(vbotValue, 2));
	}

	return retValue;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServCAMOutputFile::getPrecip()"
//BOPI
// !ROUTINE:  ESMCI_WebServCAMOutputFile::getPrecip()
//
// !INTERFACE:
double  ESMCI_WebServCAMOutputFile::getPrecip(
//
// !RETURN VALUE:
//   double  precipitation value for the specified time, lat and lon
//
// !ARGUMENTS:
//
  double  timeValue,		// time value of data value to lookup
  double  latValue,		// latitude value name of data value to lookup
  double  lonValue		// longitude value name of data value to lookup
  )
//
// !DESCRIPTION:
//    Looks up the precipitation value for the specified time, latitude
//    and longitude.
//
//EOPI
//-----------------------------------------------------------------------------
{
	double	retValue = 0.0;

	//***
	// Make sure the PRECT Values were loaded
	//***
	if (thePRECTValues == NULL)
	{
		return retValue;
	}

	int	latIdx = getLatIndex(latValue);
	int	lonIdx = getLonIndex(lonValue);

	if ((latIdx >= 0)  &&  (lonIdx >= 0))
	{
		double	prectValue = thePRECTValues[(latIdx * theNumLonValues) + lonIdx];
		retValue = prectValue * 86400 * 1000;
	}

	return retValue;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServCAMOutputFile::getSolarRad()"
//BOPI
// !ROUTINE:  ESMCI_WebServCAMOutputFile::getSolarRad()
//
// !INTERFACE:
double  ESMCI_WebServCAMOutputFile::getSolarRad(
//
// !RETURN VALUE:
//   double  solar radiation value for the specified time, lat and lon
//
// !ARGUMENTS:
//
  double  timeValue,		// time value of data value to lookup
  double  latValue,		// latitude value name of data value to lookup
  double  lonValue		// longitude value name of data value to lookup
  )
//
// !DESCRIPTION:
//    Looks up the solar radiation value for the specified time, latitude
//    and longitude.
//
//EOPI
//-----------------------------------------------------------------------------
{
	double	retValue = 0.0;

	//***
	// Make sure the SOLIN Values were loaded
	//***
	if (theSOLINValues == NULL)
	{
		return retValue;
	}

	int	latIdx = getLatIndex(latValue);
	int	lonIdx = getLonIndex(lonValue);

	if ((latIdx >= 0)  &&  (lonIdx >= 0))
	{
		double	solinValue = theSOLINValues[(latIdx * theNumLonValues) + lonIdx];
		retValue = solinValue * 86400 * 10;
	}

	return retValue;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServCAMOutputFile::getRelHumid()"
//BOPI
// !ROUTINE:  ESMCI_WebServCAMOutputFile::getRelHumid()
//
// !INTERFACE:
double  ESMCI_WebServCAMOutputFile::getRelHumid(
//
// !RETURN VALUE:
//   double  relative humidity value for the specified time, lat and lon
//
// !ARGUMENTS:
//
  double  timeValue,		// time value of data value to lookup
  double  latValue,		// latitude value name of data value to lookup
  double  lonValue		// longitude value name of data value to lookup
  )
//
// !DESCRIPTION:
//    Looks up the relative humidity value for the specified time, latitude
//    and longitude.
//
//EOPI
//-----------------------------------------------------------------------------
{
	double	retValue = 0.0;

	//***
	// Make sure the RELHUM Values were loaded
	//***
	if (theRELHUMValues == NULL)
	{
		return retValue;
	}

	int	latIdx = getLatIndex(latValue);
	int	lonIdx = getLonIndex(lonValue);

	if ((latIdx >= 0)  &&  (lonIdx >= 0))
	{
		double	relHumValue = 
						theRELHUMValues[(latIdx * theNumLonValues) + lonIdx];
		retValue = relHumValue / 100;
	}

	return retValue;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServCAMOutputFile::printSourceValues()"
//BOPI
// !ROUTINE:  ESMCI_WebServCAMOutputFile::printSourceValues()
//
// !INTERFACE:
void  ESMCI_WebServCAMOutputFile::printSourceValues(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  )
//
// !DESCRIPTION:
//    Prints out the data values (including time, lat and lon) as they were
//    read from the output file.
//
//EOPI
//-----------------------------------------------------------------------------
{
	for (int i = 0; i < theNumLatValues; ++i)
	{
		cout << "Lat value[" << i << "]: " << theLatValues[i] << endl;
	}

	for (int i = 0; i < theNumLonValues; ++i)
	{
		cout << "Lon value[" << i << "]: " << theLonValues[i] << endl;
	}

	for (int i = 0; i < theNumTimeValues; ++i)
	{
		cout << "Time value[" << i << "]: " << theTimeValues[i] << endl;
	}

	for (int i = 0; i < theNumLatValues; ++i)
	{
		for (int j = 0; j < theNumLonValues; ++j)
		{
			cout << thePRECTValues[(i * theNumLonValues) + j] << "  ";
		}
		cout << endl;
	}

	for (int i = 0; i < theNumLatValues; ++i)
	{
		for (int j = 0; j < theNumLonValues; ++j)
		{
			cout << theTSValues[(i * theNumLonValues) + j] << "  ";
		}
		cout << endl;
	}

	for (int i = 0; i < theNumLatValues; ++i)
	{
		for (int j = 0; j < theNumLonValues; ++j)
		{
			cout << theSOLINValues[(i * theNumLonValues) + j] << "  ";
		}
		cout << endl;
	}

	for (int i = 0; i < theNumLatValues; ++i)
	{
		for (int j = 0; j < theNumLonValues; ++j)
		{
			cout << theRELHUMValues[(i * theNumLonValues) + j] << "  ";
		}
		cout << endl;
	}

	for (int i = 0; i < theNumLatValues; ++i)
	{
		for (int j = 0; j < theNumLonValues; ++j)
		{
			cout << theUBOTValues[(i * theNumLonValues) + j] << "  ";
		}
		cout << endl;
	}

	for (int i = 0; i < theNumLatValues; ++i)
	{
		for (int j = 0; j < theNumLonValues; ++j)
		{
			cout << theVBOTValues[(i * theNumLonValues) + j] << "  ";
		}
		cout << endl;
	}
}

} // end namespace
