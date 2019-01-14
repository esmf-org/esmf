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
//==============================================================================
#define ESMC_FILENAME "ESMCI_WebServDataDesc.C"
//==============================================================================
//
// ESMC WebServDataDesc method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
//
//-----------------------------------------------------------------------------

#include "ESMCI_WebServDataDesc.h"

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
#define ESMC_METHOD "ESMCI_WebServDataDesc::ESMCI_WebServDataDesc()"
//BOPI
// !ROUTINE:  ESMCI_WebServDataDesc::ESMCI_WebServDataDesc()
//
// !INTERFACE:
ESMCI_WebServDataDesc::ESMCI_WebServDataDesc(
//
//
// !ARGUMENTS:
//
  int           numVars,                        // the number of variables in the set of data
  string*       varNames,       // the names of the variables contained in the set
  int           numLatValues,   // the number of latitudes for the data
  double*       latValues,      // the latitude values for the data
  int           numLonValues,   // the number of longitudes for the data
  double*       lonValues       // the longitude values for the data
  )
//
// !DESCRIPTION:
//    Creates and sets up a container for a set of grid-based data variables
//    for the specified timestamp.
//
//EOPI
//-----------------------------------------------------------------------------
{
        theNumVars      = numVars;
        theVarNames     = new string[numVars];
        for (int i = 0; i < numVars; ++i)
        {
                theVarNames[i] = varNames[i];
        }

   theNumLatValues = numLatValues;
        theLatValues    = new double[numLatValues];
        for (int i = 0; i < numLatValues; ++i)
        {
                theLatValues[i] = latValues[i];
        }

        theNumLonValues = numLonValues;
        theLonValues    = new double[numLonValues];;
        for (int i = 0; i < numLonValues; ++i)
        {
                theLonValues[i] = lonValues[i];
        }
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServDataDesc::~ESMCI_WebServDataDesc()"
//BOPI
// !ROUTINE:  ESMCI_WebServDataDesc::~ESMCI_WebServDataDesc()
//
// !INTERFACE:
ESMCI_WebServDataDesc::~ESMCI_WebServDataDesc(
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
        if (theVarNames)
        {
                delete theVarNames;
        }

        if (theLatValues)
        {
                delete theLatValues;
        }

        if (theLonValues)
        {
                delete theLonValues;
        }
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServDataDesc::getVarIndex()"
//BOPI
// !ROUTINE:  ESMCI_WebServDataDesc::getVarIndex()
//
// !INTERFACE:
int  ESMCI_WebServDataDesc::getVarIndex(
//
// !RETURN VALUE:
//   int  index of specified time value in array of time values
//
// !ARGUMENTS:
//
  string  varName       // the variable name to lookup in the array of names
  )
//
// !DESCRIPTION:
//    Looks up the index of the specified variable name in the array of
//    variable names.
//
//EOPI
//-----------------------------------------------------------------------------
{
        return -1;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServDataDesc::getLatIndex()"
//BOPI
// !ROUTINE:  ESMCI_WebServDataDesc::getLatIndex()
//
// !INTERFACE:
int  ESMCI_WebServDataDesc::getLatIndex(
//
// !RETURN VALUE:
//   int  index of specified lat value in array of lat values; -1 if there's
//        a problem.
//
// !ARGUMENTS:
//
  double  latValue      // the lat value to lookup in the array of lat values
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
        int     latIndex = 0;

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
        double  lowValue = theLatValues[0];
        int             valueIdx = 1;

        while ((valueIdx < theNumLatValues)  &&  (theLatValues[valueIdx] < latValue))
        {
                lowValue = theLatValues[valueIdx];
                ++valueIdx;
        }

        double  highValue = theLatValues[valueIdx];

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
#define ESMC_METHOD "ESMCI_WebServDataDesc::getLonIndex()"
//BOPI
// !ROUTINE:  ESMCI_WebServDataDesc::getLonIndex()
//
// !INTERFACE:
int  ESMCI_WebServDataDesc::getLonIndex(
//
// !RETURN VALUE:
//   int  index of specified lon value in array of lon values; -1 if there's
//        a problem.
//
// !ARGUMENTS:
//
  double  lonValue      // the lon value to lookup in the array of lon values
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
        int     lonIndex = 0;

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
        double  lowValue = theLonValues[0];
        int             valueIdx = 1;

        while ((valueIdx < theNumLonValues)  &&  (theLonValues[valueIdx] < lonValue))
        {
                lowValue = theLonValues[valueIdx];
                ++valueIdx;
        }

        //***
        // Because the CAM Output does not use the upper value for longitudes,
        // which is the same as zero, we have to adjust the algorithm a bit
        //***
        double  highValue = 0;
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
#define ESMC_METHOD "ESMCI_WebServDataDesc::print()"
//BOPI
// !ROUTINE:  ESMCI_WebServDataDesc::print()
//
// !INTERFACE:
void  ESMCI_WebServDataDesc::print(
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
        cout << "*** Number of Variables: " << theNumVars << endl;
        for (int i = 0; i < theNumVars; ++i)
        {
                cout << "Var Name[" << i << "]: " << theVarNames[i] << endl;
        }

        cout << "*** Number of Latitudes: " << theNumLatValues << endl;
        for (int i = 0; i < theNumLatValues; ++i)
        {
                cout << "Lat value[" << i << "]: " << theLatValues[i] << endl;
        }

        cout << "*** Number of Longitudes: " << theNumLonValues << endl;
        for (int i = 0; i < theNumLonValues; ++i)
        {
                cout << "Lon value[" << i << "]: " << theLonValues[i] << endl;
        }
}

} // end namespace
