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
//==============================================================================
#define ESMC_FILENAME "ESMCI_WebServDataContent.C"
//==============================================================================
//
// ESMC WebServDataContent method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ DataContent methods declared
// in the companion file ESMCI_WebServDataContent.h.  This code
// provides access methods to the CAM output file.
//
// (KDS: This code is very specific to the CCSM/CAM prototype that only
//       had to fetch a few specific values.  I don't think this is a
//       class we're going to want to keep in the ESMF baseline.)
//
//-----------------------------------------------------------------------------

#include "ESMCI_WebServDataContent.h"

#include <stdlib.h>
#include <math.h>
#include <iostream>
#include <string.h>

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
#define ESMC_METHOD "ESMCI_WebServDataContent::ESMCI_WebServDataContent()"
//BOPI
// !ROUTINE:  ESMCI_WebServDataContent::ESMCI_WebServDataContent()
//
// !INTERFACE:
ESMCI_WebServDataContent::ESMCI_WebServDataContent(
//
//
// !ARGUMENTS:
//
  int           numLatValues,
  int           numLonValues
  )
//
// !DESCRIPTION:
//    Creates and sets up a container for a set of grid-based data variables
//    for the specified timestamp.
//
//EOPI
//-----------------------------------------------------------------------------
{
        theNumLatValues = numLatValues;
        theNumLonValues = numLonValues;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServDataContent::~ESMCI_WebServDataContent()"
//BOPI
// !ROUTINE:  ESMCI_WebServDataContent::~ESMCI_WebServDataContent()
//
// !INTERFACE:
ESMCI_WebServDataContent::~ESMCI_WebServDataContent(
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
#define ESMC_METHOD "ESMCI_WebServDataContent::setTimeStamp()"
//BOPI
// !ROUTINE:  ESMCI_WebServDataContent::setTimeStamp()
//
// !INTERFACE:
void  ESMCI_WebServDataContent::setTimeStamp(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  double  timestamp             // the timestamp for the data values
  )
//
// !DESCRIPTION:
//    Sets the timestamp for the data values.
//
//EOPI
//-----------------------------------------------------------------------------
{
        theTimeStamp = timestamp;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServDataContent::addDataValues()"
//BOPI
// !ROUTINE:  ESMCI_WebServDataContent::addDataValues()
//
// !INTERFACE:
void  ESMCI_WebServDataContent::addDataValues(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  string   varName,             // the variable name
  double*  dataValues   // array of doubles containing the data values
  )
//
// !DESCRIPTION:
//    Sets the timestamp for the data values.
//
//EOPI
//-----------------------------------------------------------------------------
{
        int             numValues = theNumLatValues * theNumLonValues;
        double*         valuesCopy = new double[numValues];

        memcpy(valuesCopy, dataValues, sizeof(double) * numValues);

        theDataValues[varName] = valuesCopy;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServDataContent::getDataValues()"
//BOPI
// !ROUTINE:  ESMCI_WebServDataContent::getDataValues()
//
// !INTERFACE:
double*  ESMCI_WebServDataContent::getDataValues(
//
// !RETURN VALUE:
//   double  data value for the specified variable, time, lat and lon
//
// !ARGUMENTS:
//
  string  varName               // variable name of data value to lookup
  )
//
// !DESCRIPTION:
//    Looks up the data value for the specified variable at the specified
//    time, latitude and longitude.
//
//EOPI
//-----------------------------------------------------------------------------
{
        double*         retValue = theDataValues[varName];

        return retValue;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServDataContent::getDataValue()"
//BOPI
// !ROUTINE:  ESMCI_WebServDataContent::getDataValue()
//
// !INTERFACE:
double  ESMCI_WebServDataContent::getDataValue(
//
// !RETURN VALUE:
//   double  data value for the specified variable, time, lat and lon
//
// !ARGUMENTS:
//
  string  varName,              // variable name of data value to lookup
  int     latValueIdx,          // latitude value name of data value to lookup
  int     lonValueIdx           // longitude value name of data value to lookup
  )
//
// !DESCRIPTION:
//    Looks up the data value for the specified variable at the specified
//    time, latitude and longitude.
//
//EOPI
//-----------------------------------------------------------------------------
{
        double  retValue = 0.0;

        return retValue;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServDataContent::print()"
//BOPI
// !ROUTINE:  ESMCI_WebServDataContent::print()
//
// !INTERFACE:
void  ESMCI_WebServDataContent::print(
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
        map<string, double*>::iterator  iter;
        double*                                                                         dataValues = NULL;

        for (iter = theDataValues.begin(); iter != theDataValues.end(); ++iter)
        {
                cout << "===========================================" << endl;
                cout << "Var Name: " << iter->first << endl;
                cout << "===========================================" << endl;

                dataValues = iter->second;

                for (int i = 0; i < theNumLatValues; ++i)
                {
                        for (int j = 0; j < theNumLonValues; ++j)
                        {
                                cout << dataValues[(j * theNumLatValues) + i] << " ";
                        }
                        cout << endl;
                }
                cout << endl;
        }
}

} // end namespace
