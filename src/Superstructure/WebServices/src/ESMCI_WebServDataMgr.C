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
#define ESMC_FILENAME "ESMCI_WebServDataMgr.C"
//==============================================================================
//
// ESMC WebServDataMgr method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ DataMgr methods declared
// in the companion file ESMCI_WebServDataMgr.h.  This code
// provides access methods to the CAM output file.
//
// (KDS: This code is very specific to the CCSM/CAM prototype that only
//       had to fetch a few specific values.  I don't think this is a
//       class we're going to want to keep in the ESMF baseline.)
//
//-----------------------------------------------------------------------------

#include "ESMCI_WebServDataMgr.h"

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
#define ESMC_METHOD "ESMCI_WebServDataMgr::ESMCI_WebServDataMgr()"
//BOPI
// !ROUTINE:  ESMCI_WebServDataMgr::ESMCI_WebServDataMgr()
//
// !INTERFACE:
ESMCI_WebServDataMgr::ESMCI_WebServDataMgr(
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
//    Creates and sets up the metadata for a set of grid-based data variables.
//
//EOPI
//-----------------------------------------------------------------------------
{
        theDataDesc = new ESMCI_WebServDataDesc(numVars,
                                           varNames,
                                           numLatValues,
                                           latValues,
                                           numLonValues,
                                           lonValues);
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServDataMgr::ESMCI_WebServDataMgr()"
//BOPI
// !ROUTINE:  ESMCI_WebServDataMgr::ESMCI_WebServDataMgr()
//
// !INTERFACE:
ESMCI_WebServDataMgr::ESMCI_WebServDataMgr(
//
//
// !ARGUMENTS:
//
  ESMCI_WebServDataDesc*   desc         // the data description
  )
//
// !DESCRIPTION:
//    Creates and sets up the metadata for a set of grid-based data variables.
//
//EOPI
//-----------------------------------------------------------------------------
{
        theDataDesc = new ESMCI_WebServDataDesc(desc->getNumVars(),
                                           desc->getVarNames(),
                                           desc->getNumLatValues(),
                                           desc->getLatValues(),
                                           desc->getNumLonValues(),
                                           desc->getLonValues());
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServDataMgr::~ESMCI_WebServDataMgr()"
//BOPI
// !ROUTINE:  ESMCI_WebServDataMgr::~ESMCI_WebServDataMgr()
//
// !INTERFACE:
ESMCI_WebServDataMgr::~ESMCI_WebServDataMgr(
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
        if (theDataDesc)
        {
                delete theDataDesc;
        }
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServDataMgr::getNumVars()"
//BOPI
// !ROUTINE:  ESMCI_WebServDataMgr::getNumVars()
//
// !INTERFACE:
int  ESMCI_WebServDataMgr::getNumVars(
//
// !RETURN VALUE:
//   int  number of variable names described in the data.
//
// !ARGUMENTS:
//
  )
//
// !DESCRIPTION:
//    Returns the number of variable names.
//
//EOPI
//-----------------------------------------------------------------------------
{
        int     numVars = 0;

        if (theDataDesc)
        {
                numVars = theDataDesc->getNumVars();
        }

        return numVars;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServDataMgr::getVarNames()"
//BOPI
// !ROUTINE:  ESMCI_WebServDataMgr::getVarNames()
//
// !INTERFACE:
string*  ESMCI_WebServDataMgr::getVarNames(
//
// !RETURN VALUE:
//   string*  array of strings containing the variable names.
//
// !ARGUMENTS:
//
  )
//
// !DESCRIPTION:
//    Returns and array of strings containing the variable names.
//
//EOPI
//-----------------------------------------------------------------------------
{
        string*         varNames = 0;

        if (theDataDesc)
        {
                varNames = theDataDesc->getVarNames();
        }

        return varNames;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServDataMgr::getNumLatValues()"
//BOPI
// !ROUTINE:  ESMCI_WebServDataMgr::getNumLatValues()
//
// !INTERFACE:
int  ESMCI_WebServDataMgr::getNumLatValues(
//
// !RETURN VALUE:
//   int  number of latitude values described in the data.
//
// !ARGUMENTS:
//
  )
//
// !DESCRIPTION:
//    Returns the number of latitude values.
//
//EOPI
//-----------------------------------------------------------------------------
{
        int     numLats = 0;

        if (theDataDesc)
        {
                numLats = theDataDesc->getNumLatValues();
        }

        return numLats;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServDataMgr::getLatValues()"
//BOPI
// !ROUTINE:  ESMCI_WebServDataMgr::getLatValues()
//
// !INTERFACE:
double*  ESMCI_WebServDataMgr::getLatValues(
//
// !RETURN VALUE:
//   double*  array of doubles containing the latitude values.
//
// !ARGUMENTS:
//
  )
//
// !DESCRIPTION:
//    Returns and array of doubles containing the latitude values.
//
//EOPI
//-----------------------------------------------------------------------------
{
        double*         latValues = 0;

        if (theDataDesc)
        {
                latValues = theDataDesc->getLatValues();
        }

        return latValues;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServDataMgr::getNumLonValues()"
//BOPI
// !ROUTINE:  ESMCI_WebServDataMgr::getNumLonValues()
//
// !INTERFACE:
int  ESMCI_WebServDataMgr::getNumLonValues(
//
// !RETURN VALUE:
//   int  number of longitude values described in the data.
//
// !ARGUMENTS:
//
  )
//
// !DESCRIPTION:
//    Returns the number of longitude values.
//
//EOPI
//-----------------------------------------------------------------------------
{
        int     numLons = 0;

        if (theDataDesc)
        {
                numLons = theDataDesc->getNumLonValues();
        }

        return numLons;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServDataMgr::getLonValues()"
//BOPI
// !ROUTINE:  ESMCI_WebServDataMgr::getLonValues()
//
// !INTERFACE:
double*  ESMCI_WebServDataMgr::getLonValues(
//
// !RETURN VALUE:
//   double*  array of doubles containing the longitude values.
//
// !ARGUMENTS:
//
  )
//
// !DESCRIPTION:
//    Returns and array of doubles containing the longitude values.
//
//EOPI
//-----------------------------------------------------------------------------
{
        double*         lonValues = 0;

        if (theDataDesc)
        {
                lonValues = theDataDesc->getLonValues();
        }

        return lonValues;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServDataMgr::getVarIndex()"
//BOPI
// !ROUTINE:  ESMCI_WebServDataMgr::getVarIndex()
//
// !INTERFACE:
int  ESMCI_WebServDataMgr::getVarIndex(
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
        int     varIndex = -1;

        if (theDataDesc)
        {
                varIndex = theDataDesc->getVarIndex(varName);
        }

        return varIndex;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServDataMgr::getLatIndex()"
//BOPI
// !ROUTINE:  ESMCI_WebServDataMgr::getLatIndex()
//
// !INTERFACE:
int  ESMCI_WebServDataMgr::getLatIndex(
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
        int     latIndex = -1;

        if (theDataDesc)
        {
                latIndex = theDataDesc->getLatIndex(latValue);
        }

        return latIndex;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServDataMgr::getLonIndex()"
//BOPI
// !ROUTINE:  ESMCI_WebServDataMgr::getLonIndex()
//
// !INTERFACE:
int  ESMCI_WebServDataMgr::getLonIndex(
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
        int     lonIndex = -1;

        if (theDataDesc)
        {
                lonIndex = theDataDesc->getLonIndex(lonValue);
        }

        return lonIndex;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServDataMgr::getDataValues()"
//BOPI
// !ROUTINE:  ESMCI_WebServDataMgr::getDataValues()
//
// !INTERFACE:
ESMCI_WebServDataContent*  ESMCI_WebServDataMgr::getDataValues(
//
// !RETURN VALUE:
//   ESMCI_WebServDataContent  data content for the specified timestamp
//
// !ARGUMENTS:
//
  double  timeStamp             // longitude value name of data value to lookup
  )
//
// !DESCRIPTION:
//    Looks up the data value for the specified timestamp.
//
//EOPI
//-----------------------------------------------------------------------------
{
        ESMCI_WebServDataContent*       retValue = NULL;

        if (theDataDesc)
        {
                int     numLatValues = theDataDesc->getNumLatValues();
                int     numLonValues = theDataDesc->getNumLonValues();

                retValue = new ESMCI_WebServDataContent(numLatValues, numLonValues);

                retValue->setTimeStamp(1.1);

                double*         dataValues = new double[numLatValues * numLonValues];
                for (int i = 0; i < numLatValues; ++i)
                {
                        for (int j = 0; j < numLonValues; ++j)
                        {
                                dataValues[(j * numLatValues) + i] =
                                        (double)(i + (double)(j / 100.0));
                        }
                }

                retValue->addDataValues("temp", dataValues);
                retValue->addDataValues("wind", dataValues);
                retValue->addDataValues("precip", dataValues);
                retValue->addDataValues("solarrad", dataValues);
                retValue->addDataValues("relhumid", dataValues);
        }

        return retValue;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServDataMgr::printSourceValues()"
//BOPI
// !ROUTINE:  ESMCI_WebServDataMgr::printSourceValues()
//
// !INTERFACE:
void  ESMCI_WebServDataMgr::printSourceValues(
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
        if (theDataDesc)
        {
                theDataDesc->print();

                list<ESMCI_WebServDataContent*>::iterator       iter = theDataValues.begin();
                ESMCI_WebServDataContent*                                               thisDataPtr;

                while (iter != theDataValues.end())
                {
                        thisDataPtr = *iter;
                        thisDataPtr->print();
                }
        }
}

} // end namespace
