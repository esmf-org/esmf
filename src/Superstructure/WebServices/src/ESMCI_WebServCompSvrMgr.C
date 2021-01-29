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
//==============================================================================
#define ESMC_FILENAME "ESMCI_WebServCompSvrMgr.C"
//==============================================================================
//
// ESMC WebServCompSvrMgr method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ CompSvrMgr methods declared
// in the companion file ESMCI_WebServCompSvrMgr.h.  This code
// provides access methods to the CAM output file.
//
// (KDS: This code is very specific to the CCSM/CAM prototype that only
//       had to fetch a few specific values.  I don't think this is a
//       class we're going to want to keep in the ESMF baseline.)
//
//-----------------------------------------------------------------------------

#include "ESMCI_WebServCompSvrMgr.h"

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
#define ESMC_METHOD "ESMCI_WebServCompSvrMgr::ESMCI_WebServCompSvrMgr()"
//BOPI
// !ROUTINE:  ESMCI_WebServCompSvrMgr::ESMCI_WebServCompSvrMgr()
//
// !INTERFACE:
ESMCI_WebServCompSvrMgr::ESMCI_WebServCompSvrMgr(
//
//
// !ARGUMENTS:
//
  string                hostName,
  string                scriptDir,
  string                scriptName
  )
//
// !DESCRIPTION:
//    Instantiates a Component Service Manager, setting the system information.
//
//EOPI
//-----------------------------------------------------------------------------
{
        theHostName = hostName;
        theScriptDir = scriptDir;
        theScriptName = scriptName;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServCompSvrMgr::~ESMCI_WebServCompSvrMgr()"
//BOPI
// !ROUTINE:  ESMCI_WebServCompSvrMgr::~ESMCI_WebServCompSvrMgr()
//
// !INTERFACE:
ESMCI_WebServCompSvrMgr::~ESMCI_WebServCompSvrMgr(
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


} // end namespace
