// $Id: ESMC_IO_NetCDFUTest.C,v 1.2.2.1 2010/02/05 19:58:03 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================

#include <stdlib.h>
#include <string.h>

// ESMF header
#include "ESMC.h"

// ESMF Test header
#include "ESMC_Test.h"
#include <ESMCI_IO_NetCDF.h>

using namespace ESMCI;

//==============================================================================
//BOP
// !PROGRAM: ESMC_IO_NetCDFUTest - Check ESMCI_IO_NetCDF functionality
//
// !DESCRIPTION:
//
//EOP
//-----------------------------------------------------------------------------
// TODO:  migrate to F90 unit test when F90 API done.  These tests are for the
//        internal ESMCI interface, not the TBD public C interface.

int main(void)
{

  char name[80];
  char failMsg[80];
  int result = 0;
  int rc;
  bool netcdfNotPresent = false;

  const char dummy_nc_filename[]  = "dummy_netcdf_filename";
  const char input_nc_filename[]  = "io_netcdf_testdata.nc";
  const char output_nc_filename[] = "io_netcdf_testdata_out.nc";

  State* state;
  IO_NetCDF* nctestIO;

  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Create ESMCI_IO_NetCDF object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  nctestIO = ESMCI_IO_NetCDFCreate(strlen(dummy_nc_filename),
                                          dummy_nc_filename,
				          ESMC_NULL_POINTER, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Create ESMCI_State object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  state = State::create("dummy_state_filename", &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  nctestIO->setState(state);

  //----------------------------------------------------------------------------
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "read netcdf data into ESMCI_IO_NetCDF object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = nctestIO->read(strlen(input_nc_filename), input_nc_filename);
  if (rc==ESMF_RC_LIB_NOT_PRESENT) netcdfNotPresent = true;
  ESMC_Test((rc==ESMF_SUCCESS || netcdfNotPresent), name, failMsg,
             &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "write netcdf data from ESMCI_IO_NetCDF object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = nctestIO->write(strlen(output_nc_filename), output_nc_filename);
  ESMC_Test((rc==ESMF_SUCCESS || netcdfNotPresent), name, failMsg,
             &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Destroy ESMCI_State object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = State::destroy(state);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Destroy ESMCI_IO_NetCDF object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMCI_IO_NetCDFDestroy(&nctestIO);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  ESMC_TestEnd(result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  return 0;
}
