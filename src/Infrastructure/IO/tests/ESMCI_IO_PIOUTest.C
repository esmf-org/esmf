// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2022, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================

// PIO header
#if defined (ESMF_PIO)
#include <pio.h>
#endif

// ESMF header
#include "ESMC.h"

// ESMF Test header
#include "ESMC_Test.h"

// Standard C headers
#include <cstring>
#include <iostream>
using namespace std;

//==============================================================================
//BOP
// !PROGRAM: ESMC_IO_PIOUTest - Check C++ interface to PIO functionality
//
// !DESCRIPTION:
//
//EOP
//-----------------------------------------------------------------------------

int main(void){

  char name[80];
  char disname[80];
  char failMsg[80];
  int result = 0;
  int rc;

  int iotype;
#define NDIMS 1
  int dims[NDIMS];
  int dims2[NDIMS+1];
#define DIM_X 10
  double test_data[DIM_X], read_data[DIM_X];
  int dimid_x;
  int dimid_t;
  string fname;
  int pioerr;
#if defined (ESMF_PIO)
  int pio_file1;
  int iodesc1;
  int pio_vardesc1;
  MPI_Offset compdof[DIM_X];
  int amode_in;

  int answer;
  float float_att_value;
  double fillvalue;
#endif

  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  ESMC_VM vm;
  ESMC_VM vmCurrent;
  int localPet, petCount, peCount;
  int pthreadsEnabledFlag, openMPEnabledFlag;
  MPI_Comm mpiCommunicator;

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Get the global VM
  strcpy(name, "VMGetGlobal");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  vm = ESMC_VMGetGlobal(&rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Get the global VM
  strcpy(name, "Get VM pet info");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_VMGet(vm, &localPet, &petCount, &peCount,
	 &mpiCommunicator, &pthreadsEnabledFlag, &openMPEnabledFlag);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
//  cout << "I am PET " << localPet << " of " << petCount << endl;
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Get the global VM
  strcpy(name, "Initialize a PIO instance");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  int num_iotasks=petCount;
  int num_aggregators=1;
  int stride=1;
#if defined (ESMF_PIO)
  int rearr=PIO_REARR_SUBSET;
#endif
  int iosys_handle;
  int base=0;
#if defined (ESMF_PIO)
  rc = PIOc_Init_Intracomm(mpiCommunicator,
      num_iotasks, stride, base, rearr,
      &iosys_handle);
#else
  rc = ESMF_SUCCESS;
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //------------------------------------------------------------------------
  // Test NETCDF
  //------------------------------------------------------------------------
#if defined (ESMF_PIO)
  iotype = PIO_IOTYPE_NETCDF;
#endif
  fname="pio_file1c_netcdf.nc";

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Create a file
  strcpy(name, "Create PIO NETCDF mode file");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#if defined (ESMF_PIO) && defined (ESMF_NETCDF)
  amode_in=PIO_CLOBBER;
  pioerr = PIOc_createfile(iosys_handle, &pio_file1, &iotype,
	   fname.c_str(), amode_in);
  rc = (pioerr == PIO_NOERR) ? ESMF_SUCCESS : ESMF_FAILURE;
#else
  strcpy(disname, "DISABLED: ");
  strcat(disname, name);
  strcpy(name, disname);
  rc = ESMF_SUCCESS;
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Add a global string attribute to the the file to be written
  strcpy(name, "PIO NETCDF mode add global string attribute");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#if defined (ESMF_PIO) && defined (ESMF_NETCDF)
  pioerr = PIOc_put_att_text (pio_file1, PIO_GLOBAL,
			      "filename", strlen(fname.c_str()),fname.c_str());
  rc = (pioerr == PIO_NOERR) ? ESMF_SUCCESS : ESMF_FAILURE;
#else
  strcpy(disname, "DISABLED: ");
  strcat(disname, name);
  strcpy(name, disname);
  rc = ESMF_SUCCESS;
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Create a PIO mapping
  strcpy(name, "Create PIO NETCDF mode mapping");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  dims[0] = DIM_X * petCount;
#if defined (ESMF_PIO) && defined (ESMF_NETCDF)
  for (int i=0; i<DIM_X; i++)
    compdof[i] = (localPet*DIM_X + 1) + i;
  PIOc_InitDecomp(iosys_handle, PIO_DOUBLE, NDIMS, dims,
		  DIM_X, compdof, &iodesc1, NULL, NULL, NULL);
  rc = ESMF_SUCCESS;
#else
  strcpy(disname, "DISABLED: ");
  strcat(disname, name);
  strcpy(name, disname);
  rc = ESMF_SUCCESS;
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Start defining the dimensions of the variable to be written
  strcpy(name, "PIO NETCDF mode define dimension");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  dims[0] = DIM_X * petCount;
#if defined (ESMF_PIO) && defined (ESMF_NETCDF)
  pioerr = PIOc_def_dim (pio_file1, "x", dims[0], &dimid_x);
  rc = (pioerr == PIO_NOERR) ? ESMF_SUCCESS : ESMF_FAILURE;
#else
  strcpy(disname, "DISABLED: ");
  strcat(disname, name);
  strcpy(name, disname);
  rc = ESMF_SUCCESS;
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Start defining the the variable to be written
  strcpy(name, "PIO NETCDF mode define variable");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#if defined (ESMF_PIO) && defined (ESMF_NETCDF)
  pioerr = PIOc_def_var (pio_file1, "testdata", PIO_DOUBLE,
			 1, &dimid_x, &pio_vardesc1);
  rc = (pioerr == PIO_NOERR) ? ESMF_SUCCESS : ESMF_FAILURE;
#else
  strcpy(disname, "DISABLED: ");
  strcat(disname, name);
  strcpy(name, disname);
  rc = ESMF_SUCCESS;
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Add a string attribute to the the variable to be written
  strcpy(name, "PIO NETCDF mode add string attribute");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#if defined (ESMF_PIO) && defined (ESMF_NETCDF)
  pioerr = PIOc_put_att_text (pio_file1, pio_vardesc1,
			      "units", 7, "ordinal");
  rc = (pioerr == PIO_NOERR) ? ESMF_SUCCESS : ESMF_FAILURE;
#else
  strcpy(disname, "DISABLED: ");
  strcat(disname, name);
  strcpy(name, disname);
  rc = ESMF_SUCCESS;
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Add a scalar integer attribute to the the variable to be written
  strcpy(name, "PIO NETCDF mode add int attribute");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#if defined (ESMF_PIO) && defined (ESMF_NETCDF)
  answer = 42;
  pioerr = PIOc_put_att (pio_file1, pio_vardesc1,
			 "answer", PIO_INT, 1, &answer);
  rc = (pioerr == PIO_NOERR) ? ESMF_SUCCESS : ESMF_FAILURE;
#else
  strcpy(disname, "DISABLED: ");
  strcat(disname, name);
  strcpy(name, disname);
  rc = ESMF_SUCCESS;
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Add a scalar float attribute to the the variable to be written
  strcpy(name, "PIO NETCDF mode add float attribute");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#if defined (ESMF_PIO) && defined (ESMF_NETCDF)
  float_att_value = 0.0001f;
  pioerr = PIOc_put_att (pio_file1, pio_vardesc1,
			 "float_att_value", PIO_FLOAT, 1, &float_att_value);
  rc = (pioerr == PIO_NOERR) ? ESMF_SUCCESS : ESMF_FAILURE;
#else
  strcpy(disname, "DISABLED: ");
  strcat(disname, name);
  strcpy(name, disname);
  rc = ESMF_SUCCESS;
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Add a scalar double attribute to the the variable to be written
  strcpy(name, "PIO NETCDF mode add scalar double attribute");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#if defined (ESMF_PIO) && defined (ESMF_NETCDF)
  fillvalue = -9999.99;
  pioerr = PIOc_put_att (pio_file1, pio_vardesc1,
			 "_FillValue", PIO_DOUBLE, 1, &fillvalue);
  rc = (pioerr == PIO_NOERR) ? ESMF_SUCCESS : ESMF_FAILURE;
#else
  strcpy(disname, "DISABLED: ");
  strcat(disname, name);
  strcpy(name, disname);
  rc = ESMF_SUCCESS;
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // End define mode
  strcpy(name, "End PIO NETCDF mode define mode");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#if defined (ESMF_PIO) && defined (ESMF_NETCDF)
  pioerr = PIOc_enddef (pio_file1);
  rc = (pioerr == PIO_NOERR) ? ESMF_SUCCESS : ESMF_FAILURE;
#else
  strcpy(disname, "DISABLED: ");
  strcat(disname, name);
  strcpy(name, disname);
  rc = ESMF_SUCCESS;
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Write some data
  strcpy(name, "Write data to PIO NETCDF mode file");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  for (int i=0; i<DIM_X; i++)
    test_data[i] = i + 1 + localPet*100;
  dims[0] = DIM_X;
#if defined (ESMF_PIO) && defined (ESMF_NETCDF)
  pioerr = PIOc_write_darray (pio_file1, pio_vardesc1, iodesc1,
			      DIM_X, test_data, NULL);
  rc = (pioerr == PIO_NOERR) ? ESMF_SUCCESS : ESMF_FAILURE;
#else
  strcpy(disname, "DISABLED: ");
  strcat(disname, name);
  strcpy(name, disname);
  rc = ESMF_SUCCESS;
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Close file after write
  strcpy(name, "PIO NETCDF mode closefile after write test");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#if defined (ESMF_PIO) && defined (ESMF_NETCDF)
  pioerr = PIOc_closefile (pio_file1);
  rc = (pioerr == PIO_NOERR) ? ESMF_SUCCESS : ESMF_FAILURE;
#else
  strcpy(disname, "DISABLED: ");
  strcat(disname, name);
  strcpy(name, disname);
  rc = ESMF_SUCCESS;
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Open PIO file for read
  strcpy(name, "Open PIO NETCDF mode file for read");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#if defined (ESMF_PIO) && defined (ESMF_NETCDF)
  pioerr = PIOc_openfile(iosys_handle, &pio_file1, &iotype,
			 fname.c_str(), 0);
  rc = (pioerr == PIO_NOERR) ? ESMF_SUCCESS : ESMF_FAILURE;
#else
  strcpy(disname, "DISABLED: ");
  strcat(disname, name);
  strcpy(name, disname);
  rc = ESMF_SUCCESS;
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_disabled_UTest
  // Set variable descriptor frame number
//  strcpy(name, "PIO NETCDF mode setframe number for read test");
//  strcpy(failMsg, "Did not return ESMF_SUCCESS");
//#if defined (ESMF_PIO) && defined (ESMF_NETCDF)
//  pioerr = PIOc_setframe (pio_file1, pio_vardesc1, 1);
//  rc = (pioerr == PIO_NOERR) ? ESMF_SUCCESS : ESMF_FAILURE;
//#else
//  strcpy(disname, "DISABLED: ");
//  strcat(disname, name);
//  strcpy(name, disname);
//  rc = ESMF_SUCCESS;
//#endif
//  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Read back data
  strcpy(name, "Read data from PIO NETCDF mode file");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  dims[0] = DIM_X;
  for (int i=0; i<DIM_X; i++)
    read_data[i] = -42.42;
#if defined (ESMF_PIO) && defined (ESMF_NETCDF)
  pioerr = PIOc_read_darray (pio_file1, pio_vardesc1, iodesc1,
			     DIM_X, read_data);
  rc = (pioerr == PIO_NOERR) ? ESMF_SUCCESS : ESMF_FAILURE;
#else
  strcpy(disname, "DISABLED: ");
  strcat(disname, name);
  strcpy(name, disname);
  rc = ESMF_SUCCESS;
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Compare results
  strcpy(name, "Compare written data with data read back");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#if defined (ESMF_PIO) && defined (ESMF_NETCDF)
  rc = ESMF_SUCCESS;
  for (int i=0; i<DIM_X; i++)
    if (test_data[i] != read_data[i]) {
      rc = ESMF_FAILURE;
      cout << "Comparison failed at element " << i
	  << test_data[i] << " != " << read_data[i] << endl;
      break;
    };
#else
  strcpy(disname, "DISABLED: ");
  strcat(disname, name);
  strcpy(name, disname);
  rc = ESMF_SUCCESS;
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Free the decomposition
  strcpy(name, "Free PIO NETCDF mode decomposition");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#if defined (ESMF_PIO) && defined (ESMF_NETCDF)
  pioerr = PIOc_freedecomp (iosys_handle, iodesc1);
  rc = (pioerr == PIO_NOERR) ? ESMF_SUCCESS : ESMF_FAILURE;
#else
  strcpy(disname, "DISABLED: ");
  strcat(disname, name);
  strcpy(name, disname);
  rc = ESMF_SUCCESS;
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Close file after read
  strcpy(name, "PIO NETCDF mode closefile after read test");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#if defined (ESMF_PIO) && defined (ESMF_NETCDF)
  pioerr = PIOc_closefile (pio_file1);
  rc = (pioerr == PIO_NOERR) ? ESMF_SUCCESS : ESMF_FAILURE;
#else
  strcpy(disname, "DISABLED: ");
  strcat(disname, name);
  strcpy(name, disname);
  rc = ESMF_SUCCESS;
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //------------------------------------------------------------------------
  // Test PNETCDF
  //------------------------------------------------------------------------
#if defined (ESMF_PIO)
  iotype = PIO_IOTYPE_PNETCDF;
#endif
  fname="pio_file1c_pnetcdf.nc";

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Create a file
  strcpy(name, "Create PIO PNETCDF mode file");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#if defined (ESMF_PIO) && defined (ESMF_PNETCDF)
  amode_in=PIO_CLOBBER;
  pioerr = PIOc_createfile(iosys_handle, &pio_file1, &iotype,
	   fname.c_str(), amode_in);
  rc = (pioerr == PIO_NOERR) ? ESMF_SUCCESS : ESMF_FAILURE;
#else
  strcpy(disname, "DISABLED: ");
  strcat(disname, name);
  strcpy(name, disname);
  rc = ESMF_SUCCESS;
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Add a global string attribute to the the file to be written
  strcpy(name, "PIO PNETCDF mode add global string attribute");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#if defined (ESMF_PIO) && defined (ESMF_PNETCDF)
  pioerr = PIOc_put_att_text (pio_file1, PIO_GLOBAL,
			      "filename", strlen(fname.c_str()), fname.c_str());
  rc = (pioerr == PIO_NOERR) ? ESMF_SUCCESS : ESMF_FAILURE;
#else
  strcpy(disname, "DISABLED: ");
  strcat(disname, name);
  strcpy(name, disname);
  rc = ESMF_SUCCESS;
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Create a PIO mapping
  strcpy(name, "Create PIO PNETCDF mode mapping");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  dims[0] = DIM_X * petCount;
#if defined (ESMF_PIO) && defined (ESMF_PNETCDF)
  for (int i=0; i<DIM_X; i++)
    compdof[i] = (localPet*DIM_X + 1) + i;
  rc = PIOc_InitDecomp(iosys_handle, PIO_DOUBLE, NDIMS, dims,
		       DIM_X, compdof, &iodesc1, NULL, NULL, NULL);
#else
  strcpy(disname, "DISABLED: ");
  strcat(disname, name);
  strcpy(name, disname);
  rc = ESMF_SUCCESS;
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Start defining the dimensions of the variable to be written
  strcpy(name, "PIO PNETCDF mode define dimension");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  dims[0] = DIM_X * petCount;
#if defined (ESMF_PIO) && defined (ESMF_PNETCDF)
  pioerr = PIOc_def_dim (pio_file1, "x", dims[0], &dimid_x);
  rc = (pioerr == PIO_NOERR) ? ESMF_SUCCESS : ESMF_FAILURE;
#else
  strcpy(disname, "DISABLED: ");
  strcat(disname, name);
  strcpy(name, disname);
  rc = ESMF_SUCCESS;
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Start defining the the variable to be written
  strcpy(name, "PIO PNETCDF mode define variable");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#if defined (ESMF_PIO) && defined (ESMF_PNETCDF)
  dims[0] = dimid_x;
  pioerr = PIOc_def_var (pio_file1, "testdata", PIO_DOUBLE,
			 1, dims, &pio_vardesc1);
  rc = (pioerr == PIO_NOERR) ? ESMF_SUCCESS : ESMF_FAILURE;
#else
  strcpy(disname, "DISABLED: ");
  strcat(disname, name);
  strcpy(name, disname);
  rc = ESMF_SUCCESS;
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Add a string attribute to the the variable to be written
  strcpy(name, "PIO PNETCDF mode add string attribute");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#if defined (ESMF_PIO) && defined (ESMF_PNETCDF)
  pioerr = PIOc_put_att_text (pio_file1, pio_vardesc1,
			      "units", 7, "ordinal");
  rc = (pioerr == PIO_NOERR) ? ESMF_SUCCESS : ESMF_FAILURE;
#else
  strcpy(disname, "DISABLED: ");
  strcat(disname, name);
  strcpy(name, disname);
  rc = ESMF_SUCCESS;
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Add a scalar integer attribute to the the variable to be written
  strcpy(name, "PIO PNETCDF mode add int attribute");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#if defined (ESMF_PIO) && defined (ESMF_PNETCDF)
  answer = 42;
  pioerr = PIOc_put_att (pio_file1, pio_vardesc1,
			     "answer", PIO_INT, 1, &answer);
  rc = (pioerr == PIO_NOERR) ? ESMF_SUCCESS : ESMF_FAILURE;
#else
  strcpy(disname, "DISABLED: ");
  strcat(disname, name);
  strcpy(name, disname);
  rc = ESMF_SUCCESS;
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Add a scalar float attribute to the the variable to be written
  strcpy(name, "PIO PNETCDF mode add float attribute");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#if defined (ESMF_PIO) && defined (ESMF_PNETCDF)
  float_att_value = 0.0001f;
  pioerr = PIOc_put_att (pio_file1, pio_vardesc1,
			 "float_att_value", PIO_FLOAT, 1, &float_att_value);
  rc = (pioerr == PIO_NOERR) ? ESMF_SUCCESS : ESMF_FAILURE;
#else
  strcpy(disname, "DISABLED: ");
  strcat(disname, name);
  strcpy(name, disname);
  rc = ESMF_SUCCESS;
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Add a scalar double attribute to the the variable to be written
  strcpy(name, "PIO PNETCDF mode add scalar double attribute");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#if defined (ESMF_PIO) && defined (ESMF_PNETCDF)
  fillvalue = -9999.99;
  pioerr = PIOc_put_att (pio_file1, pio_vardesc1,
				"_FillValue", PIO_DOUBLE, 1, &fillvalue);
  rc = (pioerr == PIO_NOERR) ? ESMF_SUCCESS : ESMF_FAILURE;
#else
  strcpy(disname, "DISABLED: ");
  strcat(disname, name);
  strcpy(name, disname);
  rc = ESMF_SUCCESS;
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // End define mode
  strcpy(name, "End PIO PNETCDF mode define mode");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#if defined (ESMF_PIO) && defined (ESMF_PNETCDF)
  pioerr = PIOc_enddef (pio_file1);
  rc = (pioerr == PIO_NOERR) ? ESMF_SUCCESS : ESMF_FAILURE;
#else
  strcpy(disname, "DISABLED: ");
  strcat(disname, name);
  strcpy(name, disname);
  rc = ESMF_SUCCESS;
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Write some data
  strcpy(name, "Write data to PIO PNETCDF mode file");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  for (int i=0; i<DIM_X; i++)
    test_data[i] = i + 1 + localPet*100;
  dims[0] = DIM_X;
#if defined (ESMF_PIO) && defined (ESMF_PNETCDF)
  pioerr = PIOc_write_darray (pio_file1, pio_vardesc1, iodesc1,
			      DIM_X, test_data, NULL);
  rc = (pioerr == PIO_NOERR) ? ESMF_SUCCESS : ESMF_FAILURE;
#else
  strcpy(disname, "DISABLED: ");
  strcat(disname, name);
  strcpy(name, disname);
  rc = ESMF_SUCCESS;
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Close file after write
  strcpy(name, "PIO PNETCDF mode closefile after write test");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#if defined (ESMF_PIO) && defined (ESMF_PNETCDF)
  pioerr = PIOc_closefile (pio_file1);
  rc = (pioerr == PIO_NOERR) ? ESMF_SUCCESS : ESMF_FAILURE;
#else
  strcpy(disname, "DISABLED: ");
  strcat(disname, name);
  strcpy(name, disname);
  rc = ESMF_SUCCESS;
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Open PIO file for read
  strcpy(name, "Open PIO PNETCDF mode file for read");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#if defined (ESMF_PIO) && defined (ESMF_PNETCDF)
  pioerr = PIOc_openfile(iosys_handle, &pio_file1, &iotype,
	   fname.c_str(), 0);
  rc = (pioerr == PIO_NOERR) ? ESMF_SUCCESS : ESMF_FAILURE;
#else
  strcpy(disname, "DISABLED: ");
  strcat(disname, name);
  strcpy(name, disname);
  rc = ESMF_SUCCESS;
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Set variable descriptor frame number
  strcpy(name, "PIO PNETCDF mode setframe number for read test");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#if defined (ESMF_PIO) && defined (ESMF_PNETCDF)
  PIOc_setframe (pio_file1, pio_vardesc1, 1);
  rc = ESMF_SUCCESS;
#else
  strcpy(disname, "DISABLED: ");
  strcat(disname, name);
  strcpy(name, disname);
  rc = ESMF_SUCCESS;
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Read back data
  strcpy(name, "Read data from PIO PNETCDF mode file");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  dims[0] = DIM_X;
  for (int i=0; i<DIM_X; i++)
    read_data[i] = -42.42;
#if defined (ESMF_PIO) && defined (ESMF_PNETCDF)
  pioerr = PIOc_read_darray (pio_file1, pio_vardesc1, iodesc1,
			     DIM_X, read_data);
  rc = (pioerr == PIO_NOERR) ? ESMF_SUCCESS : ESMF_FAILURE;
#else
  strcpy(disname, "DISABLED: ");
  strcat(disname, name);
  strcpy(name, disname);
  rc = ESMF_SUCCESS;
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Compare results
  strcpy(name, "Compare written data with data read back");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#if defined (ESMF_PIO) && defined (ESMF_PNETCDF)
  rc = ESMF_SUCCESS;
  for (int i=0; i<DIM_X; i++)
    if (test_data[i] != read_data[i]) {
      rc = ESMF_FAILURE;
      cout << "Comparison failed at element " << i
	  << test_data[i] << " != " << read_data[i] << endl;
      break;
    };
#else
  strcpy(disname, "DISABLED: ");
  strcat(disname, name);
  strcpy(name, disname);
  rc = ESMF_SUCCESS;
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Free the decomposition
  strcpy(name, "Free PIO PNETCDF mode decomposition");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");

#if defined (ESMF_PIO) && defined (ESMF_PNETCDF)
  rc = PIOc_freedecomp(iosys_handle, iodesc1);
#else
  strcpy(disname, "DISABLED: ");
  strcat(disname, name);
  strcpy(name, disname);
  rc = ESMF_SUCCESS;
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Close file after read
  strcpy(name, "PIO PNETCDF mode closefile after read test");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#if defined (ESMF_PIO) && defined (ESMF_PNETCDF)
  rc = PIOc_closefile (pio_file1);
#else
  strcpy(disname, "DISABLED: ");
  strcat(disname, name);
  strcpy(name, disname);
  rc = ESMF_SUCCESS;
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Finalize the iosystem
  strcpy(name, "Finalization of a PIO iosystem handle");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#if defined (ESMF_PIO)
  rc = PIOc_free_iosystem(iosys_handle);
#else
  rc = ESMF_SUCCESS;
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  return 0;
}
