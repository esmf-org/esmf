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

// PIO header
#if defined (ESMF_PIO)
#include "pio.h"
#include "pio_types.h"
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
  char failMsg[80];
  int result = 0;
  int rc;

  int iotype;
#define NDIMS 1
  int dims[NDIMS];
#define DIM_X 10
  double test_data[DIM_X], read_data[DIM_X];
  int dimid_x;
  string fname;
  int pioerr;
#if defined (ESMF_PIO)
  pio_file_desc_t pio_file1[PIO_SIZE_FILE_DESC];
  pio_io_desc_t iodesc1[PIO_SIZE_IO_DESC];
  pio_var_desc_t pio_vardesc1[PIO_SIZE_VAR_DESC];
  pio_dof_t compdof[DIM_X];
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
  int rearr=0;
  int iosys_handle;
  int base=0;
#if defined (ESMF_PIO)
  pio_cpp_init_intracom(localPet, mpiCommunicator,
      num_iotasks, num_aggregators,
      stride, rearr,
      &iosys_handle,
      base);
#else
  rc = ESMF_SUCCESS;
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
 
  //------------------------------------------------------------------------
  // Test NETCDF
  //------------------------------------------------------------------------
#if defined (ESMF_PIO)
  iotype = PIO_iotype_netcdf;
#endif
  fname="pio_file1c_netcdf.nc";

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Create a file
  strcpy(name, "Create PIO NETCDF mode file");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#if defined (ESMF_PIO) && defined (ESMF_NETCDF)
  amode_in=PIO_CLOBBER;
  pioerr = pio_cpp_createfile(&iosys_handle, &pio_file1, iotype,
           fname.c_str(), amode_in);
  rc = (pioerr == 0) ? ESMF_SUCCESS : ESMF_FAILURE;
#else
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
  pioerr = pio_cpp_put_att_string (pio_file1, NULL,
      "filename", fname.c_str());
  rc = (pioerr == 0) ? ESMF_SUCCESS : ESMF_FAILURE;
#else
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
  pio_cpp_initdecomp_dof(&iosys_handle, PIO_double, dims, NDIMS,
      compdof, DIM_X, iodesc1);
  rc = ESMF_SUCCESS;
#else
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
  pioerr = pio_cpp_def_dim (pio_file1, "x", DIM_X, &dimid_x);
  rc = (pioerr == 0) ? ESMF_SUCCESS : ESMF_FAILURE;
#else
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
  pioerr = pio_cpp_def_var_md (pio_file1, "testdata", PIO_double,
       &dimid_x, 1, &pio_vardesc1);
  rc = (pioerr == 0) ? ESMF_SUCCESS : ESMF_FAILURE;
#else
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
  pioerr = pio_cpp_put_att_string (pio_file1, &pio_vardesc1,
      "units", "ordinal");
  rc = (pioerr == 0) ? ESMF_SUCCESS : ESMF_FAILURE;
#else
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
  pioerr = pio_cpp_put_att_ints (pio_file1, &pio_vardesc1,
      "answer", &answer, 1);
  rc = (pioerr == 0) ? ESMF_SUCCESS : ESMF_FAILURE;
#else
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
  pioerr = pio_cpp_put_att_floats (pio_file1, &pio_vardesc1,
      "float_att_value", &float_att_value, 1);
  rc = (pioerr == 0) ? ESMF_SUCCESS : ESMF_FAILURE;
#else
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
  pioerr = pio_cpp_put_att_doubles (pio_file1, &pio_vardesc1,
      "_FillValue", &fillvalue, 1);
  rc = (pioerr == 0) ? ESMF_SUCCESS : ESMF_FAILURE;
#else
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
  pioerr = pio_cpp_enddef (&pio_file1);
  rc = (pioerr == 0) ? ESMF_SUCCESS : ESMF_FAILURE;
#else
  rc = ESMF_SUCCESS;
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Set variable descriptor frame number
  strcpy(name, "PIO NETCDF mode setframe test");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#if defined (ESMF_PIO) && defined (ESMF_NETCDF)
  pio_cpp_setframe (pio_vardesc1, 1);
  rc = ESMF_SUCCESS;
#else
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
  pio_cpp_write_darray_double (&pio_file1, &pio_vardesc1, iodesc1,
      test_data, dims, 1, &pioerr);
  rc = (pioerr == 0) ? ESMF_SUCCESS : ESMF_FAILURE;
#else
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
  pio_cpp_closefile (&pio_file1);
  rc = ESMF_SUCCESS;
#else
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
  pioerr = pio_cpp_openfile(&iosys_handle, &pio_file1, iotype,
           fname.c_str(), 0);
  rc = (pioerr == 0) ? ESMF_SUCCESS : ESMF_FAILURE;
#else
  rc = ESMF_SUCCESS;
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Set variable descriptor frame number
  strcpy(name, "PIO NETCDF mode setframe number for read test");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#if defined (ESMF_PIO) && defined (ESMF_NETCDF)
  pio_cpp_setframe (pio_vardesc1, 1);
  rc = ESMF_SUCCESS;
#else
  rc = ESMF_SUCCESS;
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
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
  pio_cpp_read_darray_double (&pio_file1, &pio_vardesc1, iodesc1,
      read_data, dims, 1, &pioerr);
  rc = (pioerr == 0) ? ESMF_SUCCESS : ESMF_FAILURE;
#else
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
  pio_cpp_freedecomp_ios (&iosys_handle, &iodesc1);
  rc = ESMF_SUCCESS;
#else
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
  pio_cpp_closefile (&pio_file1);
  rc = ESMF_SUCCESS;
#else
  rc = ESMF_SUCCESS;
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
 
  //------------------------------------------------------------------------
  // Test PNETCDF
  //------------------------------------------------------------------------
#if defined (ESMF_PIO)
  iotype = PIO_iotype_pnetcdf;
#endif
  fname="pio_file1c_pnetcdf.nc";

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Create a file
  strcpy(name, "Create PIO PNETCDF mode file");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#if defined (ESMF_PIO) && defined (ESMF_PNETCDF)
  amode_in=PIO_CLOBBER;
  pioerr = pio_cpp_createfile(&iosys_handle, &pio_file1, iotype,
           fname.c_str(), amode_in);
  rc = (pioerr == 0) ? ESMF_SUCCESS : ESMF_FAILURE;
#else
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
  pioerr = pio_cpp_put_att_string (pio_file1, NULL,
      "filename", fname.c_str());
  rc = (pioerr == 0) ? ESMF_SUCCESS : ESMF_FAILURE;
#else
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
  pio_cpp_initdecomp_dof(&iosys_handle, PIO_double, dims, NDIMS,
      compdof, DIM_X, iodesc1);
  rc = ESMF_SUCCESS;
#else
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
  pioerr = pio_cpp_def_dim (pio_file1, "x", DIM_X, &dimid_x);
  rc = (pioerr == 0) ? ESMF_SUCCESS : ESMF_FAILURE;
#else
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
  pioerr = pio_cpp_def_var_md (pio_file1, "testdata", PIO_double,
       &dimid_x, 1, &pio_vardesc1);
  rc = (pioerr == 0) ? ESMF_SUCCESS : ESMF_FAILURE;
#else
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
  pioerr = pio_cpp_put_att_string (pio_file1, &pio_vardesc1,
      "units", "ordinal");
  rc = (pioerr == 0) ? ESMF_SUCCESS : ESMF_FAILURE;
#else
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
  pioerr = pio_cpp_put_att_ints (pio_file1, &pio_vardesc1,
      "answer", &answer, 1);
  rc = (pioerr == 0) ? ESMF_SUCCESS : ESMF_FAILURE;
#else
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
  pioerr = pio_cpp_put_att_floats (pio_file1, &pio_vardesc1,
      "float_att_value", &float_att_value, 1);
  rc = (pioerr == 0) ? ESMF_SUCCESS : ESMF_FAILURE;
#else
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
  pioerr = pio_cpp_put_att_doubles (pio_file1, &pio_vardesc1,
      "_FillValue", &fillvalue, 1);
  rc = (pioerr == 0) ? ESMF_SUCCESS : ESMF_FAILURE;
#else
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
  pioerr = pio_cpp_enddef (&pio_file1);
  rc = (pioerr == 0) ? ESMF_SUCCESS : ESMF_FAILURE;
#else
  rc = ESMF_SUCCESS;
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Set variable descriptor frame number
  strcpy(name, "PIO PNETCDF mode setframe test");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#if defined (ESMF_PIO) && defined (ESMF_PNETCDF)
  pio_cpp_setframe (pio_vardesc1, 1);
  rc = ESMF_SUCCESS;
#else
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
  pio_cpp_write_darray_double (&pio_file1, &pio_vardesc1, iodesc1,
      test_data, dims, 1, &pioerr);
  rc = (pioerr == 0) ? ESMF_SUCCESS : ESMF_FAILURE;
#else
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
  pio_cpp_closefile (&pio_file1);
  rc = ESMF_SUCCESS;
#else
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
  pioerr = pio_cpp_openfile(&iosys_handle, &pio_file1, iotype,
           fname.c_str(), 0);
  rc = (pioerr == 0) ? ESMF_SUCCESS : ESMF_FAILURE;
#else
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
  pio_cpp_setframe (pio_vardesc1, 1);
  rc = ESMF_SUCCESS;
#else
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
  pio_cpp_read_darray_double (&pio_file1, &pio_vardesc1, iodesc1,
      read_data, dims, 1, &pioerr);
  rc = (pioerr == 0) ? ESMF_SUCCESS : ESMF_FAILURE;
#else
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
  pio_cpp_freedecomp_ios (&iosys_handle, iodesc1);
  rc = ESMF_SUCCESS;
#else
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
  pio_cpp_closefile (&pio_file1);
  rc = ESMF_SUCCESS;
#else
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
  pio_cpp_finalize (&iosys_handle, &rc);
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




