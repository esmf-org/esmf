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
//-------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements C++ methods used to verify that PIO
// derived types can be passed correctly between F90 and the ESMF C++
// interface.
//
//-------------------------------------------------------------------------
//
#define ESMC_FILENAME "ESMCI_WordsizeSubr.C"

#include <cstddef>
#include <string>
using namespace std;

// ESMF header
#include "ESMC.h"

// ESMF Test header
#include "ESMCI_Test.h"

using namespace ESMCI;

#include "pio_kinds.h"

extern "C" {

void FTN_X(esmci_pio_size_check)(const char *strp,
     const char *arg1, const char *arg2,
     int *addr_diff,  // out: address delta between arg1 and arg2 in bytes
     int *desc_len,   // out: sizeof the requested descriptor in bytes
     int *rc, ESMCI_FortranStrLenArg slen) {

  struct table_entry {
    const char *name;
    int len;};

  table_entry table[] = {
    { "iosystem_desc_t", PIO_SIZE_IOSYSTEM_DESC },
    { "file_desc_t",     PIO_SIZE_FILE_DESC },
    { "io_desc_t",       PIO_SIZE_IO_DESC },
    { "var_desc_t",      PIO_SIZE_VAR_DESC}
  };
  const int table_size = sizeof (table) / sizeof (table_entry);

  string str(strp, slen);
  ptrdiff_t addr_diff_local = arg2 - arg1;
  *addr_diff = addr_diff_local;
  *desc_len = -1;

  int i;
  *rc = ESMF_FAILURE;
  for (i=0; i<table_size; i++) {
    if (str == table[i].name) {
      *desc_len = table[i].len;
      *rc = (addr_diff_local <= table[i].len) ? ESMF_SUCCESS : ESMF_FAILURE;
      break;
    }
  }
}

}
