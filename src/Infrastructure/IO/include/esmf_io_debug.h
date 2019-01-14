// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

#ifndef __ESMF_IO_DEBUG_H_INCLUDED_
#define __ESMF_IO_DEBUG_H_INCLUDED_

#ifdef ESMFIO_DEBUG
// Macros for printing out debug messages
// Define __PRINTSTDOUT__ to print dbg messages to stdout instead of log files

#ifdef ESMFIO_PRINTSTDOUT
#include "ESMC_VM.h"
#include "ESMCI_VM.h"
// Debug version
static int dbgio_getrank(void) {
  static int my_rank = -1;
  if (-1 == my_rank) {
    int rc;
    ESMC_VM evm;
#ifndef ESMFIO_NO_VM_BARRIER
    ESMCI::VM *vm;
#endif // ESMFIO_NO_VM_BARRIER
    int localPet;
    int petCount;
    int peCount;
    MPI_Comm communicator;
    evm = ESMC_VMGetCurrent(&rc);
    if (ESMF_SUCCESS == rc) {
#ifndef ESMFIO_NO_VM_BARRIER
      vm = ESMCI::VM::getCurrent(&rc);
#endif // ESMFIO_NO_VM_BARRIER
      if (ESMF_SUCCESS == rc) {
        rc = ESMC_VMGet(evm, &localPet, &petCount, &peCount,
                        &communicator, (int *)NULL, (int *)NULL);
#ifndef ESMFIO_NO_VM_BARRIER
        vm->barrier();
#endif // ESMFIO_NO_VM_BARRIER
        if (ESMF_SUCCESS == rc) {
          my_rank = localPet;
        }
      }
    }
  }
  return my_rank;
}
#define PRANKM "(" << dbgio_getrank() << "): " << ESMC_METHOD << " "
#define PPOS " " << ESMC_FILENAME << ":" << __LINE__ << "; "
#define PRINTPOS  std::cout << PRANKM << "called at" << PPOS << std::endl
#define PRINTMSG(_msg)  std::cout << PRANKM "at" << PPOS << _msg << std::endl
#else // ESMFIO_PRINTSTDOUT
#include <sstream>
static void dbgio_printmsg(const char *msg,
                           int line, const char *file, const char *method) {
  static bool setlog = true;
  if (setlog) {
    ESMC_LogSet(ESMF_TRUE);
    setlog = false;
  }
  ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO, line, file, method);
}
#define PRINTMSG(_msg) { std::stringstream errmsg;                            \
                         errmsg << _msg;                                      \
                         dbgio_printmsg(errmsg.str().c_str(), ESMC_CONTEXT); }
#define PRINTPOS dbgio_printmsg("", ESMC_CONTEXT)
#endif // ESMFIO_PRINTSTDOUT
#else // ESMFIO_DEBUG
// Non-debug version
#define PRINTPOS
#define PRINTMSG(_msg)
#endif // ESMFIO_DEBUG

#endif // __ESMF_IO_DEBUG_H_INCLUDED_
