// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// Trace include file for C++

// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMCI_TRACE_H
#define ESMCI_TRACE_H

//extern ESMCI::LogErr ESMC_LogDefault;
extern "C" {
  void FTN_X(c_esmftrc_filesys_init)(int *buf_size,  
				     const char *trace_dir,           
				     int *localPet,
				     int *rc,                        
				     ESMCI_FortranStrLenArg nlen);
  void FTN_X(c_esmftrc_filesys_fini)();
}

#endif
