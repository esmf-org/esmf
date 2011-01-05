// $Id: ESMCI_ParLog.C,v 1.6 2011/01/05 20:05:45 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2011, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <Mesh/include/ESMCI_ParLog.h>

#include <mpi.h>
#include <cstdlib>

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMCI_ParLog.C,v 1.6 2011/01/05 20:05:45 svasquez Exp $";
//-----------------------------------------------------------------------------

namespace ESMCI {

ParLog *ParLog::instance(const std::string &fstem) {
  
  if (classInstance) return classInstance;

  int rank;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  char buf[512];
  std::sprintf(buf, "%s.%d", fstem.c_str(), rank);
  classInstance = new ParLog(buf);
  return classInstance;
}

ParLog::ParLog(const std::string &fname) :
of(fname.c_str(), std::ios::out)
{
}

} // namespace
