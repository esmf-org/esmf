// $Id: ESMC_ParLog.C,v 1.1.2.3 2010/02/01 20:51:43 svasquez Exp $
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
#include <ESMC_ParLog.h>

#include <mpi.h>
#include <cstdlib>


namespace ESMCI {
namespace MESH {

ParLog *ParLog::classInstance = NULL;

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
} // namespace
