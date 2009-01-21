// $Id: ESMC_ParEnv.C,v 1.2.2.2 2009/01/21 21:25:23 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <ESMC_ParEnv.h>
#include <ESMC_Exception.h>

#include <mpi.h>
#include <cstdlib>

namespace ESMCI {
namespace MESH {

ParLog *ParLog::classInstance = NULL;

ParLog *ParLog::instance(const std::string &fstem, UInt rank) {
  
  if (classInstance) return classInstance;

  char buf[512];
  std::sprintf(buf, "%s.%d", fstem.c_str(), rank);
  classInstance = new ParLog(buf);
  return classInstance;
}

ParLog::ParLog(const std::string &fname) :
of(fname.c_str(), std::ios::out)
{
}

// ******* Env ********
int Par::rank = 0;
int Par::psize = 0;
bool Par::serial = false;
ParLog *Par::log = NULL;

void Par::Init(int &argc, char **&argv, const std::string &logfile, bool _serial) {
  serial = _serial;
  if (!serial) {
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &psize);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  } else {
    psize = 1;
    rank = 0;
  }
  log = ParLog::instance(logfile, rank);
}

void Par::Abort() {
  Par::Out() << Trace::StackTrace() << std::endl;
  std::cerr << Trace::StackTrace() << std::endl;
  std::cerr << "Process:" << Par::Rank() << " aborting!!" << std::endl;
  ParLog::flush();
  if (!serial)
    MPI_Abort(MPI_COMM_WORLD, 911);
}

void Par::End() {
  ParLog::flush();
  if (!serial) MPI_Finalize();
}

} // namespace
} // namespace
