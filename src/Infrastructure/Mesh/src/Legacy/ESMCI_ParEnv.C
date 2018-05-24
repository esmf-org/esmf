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
#include <Mesh/include/Legacy/ESMCI_ParEnv.h>
#include <Mesh/include/Legacy/ESMCI_Exception.h>

#include <mpi.h>
#include <cstdlib>
#include <cstdio>

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

namespace ESMCI {

ParLog *ParLog::classInstance = NULL;

ParLog *ParLog::instance(const std::string &fstem, UInt rank, bool use_log) {
  
  if (classInstance) return classInstance;

  char buf[512];
  std::sprintf(buf, "%s.%d", fstem.c_str(), rank);
  classInstance = new ParLog(buf, use_log);
  return classInstance;
}

ParLog::ParLog(const std::string &fname, bool _use_log) :
 use_log(_use_log)
{

#if ESMF_PARLOG
  if (use_log)
    of.open(fname.c_str(), std::ios::out);
#endif

}

// ******* Env ********
int Par::rank = 0;
int Par::psize = 0;
bool Par::serial = false;
ParLog *Par::log = NULL;
MPI_Comm Par::comm = MPI_COMM_WORLD;

void Par::Init(const std::string &logfile, bool use_log, MPI_Comm _comm) {
  int mpi_init = 0;
  MPI_Initialized(&mpi_init);
  if (!mpi_init) {
    int argc = 0;
    char **argv = 0;
    MPI_Init(&argc, &argv);
  }

  // Get info from MPI
  comm = _comm;
  MPI_Comm_size(comm, &psize);
  MPI_Comm_rank(comm, &rank);

  // Set Serial Flag
  if (psize==1) serial=true;
  else serial=false;

  // Setup log
  log = ParLog::instance(logfile, rank, use_log);
}

void Par::SetComm(MPI_Comm _comm) {
  comm = _comm;
  MPI_Comm_size(comm, &psize);
  MPI_Comm_rank(comm, &rank);
}

void Par::Abort() {
  Par::Out() << Trace::StackTrace() << std::endl;
  std::cerr << Trace::StackTrace() << std::endl;
  std::cerr << "Process:" << Par::Rank() << " aborting!!" << std::endl;
  ParLog::flush();
  if (!serial)
    MPI_Abort(comm, 911);
}

void Par::End() {
  ParLog::flush();
  if (!serial) MPI_Finalize();
}

} // namespace
