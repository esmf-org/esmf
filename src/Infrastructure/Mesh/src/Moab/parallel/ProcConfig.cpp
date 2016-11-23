/**
 * MOAB, a Mesh-Oriented datABase, is a software component for creating,
 * storing and accessing finite element mesh data.
 * 
 * Copyright 2004 Sandia Corporation.  Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S. Government
 * retains certain rights in this software.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 */

#include "moab/ProcConfig.hpp"
#include "moab/gs.hpp"

namespace moab {


//! Constructor
ProcConfig::ProcConfig(MPI_Comm proc_comm1)
    : procComm(proc_comm1),
      crystalData(0)
{
#ifdef MOAB_HAVE_MPI
  int rank, size;
  MPI_Comm_rank(procComm, &rank); 
  procRank = (unsigned int) rank;
  MPI_Comm_size(procComm, &size); 
  procSize = (unsigned int) size;
#else
  procRank = 0;
  procSize = 1;
#endif
}

gs_data::crystal_data *ProcConfig::crystal_router(bool construct_if_missing)
{
#ifdef MOAB_HAVE_MPI
  if (!crystalData && construct_if_missing) {
    crystalData = new gs_data::crystal_data(procComm);
  }
#endif

  return crystalData;
}

ProcConfig::~ProcConfig() 
{
  if (crystalData) {
#ifdef MOAB_HAVE_MPI
    crystalData->reset();
#endif
    delete crystalData;
    crystalData = 0;
  }
}


} // namespace moab
