/**
 * MOAB, a Mesh-Oriented datABase, is a software component for creating,
 * storing and accessing finite element mesh data.
 * 
 * Copyright 2004 Sandia Corporation.  Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Coroporation, the U.S. Government
 * retains certain rights in this software.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 */

#ifndef MOAB_PROC_CONFIG_HPP
#define MOAB_PROC_CONFIG_HPP

#include "moab_mpi.h"

#include "moab/Types.hpp"
#include "moab/Range.hpp"
#include "moab/gs.hpp"



namespace moab {

class Interface;

/**\brief Multi-CPU information for parallel MOAB */
class ProcConfig {
public:

  ProcConfig(MPI_Comm proc_comm);
  
  ~ProcConfig();
  
    //! Get the current processor number
  unsigned proc_rank() const 
    { return procRank; }
      
    //! Get the number of processors
  unsigned proc_size() const 
    { return procSize; }
      
    //! get a crystal router for this parallel job
  gs_data::crystal_data *crystal_router(bool construct_if_missing = true);

    //! get/set the communicator for this proc config
  MPI_Comm proc_comm() const {return procComm;}
  void proc_comm(MPI_Comm this_comm) {procComm = this_comm;}

    //! set rank/size; USED FOR TESTING ONLY!
  void proc_rank(unsigned r) {procRank = r;}
  void proc_size(unsigned s) {procSize = s;}

private:

    //! MPI communicator set for this instance
  MPI_Comm procComm;

    //! rank of this processor
  unsigned procRank;
  
    //! number of processors
  unsigned procSize;
  
    //! crystal router for this parallel job
  gs_data::crystal_data* crystalData;
  
};

} // namespace moab

#endif
