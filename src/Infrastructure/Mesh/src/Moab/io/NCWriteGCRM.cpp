/*
 * NCWriteGCRM.cpp
 *
 *  Created on: April 9, 2014
 */

#include "NCWriteGCRM.hpp"
#include "MBTagConventions.hpp"

namespace moab {

NCWriteGCRM::~NCWriteGCRM()
{
  // TODO Auto-generated destructor stub
}

ErrorCode NCWriteGCRM::collect_mesh_info()
{
  Interface*& mbImpl = _writeNC->mbImpl;
  std::vector<std::string>& dimNames = _writeNC->dimNames;
  std::vector<int>& dimLens = _writeNC->dimLens;
  Tag& mGlobalIdTag = _writeNC->mGlobalIdTag;

  ErrorCode rval;

  // Look for time dimension
  std::vector<std::string>::iterator vecIt;
  if ((vecIt = std::find(dimNames.begin(), dimNames.end(), "Time")) != dimNames.end())
    tDim = vecIt - dimNames.begin();
  else if ((vecIt = std::find(dimNames.begin(), dimNames.end(), "time")) != dimNames.end())
    tDim = vecIt - dimNames.begin();
  else {
    MB_SET_ERR(MB_FAILURE, "Couldn't find 'Time' or 'time' dimension");
  }
  nTimeSteps = dimLens[tDim];

  // Get number of levels
  if ((vecIt = std::find(dimNames.begin(), dimNames.end(), "layers")) != dimNames.end())
    levDim = vecIt - dimNames.begin();
  else {
    MB_SET_ERR(MB_FAILURE, "Couldn't find 'layers' dimension");
  }
  nLevels = dimLens[levDim];

  // Get local vertices
  rval = mbImpl->get_entities_by_dimension(_fileSet, 0, localVertsOwned);MB_CHK_SET_ERR(rval, "Trouble getting local vertices in current file set");
  assert(!localVertsOwned.empty());

  // Get local edges
  rval = mbImpl->get_entities_by_dimension(_fileSet, 1, localEdgesOwned);MB_CHK_SET_ERR(rval, "Trouble getting local edges in current file set");
  // There are no edges if NO_EDGES read option is set

  // Get local cells
  rval = mbImpl->get_entities_by_dimension(_fileSet, 2, localCellsOwned);MB_CHK_SET_ERR(rval, "Trouble getting local cells in current file set");
  assert(!localCellsOwned.empty());

#ifdef MOAB_HAVE_MPI
  bool& isParallel = _writeNC->isParallel;
  if (isParallel) {
    ParallelComm*& myPcomm = _writeNC->myPcomm;
    int rank = myPcomm->proc_config().proc_rank();
    int procs = myPcomm->proc_config().proc_size();
    if (procs > 1) {
#ifndef NDEBUG
      unsigned int num_local_verts = localVertsOwned.size();
#endif
      rval = myPcomm->filter_pstatus(localVertsOwned, PSTATUS_NOT_OWNED, PSTATUS_NOT);MB_CHK_SET_ERR(rval, "Trouble getting owned vertices in current file set");

      // Assume that PARALLEL_RESOLVE_SHARED_ENTS option is set
      // Verify that not all local vertices are owned by the last processor
      if (procs - 1 == rank)
        assert("PARALLEL_RESOLVE_SHARED_ENTS option is set" && localVertsOwned.size() < num_local_verts);

      if (!localEdgesOwned.empty()) {
        rval = myPcomm->filter_pstatus(localEdgesOwned, PSTATUS_NOT_OWNED, PSTATUS_NOT);MB_CHK_SET_ERR(rval, "Trouble getting owned edges in current file set");
      }

      rval = myPcomm->filter_pstatus(localCellsOwned, PSTATUS_NOT_OWNED, PSTATUS_NOT);MB_CHK_SET_ERR(rval, "Trouble getting owned cells in current file set");
    }
  }
#endif

  std::vector<int> gids(localVertsOwned.size());
  rval = mbImpl->tag_get_data(mGlobalIdTag, localVertsOwned, &gids[0]);MB_CHK_SET_ERR(rval, "Trouble getting global IDs on local vertices");

  // Get localGidVertsOwned
  std::copy(gids.rbegin(), gids.rend(), range_inserter(localGidVertsOwned));

  if (!localEdgesOwned.empty()) {
    gids.resize(localEdgesOwned.size());
    rval = mbImpl->tag_get_data(mGlobalIdTag, localEdgesOwned, &gids[0]);MB_CHK_SET_ERR(rval, "Trouble getting global IDs on local edges");

    // Get localGidEdgesOwned
    std::copy(gids.rbegin(), gids.rend(), range_inserter(localGidEdgesOwned));
  }

  gids.resize(localCellsOwned.size());
  rval = mbImpl->tag_get_data(mGlobalIdTag, localCellsOwned, &gids[0]);MB_CHK_SET_ERR(rval, "Trouble getting global IDs on local cells");

  // Get localGidCellsOwned
  std::copy(gids.rbegin(), gids.rend(), range_inserter(localGidCellsOwned));

  return MB_SUCCESS;
}

ErrorCode NCWriteGCRM::collect_variable_data(std::vector<std::string>& var_names, std::vector<int>& tstep_nums)
{
  NCWriteHelper::collect_variable_data(var_names, tstep_nums);

  std::vector<std::string>& dimNames = _writeNC->dimNames;
  std::vector<int>& dimLens = _writeNC->dimLens;

  // Dimension indices for other optional levels
  std::vector<unsigned int> opt_lev_dims;

  unsigned int lev_idx;
  std::vector<std::string>::iterator vecIt;

  // Get index of interface levels
  if ((vecIt = std::find(dimNames.begin(), dimNames.end(), "interfaces")) != dimNames.end()) {
    lev_idx = vecIt - dimNames.begin();
    opt_lev_dims.push_back(lev_idx);
  }

  std::map<std::string, WriteNC::VarData>& varInfo = _writeNC->varInfo;

  for (size_t i = 0; i < var_names.size(); i++) {
    std::string varname = var_names[i];
    std::map<std::string, WriteNC::VarData>::iterator vit = varInfo.find(varname);
    if (vit == varInfo.end())
      MB_SET_ERR(MB_FAILURE, "Can't find variable " << varname);

    WriteNC::VarData& currentVarData = vit->second;
    std::vector<int>& varDims = currentVarData.varDims;

    // Skip edge variables, if there are no edges
    if (localEdgesOwned.empty() && currentVarData.entLoc == WriteNC::ENTLOCEDGE)
      continue;

    // If layers dimension is not found, try other optional levels such as interfaces
    if (std::find(varDims.begin(), varDims.end(), levDim) == varDims.end()) {
      for (unsigned int j = 0; j < opt_lev_dims.size(); j++) {
        if (std::find(varDims.begin(), varDims.end(), opt_lev_dims[j]) != varDims.end()) {
          currentVarData.numLev = dimLens[opt_lev_dims[j]];
          break;
        }
      }
    }

    // Skip set variables, which were already processed in NCWriteHelper::collect_variable_data()
    if (WriteNC::ENTLOCSET == currentVarData.entLoc)
      continue;

    // Set up writeStarts and writeCounts (maximum number of dimensions is 3)
    currentVarData.writeStarts.resize(3);
    currentVarData.writeCounts.resize(3);
    unsigned int dim_idx = 0;

    // First: time
    if (currentVarData.has_tsteps) {
      // Non-set variables with timesteps
      // 3 dimensions like (time, cells, layers)
      // 2 dimensions like (time, cells)
      assert(3 == varDims.size() || 2 == varDims.size());

      // Time should be the first dimension
      assert(tDim == varDims[0]);

      currentVarData.writeStarts[dim_idx] = 0; // This value is timestep dependent, will be set later
      currentVarData.writeCounts[dim_idx] = 1;
      dim_idx++;
    }
    else {
      // Non-set variables without timesteps
      // 2 dimensions like (cells, layers)
      // 1 dimension like (cells)
      assert(2 == varDims.size() || 1 == varDims.size());
    }

    // Next: corners / cells / edges
    switch (currentVarData.entLoc) {
      case WriteNC::ENTLOCVERT:
        // Vertices
        // Start from the first localGidVerts
        // Actually, this will be reset later for writing
        currentVarData.writeStarts[dim_idx] = localGidVertsOwned[0] - 1;
        currentVarData.writeCounts[dim_idx] = localGidVertsOwned.size();
        break;
      case WriteNC::ENTLOCFACE:
        // Faces
        // Start from the first localGidCells
        // Actually, this will be reset later for writing
        currentVarData.writeStarts[dim_idx] = localGidCellsOwned[0] - 1;
        currentVarData.writeCounts[dim_idx] = localGidCellsOwned.size();
        break;
      case WriteNC::ENTLOCEDGE:
        // Edges
        // Start from the first localGidEdges
        // Actually, this will be reset later for writing
        currentVarData.writeStarts[dim_idx] = localGidEdgesOwned[0] - 1;
        currentVarData.writeCounts[dim_idx] = localGidEdgesOwned.size();
        break;
      default:
        MB_SET_ERR(MB_FAILURE, "Unexpected entity location type for variable " << varname);
    }
    dim_idx++;

    // Finally: layers or other optional levels, it is possible that there is no
    // level dimension (numLev is 0) for this non-set variable
    if (currentVarData.numLev > 0) {
      // Non-set variables with levels
      // 3 dimensions like (time, cells, layers)
      // 2 dimensions like (cells, layers)
      assert(3 == varDims.size() || 2 == varDims.size());

      currentVarData.writeStarts[dim_idx] = 0;
      currentVarData.writeCounts[dim_idx] = currentVarData.numLev;
      dim_idx++;
    }
    else {
      // Non-set variables without levels
      // 2 dimensions like (time, cells)
      // 1 dimension like (cells)
      assert(2 == varDims.size() || 1 == varDims.size());
    }

    // Get variable size
    currentVarData.sz = 1;
    for (std::size_t idx = 0; idx < dim_idx; idx++)
      currentVarData.sz *= currentVarData.writeCounts[idx];
  } // for (size_t i = 0; i < var_names.size(); i++)

  return MB_SUCCESS;
}

ErrorCode NCWriteGCRM::write_nonset_variables(std::vector<WriteNC::VarData>& vdatas, std::vector<int>& tstep_nums)
{
  Interface*& mbImpl = _writeNC->mbImpl;

  int success;

  // For each indexed variable tag, write a time step data
  for (unsigned int i = 0; i < vdatas.size(); i++) {
    WriteNC::VarData& variableData = vdatas[i];

    // Skip edge variables, if there are no edges
    if (localEdgesOwned.empty() && variableData.entLoc == WriteNC::ENTLOCEDGE)
      continue;

    // Get local owned entities of this variable
    Range* pLocalEntsOwned = NULL;
    Range* pLocalGidEntsOwned = NULL;
    switch (variableData.entLoc) {
      case WriteNC::ENTLOCVERT:
        // Vertices
        pLocalEntsOwned = &localVertsOwned;
        pLocalGidEntsOwned = &localGidVertsOwned;
        break;
      case WriteNC::ENTLOCEDGE:
        // Edges
        pLocalEntsOwned = &localEdgesOwned;
        pLocalGidEntsOwned = &localGidEdgesOwned;
        break;
      case WriteNC::ENTLOCFACE:
        // Cells
        pLocalEntsOwned = &localCellsOwned;
        pLocalGidEntsOwned = &localGidCellsOwned;
        break;
      default:
        MB_SET_ERR(MB_FAILURE, "Unexpected entity location type for variable " << variableData.varName);
    }

    unsigned int num_timesteps;
    unsigned int ents_idx = 0;
    if (variableData.has_tsteps) {
      // Non-set variables with timesteps
      // 3 dimensions like (time, cells, layers)
      // 2 dimensions like (time, cells)
      num_timesteps = tstep_nums.size();
      ents_idx++;
    }
    else {
      // Non-set variables without timesteps
      // 2 dimensions like (cells, layers)
      // 1 dimension like (cells)
      num_timesteps = 1;
    }

    unsigned int num_lev;
    if (variableData.numLev > 0) {
      // Non-set variables with levels
      // 3 dimensions like (time, cells, layers)
      // 2 dimensions like (cells, layers)
      num_lev = variableData.numLev;
    }
    else {
      // Non-set variables without levels
      // 2 dimensions like (time, cells)
      // 1 dimension like (cells)
      num_lev = 1;
    }

    for (unsigned int t = 0; t < num_timesteps; t++) {
      // We will write one time step, and count will be one; start will be different
      // Use tag_get_data instead of tag_iterate to copy tag data, as localEntsOwned
      // might not be contiguous.
      if (tDim == variableData.varDims[0])
        variableData.writeStarts[0] = t; // This is start for time
      std::vector<double> tag_data(pLocalEntsOwned->size() * num_lev);
      ErrorCode rval = mbImpl->tag_get_data(variableData.varTags[t], *pLocalEntsOwned,
                                            &tag_data[0]);MB_CHK_SET_ERR(rval, "Trouble getting tag data on owned entities");

#ifdef MOAB_HAVE_PNETCDF
      size_t nb_writes = pLocalGidEntsOwned->psize();
      std::vector<int> requests(nb_writes), statuss(nb_writes);
      size_t idxReq = 0;
#endif

      // Now write copied tag data
      // Use nonblocking put (request aggregation)
      switch (variableData.varDataType) {
        case NC_DOUBLE: {
          size_t indexInDoubleArray = 0;
          size_t ic = 0;
          for (Range::pair_iterator pair_iter = pLocalGidEntsOwned->pair_begin();
              pair_iter != pLocalGidEntsOwned->pair_end(); ++pair_iter, ic++) {
            EntityHandle starth = pair_iter->first;
            EntityHandle endh = pair_iter->second;
            variableData.writeStarts[ents_idx] = (NCDF_SIZE)(starth - 1);
            variableData.writeCounts[ents_idx] = (NCDF_SIZE)(endh - starth + 1);

            // Do a partial write, in each subrange
#ifdef MOAB_HAVE_PNETCDF
            // Wait outside this loop
            success = NCFUNCREQP(_vara_double)(_fileId, variableData.varId,
                &(variableData.writeStarts[0]), &(variableData.writeCounts[0]),
                           &(tag_data[indexInDoubleArray]), &requests[idxReq++]);
#else
            success = NCFUNCAP(_vara_double)(_fileId, variableData.varId,
                &(variableData.writeStarts[0]), &(variableData.writeCounts[0]),
                           &(tag_data[indexInDoubleArray]));
#endif
            if (success)
              MB_SET_ERR(MB_FAILURE, "Failed to write double data in a loop for variable " << variableData.varName);
            // We need to increment the index in double array for the
            // next subrange
            indexInDoubleArray += (endh - starth + 1) * num_lev;
          }
          assert(ic == pLocalGidEntsOwned->psize());
#ifdef MOAB_HAVE_PNETCDF
          success = ncmpi_wait_all(_fileId, requests.size(), &requests[0], &statuss[0]);
          if (success)
            MB_SET_ERR(MB_FAILURE, "Failed on wait_all");
#endif
          break;
        }
        default:
          MB_SET_ERR(MB_NOT_IMPLEMENTED, "Writing non-double data is not implemented yet");
      }
    }
  }

  return MB_SUCCESS;
}

} /* namespace moab */
