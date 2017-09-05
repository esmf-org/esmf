#include "NCHelperGCRM.hpp"
#include "moab/ReadUtilIface.hpp"
#include "moab/FileOptions.hpp"
#include "moab/SpectralMeshTool.hpp"
#include "MBTagConventions.hpp"

#ifdef MOAB_HAVE_ZOLTAN
#include "moab/ZoltanPartitioner.hpp"
#endif

#include <cmath>

namespace moab {

// GCRM cells are either pentagons or hexagons, and pentagons are always padded to hexagons
const int EDGES_PER_CELL = 6;

NCHelperGCRM::NCHelperGCRM(ReadNC* readNC, int fileId, const FileOptions& opts, EntityHandle fileSet)
: UcdNCHelper(readNC, fileId, opts, fileSet)
, createGatherSet(false)
{
  // Ignore variables containing topological information
  ignoredVarNames.insert("grid");
  ignoredVarNames.insert("cell_corners");
  ignoredVarNames.insert("cell_edges");
  ignoredVarNames.insert("edge_corners");
  ignoredVarNames.insert("cell_neighbors");
}

bool NCHelperGCRM::can_read_file(ReadNC* readNC)
{
  std::vector<std::string>& dimNames = readNC->dimNames;

  // If dimension name "cells" exists then it should be the GCRM grid
  if (std::find(dimNames.begin(), dimNames.end(), std::string("cells")) != dimNames.end())
    return true;

  return false;
}

ErrorCode NCHelperGCRM::init_mesh_vals()
{
  std::vector<std::string>& dimNames = _readNC->dimNames;
  std::vector<int>& dimLens = _readNC->dimLens;
  std::map<std::string, ReadNC::VarData>& varInfo = _readNC->varInfo;

  ErrorCode rval;
  unsigned int idx;
  std::vector<std::string>::iterator vit;

  // Look for time dimension
  if ((vit = std::find(dimNames.begin(), dimNames.end(), "Time")) != dimNames.end())
    idx = vit - dimNames.begin();
  else if ((vit = std::find(dimNames.begin(), dimNames.end(), "time")) != dimNames.end())
    idx = vit - dimNames.begin();
  else {
    MB_SET_ERR(MB_FAILURE, "Couldn't find 'Time' or 'time' dimension");
  }
  tDim = idx;
  nTimeSteps = dimLens[idx];

  // Get number of cells
  if ((vit = std::find(dimNames.begin(), dimNames.end(), "cells")) != dimNames.end())
    idx = vit - dimNames.begin();
  else {
    MB_SET_ERR(MB_FAILURE, "Couldn't find 'cells' dimension");
  }
  cDim = idx;
  nCells = dimLens[idx];

  // Get number of edges
  if ((vit = std::find(dimNames.begin(), dimNames.end(), "edges")) != dimNames.end())
    idx = vit - dimNames.begin();
  else {
    MB_SET_ERR(MB_FAILURE, "Couldn't find 'edges' dimension");
  }
  eDim = idx;
  nEdges = dimLens[idx];

  // Get number of vertices
  vDim = -1;
  if ((vit = std::find(dimNames.begin(), dimNames.end(), "corners")) != dimNames.end())
    idx = vit - dimNames.begin();
  else {
    MB_SET_ERR(MB_FAILURE, "Couldn't find 'corners' dimension");
  }
  vDim = idx;
  nVertices = dimLens[idx];

  // Get number of layers
  if ((vit = std::find(dimNames.begin(), dimNames.end(), "layers")) != dimNames.end())
    idx = vit - dimNames.begin();
  else {
    MB_SET_ERR(MB_FAILURE, "Couldn't find 'layers' dimension");
  }
  levDim = idx;
  nLevels = dimLens[idx];

  // Dimension indices for other optional levels
  std::vector<unsigned int> opt_lev_dims;

  // Get index of interface levels
  if ((vit = std::find(dimNames.begin(), dimNames.end(), "interfaces")) != dimNames.end()) {
    idx = vit - dimNames.begin();
    opt_lev_dims.push_back(idx);
  }

  std::map<std::string, ReadNC::VarData>::iterator vmit;

  // Store time coordinate values in tVals
  if (nTimeSteps > 0) {
    if ((vmit = varInfo.find("time")) != varInfo.end()) {
      rval = read_coordinate("time", 0, nTimeSteps - 1, tVals);MB_CHK_SET_ERR(rval, "Trouble reading 'time' variable");
    }
    else if ((vmit = varInfo.find("t")) != varInfo.end()) {
      rval = read_coordinate("t", 0, nTimeSteps - 1, tVals);MB_CHK_SET_ERR(rval, "Trouble reading 't' variable");
    }
    else {
      // If expected time variable is not available, set dummy time coordinate values to tVals
      for (int t = 0; t < nTimeSteps; t++)
        tVals.push_back((double)t);
    }
  }

  // For each variable, determine the entity location type and number of levels
  for (vmit = varInfo.begin(); vmit != varInfo.end(); ++vmit) {
    ReadNC::VarData& vd = (*vmit).second;

    // Default entLoc is ENTLOCSET
    if (std::find(vd.varDims.begin(), vd.varDims.end(), tDim) != vd.varDims.end()) {
      if (std::find(vd.varDims.begin(), vd.varDims.end(), vDim) != vd.varDims.end())
        vd.entLoc = ReadNC::ENTLOCVERT;
      else if (std::find(vd.varDims.begin(), vd.varDims.end(), eDim) != vd.varDims.end())
        vd.entLoc = ReadNC::ENTLOCEDGE;
      else if (std::find(vd.varDims.begin(), vd.varDims.end(), cDim) != vd.varDims.end())
        vd.entLoc = ReadNC::ENTLOCFACE;
    }

    // Default numLev is 0
    if (std::find(vd.varDims.begin(), vd.varDims.end(), levDim) != vd.varDims.end())
      vd.numLev = nLevels;
    else {
      // If layers dimension is not found, try other optional levels such as interfaces
      for (unsigned int i = 0; i < opt_lev_dims.size(); i++) {
        if (std::find(vd.varDims.begin(), vd.varDims.end(), opt_lev_dims[i]) != vd.varDims.end()) {
          vd.numLev = dimLens[opt_lev_dims[i]];
          break;
        }
      }
    }
  }

  // Hack: create dummy variables for dimensions (like cells) with no corresponding coordinate variables
  rval = create_dummy_variables();MB_CHK_SET_ERR(rval, "Failed to create dummy variables");

  return MB_SUCCESS;
}

// When noMesh option is used on this read, the old ReadNC class instance for last read can get out
// of scope (and deleted). The old instance initialized some variables properly when the mesh was
// created, but they are now lost. The new instance (will not create the mesh with noMesh option)
// has to restore them based on the existing mesh from last read
ErrorCode NCHelperGCRM::check_existing_mesh()
{
  Interface*& mbImpl = _readNC->mbImpl;
  Tag& mGlobalIdTag = _readNC->mGlobalIdTag;
  bool& noMesh = _readNC->noMesh;

  if (noMesh) {
    ErrorCode rval;

    if (localGidVerts.empty()) {
      // Get all vertices from current file set (it is the input set in no_mesh scenario)
      Range local_verts;
      rval = mbImpl->get_entities_by_dimension(_fileSet, 0, local_verts);MB_CHK_SET_ERR(rval, "Trouble getting local vertices in current file set");

      if (!local_verts.empty()) {
        std::vector<int> gids(local_verts.size());

        // !IMPORTANT : this has to be the GLOBAL_ID tag
        rval = mbImpl->tag_get_data(mGlobalIdTag, local_verts, &gids[0]);MB_CHK_SET_ERR(rval, "Trouble getting local gid values of vertices");

        // Restore localGidVerts
        std::copy(gids.rbegin(), gids.rend(), range_inserter(localGidVerts));
        nLocalVertices = localGidVerts.size();
      }
    }

    if (localGidEdges.empty()) {
      // Get all edges from current file set (it is the input set in no_mesh scenario)
      Range local_edges;
      rval = mbImpl->get_entities_by_dimension(_fileSet, 1, local_edges);MB_CHK_SET_ERR(rval, "Trouble getting local edges in current file set");

      if (!local_edges.empty()) {
        std::vector<int> gids(local_edges.size());

        // !IMPORTANT : this has to be the GLOBAL_ID tag
        rval = mbImpl->tag_get_data(mGlobalIdTag, local_edges, &gids[0]);MB_CHK_SET_ERR(rval, "Trouble getting local gid values of edges");

        // Restore localGidEdges
        std::copy(gids.rbegin(), gids.rend(), range_inserter(localGidEdges));
        nLocalEdges = localGidEdges.size();
      }
    }

    if (localGidCells.empty()) {
      // Get all cells from current file set (it is the input set in no_mesh scenario)
      Range local_cells;
      rval = mbImpl->get_entities_by_dimension(_fileSet, 2, local_cells);MB_CHK_SET_ERR(rval, "Trouble getting local cells in current file set");

      if (!local_cells.empty()) {
        std::vector<int> gids(local_cells.size());

        // !IMPORTANT : this has to be the GLOBAL_ID tag
        rval = mbImpl->tag_get_data(mGlobalIdTag, local_cells, &gids[0]);MB_CHK_SET_ERR(rval, "Trouble getting local gid values of cells");

        // Restore localGidCells
        std::copy(gids.rbegin(), gids.rend(), range_inserter(localGidCells));
        nLocalCells = localGidCells.size();
      }
    }
  }

  return MB_SUCCESS;
}

ErrorCode NCHelperGCRM::create_mesh(Range& faces)
{
  bool& noEdges = _readNC->noEdges;
  DebugOutput& dbgOut = _readNC->dbgOut;

#ifdef MOAB_HAVE_MPI
  int rank = 0;
  int procs = 1;
  bool& isParallel = _readNC->isParallel;
  if (isParallel) {
    ParallelComm*& myPcomm = _readNC->myPcomm;
    rank = myPcomm->proc_config().proc_rank();
    procs = myPcomm->proc_config().proc_size();
  }

  // Need to know whether we'll be creating gather mesh
  if (rank == _readNC->gatherSetRank)
    createGatherSet = true;

  if (procs >= 2) {
    // Shift rank to obtain a rotated trivial partition
    int shifted_rank = rank;
    int& trivialPartitionShift = _readNC->trivialPartitionShift;
    if (trivialPartitionShift > 0)
      shifted_rank = (rank + trivialPartitionShift) % procs;

    // Compute the number of local cells on this proc
    nLocalCells = int(std::floor(1.0 * nCells / procs));

    // The starting global cell index in the GCRM file for this proc
    int start_cell_idx = shifted_rank * nLocalCells;

    // Number of extra cells after equal split over procs
    int iextra = nCells % procs;

    // Allocate extra cells over procs
    if (shifted_rank < iextra)
      nLocalCells++;
    start_cell_idx += std::min(shifted_rank, iextra);

    start_cell_idx++; // 0 based -> 1 based

    // Redistribute local cells after trivial partition (e.g. apply Zoltan partition)
    ErrorCode rval = redistribute_local_cells(start_cell_idx);MB_CHK_SET_ERR(rval, "Failed to redistribute local cells after trivial partition");
  }
  else {
    nLocalCells = nCells;
    localGidCells.insert(1, nLocalCells);
  }
#else
  nLocalCells = nCells;
  localGidCells.insert(1, nLocalCells);
#endif
  dbgOut.tprintf(1, " localGidCells.psize() = %d\n", (int)localGidCells.psize());
  dbgOut.tprintf(1, " localGidCells.size() = %d\n", (int)localGidCells.size());

  // Read vertices on each local cell, to get localGidVerts and cell connectivity later
  int verticesOnCellVarId;
  int success = NCFUNC(inq_varid)(_fileId, "cell_corners", &verticesOnCellVarId);
  if (success)
    MB_SET_ERR(MB_FAILURE, "Failed to get variable id of cell_corners");
  std::vector<int> vertices_on_local_cells(nLocalCells * EDGES_PER_CELL);
  dbgOut.tprintf(1, " nLocalCells = %d\n", (int)nLocalCells);
  dbgOut.tprintf(1, " vertices_on_local_cells.size() = %d\n", (int)vertices_on_local_cells.size());
#ifdef MOAB_HAVE_PNETCDF
  size_t nb_reads = localGidCells.psize();
  std::vector<int> requests(nb_reads);
  std::vector<int> statuss(nb_reads);
  size_t idxReq = 0;
#endif
  size_t indexInArray = 0;
  for (Range::pair_iterator pair_iter = localGidCells.pair_begin();
       pair_iter != localGidCells.pair_end();
       ++pair_iter) {
    EntityHandle starth = pair_iter->first;
    EntityHandle endh = pair_iter->second;
    dbgOut.tprintf(1, " cell_corners starth = %d\n", (int)starth);
    dbgOut.tprintf(1, " cell_corners   endh = %d\n", (int)endh);
    NCDF_SIZE read_starts[2] = {static_cast<NCDF_SIZE>(starth - 1), 0};
    NCDF_SIZE read_counts[2] = {static_cast<NCDF_SIZE>(endh - starth + 1), 
                                static_cast<NCDF_SIZE>(EDGES_PER_CELL)};

    // Do a partial read in each subrange
#ifdef MOAB_HAVE_PNETCDF
    success = NCFUNCREQG(_vara_int)(_fileId, verticesOnCellVarId, read_starts, read_counts,
                                      &(vertices_on_local_cells[indexInArray]), &requests[idxReq++]);
#else
    success = NCFUNCAG(_vara_int)(_fileId, verticesOnCellVarId, read_starts, read_counts,
                                      &(vertices_on_local_cells[indexInArray]));
#endif
    if (success)
      MB_SET_ERR(MB_FAILURE, "Failed to read cell_corners data in a loop");

    // Increment the index for next subrange
    indexInArray += (endh - starth + 1) * EDGES_PER_CELL;
  }

#ifdef MOAB_HAVE_PNETCDF
  // Wait outside the loop
  success = NCFUNC(wait_all)(_fileId, requests.size(), &requests[0], &statuss[0]);
  if (success)
    MB_SET_ERR(MB_FAILURE, "Failed on wait_all");
#endif

  // Correct vertices_on_local_cells array. Pentagons as hexagons should have
  // a connectivity like 123455 and not 122345
  for (int local_cell_idx = 0; local_cell_idx < nLocalCells; local_cell_idx++) {
    int* pvertex = &vertices_on_local_cells[local_cell_idx * EDGES_PER_CELL];
    for (int k = 0; k < EDGES_PER_CELL - 2; k++) {
      if (*(pvertex + k) == *(pvertex + k + 1)) {
        // Shift the connectivity
        for (int kk = k + 1; kk < EDGES_PER_CELL - 1; kk++)
          *(pvertex + kk) = *(pvertex + kk + 1);
        // No need to try next k
        break;
      }
    }
  }

  // GCRM is 0 based, convert vertex indices from 0 to 1 based
  for (std::size_t idx = 0; idx < vertices_on_local_cells.size(); idx++)
    vertices_on_local_cells[idx] += 1;

  // Create local vertices
  EntityHandle start_vertex;
  ErrorCode rval = create_local_vertices(vertices_on_local_cells, start_vertex);MB_CHK_SET_ERR(rval, "Failed to create local vertices for GCRM mesh");

  // Create local edges (unless NO_EDGES read option is set)
  if (!noEdges) {
    rval = create_local_edges(start_vertex);MB_CHK_SET_ERR(rval, "Failed to create local edges for GCRM mesh");
  }

  // Create local cells, padded
  rval = create_padded_local_cells(vertices_on_local_cells, start_vertex, faces);MB_CHK_SET_ERR(rval, "Failed to create padded local cells for GCRM mesh");

  if (createGatherSet) {
    EntityHandle gather_set;
    rval = _readNC->readMeshIface->create_gather_set(gather_set);MB_CHK_SET_ERR(rval, "Failed to create gather set");

    // Create gather set vertices
    EntityHandle start_gather_set_vertex;
    rval = create_gather_set_vertices(gather_set, start_gather_set_vertex);MB_CHK_SET_ERR(rval, "Failed to create gather set vertices for GCRM mesh");

    // Create gather set edges (unless NO_EDGES read option is set)
    if (!noEdges) {
      rval = create_gather_set_edges(gather_set, start_gather_set_vertex);MB_CHK_SET_ERR(rval, "Failed to create gather set edges for GCRM mesh");
    }

    // Create gather set cells with padding
    rval = create_padded_gather_set_cells(gather_set, start_gather_set_vertex);MB_CHK_SET_ERR(rval, "Failed to create padded gather set cells for GCRM mesh");
  }

  return MB_SUCCESS;
}

ErrorCode NCHelperGCRM::read_ucd_variables_to_nonset_allocate(std::vector<ReadNC::VarData>& vdatas, std::vector<int>& tstep_nums)
{
  Interface*& mbImpl = _readNC->mbImpl;
  std::vector<int>& dimLens = _readNC->dimLens;
  bool& noEdges = _readNC->noEdges;
  DebugOutput& dbgOut = _readNC->dbgOut;

  Range* range = NULL;

  // Get vertices
  Range verts;
  ErrorCode rval = mbImpl->get_entities_by_dimension(_fileSet, 0, verts);MB_CHK_SET_ERR(rval, "Trouble getting vertices in current file set");
  assert("Should only have a single vertex subrange, since they were read in one shot" && verts.psize() == 1);

  // Get edges
  Range edges;
  rval = mbImpl->get_entities_by_dimension(_fileSet, 1, edges);MB_CHK_SET_ERR(rval, "Trouble getting edges in current file set");

  // Get faces
  Range faces;
  rval = mbImpl->get_entities_by_dimension(_fileSet, 2, faces);MB_CHK_SET_ERR(rval, "Trouble getting faces in current file set");
  assert("Should only have a single face subrange, since they were read in one shot" && faces.psize() == 1);

#ifdef MOAB_HAVE_MPI
  bool& isParallel = _readNC->isParallel;
  if (isParallel) {
    ParallelComm*& myPcomm = _readNC->myPcomm;
    rval = myPcomm->filter_pstatus(faces, PSTATUS_NOT_OWNED, PSTATUS_NOT, -1, &facesOwned);MB_CHK_SET_ERR(rval, "Trouble getting owned faces in current file set");
  }
  else
    facesOwned = faces; // not running in parallel, but still with MPI
#else
  facesOwned = faces;
#endif

  for (unsigned int i = 0; i < vdatas.size(); i++) {
    // Skip edge variables, if specified by the read options
    if (noEdges && vdatas[i].entLoc == ReadNC::ENTLOCEDGE)
      continue;

    // Support non-set variables with 3 dimensions like (time, cells, layers), or
    // 2 dimensions like (time, cells)
    assert(3 == vdatas[i].varDims.size() || 2 == vdatas[i].varDims.size());

    // For a non-set variable, time should be the first dimension
    assert(tDim == vdatas[i].varDims[0]);

    // Set up readStarts and readCounts
    vdatas[i].readStarts.resize(3);
    vdatas[i].readCounts.resize(3);

    // First: Time
    vdatas[i].readStarts[0] = 0; // This value is timestep dependent, will be set later
    vdatas[i].readCounts[0] = 1;

    // Next: nVertices / nCells / nEdges
    switch (vdatas[i].entLoc) {
      case ReadNC::ENTLOCVERT:
        // Vertices
        // Start from the first localGidVerts
        // Actually, this will be reset later on in a loop
        vdatas[i].readStarts[1] = localGidVerts[0] - 1;
        vdatas[i].readCounts[1] = nLocalVertices;
        range = &verts;
        break;
      case ReadNC::ENTLOCFACE:
        // Faces
        // Start from the first localGidCells
        // Actually, this will be reset later on in a loop
        vdatas[i].readStarts[1] = localGidCells[0] - 1;
        vdatas[i].readCounts[1] = nLocalCells;
        range = &facesOwned;
        break;
      case ReadNC::ENTLOCEDGE:
        // Edges
        // Start from the first localGidEdges
        // Actually, this will be reset later on in a loop
        vdatas[i].readStarts[1] = localGidEdges[0] - 1;
        vdatas[i].readCounts[1] = nLocalEdges;
        range = &edges;
        break;
      default:
        MB_SET_ERR(MB_FAILURE, "Unexpected entity location type for variable " << vdatas[i].varName);
    }

    // Finally: layers or other optional levels, it is possible that there is no
    // level dimension (numLev is 0) for this non-set variable
    if (vdatas[i].numLev < 1)
      vdatas[i].numLev = 1;
    vdatas[i].readStarts[2] = 0;
    vdatas[i].readCounts[2] = vdatas[i].numLev;

    // Get variable size
    vdatas[i].sz = 1;
    for (std::size_t idx = 0; idx != 3; idx++)
      vdatas[i].sz *= vdatas[i].readCounts[idx];

    for (unsigned int t = 0; t < tstep_nums.size(); t++) {
      dbgOut.tprintf(2, "Reading variable %s, time step %d\n", vdatas[i].varName.c_str(), tstep_nums[t]);

      if (tstep_nums[t] >= dimLens[tDim]) {
        MB_SET_ERR(MB_INDEX_OUT_OF_RANGE, "Wrong value for timestep number " << tstep_nums[t]);
      }

      // Get the tag to read into
      if (!vdatas[i].varTags[t]) {
        rval = get_tag_to_nonset(vdatas[i], tstep_nums[t], vdatas[i].varTags[t], vdatas[i].numLev);MB_CHK_SET_ERR(rval, "Trouble getting tag for variable " << vdatas[i].varName);
      }

      // Get ptr to tag space
      assert(1 == range->psize());
      void* data;
      int count;
      rval = mbImpl->tag_iterate(vdatas[i].varTags[t], range->begin(), range->end(), count, data);MB_CHK_SET_ERR(rval, "Failed to iterate tag for variable " << vdatas[i].varName);
      assert((unsigned)count == range->size());
      vdatas[i].varDatas[t] = data;
    }
  }

  return rval;
}

#ifdef MOAB_HAVE_PNETCDF
ErrorCode NCHelperGCRM::read_ucd_variables_to_nonset_async(std::vector<ReadNC::VarData>& vdatas, std::vector<int>& tstep_nums)
{
  bool& noEdges = _readNC->noEdges;
  DebugOutput& dbgOut = _readNC->dbgOut;

  ErrorCode rval = read_ucd_variables_to_nonset_allocate(vdatas, tstep_nums);MB_CHK_SET_ERR(rval, "Trouble allocating space to read non-set variables");

  // Finally, read into that space
  int success;
  Range* pLocalGid = NULL;

  for (unsigned int i = 0; i < vdatas.size(); i++) {
    // Skip edge variables, if specified by the read options
    if (noEdges && vdatas[i].entLoc == ReadNC::ENTLOCEDGE)
      continue;

    switch (vdatas[i].entLoc) {
      case ReadNC::ENTLOCVERT:
        pLocalGid = &localGidVerts;
        break;
      case ReadNC::ENTLOCFACE:
        pLocalGid = &localGidCells;
        break;
      case ReadNC::ENTLOCEDGE:
        pLocalGid = &localGidEdges;
        break;
      default:
        MB_SET_ERR(MB_FAILURE, "Unexpected entity location type for variable " << vdatas[i].varName);
    }

    std::size_t sz = vdatas[i].sz;

    for (unsigned int t = 0; t < tstep_nums.size(); t++) {
      // We will synchronize all these reads with the other processors,
      // so the wait will be inside this double loop; is it too much?
      size_t nb_reads = pLocalGid->psize();
      std::vector<int> requests(nb_reads), statuss(nb_reads);
      size_t idxReq = 0;

      // Set readStart for each timestep along time dimension
      vdatas[i].readStarts[0] = tstep_nums[t];

      switch (vdatas[i].varDataType) {
        case NC_FLOAT:
        case NC_DOUBLE: {
          // Read float as double
          std::vector<double> tmpdoubledata(sz);

          // In the case of ucd mesh, and on multiple proc,
          // we need to read as many times as subranges we have in the
          // localGid range;
          // basically, we have to give a different point
          // for data to start, for every subrange :(
          size_t indexInDoubleArray = 0;
          size_t ic = 0;
          for (Range::pair_iterator pair_iter = pLocalGid->pair_begin();
              pair_iter != pLocalGid->pair_end();
              ++pair_iter, ic++) {
            EntityHandle starth = pair_iter->first;
            EntityHandle endh = pair_iter->second; // inclusive
            vdatas[i].readStarts[1] = (NCDF_SIZE) (starth - 1);
            vdatas[i].readCounts[1] = (NCDF_SIZE) (endh - starth + 1);

            // Do a partial read, in each subrange
            // wait outside this loop
            success = NCFUNCREQG(_vara_double)(_fileId, vdatas[i].varId,
                &(vdatas[i].readStarts[0]), &(vdatas[i].readCounts[0]),
                            &(tmpdoubledata[indexInDoubleArray]), &requests[idxReq++]);
            if (success)
              MB_SET_ERR(MB_FAILURE, "Failed to read double data in a loop for variable " << vdatas[i].varName);
            // We need to increment the index in double array for the
            // next subrange
            indexInDoubleArray += (endh - starth + 1) * 1 * vdatas[i].numLev;
          }
          assert(ic == pLocalGid->psize());

          success = NCFUNC(wait_all)(_fileId, requests.size(), &requests[0], &statuss[0]);
          if (success)
            MB_SET_ERR(MB_FAILURE, "Failed on wait_all");

          void* data = vdatas[i].varDatas[t];
          for (std::size_t idx = 0; idx != tmpdoubledata.size(); idx++)
            ((double*) data)[idx] = tmpdoubledata[idx];

          break;
        }
        default:
          MB_SET_ERR(MB_FAILURE, "Unexpected data type for variable " << vdatas[i].varName);
      }
    }
  }

  // Debug output, if requested
  if (1 == dbgOut.get_verbosity()) {
    dbgOut.printf(1, "Read variables: %s", vdatas.begin()->varName.c_str());
    for (unsigned int i = 1; i < vdatas.size(); i++)
      dbgOut.printf(1, ", %s ", vdatas[i].varName.c_str());
    dbgOut.tprintf(1, "\n");
  }

  return rval;
}
#else
ErrorCode NCHelperGCRM::read_ucd_variables_to_nonset(std::vector<ReadNC::VarData>& vdatas, std::vector<int>& tstep_nums)
{
  bool& noEdges = _readNC->noEdges;
  DebugOutput& dbgOut = _readNC->dbgOut;

  ErrorCode rval = read_ucd_variables_to_nonset_allocate(vdatas, tstep_nums);MB_CHK_SET_ERR(rval, "Trouble allocating space to read non-set variables");

  // Finally, read into that space
  int success;
  Range* pLocalGid = NULL;

  for (unsigned int i = 0; i < vdatas.size(); i++) {
    // Skip edge variables, if specified by the read options
    if (noEdges && vdatas[i].entLoc == ReadNC::ENTLOCEDGE)
      continue;

    switch (vdatas[i].entLoc) {
      case ReadNC::ENTLOCVERT:
        pLocalGid = &localGidVerts;
        break;
      case ReadNC::ENTLOCFACE:
        pLocalGid = &localGidCells;
        break;
      case ReadNC::ENTLOCEDGE:
        pLocalGid = &localGidEdges;
        break;
      default:
        MB_SET_ERR(MB_FAILURE, "Unexpected entity location type for variable " << vdatas[i].varName);
    }

    std::size_t sz = vdatas[i].sz;

    for (unsigned int t = 0; t < tstep_nums.size(); t++) {
      // Set readStart for each timestep along time dimension
      vdatas[i].readStarts[0] = tstep_nums[t];

      switch (vdatas[i].varDataType) {
        case NC_FLOAT:
        case NC_DOUBLE: {
          // Read float as double
          std::vector<double> tmpdoubledata(sz);

          // In the case of ucd mesh, and on multiple proc,
          // we need to read as many times as subranges we have in the
          // localGid range;
          // basically, we have to give a different point
          // for data to start, for every subrange :(
          size_t indexInDoubleArray = 0;
          size_t ic = 0;
          for (Range::pair_iterator pair_iter = pLocalGid->pair_begin();
              pair_iter != pLocalGid->pair_end();
              ++pair_iter, ic++) {
            EntityHandle starth = pair_iter->first;
            EntityHandle endh = pair_iter->second; // Inclusive
            vdatas[i].readStarts[1] = (NCDF_SIZE) (starth - 1);
            vdatas[i].readCounts[1] = (NCDF_SIZE) (endh - starth + 1);

            success = NCFUNCAG(_vara_double)(_fileId, vdatas[i].varId,
                &(vdatas[i].readStarts[0]), &(vdatas[i].readCounts[0]),
                            &(tmpdoubledata[indexInDoubleArray]));
            if (success)
              MB_SET_ERR(MB_FAILURE, "Failed to read double data in a loop for variable " << vdatas[i].varName);
            // We need to increment the index in double array for the
            // next subrange
            indexInDoubleArray += (endh - starth + 1) * 1 * vdatas[i].numLev;
          }
          assert(ic == pLocalGid->psize());

          void* data = vdatas[i].varDatas[t];
          for (std::size_t idx = 0; idx != tmpdoubledata.size(); idx++)
            ((double*) data)[idx] = tmpdoubledata[idx];

          break;
        }
        default:
          MB_SET_ERR(MB_FAILURE, "Unexpected data type for variable " << vdatas[i].varName);
      }
    }
  }

  // Debug output, if requested
  if (1 == dbgOut.get_verbosity()) {
    dbgOut.printf(1, "Read variables: %s", vdatas.begin()->varName.c_str());
    for (unsigned int i = 1; i < vdatas.size(); i++)
      dbgOut.printf(1, ", %s ", vdatas[i].varName.c_str());
    dbgOut.tprintf(1, "\n");
  }

  return rval;
}
#endif

#ifdef MOAB_HAVE_MPI
ErrorCode NCHelperGCRM::redistribute_local_cells(int start_cell_idx)
{
  // If possible, apply Zoltan partition
#ifdef MOAB_HAVE_ZOLTAN
  if (_readNC->partMethod == ScdParData::RCBZOLTAN) {
    // Read lat/lon coordinates of cell centers, then convert spherical to
    // Cartesian, and use them as input to Zoltan partition
    int xCellVarId;
    int success = NCFUNC(inq_varid)(_fileId, "grid_center_lat", &xCellVarId);
    if (success)
      MB_SET_ERR(MB_FAILURE, "Failed to get variable id of grid_center_lat");
    std::vector<double> xCell(nLocalCells);
    NCDF_SIZE read_start = static_cast<NCDF_SIZE>(start_cell_idx - 1);
    NCDF_SIZE read_count = static_cast<NCDF_SIZE>(nLocalCells);
    success = NCFUNCAG(_vara_double)(_fileId, xCellVarId, &read_start, &read_count, &xCell[0]);
    if (success)
      MB_SET_ERR(MB_FAILURE, "Failed to read grid_center_lat data");

    // Read y coordinates of cell centers
    int yCellVarId;
    success = NCFUNC(inq_varid)(_fileId, "grid_center_lon", &yCellVarId);
    if (success)
      MB_SET_ERR(MB_FAILURE, "Failed to get variable id of grid_center_lon");
    std::vector<double> yCell(nLocalCells);
    success = NCFUNCAG(_vara_double)(_fileId, yCellVarId, &read_start, &read_count, &yCell[0]);
    if (success)
      MB_SET_ERR(MB_FAILURE, "Failed to read grid_center_lon data");

    // Convert lon/lat/rad to x/y/z
    std::vector<double> zCell(nLocalCells);
    double rad = 8000.0; // This is just a approximate radius
    for (int i = 0; i < nLocalCells; i++) {
      double cosphi = cos(xCell[i]);
      double zmult = sin(xCell[i]);
      double xmult = cosphi * cos(yCell[i]);
      double ymult = cosphi * sin(yCell[i]);
      xCell[i] = rad * xmult;
      yCell[i] = rad * ymult;
      zCell[i] = rad * zmult;
    }

    // Zoltan partition using RCB; maybe more studies would be good, as to which partition
    // is better
    Interface*& mbImpl = _readNC->mbImpl;
    DebugOutput& dbgOut = _readNC->dbgOut;
    ZoltanPartitioner* mbZTool = new ZoltanPartitioner(mbImpl, false, 0, NULL);
    ErrorCode rval = mbZTool->repartition(xCell, yCell, zCell, start_cell_idx, "RCB", localGidCells);MB_CHK_SET_ERR(rval, "Error in Zoltan partitioning");
    delete mbZTool;

    dbgOut.tprintf(1, "After Zoltan partitioning, localGidCells.psize() = %d\n", (int)localGidCells.psize());
    dbgOut.tprintf(1, "                           localGidCells.size() = %d\n", (int)localGidCells.size());

    // This is important: local cells are now redistributed, so nLocalCells might be different!
    nLocalCells = localGidCells.size();

    return MB_SUCCESS;
  }
#endif

  // By default, apply trivial partition
  localGidCells.insert(start_cell_idx, start_cell_idx + nLocalCells - 1);

  return MB_SUCCESS;
}
#endif

ErrorCode NCHelperGCRM::create_local_vertices(const std::vector<int>& vertices_on_local_cells, EntityHandle& start_vertex)
{
  Interface*& mbImpl = _readNC->mbImpl;
  Tag& mGlobalIdTag = _readNC->mGlobalIdTag;
  const Tag*& mpFileIdTag = _readNC->mpFileIdTag;
  DebugOutput& dbgOut = _readNC->dbgOut;
  std::map<std::string, ReadNC::VarData>& varInfo = _readNC->varInfo;

  // Make a copy of vertices_on_local_cells for sorting (keep original one to set cell connectivity later)
  std::vector<int> vertices_on_local_cells_sorted(vertices_on_local_cells);
  std::sort(vertices_on_local_cells_sorted.begin(), vertices_on_local_cells_sorted.end());
  std::copy(vertices_on_local_cells_sorted.rbegin(), vertices_on_local_cells_sorted.rend(), range_inserter(localGidVerts));
  nLocalVertices = localGidVerts.size();

  dbgOut.tprintf(1, "   localGidVerts.psize() = %d\n", (int)localGidVerts.psize());
  dbgOut.tprintf(1, "   localGidVerts.size() = %d\n", (int)localGidVerts.size());

  // Create local vertices
  std::vector<double*> arrays;
  ErrorCode rval = _readNC->readMeshIface->get_node_coords(3, nLocalVertices, 0, start_vertex, arrays,
                                                          // Might have to create gather mesh later
                                                          (createGatherSet ? nLocalVertices + nVertices : nLocalVertices));MB_CHK_SET_ERR(rval, "Failed to create local vertices");

  // Add local vertices to current file set
  Range local_verts_range(start_vertex, start_vertex + nLocalVertices - 1);
  rval = _readNC->mbImpl->add_entities(_fileSet, local_verts_range);MB_CHK_SET_ERR(rval, "Failed to add local vertices to current file set");

  // Get ptr to GID memory for local vertices
  int count = 0;
  void* data = NULL;
  rval = mbImpl->tag_iterate(mGlobalIdTag, local_verts_range.begin(), local_verts_range.end(), count, data);MB_CHK_SET_ERR(rval, "Failed to iterate global id tag on local vertices");
  assert(count == nLocalVertices);
  int* gid_data = (int*) data;
  std::copy(localGidVerts.begin(), localGidVerts.end(), gid_data);

  // Duplicate GID data, which will be used to resolve sharing
  if (mpFileIdTag) {
    rval = mbImpl->tag_iterate(*mpFileIdTag, local_verts_range.begin(), local_verts_range.end(), count, data);MB_CHK_SET_ERR(rval, "Failed to iterate file id tag on local vertices");
    assert(count == nLocalVertices);
    int bytes_per_tag = 4;
    rval = mbImpl->tag_get_bytes(*mpFileIdTag, bytes_per_tag);MB_CHK_SET_ERR(rval, "can't get number of bytes for file id tag");
    if (4 == bytes_per_tag) {
      gid_data = (int*) data;
      std::copy(localGidVerts.begin(), localGidVerts.end(), gid_data);
    }
    else if (8 == bytes_per_tag) { // Should be a handle tag on 64 bit machine?
      long* handle_tag_data = (long*)data;
      std::copy(localGidVerts.begin(), localGidVerts.end(), handle_tag_data);
    }
  }

#ifdef MOAB_HAVE_PNETCDF
  size_t nb_reads = localGidVerts.psize();
  std::vector<int> requests(nb_reads);
  std::vector<int> statuss(nb_reads);
  size_t idxReq = 0;
#endif

  // Store lev values in levVals
  std::map<std::string, ReadNC::VarData>::iterator vmit;
  if ((vmit = varInfo.find("layers")) != varInfo.end() && (*vmit).second.varDims.size() == 1) {
    rval = read_coordinate("layers", 0, nLevels - 1, levVals);MB_CHK_SET_ERR(rval, "Trouble reading 'layers' variable");
  }
  else if ((vmit = varInfo.find("interfaces")) != varInfo.end() && (*vmit).second.varDims.size() == 1) {
    rval = read_coordinate("interfaces", 0, nLevels - 1, levVals);MB_CHK_SET_ERR(rval, "Trouble reading 'interfaces' variable");
  }
  else {
    MB_SET_ERR(MB_FAILURE, "Couldn't find 'layers' or 'interfaces' variable");
  }

  // Decide whether down is positive
  char posval[10] = {0};
  int success = NCFUNC(get_att_text)(_fileId, (*vmit).second.varId, "positive", posval);
  if (0 == success && !strncmp(posval, "down", 4)) {
    for (std::vector<double>::iterator dvit = levVals.begin(); dvit != levVals.end(); ++dvit)
      (*dvit) *= -1.0;
  }

  // Read x coordinates for local vertices
  double* xptr = arrays[0];
  int xVertexVarId;
  success = NCFUNC(inq_varid)(_fileId, "grid_corner_lon", &xVertexVarId);
  if (success)
    MB_SET_ERR(MB_FAILURE, "Failed to get variable id of grid_corner_lon");
  size_t indexInArray = 0;
  for (Range::pair_iterator pair_iter = localGidVerts.pair_begin();
       pair_iter != localGidVerts.pair_end();
       ++pair_iter) {
    EntityHandle starth = pair_iter->first;
    EntityHandle endh = pair_iter->second;
    NCDF_SIZE read_start = (NCDF_SIZE) (starth - 1);
    NCDF_SIZE read_count = (NCDF_SIZE) (endh - starth + 1);

    // Do a partial read in each subrange
#ifdef MOAB_HAVE_PNETCDF
    success = NCFUNCREQG(_vara_double)(_fileId, xVertexVarId, &read_start, &read_count,
                                      &(xptr[indexInArray]), &requests[idxReq++]);
#else
    success = NCFUNCAG(_vara_double)(_fileId, xVertexVarId, &read_start, &read_count,
                                      &(xptr[indexInArray]));
#endif
    if (success)
      MB_SET_ERR(MB_FAILURE, "Failed to read grid_corner_lon data in a loop");

    // Increment the index for next subrange
    indexInArray += (endh - starth + 1);
  }

#ifdef MOAB_HAVE_PNETCDF
  // Wait outside the loop
  success = NCFUNC(wait_all)(_fileId, requests.size(), &requests[0], &statuss[0]);
  if (success)
    MB_SET_ERR(MB_FAILURE, "Failed on wait_all");
#endif

  // Read y coordinates for local vertices
  double* yptr = arrays[1];
  int yVertexVarId;
  success = NCFUNC(inq_varid)(_fileId, "grid_corner_lat", &yVertexVarId);
  if (success)
    MB_SET_ERR(MB_FAILURE, "Failed to get variable id of grid_corner_lat");
#ifdef MOAB_HAVE_PNETCDF
  idxReq = 0;
#endif
  indexInArray = 0;
  for (Range::pair_iterator pair_iter = localGidVerts.pair_begin();
       pair_iter != localGidVerts.pair_end();
       ++pair_iter) {
    EntityHandle starth = pair_iter->first;
    EntityHandle endh = pair_iter->second;
    NCDF_SIZE read_start = (NCDF_SIZE) (starth - 1);
    NCDF_SIZE read_count = (NCDF_SIZE) (endh - starth + 1);

    // Do a partial read in each subrange
#ifdef MOAB_HAVE_PNETCDF
    success = NCFUNCREQG(_vara_double)(_fileId, yVertexVarId, &read_start, &read_count,
                                      &(yptr[indexInArray]), &requests[idxReq++]);
#else
    success = NCFUNCAG(_vara_double)(_fileId, yVertexVarId, &read_start, &read_count,
                                      &(yptr[indexInArray]));
#endif
    if (success)
      MB_SET_ERR(MB_FAILURE, "Failed to read grid_corner_lat data in a loop");

    // Increment the index for next subrange
    indexInArray += (endh - starth + 1);
  }

#ifdef MOAB_HAVE_PNETCDF
  // Wait outside the loop
  success = NCFUNC(wait_all)(_fileId, requests.size(), &requests[0], &statuss[0]);
  if (success)
    MB_SET_ERR(MB_FAILURE, "Failed on wait_all");
#endif

  // Convert lon/lat/rad to x/y/z
  double* zptr = arrays[2];
  double rad = 8000.0 + levVals[0];
  for (int i = 0; i < nLocalVertices; i++) {
    double cosphi = cos(yptr[i]);
    double zmult = sin(yptr[i]);
    double xmult = cosphi * cos(xptr[i]);
    double ymult = cosphi * sin(xptr[i]);
    xptr[i] = rad * xmult;
    yptr[i] = rad * ymult;
    zptr[i] = rad * zmult;
  }

  return MB_SUCCESS;
}

ErrorCode NCHelperGCRM::create_local_edges(EntityHandle start_vertex)
{
  Interface*& mbImpl = _readNC->mbImpl;
  Tag& mGlobalIdTag = _readNC->mGlobalIdTag;
  DebugOutput& dbgOut = _readNC->dbgOut;

  // Read edges on each local cell, to get localGidEdges
  int edgesOnCellVarId;
  int success = NCFUNC(inq_varid)(_fileId, "cell_edges", &edgesOnCellVarId);
  if (success)
    MB_SET_ERR(MB_FAILURE, "Failed to get variable id of cell_edges");

  std::vector<int> edges_on_local_cells(nLocalCells * EDGES_PER_CELL);
  dbgOut.tprintf(1, "   edges_on_local_cells.size() = %d\n", (int)edges_on_local_cells.size());

#ifdef MOAB_HAVE_PNETCDF
  size_t nb_reads = localGidCells.psize();
  std::vector<int> requests(nb_reads);
  std::vector<int> statuss(nb_reads);
  size_t idxReq = 0;
#endif
  size_t indexInArray = 0;
  for (Range::pair_iterator pair_iter = localGidCells.pair_begin();
       pair_iter != localGidCells.pair_end();
       ++pair_iter) {
    EntityHandle starth = pair_iter->first;
    EntityHandle endh = pair_iter->second;
    dbgOut.tprintf(1, "   starth = %d\n", (int)starth);
    dbgOut.tprintf(1, "   endh = %d\n", (int)endh);
    NCDF_SIZE read_starts[2] = {static_cast<NCDF_SIZE>(starth - 1), 0};
    NCDF_SIZE read_counts[2] = {static_cast<NCDF_SIZE>(endh - starth + 1), static_cast<NCDF_SIZE>(EDGES_PER_CELL)};

    // Do a partial read in each subrange
#ifdef MOAB_HAVE_PNETCDF
    success = NCFUNCREQG(_vara_int)(_fileId, edgesOnCellVarId, read_starts, read_counts,
                                      &(edges_on_local_cells[indexInArray]), &requests[idxReq++]);
#else
    success = NCFUNCAG(_vara_int)(_fileId, edgesOnCellVarId, read_starts, read_counts,
                                      &(edges_on_local_cells[indexInArray]));
#endif
    if (success)
      MB_SET_ERR(MB_FAILURE, "Failed to read cell_edges data in a loop");

    // Increment the index for next subrange
    indexInArray += (endh - starth + 1) * EDGES_PER_CELL;
  }

#ifdef MOAB_HAVE_PNETCDF
  // Wait outside the loop
  success = NCFUNC(wait_all)(_fileId, requests.size(), &requests[0], &statuss[0]);
  if (success)
    MB_SET_ERR(MB_FAILURE, "Failed on wait_all");
#endif

  // GCRM is 0 based, convert edge indices from 0 to 1 based
  for (std::size_t idx = 0; idx < edges_on_local_cells.size(); idx++)
    edges_on_local_cells[idx] += 1;

  // Collect local edges
  std::sort(edges_on_local_cells.begin(), edges_on_local_cells.end());
  std::copy(edges_on_local_cells.rbegin(), edges_on_local_cells.rend(), range_inserter(localGidEdges));
  nLocalEdges = localGidEdges.size();

  dbgOut.tprintf(1, "   localGidEdges.psize() = %d\n", (int)localGidEdges.psize());
  dbgOut.tprintf(1, "   localGidEdges.size() = %d\n", (int)localGidEdges.size());

  // Create local edges
  EntityHandle start_edge;
  EntityHandle* conn_arr_edges = NULL;
  ErrorCode rval = _readNC->readMeshIface->get_element_connect(nLocalEdges, 2, MBEDGE, 0, start_edge, conn_arr_edges,
                                                              // Might have to create gather mesh later
                                                              (createGatherSet ? nLocalEdges + nEdges : nLocalEdges));MB_CHK_SET_ERR(rval, "Failed to create local edges");

  // Add local edges to current file set
  Range local_edges_range(start_edge, start_edge + nLocalEdges - 1);
  rval = _readNC->mbImpl->add_entities(_fileSet, local_edges_range);MB_CHK_SET_ERR(rval, "Failed to add local edges to current file set");

  // Get ptr to GID memory for edges
  int count = 0;
  void* data = NULL;
  rval = mbImpl->tag_iterate(mGlobalIdTag, local_edges_range.begin(), local_edges_range.end(), count, data);MB_CHK_SET_ERR(rval, "Failed to iterate global id tag on local edges");
  assert(count == nLocalEdges);
  int* gid_data = (int*) data;
  std::copy(localGidEdges.begin(), localGidEdges.end(), gid_data);

  int verticesOnEdgeVarId;

  // Read vertices on each local edge, to get edge connectivity
  success = NCFUNC(inq_varid)(_fileId, "edge_corners", &verticesOnEdgeVarId);
  if (success)
    MB_SET_ERR(MB_FAILURE, "Failed to get variable id of edge_corners");
  // Utilize the memory storage pointed by conn_arr_edges
  int* vertices_on_local_edges = (int*) conn_arr_edges;
#ifdef MOAB_HAVE_PNETCDF
  nb_reads = localGidEdges.psize();
  requests.resize(nb_reads);
  statuss.resize(nb_reads);
  idxReq = 0;
#endif
  indexInArray = 0;
  for (Range::pair_iterator pair_iter = localGidEdges.pair_begin();
      pair_iter != localGidEdges.pair_end();
      ++pair_iter) {
    EntityHandle starth = pair_iter->first;
    EntityHandle endh = pair_iter->second;
    NCDF_SIZE read_starts[2] = {static_cast<NCDF_SIZE>(starth - 1), 0};
    NCDF_SIZE read_counts[2] = {static_cast<NCDF_SIZE>(endh - starth + 1), 2};

    // Do a partial read in each subrange
#ifdef MOAB_HAVE_PNETCDF
    success = NCFUNCREQG(_vara_int)(_fileId, verticesOnEdgeVarId, read_starts, read_counts,
                                    &(vertices_on_local_edges[indexInArray]), &requests[idxReq++]);
#else
    success = NCFUNCAG(_vara_int)(_fileId, verticesOnEdgeVarId, read_starts, read_counts,
                                    &(vertices_on_local_edges[indexInArray]));
#endif
    if (success)
      MB_SET_ERR(MB_FAILURE, "Failed to read edge_corners data in a loop");

    // Increment the index for next subrange
    indexInArray += (endh - starth + 1) * 2;
  }

#ifdef MOAB_HAVE_PNETCDF
  // Wait outside the loop
  success = NCFUNC(wait_all)(_fileId, requests.size(), &requests[0], &statuss[0]);
  if (success)
    MB_SET_ERR(MB_FAILURE, "Failed on wait_all");
#endif

  // Populate connectivity data for local edges
  // Convert in-place from int (stored in the first half) to EntityHandle
  // Reading backward is the trick
  for (int edge_vert = nLocalEdges * 2 - 1; edge_vert >= 0; edge_vert--) {
    // Note, indices stored in vertices_on_local_edges are 0 based
    int global_vert_idx = vertices_on_local_edges[edge_vert] + 1; // Global vertex index, 1 based
    int local_vert_idx = localGidVerts.index(global_vert_idx); // Local vertex index, 0 based
    assert(local_vert_idx != -1);
    conn_arr_edges[edge_vert] = start_vertex + local_vert_idx;
  }

  return MB_SUCCESS;
}

ErrorCode NCHelperGCRM::create_padded_local_cells(const std::vector<int>& vertices_on_local_cells,
                                                  EntityHandle start_vertex, Range& faces)
{
  Interface*& mbImpl = _readNC->mbImpl;
  Tag& mGlobalIdTag = _readNC->mGlobalIdTag;

  // Create cells
  EntityHandle start_element;
  EntityHandle* conn_arr_local_cells = NULL;
  ErrorCode rval = _readNC->readMeshIface->get_element_connect(nLocalCells, EDGES_PER_CELL, MBPOLYGON, 0, start_element, conn_arr_local_cells,
                                                              // Might have to create gather mesh later
                                                              (createGatherSet ? nLocalCells + nCells : nLocalCells));MB_CHK_SET_ERR(rval, "Failed to create local cells");
  faces.insert(start_element, start_element + nLocalCells - 1);

  // Add local cells to current file set
  Range local_cells_range(start_element, start_element + nLocalCells - 1);
  rval = _readNC->mbImpl->add_entities(_fileSet, local_cells_range);MB_CHK_SET_ERR(rval, "Failed to add local cells to current file set");

  // Get ptr to GID memory for local cells
  int count = 0;
  void* data = NULL;
  rval = mbImpl->tag_iterate(mGlobalIdTag, local_cells_range.begin(), local_cells_range.end(), count, data);MB_CHK_SET_ERR(rval, "Failed to iterate global id tag on local cells");
  assert(count == nLocalCells);
  int* gid_data = (int*) data;
  std::copy(localGidCells.begin(), localGidCells.end(), gid_data);

  // Set connectivity array with proper local vertices handles
  // vertices_on_local_cells array was already corrected to have
  // the last vertices repeated for pentagons, e.g. 122345 => 123455
  for (int local_cell_idx = 0; local_cell_idx < nLocalCells; local_cell_idx++) {
    for (int i = 0; i < EDGES_PER_CELL; i++) {
      // Note, indices stored in vertices_on_local_cells are 1 based
      EntityHandle global_vert_idx = vertices_on_local_cells[local_cell_idx * EDGES_PER_CELL + i]; // Global vertex index, 1 based
      int local_vert_idx = localGidVerts.index(global_vert_idx); // Local vertex index, 0 based
      assert(local_vert_idx != -1);
      conn_arr_local_cells[local_cell_idx * EDGES_PER_CELL + i] = start_vertex + local_vert_idx;
    }
  }

  return MB_SUCCESS;
}

ErrorCode NCHelperGCRM::create_gather_set_vertices(EntityHandle gather_set, EntityHandle& gather_set_start_vertex)
{
  Interface*& mbImpl = _readNC->mbImpl;
  Tag& mGlobalIdTag = _readNC->mGlobalIdTag;
  const Tag*& mpFileIdTag = _readNC->mpFileIdTag;

  // Create gather set vertices
  std::vector<double*> arrays;
  // Don't need to specify allocation number here, because we know enough vertices were created before
  ErrorCode rval = _readNC->readMeshIface->get_node_coords(3, nVertices, 0, gather_set_start_vertex, arrays);MB_CHK_SET_ERR(rval, "Failed to create gather set vertices");

  // Add vertices to the gather set
  Range gather_set_verts_range(gather_set_start_vertex, gather_set_start_vertex + nVertices - 1);
  rval = mbImpl->add_entities(gather_set, gather_set_verts_range);MB_CHK_SET_ERR(rval, "Failed to add vertices to the gather set");

  // Read x coordinates for gather set vertices
  double* xptr = arrays[0];
  int xVertexVarId;
  int success = NCFUNC(inq_varid)(_fileId, "grid_corner_lon", &xVertexVarId);
  if (success)
    MB_SET_ERR(MB_FAILURE, "Failed to get variable id of grid_corner_lon");
  NCDF_SIZE read_start = 0;
  NCDF_SIZE read_count = static_cast<NCDF_SIZE>(nVertices);
#ifdef MOAB_HAVE_PNETCDF
  // Enter independent I/O mode, since this read is only for the gather processor
  success = NCFUNC(begin_indep_data)(_fileId);
  if (success)
    MB_SET_ERR(MB_FAILURE, "Failed to begin independent I/O mode");
  success = NCFUNCG(_vara_double)(_fileId, xVertexVarId, &read_start, &read_count, xptr);
  if (success)
    MB_SET_ERR(MB_FAILURE, "Failed to read grid_corner_lon data");
  success = NCFUNC(end_indep_data)(_fileId);
  if (success)
    MB_SET_ERR(MB_FAILURE, "Failed to end independent I/O mode");
#else
  success = NCFUNCG(_vara_double)(_fileId, xVertexVarId, &read_start, &read_count, xptr);
  if (success)
    MB_SET_ERR(MB_FAILURE, "Failed to read grid_corner_lon data");
#endif

  // Read y coordinates for gather set vertices
  double* yptr = arrays[1];
  int yVertexVarId;
  success = NCFUNC(inq_varid)(_fileId, "grid_corner_lat", &yVertexVarId);
  if (success)
    MB_SET_ERR(MB_FAILURE, "Failed to get variable id of grid_corner_lat");
#ifdef MOAB_HAVE_PNETCDF
  // Enter independent I/O mode, since this read is only for the gather processor
  success = NCFUNC(begin_indep_data)(_fileId);
  if (success)
    MB_SET_ERR(MB_FAILURE, "Failed to begin independent I/O mode");
  success = NCFUNCG(_vara_double)(_fileId, yVertexVarId, &read_start, &read_count, yptr);
  if (success)
    MB_SET_ERR(MB_FAILURE, "Failed to read grid_corner_lat data");
  success = NCFUNC(end_indep_data)(_fileId);
  if (success)
    MB_SET_ERR(MB_FAILURE, "Failed to end independent I/O mode");
#else
  success = NCFUNCG(_vara_double)(_fileId, yVertexVarId, &read_start, &read_count, yptr);
  if (success)
    MB_SET_ERR(MB_FAILURE, "Failed to read grid_corner_lat data");
#endif

  // Convert lon/lat/rad to x/y/z
  double* zptr = arrays[2];
  double rad = 8000.0 + levVals[0];
  for (int i = 0; i < nVertices; i++) {
    double cosphi = cos(yptr[i]);
    double zmult = sin(yptr[i]);
    double xmult = cosphi * cos(xptr[i]);
    double ymult = cosphi * sin(xptr[i]);
    xptr[i] = rad * xmult;
    yptr[i] = rad * ymult;
    zptr[i] = rad * zmult;
  }

  // Get ptr to GID memory for gather set vertices
  int count = 0;
  void* data = NULL;
  rval = mbImpl->tag_iterate(mGlobalIdTag, gather_set_verts_range.begin(), gather_set_verts_range.end(),
                             count, data);MB_CHK_SET_ERR(rval, "Failed to iterate global id tag on gather set vertices");
  assert(count == nVertices);
  int* gid_data = (int*) data;
  for (int j = 1; j <= nVertices; j++)
    gid_data[j - 1] = j;

  // Set the file id tag too, it should be bigger something not interfering with global id
  if (mpFileIdTag) {
    rval = mbImpl->tag_iterate(*mpFileIdTag, gather_set_verts_range.begin(), gather_set_verts_range.end(),
                               count, data);MB_CHK_SET_ERR(rval, "Failed to iterate file id tag on gather set vertices");
    assert(count == nVertices);
    int bytes_per_tag = 4;
    rval = mbImpl->tag_get_bytes(*mpFileIdTag, bytes_per_tag);MB_CHK_SET_ERR(rval, "Can't get number of bytes for file id tag");
    if (4 == bytes_per_tag) {
      gid_data = (int*) data;
      for (int j = 1; j <= nVertices; j++)
        gid_data[j - 1] = nVertices + j; // Bigger than global id tag
    }
    else if (8 == bytes_per_tag) { // Should be a handle tag on 64 bit machine?
      long* handle_tag_data = (long*)data;
      for (int j = 1; j <= nVertices; j++)
        handle_tag_data[j - 1] = nVertices + j; // Bigger than global id tag
    }
  }

  return MB_SUCCESS;
}

ErrorCode NCHelperGCRM::create_gather_set_edges(EntityHandle gather_set, EntityHandle gather_set_start_vertex)
{
  Interface*& mbImpl = _readNC->mbImpl;

  // Create gather set edges
  EntityHandle start_edge;
  EntityHandle* conn_arr_gather_set_edges = NULL;
  // Don't need to specify allocation number here, because we know enough edges were created before
  ErrorCode rval = _readNC->readMeshIface->get_element_connect(nEdges, 2, MBEDGE, 0, start_edge,
                                                               conn_arr_gather_set_edges);MB_CHK_SET_ERR(rval, "Failed to create gather set edges");

  // Add edges to the gather set
  Range gather_set_edges_range(start_edge, start_edge + nEdges - 1);
  rval = mbImpl->add_entities(gather_set, gather_set_edges_range);MB_CHK_SET_ERR(rval, "Failed to add edges to the gather set");

  // Read vertices on each edge
  int verticesOnEdgeVarId;
  int success = NCFUNC(inq_varid)(_fileId, "edge_corners", &verticesOnEdgeVarId);
  if (success)
    MB_SET_ERR(MB_FAILURE, "Failed to get variable id of edge_corners");
  // Utilize the memory storage pointed by conn_arr_gather_set_edges
  int* vertices_on_gather_set_edges = (int*) conn_arr_gather_set_edges;
  NCDF_SIZE read_starts[2] = {0, 0};
  NCDF_SIZE read_counts[2] = {static_cast<NCDF_SIZE>(nEdges), 2};
 #ifdef MOAB_HAVE_PNETCDF
   // Enter independent I/O mode, since this read is only for the gather processor
   success = NCFUNC(begin_indep_data)(_fileId);
   if (success)
     MB_SET_ERR(MB_FAILURE, "Failed to begin independent I/O mode");
   success = NCFUNCG(_vara_int)(_fileId, verticesOnEdgeVarId, read_starts, read_counts, vertices_on_gather_set_edges);
   if (success)
     MB_SET_ERR(MB_FAILURE, "Failed to read edge_corners data");
   success = NCFUNC(end_indep_data)(_fileId);
   if (success)
     MB_SET_ERR(MB_FAILURE, "Failed to end independent I/O mode");
 #else
   success = NCFUNCG(_vara_int)(_fileId, verticesOnEdgeVarId, read_starts, read_counts, vertices_on_gather_set_edges);
   if (success)
     MB_SET_ERR(MB_FAILURE, "Failed to read edge_corners data");
 #endif

   // Populate connectivity data for gather set edges
   // Convert in-place from int (stored in the first half) to EntityHandle
   // Reading backward is the trick
   for (int edge_vert = nEdges * 2 - 1; edge_vert >= 0; edge_vert--) {
     // Note, indices stored in vertices_on_gather_set_edges are 0 based
     int gather_set_vert_idx = vertices_on_gather_set_edges[edge_vert]; // Global vertex index, 0 based
     // Connectivity array is shifted by where the gather set vertices start
     conn_arr_gather_set_edges[edge_vert] = gather_set_start_vertex + gather_set_vert_idx;
   }

   return MB_SUCCESS;
}

ErrorCode NCHelperGCRM::create_padded_gather_set_cells(EntityHandle gather_set, EntityHandle gather_set_start_vertex)
{
  Interface*& mbImpl = _readNC->mbImpl;

  // Create gather set cells
  EntityHandle start_element;
  EntityHandle* conn_arr_gather_set_cells = NULL;
  // Don't need to specify allocation number here, because we know enough cells were created before
  ErrorCode rval = _readNC->readMeshIface->get_element_connect(nCells, EDGES_PER_CELL, MBPOLYGON, 0, start_element,
                                                               conn_arr_gather_set_cells);MB_CHK_SET_ERR(rval, "Failed to create gather set cells");

  // Add cells to the gather set
  Range gather_set_cells_range(start_element, start_element + nCells - 1);
  rval = mbImpl->add_entities(gather_set, gather_set_cells_range);MB_CHK_SET_ERR(rval, "Failed to add cells to the gather set");

  // Read vertices on each gather set cell (connectivity)
  int verticesOnCellVarId;
  int success = NCFUNC(inq_varid)(_fileId, "cell_corners", &verticesOnCellVarId);
  if (success)
    MB_SET_ERR(MB_FAILURE, "Failed to get variable id of cell_corners");
  // Utilize the memory storage pointed by conn_arr_gather_set_cells
  int* vertices_on_gather_set_cells = (int*) conn_arr_gather_set_cells;
  NCDF_SIZE read_starts[2] = {0, 0};
  NCDF_SIZE read_counts[2] = {static_cast<NCDF_SIZE>(nCells), static_cast<NCDF_SIZE>(EDGES_PER_CELL)};
#ifdef MOAB_HAVE_PNETCDF
  // Enter independent I/O mode, since this read is only for the gather processor
  success = NCFUNC(begin_indep_data)(_fileId);
  if (success)
    MB_SET_ERR(MB_FAILURE, "Failed to begin independent I/O mode");
  success = NCFUNCG(_vara_int)(_fileId, verticesOnCellVarId, read_starts, read_counts, vertices_on_gather_set_cells);
  if (success)
    MB_SET_ERR(MB_FAILURE, "Failed to read cell_corners data");
  success = NCFUNC(end_indep_data)(_fileId);
  if (success)
    MB_SET_ERR(MB_FAILURE, "Failed to end independent I/O mode");
#else
  success = NCFUNCG(_vara_int)(_fileId, verticesOnCellVarId, read_starts, read_counts, vertices_on_gather_set_cells);
  if (success)
    MB_SET_ERR(MB_FAILURE, "Failed to read cell_corners data");
#endif

  // Correct gather set cell vertices array in the same way as local cell vertices array
  // Pentagons as hexagons should have a connectivity like 123455 and not 122345
  for (int gather_set_cell_idx = 0; gather_set_cell_idx < nCells; gather_set_cell_idx++) {
    int* pvertex = vertices_on_gather_set_cells + gather_set_cell_idx * EDGES_PER_CELL;
    for (int k = 0; k < EDGES_PER_CELL - 2; k++) {
      if (*(pvertex + k) == *(pvertex + k + 1)) {
        // Shift the connectivity
        for (int kk = k + 1; kk < EDGES_PER_CELL - 1; kk++)
          *(pvertex + kk) = *(pvertex + kk + 1);
        // No need to try next k
        break;
      }
    }
  }

  // Populate connectivity data for gather set cells
  // Convert in-place from int (stored in the first half) to EntityHandle
  // Reading backward is the trick
  for (int cell_vert = nCells * EDGES_PER_CELL - 1; cell_vert >= 0; cell_vert--) {
    // Note, indices stored in vertices_on_gather_set_cells are 0 based
    int gather_set_vert_idx = vertices_on_gather_set_cells[cell_vert]; // Global vertex index, 0 based
    // Connectivity array is shifted by where the gather set vertices start
    conn_arr_gather_set_cells[cell_vert] = gather_set_start_vertex + gather_set_vert_idx;
  }

  return MB_SUCCESS;
}

} // namespace moab
