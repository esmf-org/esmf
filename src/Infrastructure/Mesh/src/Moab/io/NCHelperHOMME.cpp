#include "NCHelperHOMME.hpp"
#include "moab/ReadUtilIface.hpp"
#include "moab/FileOptions.hpp"
#include "moab/SpectralMeshTool.hpp"

#include <cmath>

namespace moab {

NCHelperHOMME::NCHelperHOMME(ReadNC* readNC, int fileId, const FileOptions& opts, EntityHandle fileSet)
: UcdNCHelper(readNC, fileId, opts, fileSet),
_spectralOrder(-1), connectId(-1), isConnFile(false)
{
  // Calculate spectral order
  std::map<std::string, ReadNC::AttData>::iterator attIt = readNC->globalAtts.find("np");
  if (attIt != readNC->globalAtts.end()) {
    int success = NCFUNC(get_att_int)(readNC->fileId, attIt->second.attVarId, attIt->second.attName.c_str(), &_spectralOrder);
    if (0 == success)
      _spectralOrder--; // Spectral order is one less than np
  }
  else {
    // As can_read_file() returns true and there is no global attribute "np", it should be a connectivity file
    isConnFile = true;
    _spectralOrder = 3; // Assume np is 4
  }
}

bool NCHelperHOMME::can_read_file(ReadNC* readNC, int fileId)
{
  // If global attribute "np" exists then it should be the HOMME grid
  if (readNC->globalAtts.find("np") != readNC->globalAtts.end()) {
    // Make sure it is CAM grid
    std::map<std::string, ReadNC::AttData>::iterator attIt = readNC->globalAtts.find("source");
    if (attIt == readNC->globalAtts.end())
      return false;
    unsigned int sz = attIt->second.attLen;
    std::string att_data;
    att_data.resize(sz + 1);
    att_data[sz] = '\000';
    int success = NCFUNC(get_att_text)(fileId, attIt->second.attVarId, attIt->second.attName.c_str(), &att_data[0]);
    if (success)
      return false;
    if (att_data.find("CAM") == std::string::npos)
      return false;

    return true;
  }
  else {
    // If dimension names "ncol" AND "ncorners" AND "ncells" exist, then it should be the HOMME connectivity file
    // In this case, the mesh can still be created
    std::vector<std::string>& dimNames = readNC->dimNames;
    if ((std::find(dimNames.begin(), dimNames.end(), std::string("ncol")) != dimNames.end()) &&
        (std::find(dimNames.begin(), dimNames.end(), std::string("ncorners")) != dimNames.end()) &&
        (std::find(dimNames.begin(), dimNames.end(), std::string("ncells")) != dimNames.end()))
      return true;
  }

  return false;
}

ErrorCode NCHelperHOMME::init_mesh_vals()
{
  std::vector<std::string>& dimNames = _readNC->dimNames;
  std::vector<int>& dimLens = _readNC->dimLens;
  std::map<std::string, ReadNC::VarData>& varInfo = _readNC->varInfo;

  ErrorCode rval;
  unsigned int idx;
  std::vector<std::string>::iterator vit;

  // Look for time dimension
  if (isConnFile) {
    // Connectivity file might not have time dimension
  }
  else {
    if ((vit = std::find(dimNames.begin(), dimNames.end(), "time")) != dimNames.end())
      idx = vit - dimNames.begin();
    else if ((vit = std::find(dimNames.begin(), dimNames.end(), "t")) != dimNames.end())
      idx = vit - dimNames.begin();
    else {
      MB_SET_ERR(MB_FAILURE, "Couldn't find 'time' or 't' dimension");
    }
    tDim = idx;
    nTimeSteps = dimLens[idx];
  }

  // Get number of vertices (labeled as number of columns)
  if ((vit = std::find(dimNames.begin(), dimNames.end(), "ncol")) != dimNames.end())
    idx = vit - dimNames.begin();
  else {
    MB_SET_ERR(MB_FAILURE, "Couldn't find 'ncol' dimension");
  }
  vDim = idx;
  nVertices = dimLens[idx];

  // Set number of cells
  nCells = nVertices - 2;

  // Get number of levels
  if (isConnFile) {
    // Connectivity file might not have level dimension
  }
  else {
    if ((vit = std::find(dimNames.begin(), dimNames.end(), "lev")) != dimNames.end())
      idx = vit - dimNames.begin();
    else if ((vit = std::find(dimNames.begin(), dimNames.end(), "ilev")) != dimNames.end())
      idx = vit - dimNames.begin();
    else {
      MB_SET_ERR(MB_FAILURE, "Couldn't find 'lev' or 'ilev' dimension");
    }
    levDim = idx;
    nLevels = dimLens[idx];
  }

  // Store lon values in xVertVals
  std::map<std::string, ReadNC::VarData>::iterator vmit;
  if ((vmit = varInfo.find("lon")) != varInfo.end() && (*vmit).second.varDims.size() == 1) {
    rval = read_coordinate("lon", 0, nVertices - 1, xVertVals);MB_CHK_SET_ERR(rval, "Trouble reading 'lon' variable");
  }
  else {
    MB_SET_ERR(MB_FAILURE, "Couldn't find 'lon' variable");
  }

  // Store lat values in yVertVals
  if ((vmit = varInfo.find("lat")) != varInfo.end() && (*vmit).second.varDims.size() == 1) {
    rval = read_coordinate("lat", 0, nVertices - 1, yVertVals);MB_CHK_SET_ERR(rval, "Trouble reading 'lat' variable");
  }
  else {
    MB_SET_ERR(MB_FAILURE, "Couldn't find 'lat' variable");
  }

  // Store lev values in levVals
  if (isConnFile) {
    // Connectivity file might not have level variable
  }
  else {
    if ((vmit = varInfo.find("lev")) != varInfo.end() && (*vmit).second.varDims.size() == 1) {
      rval = read_coordinate("lev", 0, nLevels - 1, levVals);MB_CHK_SET_ERR(rval, "Trouble reading 'lev' variable");

      // Decide whether down is positive
      char posval[10] = {0};
      int success = NCFUNC(get_att_text)(_fileId, (*vmit).second.varId, "positive", posval);
      if (0 == success && !strcmp(posval, "down")) {
        for (std::vector<double>::iterator dvit = levVals.begin(); dvit != levVals.end(); ++dvit)
          (*dvit) *= -1.0;
      }
    }
    else {
      MB_SET_ERR(MB_FAILURE, "Couldn't find 'lev' variable");
    }
  }

  // Store time coordinate values in tVals
  if (isConnFile) {
    // Connectivity file might not have time variable
  }
  else {
    if ((vmit = varInfo.find("time")) != varInfo.end() && (*vmit).second.varDims.size() == 1) {
      rval = read_coordinate("time", 0, nTimeSteps - 1, tVals);MB_CHK_SET_ERR(rval, "Trouble reading 'time' variable");
    }
    else if ((vmit = varInfo.find("t")) != varInfo.end() && (*vmit).second.varDims.size() == 1) {
      rval = read_coordinate("t", 0, nTimeSteps - 1, tVals);MB_CHK_SET_ERR(rval, "Trouble reading 't' variable");
    }
    else {
      // If expected time variable does not exist, set dummy values to tVals
      for (int t = 0; t < nTimeSteps; t++)
        tVals.push_back((double)t);
    }
  }

  // For each variable, determine the entity location type and number of levels
  std::map<std::string, ReadNC::VarData>::iterator mit;
  for (mit = varInfo.begin(); mit != varInfo.end(); ++mit) {
    ReadNC::VarData& vd = (*mit).second;

    // Default entLoc is ENTLOCSET
    if (std::find(vd.varDims.begin(), vd.varDims.end(), tDim) != vd.varDims.end()) {
      if (std::find(vd.varDims.begin(), vd.varDims.end(), vDim) != vd.varDims.end())
        vd.entLoc = ReadNC::ENTLOCVERT;
    }

    // Default numLev is 0
    if (std::find(vd.varDims.begin(), vd.varDims.end(), levDim) != vd.varDims.end())
      vd.numLev = nLevels;
  }

  // Hack: create dummy variables for dimensions (like ncol) with no corresponding coordinate variables
  rval = create_dummy_variables();MB_CHK_SET_ERR(rval, "Failed to create dummy variables");

  return MB_SUCCESS;
}

// When noMesh option is used on this read, the old ReadNC class instance for last read can get out
// of scope (and deleted). The old instance initialized localGidVerts properly when the mesh was
// created, but it is now lost. The new instance (will not create the mesh with noMesh option) has
// to restore it based on the existing mesh from last read
ErrorCode NCHelperHOMME::check_existing_mesh()
{
  Interface*& mbImpl = _readNC->mbImpl;
  Tag& mGlobalIdTag = _readNC->mGlobalIdTag;
  bool& noMesh = _readNC->noMesh;

  if (noMesh && localGidVerts.empty()) {
    // We need to populate localGidVerts range with the gids of vertices from current file set
    // localGidVerts is important in reading the variable data into the nodes
    // Also, for our purposes, localGidVerts is truly the GLOBAL_ID tag data, not other
    // file_id tags that could get passed around in other scenarios for parallel reading

    // Get all vertices from current file set (it is the input set in no_mesh scenario)
    Range local_verts;
    ErrorCode rval = mbImpl->get_entities_by_dimension(_fileSet, 0, local_verts);MB_CHK_SET_ERR(rval, "Trouble getting local vertices in current file set");

    if (!local_verts.empty()) {
      std::vector<int> gids(local_verts.size());

      // !IMPORTANT : this has to be the GLOBAL_ID tag
      rval = mbImpl->tag_get_data(mGlobalIdTag, local_verts, &gids[0]);MB_CHK_SET_ERR(rval, "Trouble getting local gid values of vertices");

      // Restore localGidVerts
      std::copy(gids.rbegin(), gids.rend(), range_inserter(localGidVerts));
      nLocalVertices = localGidVerts.size();
    }
  }

  return MB_SUCCESS;
}

ErrorCode NCHelperHOMME::create_mesh(Range& faces)
{
  Interface*& mbImpl = _readNC->mbImpl;
  std::string& fileName = _readNC->fileName;
  Tag& mGlobalIdTag = _readNC->mGlobalIdTag;
  const Tag*& mpFileIdTag = _readNC->mpFileIdTag;
  DebugOutput& dbgOut = _readNC->dbgOut;
  bool& spectralMesh = _readNC->spectralMesh;
  int& gatherSetRank = _readNC->gatherSetRank;
  int& trivialPartitionShift = _readNC->trivialPartitionShift;

  int rank = 0;
  int procs = 1;
#ifdef MOAB_HAVE_MPI
  bool& isParallel = _readNC->isParallel;
  if (isParallel) {
    ParallelComm*& myPcomm = _readNC->myPcomm;
    rank = myPcomm->proc_config().proc_rank();
    procs = myPcomm->proc_config().proc_size();
  }
#endif

  ErrorCode rval;
  int success = 0;

  // Need to get/read connectivity data before creating elements
  std::string conn_fname;

  if (isConnFile) {
    // Connectivity file has already been read
    connectId = _readNC->fileId;
  }
  else {
    // Try to open the connectivity file through CONN option, if used
    rval = _opts.get_str_option("CONN", conn_fname);
    if (MB_SUCCESS != rval) {
      // Default convention for reading HOMME is a file HommeMapping.nc in same dir as data file
      conn_fname = std::string(fileName);
      size_t idx = conn_fname.find_last_of("/");
      if (idx != std::string::npos)
        conn_fname = conn_fname.substr(0, idx).append("/HommeMapping.nc");
      else
        conn_fname = "HommeMapping.nc";
    }
#ifdef MOAB_HAVE_PNETCDF
#ifdef MOAB_HAVE_MPI
    if (isParallel) {
      ParallelComm*& myPcomm = _readNC->myPcomm;
      success = NCFUNC(open)(myPcomm->proc_config().proc_comm(), conn_fname.c_str(), 0, MPI_INFO_NULL, &connectId);
    }
    else
      success = NCFUNC(open)(MPI_COMM_SELF, conn_fname.c_str(), 0, MPI_INFO_NULL, &connectId);
#endif
#else
    success = NCFUNC(open)(conn_fname.c_str(), 0, &connectId);
#endif
    if (success)
      MB_SET_ERR(MB_FAILURE, "Failed on open");
  }

  std::vector<std::string> conn_names;
  std::vector<int> conn_vals;
  rval = _readNC->get_dimensions(connectId, conn_names, conn_vals);MB_CHK_SET_ERR(rval, "Failed to get dimensions for connectivity");

  // Read connectivity into temporary variable
  int num_fine_quads = 0;
  int num_coarse_quads = 0;
  int start_idx = 0;
  std::vector<std::string>::iterator vit;
  int idx = 0;
  if ((vit = std::find(conn_names.begin(), conn_names.end(), "ncells")) != conn_names.end())
    idx = vit - conn_names.begin();
  else if ((vit = std::find(conn_names.begin(), conn_names.end(), "ncenters")) != conn_names.end())
    idx = vit - conn_names.begin();
  else {
    MB_SET_ERR(MB_FAILURE, "Failed to get number of quads");
  }
  int num_quads = conn_vals[idx];
  if (!isConnFile && num_quads != nCells) {
    dbgOut.tprintf(1, "Warning: number of quads from %s and cells from %s are inconsistent; num_quads = %d, nCells = %d.\n",
        conn_fname.c_str(), fileName.c_str(), num_quads, nCells);
  }

  // Get the connectivity into tmp_conn2 and permute into tmp_conn
  int cornerVarId;
  success = NCFUNC(inq_varid)(connectId, "element_corners", &cornerVarId);
  if (success)
    MB_SET_ERR(MB_FAILURE, "Failed to get variable id of 'element_corners'");
  NCDF_SIZE tmp_starts[2] = {0, 0};
  NCDF_SIZE tmp_counts[2] = {4, static_cast<NCDF_SIZE>(num_quads)};
  std::vector<int> tmp_conn(4 * num_quads), tmp_conn2(4 * num_quads);
  success = NCFUNCAG(_vara_int)(connectId, cornerVarId, tmp_starts, tmp_counts, &tmp_conn2[0]);
  if (success)
    MB_SET_ERR(MB_FAILURE, "Failed to get temporary connectivity");
  if (isConnFile) {
    // This data/connectivity file will be closed later in ReadNC::load_file()
  }
  else {
    success = NCFUNC(close)(connectId);
    if (success)
      MB_SET_ERR(MB_FAILURE, "Failed on close");
  }
  // Permute the connectivity
  for (int i = 0; i < num_quads; i++) {
    tmp_conn[4 * i] = tmp_conn2[i];
    tmp_conn[4 * i + 1] = tmp_conn2[i + 1 * num_quads];
    tmp_conn[4 * i + 2] = tmp_conn2[i + 2 * num_quads];
    tmp_conn[4 * i + 3] = tmp_conn2[i + 3 * num_quads];
  }

  // Need to know whether we'll be creating gather mesh later, to make sure
  // we allocate enough space in one shot
  bool create_gathers = false;
  if (rank == gatherSetRank)
    create_gathers = true;

  // Shift rank to obtain a rotated trivial partition
  int shifted_rank = rank;
  if (procs >= 2 && trivialPartitionShift > 0)
    shifted_rank = (rank + trivialPartitionShift) % procs;

  // Compute the number of local quads, accounting for coarse or fine representation
  // spectral_unit is the # fine quads per coarse quad, or spectralOrder^2
  int spectral_unit = (spectralMesh ? _spectralOrder * _spectralOrder : 1);
  // num_coarse_quads is the number of quads instantiated in MOAB; if !spectralMesh, num_coarse_quads = num_fine_quads
  num_coarse_quads = int(std::floor(1.0 * num_quads / (spectral_unit * procs)));
  // start_idx is the starting index in the HommeMapping connectivity list for this proc, before converting to coarse quad representation
  start_idx = 4 * shifted_rank * num_coarse_quads * spectral_unit;
  // iextra = # coarse quads extra after equal split over procs
  int iextra = num_quads % (procs * spectral_unit);
  if (shifted_rank < iextra)
    num_coarse_quads++;
  start_idx += 4 * spectral_unit * std::min(shifted_rank, iextra);
  // num_fine_quads is the number of quads in the connectivity list in HommeMapping file assigned to this proc
  num_fine_quads = spectral_unit * num_coarse_quads;

  // Now create num_coarse_quads
  EntityHandle* conn_arr;
  EntityHandle start_vertex;
  Range tmp_range;

  // Read connectivity into that space
  EntityHandle* sv_ptr = NULL;
  EntityHandle start_quad;
  SpectralMeshTool smt(mbImpl, _spectralOrder);
  if (!spectralMesh) {
    rval = _readNC->readMeshIface->get_element_connect(num_coarse_quads, 4,
                                                      MBQUAD, 0, start_quad, conn_arr,
                                                      // Might have to create gather mesh later
                                                      (create_gathers ? num_coarse_quads + num_quads : num_coarse_quads));MB_CHK_SET_ERR(rval, "Failed to create local quads");
    tmp_range.insert(start_quad, start_quad + num_coarse_quads - 1);
    int* tmp_conn_end = (&tmp_conn[start_idx + 4 * num_fine_quads-1])+1;
    std::copy(&tmp_conn[start_idx], tmp_conn_end, conn_arr);
    std::copy(conn_arr, conn_arr + 4 * num_fine_quads, range_inserter(localGidVerts));
  }
  else {
    rval = smt.create_spectral_elems(&tmp_conn[0], num_fine_quads, 2, tmp_range, start_idx, &localGidVerts);MB_CHK_SET_ERR(rval, "Failed to create spectral elements");
    int count, v_per_e;
    rval = mbImpl->connect_iterate(tmp_range.begin(), tmp_range.end(), conn_arr, v_per_e, count);MB_CHK_SET_ERR(rval, "Failed to get connectivity of spectral elements");
    rval = mbImpl->tag_iterate(smt.spectral_vertices_tag(true), tmp_range.begin(), tmp_range.end(),
                               count, (void*&)sv_ptr);MB_CHK_SET_ERR(rval, "Failed to get fine connectivity of spectral elements");
  }

  // Create vertices
  nLocalVertices = localGidVerts.size();
  std::vector<double*> arrays;
  rval = _readNC->readMeshIface->get_node_coords(3, nLocalVertices, 0, start_vertex, arrays,
                                                // Might have to create gather mesh later
                                                (create_gathers ? nLocalVertices + nVertices : nLocalVertices));MB_CHK_SET_ERR(rval, "Failed to create local vertices");

  // Set vertex coordinates
  Range::iterator rit;
  double* xptr = arrays[0];
  double* yptr = arrays[1];
  double* zptr = arrays[2];
  int i;
  for (i = 0, rit = localGidVerts.begin(); i < nLocalVertices; i++, ++rit) {
    assert(*rit < xVertVals.size() + 1);
    xptr[i] = xVertVals[(*rit) - 1]; // lon
    yptr[i] = yVertVals[(*rit) - 1]; // lat
  }

  // Convert lon/lat/rad to x/y/z
  const double pideg = acos(-1.0) / 180.0;
  double rad = (isConnFile) ? 8000.0 : 8000.0 + levVals[0];
  for (i = 0; i < nLocalVertices; i++) {
    double cosphi = cos(pideg * yptr[i]);
    double zmult = sin(pideg * yptr[i]);
    double xmult = cosphi * cos(xptr[i] * pideg);
    double ymult = cosphi * sin(xptr[i] * pideg);
    xptr[i] = rad * xmult;
    yptr[i] = rad * ymult;
    zptr[i] = rad * zmult;
  }

  // Get ptr to gid memory for vertices
  Range vert_range(start_vertex, start_vertex + nLocalVertices - 1);
  void* data;
  int count;
  rval = mbImpl->tag_iterate(mGlobalIdTag, vert_range.begin(), vert_range.end(),
                             count, data);MB_CHK_SET_ERR(rval, "Failed to iterate global id tag on local vertices");
  assert(count == nLocalVertices);
  int* gid_data = (int*) data;
  std::copy(localGidVerts.begin(), localGidVerts.end(), gid_data);

  // Duplicate global id data, which will be used to resolve sharing
  if (mpFileIdTag) {
    rval = mbImpl->tag_iterate(*mpFileIdTag, vert_range.begin(), vert_range.end(),
                               count, data);MB_CHK_SET_ERR(rval, "Failed to iterate file id tag on local vertices");
    assert(count == nLocalVertices);
    int bytes_per_tag = 4;
    rval = mbImpl->tag_get_bytes(*mpFileIdTag, bytes_per_tag);MB_CHK_SET_ERR(rval, "Can't get number of bytes for file id tag");
    if (4 == bytes_per_tag) {
      gid_data = (int*) data;
      std::copy(localGidVerts.begin(), localGidVerts.end(), gid_data);
    }
    else if (8 == bytes_per_tag) { // Should be a handle tag on 64 bit machine?
      long* handle_tag_data = (long*)data;
      std::copy(localGidVerts.begin(), localGidVerts.end(), handle_tag_data);
    }
  }

  // Create map from file ids to vertex handles, used later to set connectivity
  std::map<EntityHandle, EntityHandle> vert_handles;
  for (rit = localGidVerts.begin(), i = 0; rit != localGidVerts.end(); ++rit, i++)
    vert_handles[*rit] = start_vertex + i;

  // Compute proper handles in connectivity using offset
  for (int q = 0; q < 4 * num_coarse_quads; q++) {
    conn_arr[q] = vert_handles[conn_arr[q]];
    assert(conn_arr[q]);
  }
  if (spectralMesh) {
    int verts_per_quad = (_spectralOrder + 1) * (_spectralOrder + 1);
    for (int q = 0; q < verts_per_quad * num_coarse_quads; q++) {
      sv_ptr[q] = vert_handles[sv_ptr[q]];
      assert(sv_ptr[q]);
    }
  }

  // Add new vertices and quads to current file set
  faces.merge(tmp_range);
  tmp_range.insert(start_vertex, start_vertex + nLocalVertices - 1);
  rval = mbImpl->add_entities(_fileSet, tmp_range);MB_CHK_SET_ERR(rval, "Failed to add new vertices and quads to current file set");

  // Mark the set with the spectral order
  Tag sporder;
  rval = mbImpl->tag_get_handle("SPECTRAL_ORDER", 1, MB_TYPE_INTEGER, sporder,
                                MB_TAG_SPARSE | MB_TAG_CREAT);MB_CHK_SET_ERR(rval, "Trouble creating SPECTRAL_ORDER tag");
  rval = mbImpl->tag_set_data(sporder, &_fileSet, 1, &_spectralOrder);MB_CHK_SET_ERR(rval, "Trouble setting data to SPECTRAL_ORDER tag");

  if (create_gathers) {
    EntityHandle gather_set;
    rval = _readNC->readMeshIface->create_gather_set(gather_set);MB_CHK_SET_ERR(rval, "Failed to create gather set");

    // Create vertices
    arrays.clear();
    // Don't need to specify allocation number here, because we know enough verts were created before
    rval = _readNC->readMeshIface->get_node_coords(3, nVertices, 0, start_vertex, arrays);MB_CHK_SET_ERR(rval, "Failed to create gather set vertices");

    xptr = arrays[0];
    yptr = arrays[1];
    zptr = arrays[2];
    for (i = 0; i < nVertices; i++) {
      double cosphi = cos(pideg * yVertVals[i]);
      double zmult = sin(pideg * yVertVals[i]);
      double xmult = cosphi * cos(xVertVals[i] * pideg);
      double ymult = cosphi * sin(xVertVals[i] * pideg);
      xptr[i] = rad * xmult;
      yptr[i] = rad * ymult;
      zptr[i] = rad * zmult;
    }

    // Get ptr to gid memory for vertices
    Range gather_set_verts_range(start_vertex, start_vertex + nVertices - 1);
    rval = mbImpl->tag_iterate(mGlobalIdTag, gather_set_verts_range.begin(), gather_set_verts_range.end(),
                               count, data);MB_CHK_SET_ERR(rval, "Failed to iterate global id tag on gather set vertices");
    assert(count == nVertices);
    gid_data = (int*) data;
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
        gid_data = (int*)data;
        for (int j = 1; j <= nVertices; j++)
          gid_data[j - 1] = nVertices + j; // Bigger than global id tag
      }
      else if (8 == bytes_per_tag) { // Should be a handle tag on 64 bit machine?
        long* handle_tag_data = (long*)data;
        for (int j = 1; j <= nVertices; j++)
          handle_tag_data[j - 1] = nVertices + j; // Bigger than global id tag
      }
    }

    rval = mbImpl->add_entities(gather_set, gather_set_verts_range);MB_CHK_SET_ERR(rval, "Failed to add vertices to the gather set");

    // Create quads
    Range gather_set_quads_range;
    // Don't need to specify allocation number here, because we know enough quads were created before
    rval = _readNC->readMeshIface->get_element_connect(num_quads, 4, MBQUAD, 0,
                                                       start_quad, conn_arr);MB_CHK_SET_ERR(rval, "Failed to create gather set quads");
    gather_set_quads_range.insert(start_quad, start_quad + num_quads - 1);
    int* tmp_conn_end = (&tmp_conn[4 * num_quads-1]) + 1;
    std::copy(&tmp_conn[0], tmp_conn_end, conn_arr);
    for (i = 0; i != 4 * num_quads; i++)
      conn_arr[i] += start_vertex - 1; // Connectivity array is shifted by where the gather verts start
    rval = mbImpl->add_entities(gather_set, gather_set_quads_range);MB_CHK_SET_ERR(rval, "Failed to add quads to the gather set");
  }

  return MB_SUCCESS;
}

ErrorCode NCHelperHOMME::read_ucd_variables_to_nonset_allocate(std::vector<ReadNC::VarData>& vdatas, std::vector<int>& tstep_nums)
{
  Interface*& mbImpl = _readNC->mbImpl;
  std::vector<int>& dimLens = _readNC->dimLens;
  DebugOutput& dbgOut = _readNC->dbgOut;

  Range* range = NULL;

  // Get vertices
  Range verts;
  ErrorCode rval = mbImpl->get_entities_by_dimension(_fileSet, 0, verts);MB_CHK_SET_ERR(rval, "Trouble getting vertices in current file set");
  assert("Should only have a single vertex subrange, since they were read in one shot" && verts.psize() == 1);

  for (unsigned int i = 0; i < vdatas.size(); i++) {
    // Support non-set variables with 3 dimensions like (time, lev, ncol)
    assert(3 == vdatas[i].varDims.size());

    // For a non-set variable, time should be the first dimension
    assert(tDim == vdatas[i].varDims[0]);

    // Set up readStarts and readCounts
    vdatas[i].readStarts.resize(3);
    vdatas[i].readCounts.resize(3);

    // First: time
    vdatas[i].readStarts[0] = 0; // This value is timestep dependent, will be set later
    vdatas[i].readCounts[0] = 1;

    // Next: lev
    vdatas[i].readStarts[1] = 0;
    vdatas[i].readCounts[1] = vdatas[i].numLev;

    // Finally: ncol
    switch (vdatas[i].entLoc) {
      case ReadNC::ENTLOCVERT:
        // Vertices
        // Start from the first localGidVerts
        // Actually, this will be reset later on in a loop
        vdatas[i].readStarts[2] = localGidVerts[0] - 1;
        vdatas[i].readCounts[2] = nLocalVertices;
        range = &verts;
        break;
      default:
        MB_SET_ERR(MB_FAILURE, "Unexpected entity location type for variable " << vdatas[i].varName);
    }

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
ErrorCode NCHelperHOMME::read_ucd_variables_to_nonset_async(std::vector<ReadNC::VarData>& vdatas, std::vector<int>& tstep_nums)
{
  DebugOutput& dbgOut = _readNC->dbgOut;

  ErrorCode rval = read_ucd_variables_to_nonset_allocate(vdatas, tstep_nums);MB_CHK_SET_ERR(rval, "Trouble allocating space to read non-set variables");

  // Finally, read into that space
  int success;

  for (unsigned int i = 0; i < vdatas.size(); i++) {
    std::size_t sz = vdatas[i].sz;

    // A typical supported variable: float T(time, lev, ncol)
    // For tag values, need transpose (lev, ncol) to (ncol, lev)
    size_t ni = vdatas[i].readCounts[2]; // ncol
    size_t nj = 1; // Here we should just set nj to 1
    size_t nk = vdatas[i].readCounts[1]; // lev

    for (unsigned int t = 0; t < tstep_nums.size(); t++) {
      // We will synchronize all these reads with the other processors,
      // so the wait will be inside this double loop; is it too much?
      size_t nb_reads = localGidVerts.psize();
      std::vector<int> requests(nb_reads), statuss(nb_reads);
      size_t idxReq = 0;

      // Tag data for this timestep
      void* data = vdatas[i].varDatas[t];

      // Set readStart for each timestep along time dimension
      vdatas[i].readStarts[0] = tstep_nums[t];

      switch (vdatas[i].varDataType) {
        case NC_FLOAT:
        case NC_DOUBLE: {
          // Read float as double
          std::vector<double> tmpdoubledata(sz);

          // In the case of ucd mesh, and on multiple proc,
          // we need to read as many times as subranges we have in the
          // localGidVerts range;
          // basically, we have to give a different point
          // for data to start, for every subrange :(
          size_t indexInDoubleArray = 0;
          size_t ic = 0;
          for (Range::pair_iterator pair_iter = localGidVerts.pair_begin();
              pair_iter != localGidVerts.pair_end();
              ++pair_iter, ic++) {
            EntityHandle starth = pair_iter->first;
            EntityHandle endh = pair_iter->second; // Inclusive
            vdatas[i].readStarts[2] = (NCDF_SIZE) (starth - 1);
            vdatas[i].readCounts[2] = (NCDF_SIZE) (endh - starth + 1);

            // Do a partial read, in each subrange
            // Wait outside this loop
            success = NCFUNCREQG(_vara_double)(_fileId, vdatas[i].varId,
                            &(vdatas[i].readStarts[0]), &(vdatas[i].readCounts[0]),
                            &(tmpdoubledata[indexInDoubleArray]), &requests[idxReq++]);
            if (success)
              MB_SET_ERR(MB_FAILURE, "Failed to read double data in a loop for variable " << vdatas[i].varName);
            // We need to increment the index in double array for the
            // next subrange
            indexInDoubleArray += (endh - starth + 1) * 1 * vdatas[i].numLev;
          }
          assert(ic == localGidVerts.psize());

          success = ncmpi_wait_all(_fileId, requests.size(), &requests[0], &statuss[0]);
          if (success)
            MB_SET_ERR(MB_FAILURE, "Failed on wait_all");

          if (vdatas[i].numLev > 1)
            // Transpose (lev, ncol) to (ncol, lev)
            kji_to_jik_stride(ni, nj, nk, data, &tmpdoubledata[0], localGidVerts);
          else {
            for (std::size_t idx = 0; idx != tmpdoubledata.size(); idx++)
              ((double*) data)[idx] = tmpdoubledata[idx];
          }

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
ErrorCode NCHelperHOMME::read_ucd_variables_to_nonset(std::vector<ReadNC::VarData>& vdatas, std::vector<int>& tstep_nums)
{
  DebugOutput& dbgOut = _readNC->dbgOut;

  ErrorCode rval = read_ucd_variables_to_nonset_allocate(vdatas, tstep_nums);MB_CHK_SET_ERR(rval, "Trouble allocating space to read non-set variables");

  // Finally, read into that space
  int success;
  for (unsigned int i = 0; i < vdatas.size(); i++) {
    std::size_t sz = vdatas[i].sz;

    // A typical supported variable: float T(time, lev, ncol)
    // For tag values, need transpose (lev, ncol) to (ncol, lev)
    size_t ni = vdatas[i].readCounts[2]; // ncol
    size_t nj = 1; // Here we should just set nj to 1
    size_t nk = vdatas[i].readCounts[1]; // lev

    for (unsigned int t = 0; t < tstep_nums.size(); t++) {
      // Tag data for this timestep
      void* data = vdatas[i].varDatas[t];

      // Set readStart for each timestep along time dimension
      vdatas[i].readStarts[0] = tstep_nums[t];

      switch (vdatas[i].varDataType) {
        case NC_FLOAT:
        case NC_DOUBLE: {
          // Read float as double
          std::vector<double> tmpdoubledata(sz);

          // In the case of ucd mesh, and on multiple proc,
          // we need to read as many times as subranges we have in the
          // localGidVerts range;
          // basically, we have to give a different point
          // for data to start, for every subrange :(
          size_t indexInDoubleArray = 0;
          size_t ic = 0;
          for (Range::pair_iterator pair_iter = localGidVerts.pair_begin();
              pair_iter != localGidVerts.pair_end();
              ++pair_iter, ic++) {
            EntityHandle starth = pair_iter->first;
            EntityHandle endh = pair_iter->second; // Inclusive
            vdatas[i].readStarts[2] = (NCDF_SIZE) (starth - 1);
            vdatas[i].readCounts[2] = (NCDF_SIZE) (endh - starth + 1);

            success = NCFUNCAG(_vara_double)(_fileId, vdatas[i].varId,
                            &(vdatas[i].readStarts[0]), &(vdatas[i].readCounts[0]),
                            &(tmpdoubledata[indexInDoubleArray]));
            if (success)
              MB_SET_ERR(MB_FAILURE, "Failed to read double data in a loop for variable " << vdatas[i].varName);
            // We need to increment the index in double array for the
            // next subrange
            indexInDoubleArray += (endh - starth + 1) * 1 * vdatas[i].numLev;
          }
          assert(ic == localGidVerts.psize());

          if (vdatas[i].numLev > 1)
            // Transpose (lev, ncol) to (ncol, lev)
            kji_to_jik_stride(ni, nj, nk, data, &tmpdoubledata[0], localGidVerts);
          else {
            for (std::size_t idx = 0; idx != tmpdoubledata.size(); idx++)
              ((double*) data)[idx] = tmpdoubledata[idx];
          }

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

} // namespace moab
