#include "NCHelperFV.hpp"
#include "moab/FileOptions.hpp"

#include <cmath>
#include <sstream>

namespace moab {

bool NCHelperFV::can_read_file(ReadNC* readNC, int fileId)
{
  std::vector<std::string>& dimNames = readNC->dimNames;

  // If dimension names "lon" AND "lat" AND "slon" AND "slat" exist then it should be the FV grid
  if ((std::find(dimNames.begin(), dimNames.end(), std::string("lon")) != dimNames.end()) && (std::find(dimNames.begin(),
      dimNames.end(), std::string("lat")) != dimNames.end()) && (std::find(dimNames.begin(), dimNames.end(), std::string("slon"))
      != dimNames.end()) && (std::find(dimNames.begin(), dimNames.end(), std::string("slat")) != dimNames.end())) {
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

  return false;
}

ErrorCode NCHelperFV::init_mesh_vals()
{
  Interface*& mbImpl = _readNC->mbImpl;
  std::vector<std::string>& dimNames = _readNC->dimNames;
  std::vector<int>& dimLens = _readNC->dimLens;
  std::map<std::string, ReadNC::VarData>& varInfo = _readNC->varInfo;
  DebugOutput& dbgOut = _readNC->dbgOut;
  bool& isParallel = _readNC->isParallel;
  int& partMethod = _readNC->partMethod;
  ScdParData& parData = _readNC->parData;

  // Look for names of i/j dimensions
  // First i
  std::vector<std::string>::iterator vit;
  unsigned int idx;
  if ((vit = std::find(dimNames.begin(), dimNames.end(), "slon")) != dimNames.end())
    idx = vit - dimNames.begin();
  else {
    MB_SET_ERR(MB_FAILURE, "Couldn't find 'slon' variable");
  }
  iDim = idx;
  gDims[0] = 0;
  gDims[3] = dimLens[idx] - 1;

  // Then j
  if ((vit = std::find(dimNames.begin(), dimNames.end(), "slat")) != dimNames.end())
    idx = vit - dimNames.begin();
  else {
    MB_SET_ERR(MB_FAILURE, "Couldn't find 'slat' variable");
  }
  jDim = idx;
  gDims[1] = 0;
  gDims[4] = dimLens[idx] - 1 + 2; // Add 2 for the pole points

  // Look for names of center i/j dimensions
  // First i
  if ((vit = std::find(dimNames.begin(), dimNames.end(), "lon")) != dimNames.end())
    idx = vit - dimNames.begin();
  else {
    MB_SET_ERR(MB_FAILURE, "Couldn't find 'lon' variable");
  }
  iCDim = idx;
  gCDims[0] = 0;
  gCDims[3] = dimLens[idx] - 1;

  // Check i periodicity and set globallyPeriodic[0]
  std::vector<double> til_vals(2);
  ErrorCode rval = read_coordinate("lon", gCDims[3] - 1, gCDims[3], til_vals);MB_CHK_SET_ERR(rval, "Trouble reading 'lon' variable");
  if (std::fabs(2 * til_vals[1] - til_vals[0] - 360) < 0.001)
    globallyPeriodic[0] = 1;
  if (globallyPeriodic[0])
    assert("Number of vertices and edges should be same" && gDims[3] == gCDims[3]);
  else
    assert("Number of vertices should equal to number of edges plus one" && gDims[3] == gCDims[3] + 1);

  // Then j
  if ((vit = std::find(dimNames.begin(), dimNames.end(), "lat")) != dimNames.end())
    idx = vit - dimNames.begin();
  else {
    MB_SET_ERR(MB_FAILURE, "Couldn't find 'lat' dimension");
  }
  jCDim = idx;
  gCDims[1] = 0;
  gCDims[4] = dimLens[idx] - 1;

  // For FV models, will always be non-periodic in j
  assert(gDims[4] == gCDims[4] + 1);

  // Try a truly 2D mesh
  gDims[2] = -1;
  gDims[5] = -1;

  // Look for time dimension
  if ((vit = std::find(dimNames.begin(), dimNames.end(), "time")) != dimNames.end())
    idx = vit - dimNames.begin();
  else if ((vit = std::find(dimNames.begin(), dimNames.end(), "t")) != dimNames.end())
    idx = vit - dimNames.begin();
  else {
    MB_SET_ERR(MB_FAILURE, "Couldn't find 'time' or 't' dimension");
  }
  tDim = idx;
  nTimeSteps = dimLens[idx];

  // Get number of levels
  if ((vit = std::find(dimNames.begin(), dimNames.end(), "lev")) != dimNames.end())
    idx = vit - dimNames.begin();
  else if ((vit = std::find(dimNames.begin(), dimNames.end(), "ilev")) != dimNames.end())
    idx = vit - dimNames.begin();
  else {
    MB_SET_ERR(MB_FAILURE, "Couldn't find 'lev' or 'ilev' dimension");
  }
  levDim = idx;
  nLevels = dimLens[idx];

  // Parse options to get subset
  int rank = 0, procs = 1;
#ifdef MOAB_HAVE_MPI
  if (isParallel) {
    ParallelComm*& myPcomm = _readNC->myPcomm;
    rank = myPcomm->proc_config().proc_rank();
    procs = myPcomm->proc_config().proc_size();
  }
#endif
  if (procs > 1) {
    for (int i = 0; i < 6; i++)
      parData.gDims[i] = gDims[i];
    for (int i = 0; i < 3; i++)
      parData.gPeriodic[i] = globallyPeriodic[i];
    parData.partMethod = partMethod;
    int pdims[3];

    rval = ScdInterface::compute_partition(procs, rank, parData, lDims, locallyPeriodic, pdims);MB_CHK_ERR(rval);
    for (int i = 0; i < 3; i++)
      parData.pDims[i] = pdims[i];

    dbgOut.tprintf(1, "Partition: %dx%d (out of %dx%d)\n",
        lDims[3] - lDims[0] + 1, lDims[4] - lDims[1] + 1,
        gDims[3] - gDims[0] + 1, gDims[4] - gDims[1] + 1);
    if (0 == rank)
      dbgOut.tprintf(1, "Contiguous chunks of size %d bytes.\n", 8 * (lDims[3] - lDims[0] + 1) * (lDims[4] - lDims[1] + 1));
  }
  else {
    for (int i = 0; i < 6; i++)
      lDims[i] = gDims[i];
    locallyPeriodic[0] = globallyPeriodic[0];
  }

  _opts.get_int_option("IMIN", lDims[0]);
  _opts.get_int_option("IMAX", lDims[3]);
  _opts.get_int_option("JMIN", lDims[1]);
  _opts.get_int_option("JMAX", lDims[4]);

  // Now get actual coordinate values for vertices and cell centers
  lCDims[0] = lDims[0];
  if (locallyPeriodic[0])
    // If locally periodic, doesn't matter what global periodicity is, # vertex coords = # elem coords
    lCDims[3] = lDims[3];
  else
    lCDims[3] = lDims[3] - 1;

  // For FV models, will always be non-periodic in j
  lCDims[1] = lDims[1];
  lCDims[4] = lDims[4] - 1;

  // Resize vectors to store values later
  if (-1 != lDims[0])
    ilVals.resize(lDims[3] - lDims[0] + 1);
  if (-1 != lCDims[0])
    ilCVals.resize(lCDims[3] - lCDims[0] + 1);
  if (-1 != lDims[1])
    jlVals.resize(lDims[4] - lDims[1] + 1);
  if (-1 != lCDims[1])
    jlCVals.resize(lCDims[4] - lCDims[1] + 1);
  if (nTimeSteps > 0)
    tVals.resize(nTimeSteps);

  // Now read coord values
  std::map<std::string, ReadNC::VarData>::iterator vmit;
  if (-1 != lCDims[0]) {
    if ((vmit = varInfo.find("lon")) != varInfo.end() && (*vmit).second.varDims.size() == 1) {
      rval = read_coordinate("lon", lCDims[0], lCDims[3], ilCVals);MB_CHK_SET_ERR(rval, "Trouble reading 'lon' variable");
    }
    else {
      MB_SET_ERR(MB_FAILURE, "Couldn't find 'lon' variable");
    }
  }

  if (-1 != lCDims[1]) {
    if ((vmit = varInfo.find("lat")) != varInfo.end() && (*vmit).second.varDims.size() == 1) {
      rval = read_coordinate("lat", lCDims[1], lCDims[4], jlCVals);MB_CHK_SET_ERR(rval, "Trouble reading 'lat' variable");
    }
    else {
      MB_SET_ERR(MB_FAILURE, "Couldn't find 'lat' variable");
    }
  }

  if (-1 != lDims[0]) {
    if ((vmit = varInfo.find("slon")) != varInfo.end() && (*vmit).second.varDims.size() == 1) {
      // Last column
      if (!locallyPeriodic[0] && globallyPeriodic[0] && lDims[3] > gDims[3]) {
        assert(lDims[3] == gDims[3] + 1);
        std::vector<double> dummyVar(lDims[3] - lDims[0]);
        rval = read_coordinate("slon", lDims[0], lDims[3] - 1, dummyVar);
        double dif = dummyVar[1] - dummyVar[0];
        std::size_t i;
        for (i = 0; i != dummyVar.size(); i++)
          ilVals[i] = dummyVar[i];
        ilVals[i] = ilVals[i - 1] + dif;
      }
      else {
        rval = read_coordinate("slon", lDims[0], lDims[3], ilVals);MB_CHK_SET_ERR(rval, "Trouble reading 'slon' variable");
      }
    }
    else {
      MB_SET_ERR(MB_FAILURE, "Couldn't find 'slon' variable");
    }
  }

  if (-1 != lDims[1]) {
    if ((vmit = varInfo.find("slat")) != varInfo.end() && (*vmit).second.varDims.size() == 1) {
      if (!isParallel || ((gDims[4] - gDims[1]) == (lDims[4] - lDims[1]))) {
        std::vector<double> dummyVar(lDims[4] - lDims[1] - 1);
        rval = read_coordinate("slat", lDims[1], lDims[4] - 2, dummyVar);MB_CHK_SET_ERR(rval, "Trouble reading 'slat' variable");
        // Copy the correct piece
        jlVals[0] = -90.0;
        std::size_t i = 0;
        for (i = 1; i != dummyVar.size() + 1; i++)
          jlVals[i] = dummyVar[i - 1];
        jlVals[i] = 90.0; // Using value of i after loop exits.
      }
      else {
        // If this is the first row
        // Need to read one less then available and read it into a dummy var
        if (lDims[1] == gDims[1]) {
          std::vector<double> dummyVar(lDims[4] - lDims[1]);
          rval = read_coordinate("slat", lDims[1], lDims[4] - 1, dummyVar);MB_CHK_SET_ERR(rval, "Trouble reading 'slat' variable");
          // Copy the correct piece
          jlVals[0] = -90.0;
          for (int i = 1; i < lDims[4] + 1; i++)
            jlVals[i] = dummyVar[i - 1];
        }
        // Or if it's the last row
        else if (lDims[4] == gDims[4]) {
          std::vector<double> dummyVar(lDims[4] - lDims[1]);
          rval = read_coordinate("slat", lDims[1] - 1, lDims[4] - 2, dummyVar);MB_CHK_SET_ERR(rval, "Trouble reading 'slat' variable");
          // Copy the correct piece
          std::size_t i = 0;
          for (i = 0; i != dummyVar.size(); i++)
            jlVals[i] = dummyVar[i];
          jlVals[i] = 90.0; // Using value of i after loop exits.
        }
        // It's in the middle
        else {
          rval = read_coordinate("slat", lDims[1] - 1, lDims[4] - 1, jlVals);MB_CHK_SET_ERR(rval, "Trouble reading 'slat' variable");
        }
      }
    }
    else {
      MB_SET_ERR(MB_FAILURE, "Couldn't find 'slat' variable");
    }
  }

  // Store time coordinate values in tVals
  if (nTimeSteps > 0) {
    if ((vmit = varInfo.find("time")) != varInfo.end() && (*vmit).second.varDims.size() == 1) {
      rval = read_coordinate("time", 0, nTimeSteps - 1, tVals);MB_CHK_SET_ERR(rval, "Trouble reading 'time' variable");
    }
    else if ((vmit = varInfo.find("t")) != varInfo.end() && (*vmit).second.varDims.size() == 1) {
      rval = read_coordinate("t", 0, nTimeSteps - 1, tVals);MB_CHK_SET_ERR(rval, "Trouble reading 't' variable");
    }
    else {
      // If expected time variable is not available, set dummy time coordinate values to tVals
      for (int t = 0; t < nTimeSteps; t++)
        tVals.push_back((double)t);
    }
  }

  dbgOut.tprintf(1, "I=%d-%d, J=%d-%d\n", lDims[0], lDims[3], lDims[1], lDims[4]);
  dbgOut.tprintf(1, "%d elements, %d vertices\n", (lDims[3] - lDims[0]) * (lDims[4] - lDims[1]), (lDims[3] - lDims[0] + 1)
      * (lDims[4] - lDims[1] + 1));

  // For each variable, determine the entity location type and number of levels
  std::map<std::string, ReadNC::VarData>::iterator mit;
  for (mit = varInfo.begin(); mit != varInfo.end(); ++mit) {
    ReadNC::VarData& vd = (*mit).second;

    // Default entLoc is ENTLOCSET
    if (std::find(vd.varDims.begin(), vd.varDims.end(), tDim) != vd.varDims.end()) {
      if ((std::find(vd.varDims.begin(), vd.varDims.end(), iCDim) != vd.varDims.end()) &&
          (std::find(vd.varDims.begin(), vd.varDims.end(), jCDim) != vd.varDims.end()))
        vd.entLoc = ReadNC::ENTLOCFACE;
      else if ((std::find(vd.varDims.begin(), vd.varDims.end(), jDim) != vd.varDims.end()) &&
          (std::find(vd.varDims.begin(), vd.varDims.end(), iCDim) != vd.varDims.end()))
        vd.entLoc = ReadNC::ENTLOCNSEDGE;
      else if ((std::find(vd.varDims.begin(), vd.varDims.end(), jCDim) != vd.varDims.end()) &&
          (std::find(vd.varDims.begin(), vd.varDims.end(), iDim) != vd.varDims.end()))
        vd.entLoc = ReadNC::ENTLOCEWEDGE;
    }

    // Default numLev is 0
    if (std::find(vd.varDims.begin(), vd.varDims.end(), levDim) != vd.varDims.end())
      vd.numLev = nLevels;
  }

  std::vector<std::string> ijdimNames(4);
  ijdimNames[0] = "__slon";
  ijdimNames[1] = "__slat";
  ijdimNames[2] = "__lon";
  ijdimNames[3] = "__lat";

  std::string tag_name;
  Tag tagh;

  // __<dim_name>_LOC_MINMAX (for slon, slat, lon and lat)
  for (unsigned int i = 0; i != ijdimNames.size(); i++) {
    std::vector<int> val(2, 0);
    if (ijdimNames[i] == "__slon") {
      val[0] = lDims[0];
      val[1] = lDims[3];
    }
    else if (ijdimNames[i] == "__slat") {
      val[0] = lDims[1];
      val[1] = lDims[4];
    }
    else if (ijdimNames[i] == "__lon") {
      val[0] = lCDims[0];
      val[1] = lCDims[3];
    }
    else if (ijdimNames[i] == "__lat") {
      val[0] = lCDims[1];
      val[1] = lCDims[4];
    }
    std::stringstream ss_tag_name;
    ss_tag_name << ijdimNames[i] << "_LOC_MINMAX";
    tag_name = ss_tag_name.str();
    rval = mbImpl->tag_get_handle(tag_name.c_str(), 2, MB_TYPE_INTEGER, tagh,
                                  MB_TAG_SPARSE | MB_TAG_CREAT);MB_CHK_SET_ERR(rval, "Trouble creating conventional tag " << tag_name);
    rval = mbImpl->tag_set_data(tagh, &_fileSet, 1, &val[0]);MB_CHK_SET_ERR(rval, "Trouble setting data to conventional tag " << tag_name);
    if (MB_SUCCESS == rval)
      dbgOut.tprintf(2, "Conventional tag %s is created.\n", tag_name.c_str());
  }

  // __<dim_name>_LOC_VALS (for slon, slat, lon and lat)
  // Assume all have the same data type as lon (expected type is float or double)
  switch (varInfo["lon"].varDataType) {
    case NC_FLOAT:
    case NC_DOUBLE:
      break;
    default:
      MB_SET_ERR(MB_FAILURE, "Unexpected variable data type for 'lon'");
  }

  for (unsigned int i = 0; i != ijdimNames.size(); i++) {
    void* val = NULL;
    int val_len = 0;
    if (ijdimNames[i] == "__slon") {
      val = &ilVals[0];
      val_len = ilVals.size();
    }
    else if (ijdimNames[i] == "__slat") {
      val = &jlVals[0];
      val_len = jlVals.size();
    }
    else if (ijdimNames[i] == "__lon") {
      val = &ilCVals[0];
      val_len = ilCVals.size();
    }
    else if (ijdimNames[i] == "__lat") {
      val = &jlCVals[0];
      val_len = jlCVals.size();
    }

    std::stringstream ss_tag_name;
    ss_tag_name << ijdimNames[i] << "_LOC_VALS";
    tag_name = ss_tag_name.str();
    rval = mbImpl->tag_get_handle(tag_name.c_str(), 0, MB_TYPE_DOUBLE, tagh,
                                  MB_TAG_CREAT | MB_TAG_SPARSE | MB_TAG_VARLEN);MB_CHK_SET_ERR(rval, "Trouble creating conventional tag " << tag_name);
    rval = mbImpl->tag_set_by_ptr(tagh, &_fileSet, 1, &val, &val_len);MB_CHK_SET_ERR(rval, "Trouble setting data to conventional tag " << tag_name);
    if (MB_SUCCESS == rval)
      dbgOut.tprintf(2, "Conventional tag %s is created.\n", tag_name.c_str());
  }

  // __<dim_name>_GLOBAL_MINMAX (for slon, slat, lon and lat)
  for (unsigned int i = 0; i != ijdimNames.size(); i++) {
    std::vector<int> val(2, 0);
    if (ijdimNames[i] == "__slon") {
      val[0] = gDims[0];
      val[1] = gDims[3];
    }
    else if (ijdimNames[i] == "__slat") {
      val[0] = gDims[1];
      val[1] = gDims[4];
    }
    else if (ijdimNames[i] == "__lon") {
      val[0] = gCDims[0];
      val[1] = gCDims[3];
    }
    else if (ijdimNames[i] == "__lat") {
      val[0] = gCDims[1];
      val[1] = gCDims[4];
    }
    std::stringstream ss_tag_name;
    ss_tag_name << ijdimNames[i] << "_GLOBAL_MINMAX";
    tag_name = ss_tag_name.str();
    rval = mbImpl->tag_get_handle(tag_name.c_str(), 2, MB_TYPE_INTEGER, tagh,
                                  MB_TAG_SPARSE | MB_TAG_CREAT);MB_CHK_SET_ERR(rval, "Trouble creating conventional tag " << tag_name);
    rval = mbImpl->tag_set_data(tagh, &_fileSet, 1, &val[0]);MB_CHK_SET_ERR(rval, "Trouble setting data to conventional tag " << tag_name);
    if (MB_SUCCESS == rval)
      dbgOut.tprintf(2, "Conventional tag %s is created.\n", tag_name.c_str());
  }

  // Hack: create dummy variables, if needed, for dimensions with no corresponding coordinate variables
  rval = create_dummy_variables();MB_CHK_SET_ERR(rval, "Failed to create dummy variables");

  return MB_SUCCESS;
}

} // namespace moab
