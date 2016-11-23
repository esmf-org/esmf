#include "NCHelper.hpp"
#include "NCHelperEuler.hpp"
#include "NCHelperFV.hpp"
#include "NCHelperHOMME.hpp"
#include "NCHelperMPAS.hpp"
#include "NCHelperGCRM.hpp"

#include <sstream>

#include "MBTagConventions.hpp"

#ifdef WIN32
#ifdef size_t
#undef size_t
#endif
#endif

namespace moab {

NCHelper* NCHelper::get_nc_helper(ReadNC* readNC, int fileId, const FileOptions& opts, EntityHandle fileSet)
{
  // Check if CF convention is being followed
  bool is_CF = false;

  std::map<std::string, ReadNC::AttData>& globalAtts = readNC->globalAtts;
  std::map<std::string, ReadNC::AttData>::iterator attIt = globalAtts.find("conventions");
  if (attIt == globalAtts.end())
    attIt = globalAtts.find("Conventions");

  if (attIt != globalAtts.end()) {
    unsigned int sz = attIt->second.attLen;
    std::string att_data;
    att_data.resize(sz + 1);
    att_data[sz] = '\000';
    int success = NCFUNC(get_att_text)(fileId, attIt->second.attVarId, attIt->second.attName.c_str(), &att_data[0]);
    if (0 == success && att_data.find("CF") != std::string::npos)
      is_CF = true;
  }

  if (is_CF) {
    if (NCHelperEuler::can_read_file(readNC, fileId))
      return new (std::nothrow) NCHelperEuler(readNC, fileId, opts, fileSet);
    else if (NCHelperFV::can_read_file(readNC, fileId))
      return new (std::nothrow) NCHelperFV(readNC, fileId, opts, fileSet);
    else if (NCHelperHOMME::can_read_file(readNC, fileId))
      return new (std::nothrow) NCHelperHOMME(readNC, fileId, opts, fileSet);
  }
  else {
    if (NCHelperMPAS::can_read_file(readNC))
      return new (std::nothrow) NCHelperMPAS(readNC, fileId, opts, fileSet);
    // For a HOMME connectivity file, there might be no CF convention
    else if (NCHelperHOMME::can_read_file(readNC, fileId))
      return new (std::nothrow) NCHelperHOMME(readNC, fileId, opts, fileSet);
    // gcrm reader
    else if (NCHelperGCRM::can_read_file(readNC))
      return new (std::nothrow) NCHelperGCRM(readNC, fileId, opts, fileSet);
  }

  // Unknown NetCDF grid (will fill this in later for POP, CICE and CLM)
  return NULL;
}

ErrorCode NCHelper::create_conventional_tags(const std::vector<int>& tstep_nums)
{
  Interface*& mbImpl = _readNC->mbImpl;
  std::vector<std::string>& dimNames = _readNC->dimNames;
  std::vector<int>& dimLens = _readNC->dimLens;
  std::map<std::string, ReadNC::AttData>& globalAtts = _readNC->globalAtts;
  std::map<std::string, ReadNC::VarData>& varInfo = _readNC->varInfo;
  DebugOutput& dbgOut = _readNC->dbgOut;
  int& partMethod = _readNC->partMethod;
  ScdInterface* scdi = _readNC->scdi;

  ErrorCode rval;
  std::string tag_name;

  // <__NUM_DIMS>
  Tag numDimsTag = 0;
  tag_name = "__NUM_DIMS";
  int numDims = dimNames.size();
  rval = mbImpl->tag_get_handle(tag_name.c_str(), 1, MB_TYPE_INTEGER, numDimsTag,
                                MB_TAG_SPARSE | MB_TAG_CREAT);MB_CHK_SET_ERR(rval, "Trouble creating conventional tag " << tag_name);
  rval = mbImpl->tag_set_data(numDimsTag, &_fileSet, 1, &numDims);MB_CHK_SET_ERR(rval, "Trouble setting data to conventional tag " << tag_name);
  dbgOut.tprintf(2, "Conventional tag %s created\n", tag_name.c_str());

  // <__NUM_VARS>
  Tag numVarsTag = 0;
  tag_name = "__NUM_VARS";
  int numVars = varInfo.size();
  rval = mbImpl->tag_get_handle(tag_name.c_str(), 1, MB_TYPE_INTEGER, numVarsTag,
                                MB_TAG_SPARSE | MB_TAG_CREAT);MB_CHK_SET_ERR(rval, "Trouble creating conventional tag " << tag_name);
  rval = mbImpl->tag_set_data(numVarsTag, &_fileSet, 1, &numVars);MB_CHK_SET_ERR(rval, "Trouble setting data to conventional tag " << tag_name);
  dbgOut.tprintf(2, "Conventional tag %s created\n", tag_name.c_str());

  // <__DIM_NAMES>
  Tag dimNamesTag = 0;
  tag_name = "__DIM_NAMES";
  std::string dimnames;
  unsigned int dimNamesSz = dimNames.size();
  for (unsigned int i = 0; i != dimNamesSz; i++) {
    dimnames.append(dimNames[i]);
    dimnames.push_back('\0');
  }
  int dimnamesSz = dimnames.size();
  rval = mbImpl->tag_get_handle(tag_name.c_str(), 0, MB_TYPE_OPAQUE, dimNamesTag,
                                MB_TAG_CREAT | MB_TAG_SPARSE | MB_TAG_VARLEN);MB_CHK_SET_ERR(rval, "Trouble creating conventional tag " << tag_name);
  const void* ptr = dimnames.c_str();
  rval = mbImpl->tag_set_by_ptr(dimNamesTag, &_fileSet, 1, &ptr, &dimnamesSz);MB_CHK_SET_ERR(rval, "Trouble setting data to conventional tag " << tag_name);
  dbgOut.tprintf(2, "Conventional tag %s created\n", tag_name.c_str());

  // <__DIM_LENS>
  Tag dimLensTag = 0;
  tag_name = "__DIM_LENS";
  int dimLensSz = dimLens.size();
  rval = mbImpl->tag_get_handle(tag_name.c_str(), 0, MB_TYPE_INTEGER, dimLensTag,
                                MB_TAG_CREAT | MB_TAG_SPARSE | MB_TAG_VARLEN);MB_CHK_SET_ERR(rval, "Trouble creating conventional tag " << tag_name);
  ptr = &(dimLens[0]);
  rval = mbImpl->tag_set_by_ptr(dimLensTag, &_fileSet, 1, &ptr, &dimLensSz);MB_CHK_SET_ERR(rval, "Trouble setting data to conventional tag " << tag_name);
  dbgOut.tprintf(2, "Conventional tag %s created\n", tag_name.c_str());

  // <__VAR_NAMES>
  Tag varNamesTag = 0;
  tag_name = "__VAR_NAMES";
  std::string varnames;
  std::map<std::string, ReadNC::VarData>::iterator mapIter;
  for (mapIter = varInfo.begin(); mapIter != varInfo.end(); ++mapIter) {
    varnames.append(mapIter->first);
    varnames.push_back('\0');
  }
  int varnamesSz = varnames.size();
  rval = mbImpl->tag_get_handle(tag_name.c_str(), 0, MB_TYPE_OPAQUE, varNamesTag,
                                MB_TAG_CREAT | MB_TAG_SPARSE | MB_TAG_VARLEN);MB_CHK_SET_ERR(rval, "Trouble creating conventional tag " << tag_name);
  ptr = varnames.c_str();
  rval = mbImpl->tag_set_by_ptr(varNamesTag, &_fileSet, 1, &ptr, &varnamesSz);MB_CHK_SET_ERR(rval, "Trouble setting data to conventional tag " << tag_name);
  dbgOut.tprintf(2, "Conventional tag %s created\n", tag_name.c_str());

  // __<dim_name>_LOC_MINMAX (for time)
  for (unsigned int i = 0; i != dimNamesSz; i++) {
    if (dimNames[i] == "time" || dimNames[i] == "Time" || dimNames[i] == "t") {
      // some files might have Time dimension as 0, it will not appear in any
      // variables, so skip it
      if (nTimeSteps==0)
        continue;
      std::stringstream ss_tag_name;
      ss_tag_name << "__" << dimNames[i] << "_LOC_MINMAX";
      tag_name = ss_tag_name.str();
      Tag tagh = 0;
      std::vector<int> val(2, 0);
      val[0] = 0;
      val[1] = nTimeSteps - 1;
      rval = mbImpl->tag_get_handle(tag_name.c_str(), 2, MB_TYPE_INTEGER, tagh,
                                    MB_TAG_SPARSE | MB_TAG_CREAT);MB_CHK_SET_ERR(rval, "Trouble creating conventional tag " << tag_name);
      rval = mbImpl->tag_set_data(tagh, &_fileSet, 1, &val[0]);MB_CHK_SET_ERR(rval, "Trouble setting data to conventional tag " << tag_name);
      dbgOut.tprintf(2, "Conventional tag %s created\n", tag_name.c_str());
    }
  }

  // __<dim_name>_LOC_VALS (for time)
  for (unsigned int i = 0; i != dimNamesSz; i++) {
    if (dimNames[i] == "time" || dimNames[i] == "Time" || dimNames[i] == "t") {
      std::vector<int> val;
      if (!tstep_nums.empty())
        val = tstep_nums;
      else {
        // some files might have Time dimension as 0, it will not appear in any
        // variables, so skip it
        if (tVals.empty())
          continue;
        val.resize(tVals.size());
        for (unsigned int j = 0; j != tVals.size(); j++)
          val[j] = j;
      }
      Tag tagh = 0;
      std::stringstream ss_tag_name;
      ss_tag_name << "__" << dimNames[i] << "_LOC_VALS";
      tag_name = ss_tag_name.str();
      rval = mbImpl->tag_get_handle(tag_name.c_str(), val.size(), MB_TYPE_INTEGER, tagh,
                                    MB_TAG_SPARSE | MB_TAG_CREAT);MB_CHK_SET_ERR(rval, "Trouble creating conventional tag " << tag_name);
      rval = mbImpl->tag_set_data(tagh, &_fileSet, 1, &val[0]);MB_CHK_SET_ERR(rval, "Trouble setting data to conventional tag " << tag_name);
      dbgOut.tprintf(2, "Conventional tag %s created\n", tag_name.c_str());
    }
  }

  // __<var_name>_DIMS
  for (mapIter = varInfo.begin(); mapIter != varInfo.end(); ++mapIter) {
    Tag varNamesDimsTag = 0;
    std::stringstream ss_tag_name;
    ss_tag_name << "__" << mapIter->first << "_DIMS";
    tag_name = ss_tag_name.str();
    unsigned int varDimSz = varInfo[mapIter->first].varDims.size();
    if (varDimSz == 0)
      continue;
    std::vector<Tag> varDimTags(varDimSz);
    for (unsigned int i = 0; i != varDimSz; i++) {
      Tag tmptag = 0;
      std::string tmptagname = dimNames[varInfo[mapIter->first].varDims[i]];
      rval = mbImpl->tag_get_handle(tmptagname.c_str(), 0, MB_TYPE_OPAQUE, tmptag, MB_TAG_ANY);MB_CHK_SET_ERR(rval, "Trouble getting tag " << tmptagname);
      varDimTags[i] = tmptag;
    }
    // rval = mbImpl->tag_get_handle(tag_name.c_str(), varDimSz, MB_TYPE_HANDLE, varNamesDimsTag, MB_TAG_SPARSE | MB_TAG_CREAT);
    // We should not use MB_TYPE_HANDLE for Tag here. Tag is a pointer, which is 4 bytes on 32 bit machines and 8 bytes on 64 bit machines.
    // Normally, entity handle is 8 bytes on 64 bit machines, but it can also be configured to 4 bytes.
    rval = mbImpl->tag_get_handle(tag_name.c_str(), varDimSz*sizeof(Tag), MB_TYPE_OPAQUE, varNamesDimsTag,
                                  MB_TAG_SPARSE | MB_TAG_CREAT);MB_CHK_SET_ERR(rval, "Trouble creating conventional tag " << tag_name);
    rval = mbImpl->tag_set_data(varNamesDimsTag, &_fileSet, 1, &(varDimTags[0]));MB_CHK_SET_ERR(rval, "Trouble setting data to conventional tag " << tag_name);
    dbgOut.tprintf(2, "Conventional tag %s created\n", tag_name.c_str());
  }

  // <PARTITION_METHOD>
  Tag part_tag = scdi->part_method_tag();
  if (!part_tag)
    MB_SET_ERR(MB_FAILURE, "Trouble getting PARTITION_METHOD tag");
  rval = mbImpl->tag_set_data(part_tag, &_fileSet, 1, &partMethod);MB_CHK_SET_ERR(rval, "Trouble setting data to PARTITION_METHOD tag");
  dbgOut.tprintf(2, "Conventional tag %s created\n", tag_name.c_str());

  // <__GLOBAL_ATTRIBS>
  tag_name = "__GLOBAL_ATTRIBS";
  Tag globalAttTag = 0;
  rval = mbImpl->tag_get_handle(tag_name.c_str(), 0, MB_TYPE_OPAQUE, globalAttTag,
                                MB_TAG_CREAT | MB_TAG_SPARSE | MB_TAG_VARLEN);MB_CHK_SET_ERR(rval, "Trouble creating conventional tag " << tag_name);
  std::string gattVal;
  std::vector<int> gattLen;
  rval = create_attrib_string(globalAtts, gattVal, gattLen);MB_CHK_SET_ERR(rval, "Trouble creating global attribute string");
  const void* gattptr = gattVal.c_str();
  int globalAttSz = gattVal.size();
  rval = mbImpl->tag_set_by_ptr(globalAttTag, &_fileSet, 1, &gattptr, &globalAttSz);MB_CHK_SET_ERR(rval, "Trouble setting data to conventional tag " << tag_name);
  dbgOut.tprintf(2, "Conventional tag %s created\n", tag_name.c_str());

  // <__GLOBAL_ATTRIBS_LEN>
  tag_name = "__GLOBAL_ATTRIBS_LEN";
  Tag globalAttLenTag = 0;
  if (gattLen.size() == 0)
    gattLen.push_back(0);
  rval = mbImpl->tag_get_handle(tag_name.c_str(), gattLen.size(), MB_TYPE_INTEGER, globalAttLenTag,
                                MB_TAG_SPARSE | MB_TAG_CREAT);MB_CHK_SET_ERR(rval, "Trouble creating conventional tag " << tag_name);
  rval = mbImpl->tag_set_data(globalAttLenTag, &_fileSet, 1, &gattLen[0]);MB_CHK_SET_ERR(rval, "Trouble setting data to conventional tag " << tag_name);
  dbgOut.tprintf(2, "Conventional tag %s created\n", tag_name.c_str());

  // __<var_name>_ATTRIBS and __<var_name>_ATTRIBS_LEN
  for (mapIter = varInfo.begin(); mapIter != varInfo.end(); ++mapIter) {
    std::stringstream ssTagName;
    ssTagName << "__" << mapIter->first << "_ATTRIBS";
    tag_name = ssTagName.str();
    Tag varAttTag = 0;
    rval = mbImpl->tag_get_handle(tag_name.c_str(), 0, MB_TYPE_OPAQUE, varAttTag,
                                  MB_TAG_CREAT | MB_TAG_SPARSE | MB_TAG_VARLEN);MB_CHK_SET_ERR(rval, "Trouble creating conventional tag " << tag_name);

    std::string varAttVal;
    std::vector<int> varAttLen;
    if (mapIter->second.numAtts < 1) {
      if (dummyVarNames.find(mapIter->first) != dummyVarNames.end()) {
        // This variable is a dummy coordinate variable
        varAttVal = "DUMMY_VAR";
      }
      else {
        // This variable has no attributes
        varAttVal = "NO_ATTRIBS";
      }
    }
    else {
      rval = create_attrib_string(mapIter->second.varAtts, varAttVal, varAttLen);MB_CHK_SET_ERR(rval, "Trouble creating attribute string for variable " << mapIter->first);
    }
    const void* varAttPtr = varAttVal.c_str();
    int varAttSz = varAttVal.size();
    if (0 == varAttSz)
      varAttSz = 1;
    rval = mbImpl->tag_set_by_ptr(varAttTag, &_fileSet, 1, &varAttPtr, &varAttSz);MB_CHK_SET_ERR(rval, "Trouble setting data to conventional tag " << tag_name);
    dbgOut.tprintf(2, "Conventional tag %s created\n", tag_name.c_str());

    ssTagName << "_LEN";
    tag_name = ssTagName.str();
    Tag varAttLenTag = 0;
    if (0 == varAttLen.size())
      varAttLen.push_back(0);
    rval = mbImpl->tag_get_handle(tag_name.c_str(), varAttLen.size(), MB_TYPE_INTEGER, varAttLenTag,
                                  MB_TAG_SPARSE | MB_TAG_CREAT);MB_CHK_SET_ERR(rval, "Trouble creating conventional tag " << tag_name);
    rval = mbImpl->tag_set_data(varAttLenTag, &_fileSet, 1, &varAttLen[0]);MB_CHK_SET_ERR(rval, "Trouble setting data to conventional tag " << tag_name);
    dbgOut.tprintf(2, "Conventional tag %s created\n", tag_name.c_str());
  }

  // <__VAR_NAMES_LOCATIONS>
  tag_name = "__VAR_NAMES_LOCATIONS";
  Tag varNamesLocsTag = 0;
  std::vector<int> varNamesLocs(varInfo.size());
  rval = mbImpl->tag_get_handle(tag_name.c_str(), varNamesLocs.size(), MB_TYPE_INTEGER, varNamesLocsTag,
                                MB_TAG_CREAT | MB_TAG_SPARSE);MB_CHK_SET_ERR(rval, "Trouble creating conventional tag " << tag_name);
  for (mapIter = varInfo.begin(); mapIter != varInfo.end(); ++mapIter) {
    varNamesLocs[std::distance(varInfo.begin(), mapIter)] = mapIter->second.entLoc;
  }
  rval = mbImpl->tag_set_data(varNamesLocsTag, &_fileSet, 1, &varNamesLocs[0]);MB_CHK_SET_ERR(rval, "Trouble setting data to conventional tag " << tag_name);
  dbgOut.tprintf(2, "Conventional tag %s created\n", tag_name.c_str());

  // <__MESH_TYPE>
  Tag meshTypeTag = 0;
  tag_name = "__MESH_TYPE";
  std::string meshTypeName = get_mesh_type_name();

  rval = mbImpl->tag_get_handle(tag_name.c_str(), 0, MB_TYPE_OPAQUE, meshTypeTag,
                                MB_TAG_CREAT | MB_TAG_SPARSE | MB_TAG_VARLEN);MB_CHK_SET_ERR(rval, "Trouble creating conventional tag " << tag_name);
  ptr = meshTypeName.c_str();
  int leng = meshTypeName.size();
  rval = mbImpl->tag_set_by_ptr(meshTypeTag, &_fileSet, 1, &ptr, &leng);MB_CHK_SET_ERR(rval, "Trouble setting data to conventional tag " << tag_name);
  dbgOut.tprintf(2, "Conventional tag %s created\n", tag_name.c_str());

  return MB_SUCCESS;
}

ErrorCode NCHelper::update_time_tag_vals()
{
  Interface*& mbImpl = _readNC->mbImpl;
  std::vector<std::string>& dimNames = _readNC->dimNames;

  ErrorCode rval;

  // The time tag might be a dummy one (e.g. 'Time' for MPAS)
  std::string time_tag_name = dimNames[tDim];
  if (dummyVarNames.find(time_tag_name) != dummyVarNames.end())
    return MB_SUCCESS;

  Tag time_tag = 0;
  const void* data = NULL;
  int time_tag_size = 0;
  rval = mbImpl->tag_get_handle(time_tag_name.c_str(), 0, MB_TYPE_DOUBLE, time_tag, MB_TAG_VARLEN);MB_CHK_SET_ERR(rval, "Trouble getting tag " << time_tag_name);
  rval = mbImpl->tag_get_by_ptr(time_tag, &_fileSet, 1, &data, &time_tag_size);MB_CHK_SET_ERR(rval, "Trouble getting data of tag " << time_tag_name);
  const double* time_tag_vals = static_cast<const double*>(data);

  // Merge tVals (read from current file) to existing time tag
  // Assume that time_tag_vals and tVals are both sorted
  std::vector<double> merged_time_vals;
  merged_time_vals.reserve(time_tag_size + nTimeSteps);
  int i = 0;
  int j = 0;

  // Merge time values from time_tag_vals and tVals
  while (i < time_tag_size && j < nTimeSteps) {
    if (time_tag_vals[i] < tVals[j])
      merged_time_vals.push_back(time_tag_vals[i++]);
    else
      merged_time_vals.push_back(tVals[j++]);
  }

  // Append remaining time values of time_tag_vals (if any)
  while (i < time_tag_size)
    merged_time_vals.push_back(time_tag_vals[i++]);

  // Append remaining time values of tVals (if any)
  while (j < nTimeSteps)
    merged_time_vals.push_back(tVals[j++]);

  data = &merged_time_vals[0];
  time_tag_size = merged_time_vals.size();
  rval = mbImpl->tag_set_by_ptr(time_tag, &_fileSet, 1, &data, &time_tag_size);MB_CHK_SET_ERR(rval, "Trouble setting data to tag " << time_tag_name);

  return MB_SUCCESS;
}

ErrorCode NCHelper::read_variables_setup(std::vector<std::string>& var_names, std::vector<int>& tstep_nums,
                                        std::vector<ReadNC::VarData>& vdatas, std::vector<ReadNC::VarData>& vsetdatas)
{
  std::map<std::string, ReadNC::VarData>& varInfo = _readNC->varInfo;
  std::vector<std::string>& dimNames = _readNC->dimNames;

  std::map<std::string, ReadNC::VarData>::iterator mit;

  // If empty read them all (except ignored variables)
  if (var_names.empty()) {
    for (mit = varInfo.begin(); mit != varInfo.end(); ++mit) {
      ReadNC::VarData vd = (*mit).second;

      // If read all variables at once, skip ignored ones
      if (ignoredVarNames.find(vd.varName) != ignoredVarNames.end())
         continue;

      // Coordinate variables (include dummy ones) were read to the file set by default
      if (std::find(dimNames.begin(), dimNames.end(), vd.varName) != dimNames.end())
        continue;

      if (vd.entLoc == ReadNC::ENTLOCSET)
        vsetdatas.push_back(vd);
      else
        vdatas.push_back(vd);
    }
  }
  else {
    // Read specified variables (might include ignored ones)
    for (unsigned int i = 0; i < var_names.size(); i++) {
      mit = varInfo.find(var_names[i]);
      if (mit != varInfo.end()) {
        ReadNC::VarData vd = (*mit).second;

        // Upon creation of dummy coordinate variables, tag values have already been set
        if (dummyVarNames.find(vd.varName) != dummyVarNames.end())
           continue;

        if (vd.entLoc == ReadNC::ENTLOCSET)
          vsetdatas.push_back(vd);
        else
          vdatas.push_back(vd);
      }
      else {
        MB_SET_ERR(MB_FAILURE, "Couldn't find specified variable " << var_names[i]);
      }
    }
  }

  if (tstep_nums.empty() && nTimeSteps > 0) {
    // No timesteps input, get them all
    for (int i = 0; i < nTimeSteps; i++)
      tstep_nums.push_back(i);
  }

  if (!tstep_nums.empty()) {
    for (unsigned int i = 0; i < vdatas.size(); i++) {
      vdatas[i].varTags.resize(tstep_nums.size(), 0);
      vdatas[i].varDatas.resize(tstep_nums.size());
      // NC reader assumes that non-set variables always have timesteps
      assert(std::find(vdatas[i].varDims.begin(), vdatas[i].varDims.end(), tDim) != vdatas[i].varDims.end());
      vdatas[i].has_tsteps = true;
    }

    for (unsigned int i = 0; i < vsetdatas.size(); i++) {
      if ((std::find(vsetdatas[i].varDims.begin(), vsetdatas[i].varDims.end(), tDim) != vsetdatas[i].varDims.end())
          && (vsetdatas[i].varName != dimNames[tDim])) {
        // Set variables with timesteps: e.g. xtime(Time) or xtime(Time, StrLen)
        vsetdatas[i].varTags.resize(tstep_nums.size(), 0);
        vsetdatas[i].varDatas.resize(tstep_nums.size());
        vsetdatas[i].has_tsteps = true;
      }
      else {
        // Set variables without timesteps: no time dimension, or time itself
        vsetdatas[i].varTags.resize(1, 0);
        vsetdatas[i].varDatas.resize(1);
        vsetdatas[i].has_tsteps = false;
      }
    }
  }

  return MB_SUCCESS;
}

ErrorCode NCHelper::read_variables_to_set(std::vector<ReadNC::VarData>& vdatas, std::vector<int>& tstep_nums)
{
  Interface*& mbImpl = _readNC->mbImpl;
  DebugOutput& dbgOut = _readNC->dbgOut;

  ErrorCode rval = read_variables_to_set_allocate(vdatas, tstep_nums);MB_CHK_SET_ERR(rval, "Trouble allocating space to read set variables");

  // Finally, read into that space
  int success;
  for (unsigned int i = 0; i < vdatas.size(); i++) {
    // Note, for set variables without timesteps, loop one time and then break
    for (unsigned int t = 0; t < tstep_nums.size(); t++) {
      void* data = vdatas[i].varDatas[t];

      // Set variables with timesteps, e.g. xtime(Time) or xtime(Time, StrLen)
      if (vdatas[i].has_tsteps) {
        // Set readStart for each timestep along time dimension
        vdatas[i].readStarts[0] = tstep_nums[t];
      }

      switch (vdatas[i].varDataType) {
        case NC_BYTE:
        case NC_CHAR:
          success = NCFUNCAG(_vara_text)(_fileId, vdatas[i].varId, &vdatas[i].readStarts[0],
                                        &vdatas[i].readCounts[0], (char*) data);
          if (success)
            MB_SET_ERR(MB_FAILURE, "Failed to read byte/char data for variable " << vdatas[i].varName);
          break;
        case NC_SHORT:
        case NC_INT:
          success = NCFUNCAG(_vara_int)(_fileId, vdatas[i].varId, &vdatas[i].readStarts[0],
                                        &vdatas[i].readCounts[0], (int*) data);
          if (success)
            MB_SET_ERR(MB_FAILURE, "Failed to read short/int data for variable " << vdatas[i].varName);
          break;
        case NC_FLOAT:
        case NC_DOUBLE:
          success = NCFUNCAG(_vara_double)(_fileId, vdatas[i].varId, &vdatas[i].readStarts[0],
                                        &vdatas[i].readCounts[0], (double*) data);
          if (success)
            MB_SET_ERR(MB_FAILURE, "Failed to read float/double data for variable " << vdatas[i].varName);
          break;
        default:
          MB_SET_ERR(MB_FAILURE, "Unexpected data type for variable " << vdatas[i].varName);
      }

      dbgOut.tprintf(2, "Setting data for variable %s, time step %d\n", vdatas[i].varName.c_str(), tstep_nums[t]);
      rval = mbImpl->tag_set_by_ptr(vdatas[i].varTags[t], &_fileSet, 1, &data, &vdatas[i].sz);MB_CHK_SET_ERR(rval, "Trouble setting tag data for variable " << vdatas[i].varName);

      // Memory pointed by pointer data can be deleted, as tag_set_by_ptr() has already copied the tag values
      switch (vdatas[i].varDataType) {
        case NC_BYTE:
        case NC_CHAR:
          delete[] (char*) data;
          break;
        case NC_SHORT:
        case NC_INT:
          delete[] (int*) data;
          break;
        case NC_FLOAT:
        case NC_DOUBLE:
          delete[] (double*) data;
          break;
        default:
          break;
      }
      vdatas[i].varDatas[t] = NULL;

      // Loop continues only for set variables with timesteps, e.g. xtime(Time) or xtime(Time, StrLen)
      if (!vdatas[i].has_tsteps)
        break;
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

ErrorCode NCHelper::read_coordinate(const char* var_name, int lmin, int lmax, std::vector<double>& cvals)
{
  std::map<std::string, ReadNC::VarData>& varInfo = _readNC->varInfo;
  std::map<std::string, ReadNC::VarData>::iterator vmit = varInfo.find(var_name);
  if (varInfo.end() == vmit)
    MB_SET_ERR(MB_FAILURE, "Couldn't find variable " << var_name);

  assert(lmin >= 0 && lmax >= lmin);
  NCDF_SIZE tstart = lmin;
  NCDF_SIZE tcount = lmax - lmin + 1;
  NCDF_DIFF dum_stride = 1;
  int success;

  // Check size
  if ((std::size_t)tcount != cvals.size())
    cvals.resize(tcount);

  // Check to make sure it's a float or double
  switch ((*vmit).second.varDataType) {
    case NC_FLOAT:
    case NC_DOUBLE:
      // Read float as double
      success = NCFUNCAG(_vars_double)(_fileId, (*vmit).second.varId, &tstart, &tcount, &dum_stride, &cvals[0]);
      if (success)
        MB_SET_ERR(MB_FAILURE, "Failed to read float/double data for variable " << var_name);
      break;
    default:
      MB_SET_ERR(MB_FAILURE, "Unexpected data type for variable " << var_name);
  }

  return MB_SUCCESS;
}

ErrorCode NCHelper::get_tag_to_set(ReadNC::VarData& var_data, int tstep_num, Tag& tagh)
{
  Interface*& mbImpl = _readNC->mbImpl;
  DebugOutput& dbgOut = _readNC->dbgOut;
  int& tStepBase = _readNC->tStepBase;

  if (tStepBase > 0)
    tstep_num += tStepBase;

  std::ostringstream tag_name;
  if (var_data.has_tsteps)
    tag_name << var_data.varName << tstep_num;
  else
    tag_name << var_data.varName;

  ErrorCode rval = MB_SUCCESS;
  tagh = 0;
  switch (var_data.varDataType) {
    case NC_BYTE:
    case NC_CHAR:
      rval = mbImpl->tag_get_handle(tag_name.str().c_str(), 0, MB_TYPE_OPAQUE, tagh,
                                    MB_TAG_CREAT | MB_TAG_SPARSE | MB_TAG_VARLEN);MB_CHK_SET_ERR(rval, "Trouble creating tag " << tag_name.str());
      break;
    case NC_SHORT:
    case NC_INT:
      rval = mbImpl->tag_get_handle(tag_name.str().c_str(), 0, MB_TYPE_INTEGER, tagh,
                                    MB_TAG_CREAT | MB_TAG_SPARSE | MB_TAG_VARLEN);MB_CHK_SET_ERR(rval, "Trouble creating tag " << tag_name.str());
      break;
    case NC_FLOAT:
    case NC_DOUBLE:
      rval = mbImpl->tag_get_handle(tag_name.str().c_str(), 0, MB_TYPE_DOUBLE, tagh,
                                    MB_TAG_CREAT | MB_TAG_SPARSE | MB_TAG_VARLEN);MB_CHK_SET_ERR(rval, "Trouble creating tag " << tag_name.str());
      break;
    default:
      MB_SET_ERR(MB_FAILURE, "Unexpected data type for variable " << var_data.varName);
  }

  dbgOut.tprintf(2, "Tag %s created\n", tag_name.str().c_str());

  return rval;
}

ErrorCode NCHelper::get_tag_to_nonset(ReadNC::VarData& var_data, int tstep_num, Tag& tagh, int num_lev)
{
  Interface*& mbImpl = _readNC->mbImpl;
  DebugOutput& dbgOut = _readNC->dbgOut;
  int& tStepBase = _readNC->tStepBase;

  if (tStepBase > 0)
    tstep_num += tStepBase;

  std::ostringstream tag_name;
  tag_name << var_data.varName << tstep_num;

  ErrorCode rval = MB_SUCCESS;
  tagh = 0;
  switch (var_data.varDataType) {
    case NC_BYTE:
    case NC_CHAR:
      rval = mbImpl->tag_get_handle(tag_name.str().c_str(), num_lev, MB_TYPE_OPAQUE, tagh,
                                    MB_TAG_DENSE | MB_TAG_CREAT);MB_CHK_SET_ERR(rval, "Trouble creating tag " << tag_name.str());
      break;
    case NC_SHORT:
    case NC_INT:
      rval = mbImpl->tag_get_handle(tag_name.str().c_str(), num_lev, MB_TYPE_INTEGER, tagh,
                                    MB_TAG_DENSE | MB_TAG_CREAT);MB_CHK_SET_ERR(rval, "Trouble creating tag " << tag_name.str());
      break;
    case NC_FLOAT:
    case NC_DOUBLE:
      rval = mbImpl->tag_get_handle(tag_name.str().c_str(), num_lev, MB_TYPE_DOUBLE, tagh,
                                    MB_TAG_DENSE | MB_TAG_CREAT);MB_CHK_SET_ERR(rval, "Trouble creating tag " << tag_name.str());
      break;
    default:
      MB_SET_ERR(MB_FAILURE, "Unexpected data type for variable " << var_data.varName);
  }

  dbgOut.tprintf(2, "Tag %s created\n", tag_name.str().c_str());

  return rval;
}

ErrorCode NCHelper::create_attrib_string(const std::map<std::string, ReadNC::AttData>& attMap, std::string& attVal, std::vector<int>& attLen)
{
  int success;
  std::stringstream ssAtt;
  unsigned int sz = 0;
  std::map<std::string, ReadNC::AttData>::const_iterator attIt = attMap.begin();
  for (; attIt != attMap.end(); ++attIt) {
    ssAtt << attIt->second.attName;
    ssAtt << '\0';
    void* attData = NULL;
    switch (attIt->second.attDataType) {
      case NC_BYTE:
      case NC_CHAR:
        sz = attIt->second.attLen;
        attData = (char *) malloc(sz);
        success = NCFUNC(get_att_text)(_fileId, attIt->second.attVarId, attIt->second.attName.c_str(), (char*) attData);
        if (success)
          MB_SET_ERR(MB_FAILURE, "Failed to read byte/char data for attribute " << attIt->second.attName);
        ssAtt << "char;";
        break;
      case NC_SHORT:
        sz = attIt->second.attLen * sizeof(short);
        attData = (short *) malloc(sz);
        success = NCFUNC(get_att_short)(_fileId, attIt->second.attVarId, attIt->second.attName.c_str(), (short*) attData);
        if (success)
          MB_SET_ERR(MB_FAILURE, "Failed to read short data for attribute " << attIt->second.attName);
        ssAtt << "short;";
        break;
      case NC_INT:
        sz = attIt->second.attLen * sizeof(int);
        attData = (int *) malloc(sz);
        success = NCFUNC(get_att_int)(_fileId, attIt->second.attVarId, attIt->second.attName.c_str(), (int*) attData);
        if (success)
          MB_SET_ERR(MB_FAILURE, "Failed to read int data for attribute " << attIt->second.attName);
        ssAtt << "int;";
        break;
      case NC_FLOAT:
        sz = attIt->second.attLen * sizeof(float);
        attData = (float *) malloc(sz);
        success = NCFUNC(get_att_float)(_fileId, attIt->second.attVarId, attIt->second.attName.c_str(), (float*) attData);
        if (success)
          MB_SET_ERR(MB_FAILURE, "Failed to read float data for attribute " << attIt->second.attName);
        ssAtt << "float;";
        break;
      case NC_DOUBLE:
        sz = attIt->second.attLen * sizeof(double);
        attData = (double *) malloc(sz);
        success = NCFUNC(get_att_double)(_fileId, attIt->second.attVarId, attIt->second.attName.c_str(), (double*) attData);
        if (success)
          MB_SET_ERR(MB_FAILURE, "Failed to read double data for attribute " << attIt->second.attName);
        ssAtt << "double;";
        break;
      default:
        MB_SET_ERR(MB_FAILURE, "Unexpected data type for attribute " << attIt->second.attName);
    }
    char* tmpc = (char *) attData;
    for (unsigned int counter = 0; counter != sz; ++counter)
      ssAtt << tmpc[counter];
    free(attData);
    ssAtt << ';';
    attLen.push_back(ssAtt.str().size() - 1);
  }
  attVal = ssAtt.str();

  return MB_SUCCESS;
}

ErrorCode NCHelper::create_dummy_variables()
{
  Interface*& mbImpl = _readNC->mbImpl;
  std::vector<std::string>& dimNames = _readNC->dimNames;
  std::vector<int>& dimLens = _readNC->dimLens;
  std::map<std::string, ReadNC::VarData>& varInfo = _readNC->varInfo;
  DebugOutput& dbgOut = _readNC->dbgOut;

  // Hack: look at all dimensions, and see if we have one that does not appear in the list of varInfo names
  // Right now, candidates are from unstructured meshes, such as ncol (HOMME) and nCells (MPAS)
  // For each of them, create a dummy coordinate variable with a sparse tag to store the dimension length
  for (unsigned int i = 0; i < dimNames.size(); i++) {
    // If there is a variable with this dimension name, skip
    if (varInfo.find(dimNames[i]) != varInfo.end())
      continue;

    // Create a dummy coordinate variable
    int sizeTotalVar = varInfo.size();
    std::string var_name(dimNames[i]);
    ReadNC::VarData& data = varInfo[var_name];
    data.varName = var_name;
    data.varId = sizeTotalVar;
    data.varTags.resize(1, 0);
    data.varDataType = NC_INT;
    data.varDims.resize(1);
    data.varDims[0] = (int)i;
    data.numAtts = 0;
    data.entLoc = ReadNC::ENTLOCSET;
    dummyVarNames.insert(var_name);
    dbgOut.tprintf(2, "Dummy coordinate variable created for dimension %s\n", var_name.c_str());

    // Create a corresponding sparse tag
    Tag tagh;
    ErrorCode rval = mbImpl->tag_get_handle(var_name.c_str(), 0, MB_TYPE_INTEGER, tagh,
                                            MB_TAG_CREAT | MB_TAG_SPARSE | MB_TAG_VARLEN);MB_CHK_SET_ERR(rval, "Trouble creating tag for dummy coordinate variable " << var_name);

    // Tag value is the dimension length
    const void* ptr = &dimLens[i];
    // Tag size is 1
    int size = 1;
    rval = mbImpl->tag_set_by_ptr(tagh, &_fileSet, 1, &ptr, &size);MB_CHK_SET_ERR(rval, "Trouble setting tag data for dummy coordinate variable " << var_name);

    dbgOut.tprintf(2, "Sparse tag created for dimension %s\n", var_name.c_str());
  }

  return MB_SUCCESS;
}

ErrorCode NCHelper::read_variables_to_set_allocate(std::vector<ReadNC::VarData>& vdatas, std::vector<int>& tstep_nums)
{
  std::vector<int>& dimLens = _readNC->dimLens;
  DebugOutput& dbgOut = _readNC->dbgOut;

  ErrorCode rval = MB_SUCCESS;

  for (unsigned int i = 0; i < vdatas.size(); i++) {
    // Set up readStarts and readCounts
    if (vdatas[i].has_tsteps) {
      // First: time
      vdatas[i].readStarts.push_back(0); // This value is timestep dependent, will be set later
      vdatas[i].readCounts.push_back(1);

      // Next: other dimensions
      for (unsigned int idx = 1; idx != vdatas[i].varDims.size(); idx++) {
        vdatas[i].readStarts.push_back(0);
        vdatas[i].readCounts.push_back(dimLens[vdatas[i].varDims[idx]]);
      }
    }
    else {
      if (vdatas[i].varDims.empty()) {
        // Scalar variable
        vdatas[i].readStarts.push_back(0);
        vdatas[i].readCounts.push_back(1);
      }
      else {
        for (unsigned int idx = 0; idx != vdatas[i].varDims.size(); idx++) {
          vdatas[i].readStarts.push_back(0);
          vdatas[i].readCounts.push_back(dimLens[vdatas[i].varDims[idx]]);
        }
      }
    }

    // Get variable size
    vdatas[i].sz = 1;
    for (std::size_t idx = 0; idx != vdatas[i].readCounts.size(); idx++)
      vdatas[i].sz *= vdatas[i].readCounts[idx];

    // Note, for set variables without timesteps, loop one time and then break
    for (unsigned int t = 0; t < tstep_nums.size(); t++) {
      dbgOut.tprintf(2, "Reading variable %s, time step %d\n", vdatas[i].varName.c_str(), tstep_nums[t]);

      if (tstep_nums[t] >= dimLens[tDim]) {
        MB_SET_ERR(MB_INDEX_OUT_OF_RANGE, "Wrong value for timestep number " << tstep_nums[t]);
      }

      // Get the tag to read into
      if (!vdatas[i].varTags[t]) {
        rval = get_tag_to_set(vdatas[i], tstep_nums[t], vdatas[i].varTags[t]);MB_CHK_SET_ERR(rval, "Trouble getting tag to set variable " << vdatas[i].varName);
      }

      switch (vdatas[i].varDataType) {
        case NC_BYTE:
        case NC_CHAR:
          vdatas[i].varDatas[t] = new char[vdatas[i].sz];
          break;
        case NC_SHORT:
        case NC_INT:
          vdatas[i].varDatas[t] = new int[vdatas[i].sz];
          break;
        case NC_FLOAT:
        case NC_DOUBLE:
          vdatas[i].varDatas[t] = new double[vdatas[i].sz];
          break;
        default:
          MB_SET_ERR(MB_FAILURE, "Unexpected data type for variable " << vdatas[i].varName);
      }

      // Loop continues only for set variables with timesteps, e.g. xtime(Time) or xtime(Time, StrLen)
      if (!vdatas[i].has_tsteps)
        break;
    }
  }

  return rval;
}

ErrorCode ScdNCHelper::check_existing_mesh() {
  Interface*& mbImpl = _readNC->mbImpl;

  // Get the number of vertices
  int num_verts;
  ErrorCode rval = mbImpl->get_number_entities_by_dimension(_fileSet, 0, num_verts);MB_CHK_SET_ERR(rval, "Trouble getting number of vertices");

  /*
  // Check against parameters
  // When ghosting is used, this check might fail (to be updated later)
  if (num_verts > 0) {
    int expected_verts = (lDims[3] - lDims[0] + 1) * (lDims[4] - lDims[1] + 1) * (-1 == lDims[2] ? 1 : lDims[5] - lDims[2] + 1);
    if (num_verts != expected_verts) {
      MB_SET_ERR(MB_FAILURE, "Number of vertices doesn't match");
    }
  }
  */

  // Check the number of elements too
  int num_elems;
  rval = mbImpl->get_number_entities_by_dimension(_fileSet, (-1 == lCDims[2] ? 2 : 3), num_elems);MB_CHK_SET_ERR(rval, "Trouble getting number of elements");

  /*
  // Check against parameters
  // When ghosting is used, this check might fail (to be updated later)
  if (num_elems > 0) {
    int expected_elems = (lCDims[3] - lCDims[0] + 1) * (lCDims[4] - lCDims[1] + 1) * (-1 == lCDims[2] ? 1 : (lCDims[5] - lCDims[2] + 1));
    if (num_elems != expected_elems) {
      MB_SET_ERR(MB_FAILURE, "Number of elements doesn't match");
    }
  }
  */

  return MB_SUCCESS;
}

ErrorCode ScdNCHelper::create_mesh(Range& faces)
{
  Interface*& mbImpl = _readNC->mbImpl;
  Tag& mGlobalIdTag = _readNC->mGlobalIdTag;
  const Tag*& mpFileIdTag = _readNC->mpFileIdTag;
  DebugOutput& dbgOut = _readNC->dbgOut;
  ScdInterface* scdi = _readNC->scdi;
  ScdParData& parData = _readNC->parData;

  Range tmp_range;
  ScdBox* scd_box;

  ErrorCode rval = scdi->construct_box(HomCoord(lDims[0], lDims[1], lDims[2], 1), HomCoord(lDims[3], lDims[4], lDims[5], 1), 
                                       NULL, 0, scd_box, locallyPeriodic, &parData, true);MB_CHK_SET_ERR(rval, "Trouble creating scd vertex sequence");

  // Add verts to tmp_range first, so we can duplicate global ids in vertex ids
  tmp_range.insert(scd_box->start_vertex(), scd_box->start_vertex() + scd_box->num_vertices() - 1);

  if (mpFileIdTag) {
    int count;
    void* data;
    rval = mbImpl->tag_iterate(*mpFileIdTag, tmp_range.begin(), tmp_range.end(), count, data);MB_CHK_SET_ERR(rval, "Failed to iterate file ID tag on local vertices");
    assert(count == scd_box->num_vertices());
    int* fid_data = (int*) data;
    rval = mbImpl->tag_iterate(mGlobalIdTag, tmp_range.begin(), tmp_range.end(), count, data);MB_CHK_SET_ERR(rval, "Failed to iterate global ID tag on local vertices");
    assert(count == scd_box->num_vertices());
    int* gid_data = (int*) data;
    for (int i = 0; i < count; i++)
      fid_data[i] = gid_data[i];
  }

  // Then add box set and elements to the range, then to the file set
  tmp_range.insert(scd_box->start_element(), scd_box->start_element() + scd_box->num_elements() - 1);
  tmp_range.insert(scd_box->box_set());
  rval = mbImpl->add_entities(_fileSet, tmp_range);MB_CHK_SET_ERR(rval, "Couldn't add new vertices to current file set");

  dbgOut.tprintf(1, "scdbox %d quads, %d vertices\n", scd_box->num_elements(), scd_box->num_vertices());

  // Set the vertex coordinates
  double *xc, *yc, *zc;
  rval = scd_box->get_coordinate_arrays(xc, yc, zc);MB_CHK_SET_ERR(rval, "Couldn't get vertex coordinate arrays");

  int i, j, k, il, jl, kl;
  int dil = lDims[3] - lDims[0] + 1;
  int djl = lDims[4] - lDims[1] + 1;
  assert(dil == (int)ilVals.size() && djl == (int)jlVals.size() &&
      (-1 == lDims[2] || lDims[5] - lDims[2] + 1 == (int)levVals.size()));

  for (kl = lDims[2]; kl <= lDims[5]; kl++) {
    k = kl - lDims[2];
    for (jl = lDims[1]; jl <= lDims[4]; jl++) {
      j = jl - lDims[1];
      for (il = lDims[0]; il <= lDims[3]; il++) {
        i = il - lDims[0];
        unsigned int pos = i + j * dil + k * dil * djl;
        xc[pos] = ilVals[i];
        yc[pos] = jlVals[j];
        zc[pos] = (-1 == lDims[2] ? 0.0 : levVals[k]);
      }
    }
  }

#ifndef NDEBUG
  int num_verts = (lDims[3] - lDims[0] + 1) * (lDims[4] - lDims[1] + 1) * (-1 == lDims[2] ? 1 : lDims[5] - lDims[2] + 1);
  std::vector<int> gids(num_verts);
  Range verts(scd_box->start_vertex(), scd_box->start_vertex() + scd_box->num_vertices() - 1);
  rval = mbImpl->tag_get_data(mGlobalIdTag, verts, &gids[0]);MB_CHK_SET_ERR(rval, "Trouble getting local gid values of vertices");
  int vmin = *(std::min_element(gids.begin(), gids.end())), vmax = *(std::max_element(gids.begin(), gids.end()));
  dbgOut.tprintf(1, "Vertex gids %d-%d\n", vmin, vmax);
#endif

  // Add elements to the range passed in
  faces.insert(scd_box->start_element(), scd_box->start_element() + scd_box->num_elements() - 1);

  if (2 <= dbgOut.get_verbosity()) {
    assert(scd_box->boundary_complete());
    EntityHandle dum_ent = scd_box->start_element();
    rval = mbImpl->list_entities(&dum_ent, 1);MB_CHK_SET_ERR(rval, "Trouble listing first hex");

    std::vector<EntityHandle> connect;
    rval = mbImpl->get_connectivity(&dum_ent, 1, connect);MB_CHK_SET_ERR(rval, "Trouble getting connectivity");

    rval = mbImpl->list_entities(&connect[0], connect.size());MB_CHK_SET_ERR(rval, "Trouble listing element connectivity");
  }

  Range edges;
  mbImpl->get_adjacencies(faces, 1, true, edges, Interface::UNION);

  // Create COORDS tag for quads
  rval = create_quad_coordinate_tag();MB_CHK_SET_ERR(rval, "Trouble creating COORDS tag for quads");

  return MB_SUCCESS;
}

ErrorCode ScdNCHelper::read_variables(std::vector<std::string>& var_names, std::vector<int>& tstep_nums)
{
  std::vector<ReadNC::VarData> vdatas;
  std::vector<ReadNC::VarData> vsetdatas;

  ErrorCode rval = read_variables_setup(var_names, tstep_nums, vdatas, vsetdatas);MB_CHK_SET_ERR(rval, "Trouble setting up to read variables");

  if (!vsetdatas.empty()) {
    rval = read_variables_to_set(vsetdatas, tstep_nums);MB_CHK_SET_ERR(rval, "Trouble reading variables to set");
  }

  if (!vdatas.empty()) {
    rval = read_scd_variables_to_nonset(vdatas, tstep_nums);MB_CHK_SET_ERR(rval, "Trouble reading variables to verts/edges/faces");
  }

  return MB_SUCCESS;
}

ErrorCode ScdNCHelper::read_scd_variables_to_nonset_allocate(std::vector<ReadNC::VarData>& vdatas, std::vector<int>& tstep_nums)
{
  Interface*& mbImpl = _readNC->mbImpl;
  std::vector<int>& dimLens = _readNC->dimLens;
  DebugOutput& dbgOut = _readNC->dbgOut;

  Range* range = NULL;

  // Get vertices
  Range verts;
  ErrorCode rval = mbImpl->get_entities_by_dimension(_fileSet, 0, verts);MB_CHK_SET_ERR(rval, "Trouble getting vertices in current file set");
  assert("Should only have a single vertex subrange, since they were read in one shot" &&
      verts.psize() == 1);

  Range edges;
  rval = mbImpl->get_entities_by_dimension(_fileSet, 1, edges);MB_CHK_SET_ERR(rval, "Trouble getting edges in current file set");

  // Get faces
  Range faces;
  rval = mbImpl->get_entities_by_dimension(_fileSet, 2, faces);MB_CHK_SET_ERR(rval, "Trouble getting faces in current file set");
  assert("Should only have a single face subrange, since they were read in one shot" &&
      faces.psize() == 1);

#ifdef MOAB_HAVE_MPI
  moab::Range faces_owned;
  bool& isParallel = _readNC->isParallel;
  if (isParallel) {
    ParallelComm*& myPcomm = _readNC->myPcomm;
    rval = myPcomm->filter_pstatus(faces, PSTATUS_NOT_OWNED, PSTATUS_NOT, -1, &faces_owned);MB_CHK_SET_ERR(rval, "Trouble getting owned faces in current file set");
  }
  else
    faces_owned = faces; // Not running in parallel, but still with MPI
#endif

  for (unsigned int i = 0; i < vdatas.size(); i++) {
    // Support non-set variables with 4 dimensions like (time, lev, lat, lon)
    assert(4 == vdatas[i].varDims.size());

    // For a non-set variable, time should be the first dimension
    assert(tDim == vdatas[i].varDims[0]);

    // Set up readStarts and readCounts
    vdatas[i].readStarts.resize(4);
    vdatas[i].readCounts.resize(4);

    // First: time
    vdatas[i].readStarts[0] = 0; // This value is timestep dependent, will be set later
    vdatas[i].readCounts[0] = 1;

    // Next: lev
    vdatas[i].readStarts[1] = 0;
    vdatas[i].readCounts[1] = vdatas[i].numLev;

    // Finally: lat (or slat) and lon (or slon)
    switch (vdatas[i].entLoc) {
      case ReadNC::ENTLOCVERT:
        // Vertices
        vdatas[i].readStarts[2] = lDims[1];
        vdatas[i].readCounts[2] = lDims[4] - lDims[1] + 1;
        vdatas[i].readStarts[3] = lDims[0];
        vdatas[i].readCounts[3] = lDims[3] - lDims[0] + 1;
        range = &verts;
        break;
      case ReadNC::ENTLOCNSEDGE:
      case ReadNC::ENTLOCEWEDGE:
      case ReadNC::ENTLOCEDGE:
        // Not implemented yet, set a global error
        MB_SET_GLB_ERR(MB_NOT_IMPLEMENTED, "Reading edge data is not implemented yet");
      case ReadNC::ENTLOCFACE:
        // Faces
        vdatas[i].readStarts[2] = lCDims[1];
        vdatas[i].readCounts[2] = lCDims[4] - lCDims[1] + 1;
        vdatas[i].readStarts[3] = lCDims[0];
        vdatas[i].readCounts[3] = lCDims[3] - lCDims[0] + 1;
#ifdef MOAB_HAVE_MPI
        range = &faces_owned;
#else
        range = &faces;
#endif
        break;
      default:
        MB_SET_ERR(MB_FAILURE, "Unexpected entity location type for variable " << vdatas[i].varName);
    }

    for (unsigned int t = 0; t < tstep_nums.size(); t++) {
      dbgOut.tprintf(2, "Reading variable %s, time step %d\n", vdatas[i].varName.c_str(), tstep_nums[t]);

      if (tstep_nums[t] >= dimLens[tDim]) {
        MB_SET_ERR(MB_INDEX_OUT_OF_RANGE, "Wrong value for timestep number " << tstep_nums[t]);
      }

      // Get the tag to read into
      if (!vdatas[i].varTags[t]) {
        rval = get_tag_to_nonset(vdatas[i], tstep_nums[t], vdatas[i].varTags[t], vdatas[i].numLev);MB_CHK_SET_ERR(rval, "Trouble getting tag to non-set variable " << vdatas[i].varName);
      }

      // Get ptr to tag space
      void* data;
      int count;
      rval = mbImpl->tag_iterate(vdatas[i].varTags[t], range->begin(), range->end(), count, data);MB_CHK_SET_ERR(rval, "Failed to iterate tag for non-set variable " << vdatas[i].varName);
      assert((unsigned)count == range->size());
      vdatas[i].varDatas[t] = data;
    }

    // Get variable size
    vdatas[i].sz = 1;
    for (std::size_t idx = 0; idx != vdatas[i].readCounts.size(); idx++)
      vdatas[i].sz *= vdatas[i].readCounts[idx];
  }

  return rval;
}

ErrorCode ScdNCHelper::read_scd_variables_to_nonset(std::vector<ReadNC::VarData>& vdatas, std::vector<int>& tstep_nums)
{
  DebugOutput& dbgOut = _readNC->dbgOut;

  ErrorCode rval = read_scd_variables_to_nonset_allocate(vdatas, tstep_nums);MB_CHK_SET_ERR(rval, "Trouble allocating space to read non-set variables");

  // Finally, read into that space
  int success;
  for (unsigned int i = 0; i < vdatas.size(); i++) {
    std::size_t sz = vdatas[i].sz;

    // A typical supported variable: float T(time, lev, lat, lon)
    // For tag values, need transpose (lev, lat, lon) to (lat, lon, lev)
    size_t ni = vdatas[i].readCounts[3]; // lon or slon
    size_t nj = vdatas[i].readCounts[2]; // lat or slat
    size_t nk = vdatas[i].readCounts[1]; // lev

    for (unsigned int t = 0; t < tstep_nums.size(); t++) {
      // Tag data for this timestep
      void* data = vdatas[i].varDatas[t];

      // Set readStart for each timestep along time dimension
      vdatas[i].readStarts[0] = tstep_nums[t];

      switch (vdatas[i].varDataType) {
        case NC_BYTE:
        case NC_CHAR: {
          std::vector<char> tmpchardata(sz);
          success = NCFUNCAG(_vara_text)(_fileId, vdatas[i].varId, &vdatas[i].readStarts[0], &vdatas[i].readCounts[0],
                                        &tmpchardata[0]);
          if (success)
            MB_SET_ERR(MB_FAILURE, "Failed to read byte/char data for variable " << vdatas[i].varName);
          if (vdatas[i].numLev > 1)
            // Transpose (lev, lat, lon) to (lat, lon, lev)
            kji_to_jik(ni, nj, nk, data, &tmpchardata[0]);
          else {
            for (std::size_t idx = 0; idx != tmpchardata.size(); idx++)
              ((char*) data)[idx] = tmpchardata[idx];
          }
          break;
        }
        case NC_SHORT:
        case NC_INT: {
          std::vector<int> tmpintdata(sz);
          success = NCFUNCAG(_vara_int)(_fileId, vdatas[i].varId, &vdatas[i].readStarts[0], &vdatas[i].readCounts[0],
                                        &tmpintdata[0]);
          if (success)
            MB_SET_ERR(MB_FAILURE, "Failed to read short/int data for variable " << vdatas[i].varName);
          if (vdatas[i].numLev > 1)
            // Transpose (lev, lat, lon) to (lat, lon, lev)
            kji_to_jik(ni, nj, nk, data, &tmpintdata[0]);
          else {
            for (std::size_t idx = 0; idx != tmpintdata.size(); idx++)
              ((int*) data)[idx] = tmpintdata[idx];
          }
          break;
        }
        case NC_FLOAT:
        case NC_DOUBLE: {
          std::vector<double> tmpdoubledata(sz);
          success = NCFUNCAG(_vara_double)(_fileId, vdatas[i].varId, &vdatas[i].readStarts[0], &vdatas[i].readCounts[0],
                                          &tmpdoubledata[0]);
          if (success)
            MB_SET_ERR(MB_FAILURE, "Failed to read float/double data for variable " << vdatas[i].varName);
          if (vdatas[i].numLev > 1)
            // Transpose (lev, lat, lon) to (lat, lon, lev)
            kji_to_jik(ni, nj, nk, data, &tmpdoubledata[0]);
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

ErrorCode ScdNCHelper::create_quad_coordinate_tag() {
  Interface*& mbImpl = _readNC->mbImpl;

  Range ents;
  ErrorCode rval = mbImpl->get_entities_by_type(_fileSet, moab::MBQUAD, ents);MB_CHK_SET_ERR(rval, "Trouble getting quads");

  std::size_t numOwnedEnts = 0;
#ifdef MOAB_HAVE_MPI
  Range ents_owned;
  bool& isParallel = _readNC->isParallel;
  if (isParallel) {
    ParallelComm*& myPcomm = _readNC->myPcomm;
    rval = myPcomm->filter_pstatus(ents, PSTATUS_NOT_OWNED, PSTATUS_NOT, -1, &ents_owned);MB_CHK_SET_ERR(rval, "Trouble getting owned quads");
    numOwnedEnts = ents_owned.size();
  }
  else {
    numOwnedEnts = ents.size();
    ents_owned = ents;
  }
#else
  numOwnedEnts = ents.size();
#endif

  if (numOwnedEnts == 0)
    return MB_SUCCESS;

  assert(numOwnedEnts == ilCVals.size() * jlCVals.size());
  std::vector<double> coords(numOwnedEnts * 3);
  std::size_t pos = 0;
  for (std::size_t j = 0; j != jlCVals.size(); ++j) {
    for (std::size_t i = 0; i != ilCVals.size(); ++i) {
      pos = j * ilCVals.size() * 3 + i * 3;
      coords[pos] = ilCVals[i];
      coords[pos + 1] = jlCVals[j];
      coords[pos + 2] = 0.0;
    }
  }
  std::string tag_name = "COORDS";
  Tag tagh = 0;
  rval = mbImpl->tag_get_handle(tag_name.c_str(), 3, MB_TYPE_DOUBLE, tagh,
                                MB_TAG_DENSE | MB_TAG_CREAT);MB_CHK_SET_ERR(rval, "Trouble creating COORDS tag");

  void *data;
  int count;
#ifdef MOAB_HAVE_MPI
  rval = mbImpl->tag_iterate(tagh, ents_owned.begin(), ents_owned.end(), count, data);MB_CHK_SET_ERR(rval, "Failed to iterate COORDS tag on quads");
#else
  rval = mbImpl->tag_iterate(tagh, ents.begin(), ents.end(), count, data);MB_CHK_SET_ERR(rval, "Failed to iterate COORDS tag on quads");
#endif
  assert(count == (int)numOwnedEnts);
  double* quad_data = (double*) data;
  std::copy(coords.begin(), coords.end(), quad_data);

  return MB_SUCCESS;
}

ErrorCode UcdNCHelper::read_variables(std::vector<std::string>& var_names, std::vector<int>& tstep_nums)
{
  std::vector<ReadNC::VarData> vdatas;
  std::vector<ReadNC::VarData> vsetdatas;

  ErrorCode rval = read_variables_setup(var_names, tstep_nums, vdatas, vsetdatas);MB_CHK_SET_ERR(rval, "Trouble setting up to read variables");

  if (!vsetdatas.empty()) {
    rval = read_variables_to_set(vsetdatas, tstep_nums);MB_CHK_SET_ERR(rval, "Trouble reading variables to set");
  }

  if (!vdatas.empty()) {
#ifdef MOAB_HAVE_PNETCDF
    // With pnetcdf support, we will use async read
    rval = read_ucd_variables_to_nonset_async(vdatas, tstep_nums);MB_CHK_SET_ERR(rval, "Trouble reading variables to verts/edges/faces");
#else
    // Without pnetcdf support, we will use old read
    rval = read_ucd_variables_to_nonset(vdatas, tstep_nums);MB_CHK_SET_ERR(rval, "Trouble reading variables to verts/edges/faces");
#endif
  }

  return MB_SUCCESS;
}

} // namespace moab
