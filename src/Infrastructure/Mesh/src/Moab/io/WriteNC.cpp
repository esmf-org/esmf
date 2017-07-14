#include "WriteNC.hpp"
#include "moab/CN.hpp"
#include "MBTagConventions.hpp"
#include "MBParallelConventions.h"
#include "moab/Interface.hpp"
#include "moab/Range.hpp"
#include "moab/WriteUtilIface.hpp"
#include "moab/FileOptions.hpp"
#include "NCWriteHelper.hpp"

#include <fstream>
#include <map>
#include <set>

#include <iostream>
#include <sstream>

#ifdef WIN32
#ifdef size_t
#undef size_t
#endif
#endif

namespace moab {

WriterIface *WriteNC::factory(Interface* iface)
{
  return new WriteNC(iface);
}

WriteNC::WriteNC(Interface* impl) :
  mbImpl(impl), dbgOut(stderr),
#ifdef MOAB_HAVE_MPI
  myPcomm(NULL),
#endif
  noMesh(false), noVars(false), append(false),
  mGlobalIdTag(0), isParallel(false),
  myHelper(NULL)
{
  assert(impl != NULL);
  impl->query_interface(mWriteIface);
}

WriteNC::~WriteNC()
{
  mbImpl->release_interface(mWriteIface);
  if (myHelper != NULL)
    delete myHelper;
}

//! Writes out a file
ErrorCode WriteNC::write_file(const char* file_name,
                              const bool overwrite,
                              const FileOptions& options,
                              const EntityHandle* file_set,
                              const int num_set,
                              const std::vector<std::string>&,
                              const Tag*,
                              int,
                              int)
{
  ErrorCode rval;
  // See if opts has variable(s) specified
  std::vector<std::string> var_names;
  std::vector<std::string> desired_names;
  std::vector<int> tstep_nums;
  std::vector<double> tstep_vals;

  // Get and cache predefined tag handles
  int dum_val = 0;
  rval = mbImpl->tag_get_handle(GLOBAL_ID_TAG_NAME, 1, MB_TYPE_INTEGER, mGlobalIdTag, MB_TAG_DENSE,
                                &dum_val);MB_CHK_SET_ERR(rval, "Trouble getting global ID tag");

  // num set has to be 1, we will write only one set, the original file set used to load
  if (num_set != 1)
    MB_SET_ERR(MB_FAILURE, "We should write only one set (the file set used to read data into)");

  rval = parse_options(options, var_names, desired_names, tstep_nums, tstep_vals);MB_CHK_SET_ERR(rval, "Trouble parsing option string");

  // Important to create some data that will be used to write the file; dimensions, variables, etc
  // new variables still need to have some way of defining their dimensions
  // maybe it will be passed as write options
  rval = process_conventional_tags(*file_set);MB_CHK_SET_ERR(rval, "Trouble processing conventional tags");

  // Create or append the file
  if (append)
    dbgOut.tprintf(1, "opening file %s for appending \n", file_name);
  else
    dbgOut.tprintf(1, "creating file %s\n", file_name);
  fileName = file_name;
  int success;

  if (append) {
    int omode = NC_WRITE;
#ifdef MOAB_HAVE_PNETCDF
    if (isParallel)
      success = NCFUNC(open)(myPcomm->proc_config().proc_comm(), file_name, omode, MPI_INFO_NULL, &fileId);
    else
      success = NCFUNC(open)(MPI_COMM_SELF, file_name, omode, MPI_INFO_NULL, &fileId);
#else
    // This is a regular netcdf file, open in write mode
    success = NCFUNC(open)(file_name, omode, &fileId);
#endif
    if (success)
      MB_SET_ERR(MB_FAILURE, "Trouble opening file " << file_name << " for appending");
  }
  else { // Case when the file is new, will be overwritten, most likely
    int cmode = overwrite ? NC_CLOBBER : NC_NOCLOBBER;
#ifdef MOAB_HAVE_PNETCDF
    if (isParallel)
      success = NCFUNC(create)(myPcomm->proc_config().proc_comm(), file_name, cmode, MPI_INFO_NULL, &fileId);
    else
      success = NCFUNC(create)(MPI_COMM_SELF, file_name, cmode, MPI_INFO_NULL, &fileId);
#else
    // This is a regular netcdf file
    success = NCFUNC(create)(file_name, cmode, &fileId);
#endif
    if (success)
      MB_SET_ERR(MB_FAILURE, "Trouble creating file " << file_name << " for writing");
  }

  if (NULL != myHelper)
    delete myHelper;

  // Get appropriate helper instance for WriteNC class based on some info in the file set
  myHelper = NCWriteHelper::get_nc_helper(this, fileId, options, *file_set);
  if (NULL == myHelper) {
    MB_SET_ERR(MB_FAILURE, "Failed to get NCWriteHelper class instance");
  }

  rval = myHelper->collect_mesh_info();MB_CHK_SET_ERR(rval, "Trouble collecting mesh information");

  rval = myHelper->collect_variable_data(var_names, tstep_nums);MB_CHK_SET_ERR(rval, "Trouble collecting variable data");

  rval = myHelper->init_file(var_names, desired_names, append);MB_CHK_SET_ERR(rval, "Trouble initializing file");

  rval = myHelper->write_values(var_names, tstep_nums);MB_CHK_SET_ERR(rval, "Trouble writing values to file");

  success = NCFUNC(close)(fileId);
  if (success)
    MB_SET_ERR(MB_FAILURE, "Trouble closing file");

  return MB_SUCCESS;
}

ErrorCode WriteNC::parse_options(const FileOptions& opts, std::vector<std::string>& var_names,
                                std::vector<std::string>& desired_names, std::vector<int>& tstep_nums,
                                std::vector<double>& tstep_vals)
{
  int tmpval;
  if (MB_SUCCESS == opts.get_int_option("DEBUG_IO", 1, tmpval)) {
    dbgOut.set_verbosity(tmpval);
    dbgOut.set_prefix("NCWrite");
  }

  ErrorCode rval = opts.get_strs_option("VARIABLE", var_names);
  if (MB_TYPE_OUT_OF_RANGE == rval)
    noVars = true;
  else
    noVars = false;

  rval = opts.get_strs_option("RENAME", desired_names);
  if (MB_ENTITY_NOT_FOUND == rval) {
    if (!noVars) {
      desired_names.resize(var_names.size());
      std::copy(var_names.begin(), var_names.end(), desired_names.begin());
    }
  }
  // Either way
  assert(desired_names.size() == var_names.size());

  opts.get_ints_option("TIMESTEP", tstep_nums);
  opts.get_reals_option("TIMEVAL", tstep_vals);
  rval = opts.get_null_option("NOMESH");
  if (MB_SUCCESS == rval)
    noMesh = true;

  rval = opts.get_null_option("APPEND");
  if (MB_SUCCESS == rval)
    append = true;

  if (2 <= dbgOut.get_verbosity()) {
    if (!var_names.empty()) {
      std::cerr << "Variables requested: ";
      for (unsigned int i = 0; i < var_names.size(); i++)
        std::cerr << var_names[i];
      std::cerr << std::endl;
    }
    if (!tstep_nums.empty()) {
      std::cerr << "Timesteps requested: ";
      for (unsigned int i = 0; i < tstep_nums.size(); i++)
        std::cerr << tstep_nums[i];
      std::cerr << std::endl;
    }
    if (!tstep_vals.empty()) {
      std::cerr << "Time vals requested: ";
      for (unsigned int i = 0; i < tstep_vals.size(); i++)
        std::cerr << tstep_vals[i];
      std::cerr << std::endl;
    }
  }

// FIXME: copied from ReadNC, may need revise
#ifdef MOAB_HAVE_MPI
  isParallel = (opts.match_option("PARALLEL", "WRITE_PART") != MB_ENTITY_NOT_FOUND);

  if (!isParallel)
  // Return success here, since rval still has _NOT_FOUND from not finding option
  // in this case, myPcomm will be NULL, so it can never be used; always check for isParallel
  // before any use for myPcomm
    return MB_SUCCESS;

  int pcomm_no = 0;
  rval = opts.get_int_option("PARALLEL_COMM", pcomm_no);
  if (MB_TYPE_OUT_OF_RANGE == rval) {
    MB_SET_ERR(rval, "Invalid value for PARALLEL_COMM option");
  }

  myPcomm = ParallelComm::get_pcomm(mbImpl, pcomm_no);
  if (0 == myPcomm) {
    myPcomm = new ParallelComm(mbImpl, MPI_COMM_WORLD);
  }

#ifndef MOAB_HAVE_PNETCDF
  const int procs = myPcomm->proc_config().proc_size();
  if (procs > 1) {
    MB_SET_ERR(MB_UNSUPPORTED_OPERATION, "Attempt to launch NC writer in parallel without pnetcdf support");
  }
#endif

  const int rank = myPcomm->proc_config().proc_rank();
  dbgOut.set_rank(rank);
#endif

  return MB_SUCCESS;
}

// This is the inverse process to create conventional tags
// Will look at <pargal_source>/src/core/fileinfo.cpp, init dim, vars, atts
ErrorCode WriteNC::process_conventional_tags(EntityHandle fileSet)
{
  ErrorCode rval;

  // Start copy
  Tag dimNamesTag = 0;
  std::string tag_name = "__DIM_NAMES";
  const void* data = NULL;
  int dimNamesSz = 0;
  rval = mbImpl->tag_get_handle(tag_name.c_str(), 0, MB_TYPE_OPAQUE, dimNamesTag,
                                MB_TAG_ANY);MB_CHK_SET_ERR(rval, "Trouble getting conventional tag " << tag_name);
  rval = mbImpl->tag_get_by_ptr(dimNamesTag, &fileSet, 1, &data, &dimNamesSz);MB_CHK_SET_ERR(rval, "Trouble getting data of conventional tag " << tag_name);
  const char* p = static_cast<const char*>(data);
  dbgOut.tprintf(1, "__DIM_NAMES tag has string length %d\n", dimNamesSz);

  std::size_t start = 0;

  Tag dimLensTag = 0;
  tag_name = "__DIM_LENS";
  data = NULL;
  int dimLensSz = 0;
  rval = mbImpl->tag_get_handle(tag_name.c_str(), 0, MB_TYPE_INTEGER, dimLensTag,
                                MB_TAG_ANY);MB_CHK_SET_ERR(rval, "Trouble getting conventional tag " << tag_name);
  rval = mbImpl->tag_get_by_ptr(dimLensTag, &fileSet, 1, &data, &dimLensSz);MB_CHK_SET_ERR(rval, "Trouble getting data of conventional tag " << tag_name);
  const int* int_p = static_cast<const int*>(data);
  dbgOut.tprintf(1, "__DIM_LENS tag has %d values\n", dimLensSz);

  int idxDim = 0;
  // Dim names are separated by '\0' in the string of __DIM_NAMES tag
  for (std::size_t i = 0; i != static_cast<std::size_t>(dimNamesSz); i++) {
    if (p[i] == '\0') {
      std::string dim_name(&p[start], i - start);
      int len = int_p[idxDim];
      dimNames.push_back(dim_name);
      dimLens.push_back(len);
      dbgOut.tprintf(2, "Dimension %s has length %d\n", dim_name.c_str(), len);
      // FIXME: Need info from moab to set unlimited dimension
      // Currently assume each file has the same number of time dimensions
      /*if ((dim_name == "time") || (dim_name == "Time"))
        insert(dim_name, *(new pcdim(dim_name, len * m_file_names.size(), true)));
      else
        insert(dim_name, *(new pcdim(dim_name, len)));*/
      start = i + 1;
      idxDim++;
    }
  }

  Tag meshTypeTag = 0;
  tag_name = "__MESH_TYPE";
  data = NULL;
  int meshTypeSz = 0;
  rval = mbImpl->tag_get_handle(tag_name.c_str(), 0, MB_TYPE_OPAQUE, meshTypeTag,
                                MB_TAG_ANY);MB_CHK_SET_ERR(rval, "Trouble getting conventional tag " << tag_name);
  rval = mbImpl->tag_get_by_ptr(meshTypeTag, &fileSet, 1, &data, &meshTypeSz);MB_CHK_SET_ERR(rval, "Trouble getting data of conventional tag " << tag_name);
  p = static_cast<const char*>(data);
  grid_type = std::string(&p[0], meshTypeSz);
  dbgOut.tprintf(2, "Mesh type: %s\n", grid_type.c_str());

  // Read <__VAR_NAMES_LOCATIONS> tag
  Tag varNamesLocsTag = 0;
  tag_name = "__VAR_NAMES_LOCATIONS";
  data = NULL;
  int varNamesLocsSz = 0;
  rval = mbImpl->tag_get_handle(tag_name.c_str(), 0, MB_TYPE_INTEGER, varNamesLocsTag,
                                MB_TAG_ANY);MB_CHK_SET_ERR(rval, "Trouble getting conventional tag " << tag_name);
  rval = mbImpl->tag_get_by_ptr(varNamesLocsTag, &fileSet, 1, &data, &varNamesLocsSz);MB_CHK_SET_ERR(rval, "Trouble getting data of conventional tag " << tag_name);
  int_p = static_cast<const int*>(data);
  std::vector<int> varNamesLocs(varNamesLocsSz);
  std::copy(int_p, int_p + varNamesLocsSz, varNamesLocs.begin());

  Tag varNamesTag = 0;
  tag_name = "__VAR_NAMES";
  rval = mbImpl->tag_get_handle(tag_name.c_str(), 0, MB_TYPE_OPAQUE, varNamesTag,
                                MB_TAG_ANY);MB_CHK_SET_ERR(rval, "Trouble getting conventional tag " << tag_name);
  data = NULL;
  int varNamesSz = 0;
  rval = mbImpl->tag_get_by_ptr(varNamesTag, &fileSet, 1, &data, &varNamesSz);MB_CHK_SET_ERR(rval, "Trouble getting data of conventional tag " << tag_name);
  dbgOut.tprintf(2, "__VAR_NAMES tag has string length %d\n", varNamesSz);
  p = static_cast<const char*>(data);

  start = 0;
  int idxVar = 0;
  int sz;
  // Var names are separated by '\0' in the string of __VAR_NAMES tag
  for (std::size_t i = 0; i != static_cast<std::size_t>(varNamesSz); i++) {
    if (p[i] == '\0') {
      std::string var_name(&p[start], i - start);

      dbgOut.tprintf(2, "var name: %s index %d \n", var_name.c_str(), idxVar);
      // Process var name:
      // This will create/initiate map; we will populate variableDataStruct with info about dims, tags, etc
      // reference & is important; otherwise variableDataStruct will go out of scope, and deleted :(
      VarData& variableDataStruct = varInfo[var_name];
      variableDataStruct.varName = var_name;
      variableDataStruct.entLoc = varNamesLocs[idxVar];

      dbgOut.tprintf(2, "at var name %s varInfo size %d \n", var_name.c_str(), (int)varInfo.size());

      sz = 0;
      Tag dims_tag = 0;
      std::string dim_names = "__" + var_name + "_DIMS";
      rval = mbImpl->tag_get_handle(dim_names.c_str(), 0, MB_TYPE_OPAQUE, dims_tag, MB_TAG_ANY);
      if (MB_SUCCESS != rval) {
        if (MB_TAG_NOT_FOUND == rval) {
          dbgOut.tprintf(2, "tag : %s not found, continue \n", dim_names.c_str());
          start = i + 1;
          idxVar++;
          continue;
        }
        MB_SET_ERR(rval, "Trouble getting conventional tag " << dim_names);
      }
      rval = mbImpl->tag_get_length(dims_tag, sz);MB_CHK_SET_ERR(rval, "Trouble getting size of dimensions for variable " << var_name);
      sz /= sizeof(Tag); // The type is MB_TYPE_OPAQUE, but it is a list of tags, so we need to divide by the size of Tag
      // sz is used for number of dimension tags in this list
      dbgOut.tprintf(2, "var name: %s has %d dimensions \n", var_name.c_str(), sz);

      variableDataStruct.varDims.resize(sz);
      const void* ptr = NULL;
      rval = mbImpl->tag_get_by_ptr(dims_tag, &fileSet, 1, &ptr);

      const Tag* ptags = static_cast<const moab::Tag*>(ptr);
      for (std::size_t j = 0; j != static_cast<std::size_t>(sz); j++) {
        std::string dim_name;
        rval = mbImpl->tag_get_name(ptags[j], dim_name);MB_CHK_SET_ERR(rval, "Trouble getting dimension of variable " << var_name);
        dbgOut.tprintf(2, "var name: %s has %s as dimension \n", var_name.c_str(), dim_name.c_str());
        std::vector<std::string>::iterator vit = std::find(dimNames.begin(), dimNames.end(), dim_name);
        if (vit == dimNames.end())
          MB_SET_ERR(MB_FAILURE, "Dimension " << dim_name << " not found for variable " << var_name);
        variableDataStruct.varDims[j] = (int)(vit - dimNames.begin()); // Will be used for writing
        // This will have to change to actual file dimension, for writing
      }

      // Attributes for this variable
      std::stringstream ssTagName;
      ssTagName << "__" << var_name << "_ATTRIBS";
      tag_name = ssTagName.str();
      Tag varAttTag = 0;
      rval = mbImpl->tag_get_handle(tag_name.c_str(), 0, MB_TYPE_OPAQUE, varAttTag,
                                    MB_TAG_SPARSE | MB_TAG_VARLEN);MB_CHK_SET_ERR(rval, "Trouble getting conventional tag " << tag_name);
      const void* varAttPtr = NULL;
      int varAttSz = 0;
      rval = mbImpl->tag_get_by_ptr(varAttTag, &fileSet, 1, &varAttPtr, &varAttSz);MB_CHK_SET_ERR(rval, "Trouble getting data of conventional tag " << tag_name);
      if (MB_SUCCESS == rval)
        dbgOut.tprintf(2, "Tag retrieved for variable %s\n", tag_name.c_str());

      std::string attribString((char*)varAttPtr, (char*)varAttPtr + varAttSz);
      if (attribString == "NO_ATTRIBS") {
        // This variable has no attributes
        variableDataStruct.numAtts = 0;
      }
      else if (attribString == "DUMMY_VAR") {
        // This variable is a dummy coordinate variable
        variableDataStruct.numAtts = 0;
        dummyVarNames.insert(variableDataStruct.varName);
      }
      else {
        ssTagName << "_LEN";
        tag_name = ssTagName.str();
        Tag varAttLenTag = 0;
        rval = mbImpl->tag_get_handle(tag_name.c_str(), 0, MB_TYPE_INTEGER, varAttLenTag,
                                      MB_TAG_ANY);MB_CHK_SET_ERR(rval, "Trouble getting conventional tag " << tag_name);
        int varAttLenSz = 0;
        rval = mbImpl->tag_get_length(varAttLenTag, varAttLenSz);MB_CHK_SET_ERR(rval, "Trouble getting length of conventional tag " << tag_name);
        std::vector<int> varAttLen(varAttLenSz);
        rval = mbImpl->tag_get_data(varAttLenTag, &fileSet, 1, &varAttLen[0]);MB_CHK_SET_ERR(rval, "Trouble getting data of conventional tag " << tag_name);

        rval = process_concatenated_attribute(varAttPtr, varAttSz, varAttLen, variableDataStruct.varAtts);MB_CHK_SET_ERR(rval, "Trouble processing attributes of variable " << var_name);

        if (MB_SUCCESS == rval)
          dbgOut.tprintf(2, "Tag metadata for variable %s\n", tag_name.c_str());
      }
      // End attribute

      start = i + 1;
      idxVar++;
    } // if (p[i] == '\0')
  }

  // Global attributes
  tag_name = "__GLOBAL_ATTRIBS";
  Tag globalAttTag = 0;
  rval = mbImpl->tag_get_handle(tag_name.c_str(), 0, MB_TYPE_OPAQUE, globalAttTag,
                                MB_TAG_SPARSE | MB_TAG_VARLEN);MB_CHK_SET_ERR(rval, "Trouble getting conventional tag " << tag_name);
  std::vector<int> gattLen;

  const void* gattptr = NULL;
  int globalAttSz = 0;
  rval = mbImpl->tag_get_by_ptr(globalAttTag, &fileSet, 1, &gattptr, &globalAttSz);MB_CHK_SET_ERR(rval, "Trouble getting data of conventional tag " << tag_name);

  if (MB_SUCCESS == rval)
    dbgOut.tprintf(2, "Tag value retrieved for %s size %d\n", tag_name.c_str(), globalAttSz);

  // <__GLOBAL_ATTRIBS_LEN>
  tag_name = "__GLOBAL_ATTRIBS_LEN";
  Tag globalAttLenTag = 0;

  rval = mbImpl->tag_get_handle(tag_name.c_str(), 0, MB_TYPE_INTEGER, globalAttLenTag,
                                MB_TAG_ANY);MB_CHK_SET_ERR(rval, "Trouble getting conventional tag " << tag_name);
  int sizeGAtt = 0;
  rval = mbImpl->tag_get_length(globalAttLenTag, sizeGAtt);MB_CHK_SET_ERR(rval, "Trouble getting length of conventional tag " << tag_name);
  gattLen.resize(sizeGAtt);
  rval = mbImpl->tag_get_data(globalAttLenTag, &fileSet, 1, &gattLen[0]);MB_CHK_SET_ERR(rval, "Trouble getting data of conventional tag " << tag_name);
  if (MB_SUCCESS == rval)
    dbgOut.tprintf(2, "Tag retrieved for variable %s\n", tag_name.c_str());

  rval = process_concatenated_attribute(gattptr, globalAttSz, gattLen, globalAtts);MB_CHK_SET_ERR(rval, "Trouble processing global attributes");

  return MB_SUCCESS;
}

// Reverse process from create_attrib_string
ErrorCode WriteNC::process_concatenated_attribute(const void* attPtr, int attSz,
                                                  std::vector<int>& attLen,
                                                  std::map<std::string, AttData>& attributes)
{
  std::size_t start = 0;
  std::size_t att_counter = 0;
  std::string concatString((char*)attPtr, (char*)attPtr + attSz);

  for (std::size_t i = 0; i != (size_t)attSz; i++) {
    if (concatString[i] == '\0') {
      std::string att_name(&concatString[start], i - start);
      start = i + 1;
      while (concatString[i] != ';')
        ++i;
      std::string data_type(&concatString[start], i - start);
      ++i;
      start = i;
      i = attLen[att_counter];
      if (concatString[i] != ';')
        MB_SET_ERR(MB_FAILURE, "Error parsing attributes");

      std::string data_val(&concatString[start], i - start);
      start = i + 1;

      AttData& attrib = attributes[att_name];
      attrib.attValue = data_val;
      attrib.attLen = data_val.size();

      if (data_type == "char")
        attrib.attDataType = NC_CHAR;
      else if (data_type == "double")
        attrib.attDataType = NC_DOUBLE;
      else if (data_type == "float")
        attrib.attDataType = NC_FLOAT;
      else if (data_type == "int")
        attrib.attDataType = NC_INT;
      else if (data_type == "short")
        attrib.attDataType = NC_SHORT;

      ++att_counter;
      dbgOut.tprintf(2, "       Process attribute %s with value %s \n", att_name.c_str(), data_val.c_str());
    }
  }

  return MB_SUCCESS;
}

} // namespace moab
