#include "ReadGCRM.hpp"

#include <algorithm>
#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <cmath>
#include <cstdlib>
#include <iostream>
#include <sstream>
#include <map>
#include <dirent.h>

#include "moab/Core.hpp"
#include "moab/ReaderIface.hpp"
#include "moab/ReadUtilIface.hpp"
#include "MBTagConventions.hpp"
#include "FileOptions.hpp"

#define ERRORR(rval, str) \
    if (MB_SUCCESS != rval) {readMeshIface->report_error("%s", str); return rval;}

#define ERRORS(err, str) \
    if (err) {readMeshIface->report_error("%s", str); return MB_FAILURE;}

namespace moab 
{
    
ReaderIface* ReadGCRM::factory(Interface* iface) {
  return new ReadGCRM(iface);
}

ReadGCRM::ReadGCRM(Interface* impl) :
  CPU_WORD_SIZE(-1), IO_WORD_SIZE(-1), mbImpl(impl), fileId(-1), tMin(-1), tMax(-1), tDim(-1), 
      numUnLim(-1), mCurrentMeshHandle(0), startVertex(0), startElem(0), mGlobalIdTag(0), max_line_length(-1),
      max_str_length(-1), vertexOffset(0), dbgOut(stderr), partMethod(-1), isParallel(false)

#ifdef USE_MPI
, myPcomm(NULL)
#endif
{
  assert(impl != NULL);
  impl->query_interface(readMeshIface);
}

ReadGCRM::~ReadGCRM() 
{
  if (readMeshIface) {
    mbImpl->release_interface(readMeshIface);
    readMeshIface = 0;
  }
}

void ReadGCRM::reset() {
  CPU_WORD_SIZE = -1;
  IO_WORD_SIZE = -1;
  fileId = -1;
  tMin = tMax = -1;
  numUnLim = -1;
  mCurrentMeshHandle = 0;
  startVertex = startElem = 0;
  mGlobalIdTag = 0;
  max_line_length = -1;
  max_str_length = -1;
  vertexOffset = 0;
  dbgOut = stderr;
  mCurrentMeshHandle = 0;
  vertexOffset = 0;

#ifdef USE_MPI
  myPcomm = NULL;
#endif
}

ErrorCode ReadGCRM::load_file( const char* /* file_name */,
                               const EntityHandle* /* file_set*/,
                               const FileOptions& /* opts */,
                               const SubsetList* /* subset_list */,
                               const Tag* /*file_id_tag*/) 
{
    // guarantee failure for now
  return MB_FAILURE;
 
  /* VSM: Temporarily commenting out all the relevant code until the above guaranteed failure is fixed */
  /*  
  if (subset_list) {
      // see src/moab/ReaderIface.hpp, definition of SubsetList struct; this basically specifies
      // an integer tag and tag values for sets to read on this proc, or a part number and total # parts
      // for reading a trivial partition of entities
  }

    // save filename to member variable so we don't need to pass as an argument
    // to called functions
  fileName = file_name;

    // process options; see src/FileOptions.hpp for API for FileOptions class, and doc/metadata_info.doc for
    // a description of various options used by some of the readers in MOAB
  ErrorCode result = process_options(opts);
  if (MB_SUCCESS != result) {
    readMeshIface->report_error( "%s: problem reading options\n", fileName);
    return result;
  }

    // Open file; filePtr is member of ReadGCRM, change to whatever mechanism is used to identify file
  FILE* filePtr = fopen( fileName, "r" );
  if (!filePtr)
  {
    readMeshIface->report_error( "%s: fopen returned error.\n", fileName);
    return MB_FILE_DOES_NOT_EXIST;
  }
  
    // read number of verts, elements, sets
  long num_verts = 0, num_elems = 0, num_sets = 0;

    // read_ents keeps a running set of entities read from this file, including vertices, elements, and sets;
    // these will get added to file_set (if input) at the end of the read
  Range read_ents;
  
    // start_vertex is passed back so we know how to convert indices from the file into vertex handles; most
    // of the time this is done by adding start_vertex to the (0-based) index; if the index is 1-based, you also
    // need to subtract one; see read_elements for details
  EntityHandle start_vertex;
  result = read_vertices(num_verts, start_vertex, read_ents);
  if (MB_SUCCESS != result) return result;

    // create/read elements; this template assumes that all elements are the same type, so can be read in a single
    // call to read_elements, and kept track of with a single start_elem handle.  If there are more entity types,
    // might have to keep these start handles in an array/vector.  start_elem is only really needed if you're reading
    // sets later, and need to convert some file-based index to an entity handle
  EntityHandle start_elem;
  result = read_elements(num_elems, start_vertex, start_elem, read_ents);
  if (MB_SUCCESS != result) return result;

    // read/create entity sets; typically these sets have some tag identifying what they're for, see doc/metadata_info.doc
    // for examples of different kinds of sets and how they're marked
  result = create_sets(num_sets, start_vertex, num_verts, start_elem, num_elems, read_ents);
  if (MB_SUCCESS != result) return result;

    // finally, add all read_ents into the file set, if one was input
  if (file_set && *file_set) {
    result = mbImpl->add_entities(*file_set, read_ents);
    if (MB_SUCCESS != result) return result;
  }
  
  return result;
  */
}

ErrorCode ReadGCRM::read_vertices(int num_verts, EntityHandle &start_vertex, Range &read_ents) 
{
    // allocate nodes; these are allocated in one shot, get contiguous handles starting with start_handle,
    // and the reader is passed back double*'s pointing to MOAB's native storage for vertex coordinates
    // for those verts
  std::vector<double*> coord_arrays;
  ErrorCode result = readMeshIface->get_node_coords( 3, num_verts, 1, start_vertex, coord_arrays );
  if (MB_SUCCESS != result) {
    readMeshIface->report_error("%s: Trouble reading vertices\n", fileName);
    return result;
  }

    // fill in vertex coordinate arrays
  double *x = coord_arrays[0], 
         *y = coord_arrays[1],
         *z = coord_arrays[2];
  for(long i = 0; i < num_verts; ++i) {
      // empty statement to avoid compiler warning
    if (x || y || z) {}
      // read x/y/z
  }

  if (num_verts) read_ents.insert(start_vertex, start_vertex + num_verts - 1);
  
  return result;
}

//! read/create elements
ErrorCode ReadGCRM::read_elements(int num_elems, EntityHandle start_vertex,
                                      EntityHandle &start_elem, Range &read_ents) 
{
    // get the entity type being read
  EntityType ent_type = MBHEX;

    // get the number of vertices per entity
  int verts_per_elem = 8;
  
    // Create the element sequence; passes back a pointer to the internal storage for connectivity and the
    // starting entity handle
  EntityHandle* conn_array;
  ErrorCode result = readMeshIface->get_element_connect( num_elems, verts_per_elem, ent_type,
                                                         1, start_elem, conn_array );
  if (MB_SUCCESS != result) {
    readMeshIface->report_error("%s: Trouble reading elements\n", fileName);
    return result;
  }

    // read connectivity into conn_array directly
  for (long i = 0; i < num_elems; i++) {
      // read connectivity
  }

    // convert file-based connectivity indices to vertex handles in-place; be careful, if indices are smaller than handles,
    // need to do from the end of the list so we don't overwrite data
    //
    // here, we assume indices are smaller than handles, just for demonstration; create an integer-type pointer to connectivity
    // initialized to same start of connectivity array
  int *ind_array = reinterpret_cast<int*>(conn_array);
    // OFFSET is value of first vertex index in file; most files are 1-based, but some might be 0-based
  int OFFSET = 1;
  for (long i = num_elems*verts_per_elem-1; i >= 0; i--) {
    conn_array[i] = ind_array[i] + start_vertex + OFFSET;

      // this assert assumes last handle in read_ents is highest vertex handle in this file
    assert(conn_array[i] >= start_vertex && conn_array[i] <= *read_ents.rbegin());
  }
  
    // notify MOAB of the new elements
  result = readMeshIface->update_adjacencies(start_elem, num_elems, verts_per_elem, conn_array);
  if (MB_SUCCESS != result) return result;

    // add elements to read_ents
  if (num_elems) read_ents.insert(start_elem, start_elem+num_elems-1);
  
  return MB_SUCCESS;
}

//! read/create sets
    ErrorCode ReadGCRM::create_sets(int num_sets, EntityHandle /*start_vertex*/, int /*num_verts*/, 
                                    EntityHandle /*start_elem*/, int /*num_elems*/, Range &read_ents)
{ 
  ErrorCode result = MB_SUCCESS;
  EntityHandle this_set;
  
  for (int i = 0; i < num_sets; i++) {
      // create set
    result = mbImpl->create_meshset(MESHSET_SET, this_set);
    if (MB_SUCCESS != result) {
      readMeshIface->report_error("%s: Trouble creating set.\n", fileName);
      return result;
    }

    Range set_ents;
      // read/compute what's in this set; REMEMBER TO CONVERT THESE TO MOAB HANDLES

      // add them to the set
    result = mbImpl->add_entities(this_set, set_ents);
    if (MB_SUCCESS != result) {
      readMeshIface->report_error("%s: Trouble putting entities in set.\n", fileName);
      return result;
    }

      // add the new set to read_ents
    read_ents.insert(this_set);
  }
    
  return MB_SUCCESS;
}

ErrorCode ReadGCRM::process_options(const FileOptions &opts) 
{
    // mark all options seen, to avoid compile warning on unused variable
  opts.mark_all_seen();
  return MB_SUCCESS;
}

ErrorCode ReadGCRM::read_header() {
  CPU_WORD_SIZE = sizeof(double);
  IO_WORD_SIZE = sizeof(double);

  dbgOut.tprint(1, "Reading header...\n");

  // get the global attributes
  int numgatts;
  int success;
  success = NCFUNC(inq_natts )(fileId, &numgatts);
  ERRORS(success, "Couldn't get number of global attributes.");

  // read attributes into globalAtts
  ErrorCode result = get_attributes(NC_GLOBAL, numgatts, globalAtts);
  ERRORR(result, "Getting attributes.");
  dbgOut.tprintf(1, "Read %u attributes\n", (unsigned int) globalAtts.size());

  // read in dimensions into dimVals
  result = get_dimensions(fileId, dimNames, dimVals);
  ERRORR(result, "Getting dimensions.");
  dbgOut.tprintf(1, "Read %u dimensions\n", (unsigned int) dimVals.size());

  // read in variables into varInfo
  result = get_variables();
  ERRORR(result, "Getting variables.");
  dbgOut.tprintf(1, "Read %u variables\n", (unsigned int) varInfo.size());

  return MB_SUCCESS;
}

ErrorCode ReadGCRM::get_attributes(int var_id, int num_atts, std::map<std::string, AttData> &atts, const char *prefix) {

  char dum_name[120];

  for (int i = 0; i < num_atts; i++) {
    // get the name
    int success = NCFUNC(inq_attname)(fileId, var_id, i, dum_name);
    ERRORS(success, "Trouble getting attribute name.");

    AttData &data = atts[std::string(dum_name)];
    data.attName = std::string(dum_name);
    success = NCFUNC(inq_att)(fileId, var_id, dum_name, &data.attDataType, &data.attLen);
    ERRORS(success, "Trouble getting attribute info.");
    data.attVarId = var_id;

    dbgOut.tprintf(2, "%sAttribute %s: length=%u, varId=%d, type=%d\n", (prefix ? prefix : ""), data.attName.c_str(),
        (unsigned int) data.attLen, data.attVarId, data.attDataType);
  }

  return MB_SUCCESS;
}

ErrorCode ReadGCRM::get_dimensions(int file_id, std::vector<std::string> &dim_names, std::vector<int> &dim_vals) {
  // get the number of dimensions
  int num_dims;
  int success = NCFUNC(inq_ndims)(file_id, &num_dims);
  ERRORS(success, "Trouble getting number of dimensions.");

  if (num_dims > NC_MAX_DIMS) {
    readMeshIface->report_error("ReadGCRM: File contains %d dims but NetCDF library supports only %d\n", num_dims, (int) NC_MAX_DIMS);
    return MB_FAILURE;
  }

  char dim_name[NC_MAX_NAME + 1];
  NCDF_SIZE dum_len;
  dim_names.resize(num_dims);
  dim_vals.resize(num_dims);

  for (int i = 0; i < num_dims; i++) {
    success = NCFUNC(inq_dim)(file_id, i, dim_name, &dum_len);
    ERRORS(success, "Trouble getting dimension info.");

    dim_vals[i] = dum_len;
    dim_names[i] = std::string(dim_name);

    dbgOut.tprintf(2, "Dimension %s, length=%u\n", dim_name, (unsigned int) dum_len);
  }

  return MB_SUCCESS;
}

ErrorCode ReadGCRM::get_variables() {
  // first cache the number of time steps
  std::vector<std::string>::iterator vit = std::find(dimNames.begin(), dimNames.end(), "time");
  if (vit == dimNames.end())
    vit = std::find(dimNames.begin(), dimNames.end(), "t");

  int ntimes = 0;
  if (vit != dimNames.end())
    ntimes = dimVals[vit - dimNames.begin()];
  if (!ntimes)
    ntimes = 1;

  // get the number of variables
  int num_vars;
  int success = NCFUNC(inq_nvars)(fileId, &num_vars);
  ERRORS(success, "Trouble getting number of variables.");

  if (num_vars > NC_MAX_VARS) {
    readMeshIface->report_error("ReadGCRM: File contains %d vars but NetCDF library supports only %d\n", num_vars, (int) NC_MAX_VARS);
    return MB_FAILURE;
  }

  char var_name[NC_MAX_NAME + 1];
  int var_ndims;

  for (int i = 0; i < num_vars; i++) {
    // get the name first, so we can allocate a map iterate for this var
    success = NCFUNC(inq_varname )(fileId, i, var_name);
    ERRORS(success, "Trouble getting var name.");
    VarData &data = varInfo[std::string(var_name)];
    data.varName = std::string(var_name);
    data.varId = i;
    data.varTags.resize(ntimes, 0);

    // get the data type
    success = NCFUNC(inq_vartype)(fileId, i, &data.varDataType);
    ERRORS(success, "Trouble getting variable data type.");

    // get the number of dimensions, then the dimensions
    success = NCFUNC(inq_varndims)(fileId, i, &var_ndims);
    ERRORS(success, "Trouble getting number of dims of a variable.");
    data.varDims.resize(var_ndims);

    success = NCFUNC(inq_vardimid)(fileId, i, &data.varDims[0]);
    ERRORS(success, "Trouble getting variable dimensions.");

    // finally, get the number of attributes, then the attributes
    success = NCFUNC(inq_varnatts)(fileId, i, &data.numAtts);
    ERRORS(success, "Trouble getting number of dims of a variable.");

    // print debug info here so attribute info comes afterwards
    dbgOut.tprintf(2, "Variable %s: Id=%d, numAtts=%d, datatype=%d, num_dims=%u\n", data.varName.c_str(), data.varId, data.numAtts,
        data.varDataType, (unsigned int) data.varDims.size());

    ErrorCode rval = get_attributes(i, data.numAtts, data.varAtts, "   ");
    ERRORR(rval, "Trouble getting attributes for a variable.");

  }

  return MB_SUCCESS;
}

ErrorCode ReadGCRM::read_tag_values(const char*, const char*, const FileOptions&, std::vector<int>&, const SubsetList*) {
  return MB_FAILURE;
}

ErrorCode ReadGCRM::create_tags(ScdInterface *scdi, EntityHandle file_set, const std::vector<int>& tstep_nums) {
  ErrorCode rval;
  std::string tag_name;

  // <__NUM_DIMS>
  Tag numDimsTag = 0;
  tag_name = "__NUM_DIMS";
  int numDims = dimNames.size();
  rval = mbImpl->tag_get_handle(tag_name.c_str(), 1, MB_TYPE_INTEGER, numDimsTag, MB_TAG_SPARSE | MB_TAG_CREAT);
  ERRORR(rval, "Trouble creating __NUM_DIMS tag.");
  rval = mbImpl->tag_set_data(numDimsTag, &file_set, 1, &numDims);
  ERRORR(rval, "Trouble setting data for __NUM_DIMS tag.");
  if (MB_SUCCESS == rval)
    dbgOut.tprintf(2, "Tag created for variable %s\n", tag_name.c_str());

  // <__NUM_VARS>
  Tag numVarsTag = 0;
  tag_name = "__NUM_VARS";
  int numVars = varInfo.size();
  rval = mbImpl->tag_get_handle(tag_name.c_str(), 1, MB_TYPE_INTEGER, numVarsTag, MB_TAG_SPARSE | MB_TAG_CREAT);
  ERRORR(rval, "Trouble creating __NUM_VARS tag.");
  rval = mbImpl->tag_set_data(numVarsTag, &file_set, 1, &numVars);
  ERRORR(rval, "Trouble setting data for __NUM_VARS tag.");
  if (MB_SUCCESS == rval)
    dbgOut.tprintf(2, "Tag created for variable %s\n", tag_name.c_str());

  // <__DIM_NAMES>
  Tag dimNamesTag = 0;
  tag_name = "__DIM_NAMES";
  std::string dimnames;
  unsigned int dimNamesSz = dimNames.size();
  for (unsigned int i = 0; i != dimNamesSz; ++i) {
    dimnames.append(dimNames[i]);
    dimnames.push_back('\0');
  }
  int dimnamesSz = dimnames.size();
  rval = mbImpl->tag_get_handle(tag_name.c_str(), 0, MB_TYPE_OPAQUE, dimNamesTag, MB_TAG_CREAT | MB_TAG_SPARSE | MB_TAG_VARLEN);
  ERRORR(rval, "Trouble creating __DIM_NAMES tag.");
  const void* ptr = dimnames.c_str();
  rval = mbImpl->tag_set_by_ptr(dimNamesTag, &file_set, 1, &ptr, &dimnamesSz);
  ERRORR(rval, "Trouble setting data for __DIM_NAMES tag.");
  if (MB_SUCCESS == rval)
    dbgOut.tprintf(2, "Tag created for variable %s\n", tag_name.c_str());

  // <__VAR_NAMES>
  Tag varNamesTag = 0;
  tag_name = "__VAR_NAMES";
  std::string varnames;
  std::map<std::string, VarData>::iterator mapIter;
  for (mapIter = varInfo.begin(); mapIter != varInfo.end(); ++mapIter) {
    varnames.append(mapIter->first);
    varnames.push_back('\0');
  }
  int varnamesSz = varnames.size();
  rval = mbImpl->tag_get_handle(tag_name.c_str(), 0, MB_TYPE_OPAQUE, varNamesTag, MB_TAG_CREAT | MB_TAG_SPARSE | MB_TAG_VARLEN);
  ERRORR(rval, "Trouble creating __VAR_NAMES tag.");
  ptr = varnames.c_str();
  rval = mbImpl->tag_set_by_ptr(varNamesTag, &file_set, 1, &ptr, &varnamesSz);
  ERRORR(rval, "Trouble setting data for __VAR_NAMES tag.");
  if (MB_SUCCESS == rval)
    dbgOut.tprintf(2, "Tag created for variable %s\n", tag_name.c_str());

  // __<dim_name>_LOC_MINMAX
  for (unsigned int i = 0; i != dimNamesSz; ++i) {
    if (dimNames[i] == "time") {
      std::stringstream ss_tag_name;
      ss_tag_name << "__" << dimNames[i] << "_LOC_MINMAX";
      tag_name = ss_tag_name.str();
      Tag tagh = 0;
      std::vector<int> val(2, 0);
      val[0] = tMin;
      val[1] = tMax;
      rval = mbImpl->tag_get_handle(tag_name.c_str(), 2, MB_TYPE_INTEGER, tagh, MB_TAG_SPARSE | MB_TAG_CREAT);
      ERRORR(rval, "Trouble creating __<dim_name>_LOC_MINMAX tag.");
      rval = mbImpl->tag_set_data(tagh, &file_set, 1, &val[0]);
      ERRORR(rval, "Trouble setting data for __<dim_name>_LOC_MINMAX tag.");
      if (MB_SUCCESS == rval)
        dbgOut.tprintf(2, "Tag created for variable %s\n", tag_name.c_str());
    }
  }

  // __<dim_name>_LOC_VALS
  for (unsigned int i = 0; i != dimNamesSz; ++i) {
    if (dimNames[i] != "time")
      continue;
    std::vector<int> val;
    if (!tstep_nums.empty())
      val = tstep_nums;
    else {
      val.resize(tVals.size());
      for (unsigned int j = 0; j != tVals.size(); ++j)
        val[j] = j;
    }
    Tag tagh = 0;
    std::stringstream ss_tag_name;
    ss_tag_name << "__" << dimNames[i] << "_LOC_VALS";
    tag_name = ss_tag_name.str();
    rval = mbImpl->tag_get_handle(tag_name.c_str(), val.size(), MB_TYPE_INTEGER, tagh, MB_TAG_SPARSE | MB_TAG_CREAT);
    ERRORR(rval, "Trouble creating __<dim_name>_LOC_VALS tag.");
    rval = mbImpl->tag_set_data(tagh, &file_set, 1, &val[0]);
    ERRORR(rval, "Trouble setting data for __<dim_name>_LOC_VALS tag.");
    if (MB_SUCCESS == rval)
      dbgOut.tprintf(2, "Tag created for variable %s\n", tag_name.c_str());
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
    varInfo[mapIter->first].varTags.resize(varDimSz, 0);
    for (unsigned int i = 0; i != varDimSz; ++i) {
      Tag tmptag = 0;
      std::string tmptagname = dimNames[varInfo[mapIter->first].varDims[i]];
      mbImpl->tag_get_handle(tmptagname.c_str(), 0, MB_TYPE_OPAQUE, tmptag, MB_TAG_ANY);
      varInfo[mapIter->first].varTags[i] = tmptag;
    }
    rval = mbImpl->tag_get_handle(tag_name.c_str(), varDimSz, MB_TYPE_HANDLE, varNamesDimsTag, MB_TAG_SPARSE | MB_TAG_CREAT);
    ERRORR(rval, "Trouble creating __<var_name>_DIMS tag.");
    rval = mbImpl->tag_set_data(varNamesDimsTag, &file_set, 1, &(varInfo[mapIter->first].varTags[0]));
    ERRORR(rval, "Trouble setting data for __<var_name>_DIMS tag.");
    if (MB_SUCCESS == rval)
      dbgOut.tprintf(2, "Tag created for variable %s\n", tag_name.c_str());
  }

  // <PARTITION_METHOD>
  Tag part_tag = scdi->part_method_tag();
  if (!part_tag)
    ERRORR(MB_FAILURE, "Trouble getting partition method tag.");
  rval = mbImpl->tag_set_data(part_tag, &file_set, 1, &partMethod);
  ERRORR(rval, "Trouble setting data for PARTITION_METHOD tag.");
  if (MB_SUCCESS == rval)
    dbgOut.tprintf(2, "Tag created for variable %s\n", tag_name.c_str());

  // <__GLOBAL_ATTRIBS>
  tag_name = "__GLOBAL_ATTRIBS";
  Tag globalAttTag = 0;
  rval = mbImpl->tag_get_handle(tag_name.c_str(), 0, MB_TYPE_OPAQUE, globalAttTag, MB_TAG_CREAT | MB_TAG_SPARSE | MB_TAG_VARLEN);
  ERRORR(rval, "Trouble creating __GLOBAL_ATTRIBS tag.");
  std::string gattVal;
  std::vector<int> gattLen;
  rval = create_attrib_string(globalAtts, gattVal, gattLen);
  ERRORR(rval, "Trouble creating attribute strings.");
  const void* gattptr = gattVal.c_str();
  int globalAttSz = gattVal.size();
  rval = mbImpl->tag_set_by_ptr(globalAttTag, &file_set, 1, &gattptr, &globalAttSz);
  ERRORR(rval, "Trouble setting data for __GLOBAL_ATTRIBS tag.");
  if (MB_SUCCESS == rval)
    dbgOut.tprintf(2, "Tag created for variable %s\n", tag_name.c_str());

  // <__GLOBAL_ATTRIBS_LEN>
  tag_name = "__GLOBAL_ATTRIBS_LEN";
  Tag globalAttLenTag = 0;
  if (gattLen.size() == 0)
    gattLen.push_back(0);
  rval = mbImpl->tag_get_handle(tag_name.c_str(), gattLen.size(), MB_TYPE_INTEGER, globalAttLenTag, MB_TAG_SPARSE | MB_TAG_CREAT);
  ERRORR(rval, "Trouble creating __GLOBAL_ATTRIBS_LEN tag.");
  rval = mbImpl->tag_set_data(globalAttLenTag, &file_set, 1, &gattLen[0]);
  ERRORR(rval, "Trouble setting data for __GLOBAL_ATTRIBS_LEN tag.");
  if (MB_SUCCESS == rval)
    dbgOut.tprintf(2, "Tag created for variable %s\n", tag_name.c_str());

  // __<var_name>_ATTRIBS and __<var_name>_ATTRIBS_LEN
  for (mapIter = varInfo.begin(); mapIter != varInfo.end(); ++mapIter) {
    std::stringstream ssTagName;
    ssTagName << "__" << mapIter->first << "_ATTRIBS";
    tag_name = ssTagName.str();
    Tag varAttTag = 0;
    rval = mbImpl->tag_get_handle(tag_name.c_str(), 0, MB_TYPE_OPAQUE, varAttTag, MB_TAG_CREAT | MB_TAG_SPARSE | MB_TAG_VARLEN);
    ERRORR(rval, "Trouble creating __<var_name>_ATTRIBS tag.");
    std::string varAttVal;
    std::vector<int> varAttLen;
    rval = create_attrib_string(mapIter->second.varAtts, varAttVal, varAttLen);
    ERRORR(rval, "Trouble creating attribute strings.");
    const void* varAttPtr = varAttVal.c_str();
    int varAttSz = varAttVal.size();
    rval = mbImpl->tag_set_by_ptr(varAttTag, &file_set, 1, &varAttPtr, &varAttSz);
    ERRORR(rval, "Trouble setting data for __<var_name>_ATTRIBS tag.");
    if (MB_SUCCESS == rval)
      dbgOut.tprintf(2, "Tag created for variable %s\n", tag_name.c_str());
    if (varAttLen.size() == 0)
      varAttLen.push_back(0);
    ssTagName << "_LEN";
    tag_name = ssTagName.str();
    Tag varAttLenTag = 0;
    rval = mbImpl->tag_get_handle(tag_name.c_str(), varAttLen.size(), MB_TYPE_INTEGER, varAttLenTag, MB_TAG_SPARSE | MB_TAG_CREAT);
    ERRORR(rval, "Trouble creating __<var_name>_ATTRIBS_LEN tag.");
    rval = mbImpl->tag_set_data(varAttLenTag, &file_set, 1, &varAttLen[0]);
    ERRORR(rval, "Trouble setting data for __<var_name>_ATTRIBS_LEN tag.");
    if (MB_SUCCESS == rval)
      dbgOut.tprintf(2, "Tag created for variable %s\n", tag_name.c_str());
  }

  // <__VAR_NAMES_LOCATIONS>
  tag_name = "__VAR_NAMES_LOCATIONS";
  Tag varNamesLocsTag = 0;
  std::vector<int> varNamesLocs(varInfo.size());
  rval = mbImpl->tag_get_handle(tag_name.c_str(), varNamesLocs.size(), MB_TYPE_INTEGER, varNamesLocsTag, MB_TAG_CREAT
      | MB_TAG_SPARSE);
  ERRORR(rval, "Trouble creating __VAR_NAMES_LOCATIONS tag.");
  for (mapIter = varInfo.begin(); mapIter != varInfo.end(); ++mapIter) {
    varNamesLocs[std::distance(varInfo.begin(), mapIter)] = mapIter->second.entLoc;
  }
  rval = mbImpl->tag_set_data(varNamesLocsTag, &file_set, 1, &varNamesLocs[0]);
  ERRORR(rval, "Trouble setting data for __VAR_NAMES_LOCATIONS tag.");
  if (MB_SUCCESS == rval)
    dbgOut.tprintf(2, "Tag created for variable %s\n", tag_name.c_str());

  return MB_SUCCESS;
}

ErrorCode ReadGCRM::create_attrib_string(const std::map<std::string, AttData>& attMap, std::string& attVal, std::vector<int>& attLen) {
  int success;
  std::stringstream ssAtt;
  unsigned int sz = 0;
  std::map<std::string, AttData>::const_iterator attIt = attMap.begin();
  for (; attIt != attMap.end(); ++attIt) {
    ssAtt << attIt->second.attName;
    ssAtt << '\0';
    void* attData = NULL;
    switch (attIt->second.attDataType) {
      case NC_BYTE:
      case NC_CHAR:
        sz = attIt->second.attLen;
        attData = (char *) malloc(sz);
        success = NCFUNC(get_att_text)(fileId, attIt->second.attVarId, attIt->second.attName.c_str(), (char*) attData);
        ERRORS(success, "Failed to read attribute char data.")
        ;
        ssAtt << "char;";
        break;
      case NC_DOUBLE:
        sz = attIt->second.attLen * sizeof(double);
        attData = (double *) malloc(sz);
        success = NCFUNC(get_att_double)(fileId, attIt->second.attVarId, attIt->second.attName.c_str(), (double*) attData);
        ERRORS(success, "Failed to read attribute double data.")
        ;
        ssAtt << "double;";
        break;
      case NC_FLOAT:
        sz = attIt->second.attLen * sizeof(float);
        attData = (float *) malloc(sz);
        success = NCFUNC(get_att_float)(fileId, attIt->second.attVarId, attIt->second.attName.c_str(), (float*) attData);
        ERRORS(success, "Failed to read attribute float data.")
        ;
        ssAtt << "float;";
        break;
      case NC_INT:
        sz = attIt->second.attLen * sizeof(int);
        attData = (int *) malloc(sz);
        success = NCFUNC(get_att_int)(fileId, attIt->second.attVarId, attIt->second.attName.c_str(), (int*) attData);
        ERRORS(success, "Failed to read attribute int data.")
        ;
        ssAtt << "int;";
        break;
      case NC_SHORT:
        sz = attIt->second.attLen * sizeof(short);
        attData = (short *) malloc(sz);
        success = NCFUNC(get_att_short)(fileId, attIt->second.attVarId, attIt->second.attName.c_str(), (short*) attData);
        ERRORS(success, "Failed to read attribute short data.")
        ;
        ssAtt << "short;";
        break;
      default:
        success = 1;
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

} // namespace moab
