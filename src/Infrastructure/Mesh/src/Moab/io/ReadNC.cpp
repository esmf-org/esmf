#include "ReadNC.hpp"
#include "NCHelper.hpp"

#include "moab/ReadUtilIface.hpp"
#include "MBTagConventions.hpp"
#include "moab/FileOptions.hpp"

namespace moab {

ReaderIface* ReadNC::factory(Interface* iface)
{
  return new ReadNC(iface);
}

ReadNC::ReadNC(Interface* impl) :
  mbImpl(impl), fileId(-1), mGlobalIdTag(0), mpFileIdTag(NULL), dbgOut(stderr), isParallel(false),
  partMethod(ScdParData::ALLJORKORI), scdi(NULL),
#ifdef MOAB_HAVE_MPI
  myPcomm(NULL),
#endif
  noMesh(false), noVars(false), spectralMesh(false), noMixedElements(false), noEdges(false),
  gatherSetRank(-1), tStepBase(-1), trivialPartitionShift(0), myHelper(NULL)
{
  assert(impl != NULL);
  impl->query_interface(readMeshIface);
}

ReadNC::~ReadNC()
{
  mbImpl->release_interface(readMeshIface);
  if (myHelper != NULL)
    delete myHelper;
}

ErrorCode ReadNC::load_file(const char* file_name, const EntityHandle* file_set, const FileOptions& opts,
                            const ReaderIface::SubsetList* /*subset_list*/, const Tag* file_id_tag)
{
  // See if opts has variable(s) specified
  std::vector<std::string> var_names;
  std::vector<int> tstep_nums;
  std::vector<double> tstep_vals;

  // Get and cache predefined tag handles
  int dum_val = 0;
  ErrorCode rval = mbImpl->tag_get_handle(GLOBAL_ID_TAG_NAME, 1, MB_TYPE_INTEGER, mGlobalIdTag, MB_TAG_DENSE | MB_TAG_CREAT,
                                &dum_val);MB_CHK_SET_ERR(rval, "Trouble getting global ID tag");

  // Store the pointer to the tag; if not null, set when global id tag
  // is set too, with the same data, duplicated
  mpFileIdTag = file_id_tag;

  rval = parse_options(opts, var_names, tstep_nums, tstep_vals);MB_CHK_SET_ERR(rval, "Trouble parsing option string");

  // Open the file
  dbgOut.tprintf(1, "Opening file %s\n", file_name);
  fileName = std::string(file_name);
  int success;

#ifdef MOAB_HAVE_PNETCDF
  if (isParallel)
    success = NCFUNC(open)(myPcomm->proc_config().proc_comm(), file_name, 0, MPI_INFO_NULL, &fileId);
  else
    success = NCFUNC(open)(MPI_COMM_SELF, file_name, 0, MPI_INFO_NULL, &fileId);
#else
  success = NCFUNC(open)(file_name, 0, &fileId);
#endif
  if (success)
    MB_SET_ERR(MB_FAILURE, "Trouble opening file " << file_name);

  // Read the header (num dimensions, dimensions, num variables, global attribs)
  rval = read_header();MB_CHK_SET_ERR(rval, "Trouble reading file header");

  // Make sure there's a file set to put things in
  EntityHandle tmp_set;
  if (noMesh && !file_set) {
    MB_SET_ERR(MB_FAILURE, "NOMESH option requires non-NULL file set on input");
  }
  else if (!file_set || (file_set && *file_set == 0)) {
    rval = mbImpl->create_meshset(MESHSET_SET, tmp_set);MB_CHK_SET_ERR(rval, "Trouble creating file set");
  }
  else
    tmp_set = *file_set;

  // Get the scd interface
  scdi = NULL;
  rval = mbImpl->query_interface(scdi);
  if (NULL == scdi)
    return MB_FAILURE;

  if (NULL != myHelper)
    delete myHelper;

  // Get appropriate NC helper instance based on information read from the header
  myHelper = NCHelper::get_nc_helper(this, fileId, opts, tmp_set);
  if (NULL == myHelper) {
    MB_SET_ERR(MB_FAILURE, "Failed to get NCHelper class instance");
  }

  // Initialize mesh values
  rval = myHelper->init_mesh_vals();MB_CHK_SET_ERR(rval, "Trouble initializing mesh values");

  // Check existing mesh from last read
  if (noMesh && !noVars) {
    rval = myHelper->check_existing_mesh();MB_CHK_SET_ERR(rval, "Trouble checking mesh from last read");
  }

  // Create some conventional tags, e.g. __NUM_DIMS
  // For multiple reads to a specified file set, we assume a single file, or a series of
  // files with separated timesteps. Keep a flag on the file set to prevent conventional
  // tags from being created again on a second read
  Tag convTagsCreated = 0;
  int def_val = 0;
  rval = mbImpl->tag_get_handle("__CONV_TAGS_CREATED", 1, MB_TYPE_INTEGER, convTagsCreated,
                                MB_TAG_SPARSE | MB_TAG_CREAT, &def_val);MB_CHK_SET_ERR(rval, "Trouble getting _CONV_TAGS_CREATED tag");
  int create_conv_tags_flag = 0;
  rval = mbImpl->tag_get_data(convTagsCreated, &tmp_set, 1, &create_conv_tags_flag);
  // The first read to the file set
  if (0 == create_conv_tags_flag) {
    // Read dimensions (coordinate variables) by default to create tags like __<var_name>_DIMS
    // This is done only once (assume that all files read to the file set have the same dimensions)
    rval = myHelper->read_variables(dimNames, tstep_nums);MB_CHK_SET_ERR(rval, "Trouble reading dimensions");

    rval = myHelper->create_conventional_tags(tstep_nums);MB_CHK_SET_ERR(rval, "Trouble creating NC conventional tags");

    create_conv_tags_flag = 1;
    rval = mbImpl->tag_set_data(convTagsCreated, &tmp_set, 1, &create_conv_tags_flag);MB_CHK_SET_ERR(rval, "Trouble setting data to _CONV_TAGS_CREATED tag");
  }
  // Another read to the file set
  else {
    if (tStepBase > -1) {
      // If timesteps spread across files, merge time values read
      // from current file to existing time tag
      rval = myHelper->update_time_tag_vals();MB_CHK_SET_ERR(rval, "Trouble updating time tag values");
    }
  }

  // Create mesh vertex/edge/face sequences
  Range faces;
  if (!noMesh) {
    rval = myHelper->create_mesh(faces);MB_CHK_SET_ERR(rval, "Trouble creating mesh");
  }

  // Read specified variables onto grid
  if (!noVars) {
    if (var_names.empty()) {
      // If VARIABLE option is missing, read all variables
      rval = myHelper->read_variables(var_names, tstep_nums);MB_CHK_SET_ERR(rval, "Trouble reading all variables");
    }
    else {
      // Exclude dimensions that are read to the file set by default
      std::vector<std::string> non_dim_var_names;
      for (unsigned int i = 0; i < var_names.size(); i++) {
        if (std::find(dimNames.begin(), dimNames.end(), var_names[i]) == dimNames.end())
          non_dim_var_names.push_back(var_names[i]);
      }

      if (!non_dim_var_names.empty()) {
        rval = myHelper->read_variables(non_dim_var_names, tstep_nums);MB_CHK_SET_ERR(rval, "Trouble reading specified variables");
      }
    }
  }

#ifdef MOAB_HAVE_MPI
  // Create partition set, and populate with elements
  if (isParallel) {
    EntityHandle partn_set;
    rval = mbImpl->create_meshset(MESHSET_SET, partn_set);MB_CHK_SET_ERR(rval, "Trouble creating partition set");

    rval = mbImpl->add_entities(partn_set, faces);MB_CHK_SET_ERR(rval, "Couldn't add new faces to partition set");

    Range verts;
    rval = mbImpl->get_connectivity(faces, verts);MB_CHK_SET_ERR(rval, "Couldn't get verts of faces");

    rval = mbImpl->add_entities(partn_set, verts);MB_CHK_SET_ERR(rval, "Couldn't add new verts to partition set");

    myPcomm->partition_sets().insert(partn_set);

    // Write partition tag name on partition set
    Tag part_tag = myPcomm->partition_tag();
    int dum_rank = myPcomm->proc_config().proc_rank();
    rval = mbImpl->tag_set_data(part_tag, &partn_set, 1, &dum_rank);MB_CHK_SET_ERR(rval, "Trouble writing partition tag name on partition set");
  }
#endif

  mbImpl->release_interface(scdi);
  scdi = NULL;

  // Close the file
  success = NCFUNC(close)(fileId);
  if (success)
    MB_SET_ERR(MB_FAILURE, "Trouble closing file");

  return MB_SUCCESS;
}

ErrorCode ReadNC::parse_options(const FileOptions& opts, std::vector<std::string>& var_names, std::vector<int>& tstep_nums,
                                std::vector<double>& tstep_vals)
{
  int tmpval;
  if (MB_SUCCESS == opts.get_int_option("DEBUG_IO", 1, tmpval)) {
    dbgOut.set_verbosity(tmpval);
    dbgOut.set_prefix("NC ");
  }

  ErrorCode rval = opts.get_strs_option("VARIABLE", var_names);
  if (MB_TYPE_OUT_OF_RANGE == rval)
    noVars = true;
  else
    noVars = false;

  opts.get_ints_option("TIMESTEP", tstep_nums);
  opts.get_reals_option("TIMEVAL", tstep_vals);

  rval = opts.get_null_option("NOMESH");
  if (MB_SUCCESS == rval)
    noMesh = true;

  rval = opts.get_null_option("SPECTRAL_MESH");
  if (MB_SUCCESS == rval)
    spectralMesh = true;

  rval = opts.get_null_option("NO_MIXED_ELEMENTS");
  if (MB_SUCCESS == rval)
    noMixedElements = true;

  rval = opts.get_null_option("NO_EDGES");
  if (MB_SUCCESS == rval)
    noEdges = true;

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

  rval = opts.get_int_option("GATHER_SET", 0, gatherSetRank);
  if (MB_TYPE_OUT_OF_RANGE == rval) {
    MB_SET_ERR(rval, "Invalid value for GATHER_SET option");
  }

  rval = opts.get_int_option("TIMESTEPBASE", 0, tStepBase);
  if (MB_TYPE_OUT_OF_RANGE == rval) {
    MB_SET_ERR(rval, "Invalid value for TIMESTEPBASE option");
  }

  rval = opts.get_int_option("TRIVIAL_PARTITION_SHIFT", 1, trivialPartitionShift);
  if (MB_TYPE_OUT_OF_RANGE == rval) {
    MB_SET_ERR(rval, "Invalid value for TRIVIAL_PARTITION_SHIFT option");
  }

#ifdef MOAB_HAVE_MPI
  isParallel = (opts.match_option("PARALLEL", "READ_PART") != MB_ENTITY_NOT_FOUND);

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
  const int rank = myPcomm->proc_config().proc_rank();
  dbgOut.set_rank(rank);

  int dum;
  rval = opts.match_option("PARTITION_METHOD", ScdParData::PartitionMethodNames, dum);
  if (MB_FAILURE == rval) {
    MB_SET_ERR(rval, "Unknown partition method specified");
  }
  else if (MB_ENTITY_NOT_FOUND == rval)
    partMethod = ScdParData::ALLJORKORI;
  else
    partMethod = dum;
#endif

  return MB_SUCCESS;
}

ErrorCode ReadNC::read_header()
{
  dbgOut.tprint(1, "Reading header...\n");

  // Get the global attributes
  int numgatts;
  int success;
  success = NCFUNC(inq_natts )(fileId, &numgatts);
  if (success)
    MB_SET_ERR(MB_FAILURE, "Couldn't get number of global attributes");

  // Read attributes into globalAtts
  ErrorCode result = get_attributes(NC_GLOBAL, numgatts, globalAtts);MB_CHK_SET_ERR(result, "Trouble getting global attributes");
  dbgOut.tprintf(1, "Read %u attributes\n", (unsigned int) globalAtts.size());

  // Read in dimensions into dimNames and dimLens
  result = get_dimensions(fileId, dimNames, dimLens);MB_CHK_SET_ERR(result, "Trouble getting dimensions");
  dbgOut.tprintf(1, "Read %u dimensions\n", (unsigned int) dimNames.size());

  // Read in variables into varInfo
  result = get_variables();MB_CHK_SET_ERR(result, "Trouble getting variables");
  dbgOut.tprintf(1, "Read %u variables\n", (unsigned int) varInfo.size());

  return MB_SUCCESS;
}

ErrorCode ReadNC::get_attributes(int var_id, int num_atts, std::map<std::string, AttData>& atts, const char* prefix)
{
  char dum_name[120];

  for (int i = 0; i < num_atts; i++) {
    // Get the name
    int success = NCFUNC(inq_attname)(fileId, var_id, i, dum_name);
    if (success)
      MB_SET_ERR(MB_FAILURE, "Trouble getting attribute name");

    AttData &data = atts[std::string(dum_name)];
    data.attName = std::string(dum_name);
    success = NCFUNC(inq_att)(fileId, var_id, dum_name, &data.attDataType, &data.attLen);
    if (success)
      MB_SET_ERR(MB_FAILURE, "Trouble getting info for attribute " << data.attName);
    data.attVarId = var_id;

    dbgOut.tprintf(2, "%sAttribute %s: length=%u, varId=%d, type=%d\n", (prefix ? prefix : ""), data.attName.c_str(),
        (unsigned int) data.attLen, data.attVarId, data.attDataType);
  }

  return MB_SUCCESS;
}

ErrorCode ReadNC::get_dimensions(int file_id, std::vector<std::string>& dim_names, std::vector<int>& dim_lens)
{
  // Get the number of dimensions
  int num_dims;
  int success = NCFUNC(inq_ndims)(file_id, &num_dims);
  if (success)
    MB_SET_ERR(MB_FAILURE, "Trouble getting number of dimensions");

  if (num_dims > NC_MAX_DIMS) {
    MB_SET_ERR(MB_FAILURE, "ReadNC: File contains " << num_dims << " dims but NetCDF library supports only " << NC_MAX_DIMS);
  }

  char dim_name[NC_MAX_NAME + 1];
  NCDF_SIZE dim_len;
  dim_names.resize(num_dims);
  dim_lens.resize(num_dims);

  for (int i = 0; i < num_dims; i++) {
    success = NCFUNC(inq_dim)(file_id, i, dim_name, &dim_len);
    if (success)
      MB_SET_ERR(MB_FAILURE, "Trouble getting dimension info");

    dim_names[i] = std::string(dim_name);
    dim_lens[i] = dim_len;

    dbgOut.tprintf(2, "Dimension %s, length=%u\n", dim_name, (unsigned int) dim_len);
  }

  return MB_SUCCESS;
}

ErrorCode ReadNC::get_variables()
{
  // First cache the number of time steps
  std::vector<std::string>::iterator vit = std::find(dimNames.begin(), dimNames.end(), "time");
  if (vit == dimNames.end())
    vit = std::find(dimNames.begin(), dimNames.end(), "t");

  int ntimes = 0;
  if (vit != dimNames.end())
    ntimes = dimLens[vit - dimNames.begin()];
  if (!ntimes)
    ntimes = 1;

  // Get the number of variables
  int num_vars;
  int success = NCFUNC(inq_nvars)(fileId, &num_vars);
  if (success)
    MB_SET_ERR(MB_FAILURE, "Trouble getting number of variables");

  if (num_vars > NC_MAX_VARS) {
    MB_SET_ERR(MB_FAILURE, "ReadNC: File contains " << num_vars << " vars but NetCDF library supports only " << NC_MAX_VARS);
  }

  char var_name[NC_MAX_NAME + 1];
  int var_ndims;

  for (int i = 0; i < num_vars; i++) {
    // Get the name first, so we can allocate a map iterate for this var
    success = NCFUNC(inq_varname )(fileId, i, var_name);
    if (success)
      MB_SET_ERR(MB_FAILURE, "Trouble getting variable name");
    VarData &data = varInfo[std::string(var_name)];
    data.varName = std::string(var_name);
    data.varId = i;
    data.varTags.resize(ntimes, 0);

    // Get the data type
    success = NCFUNC(inq_vartype)(fileId, i, &data.varDataType);
    if (success)
      MB_SET_ERR(MB_FAILURE, "Trouble getting data type for variable " << data.varName);

    // Get the number of dimensions, then the dimensions
    success = NCFUNC(inq_varndims)(fileId, i, &var_ndims);
    if (success)
      MB_SET_ERR(MB_FAILURE, "Trouble getting number of dims for variable " << data.varName);
    data.varDims.resize(var_ndims);

    success = NCFUNC(inq_vardimid)(fileId, i, &data.varDims[0]);
    if (success)
      MB_SET_ERR(MB_FAILURE, "Trouble getting dimensions for variable " << data.varName);

    // Finally, get the number of attributes, then the attributes
    success = NCFUNC(inq_varnatts)(fileId, i, &data.numAtts);
    if (success)
      MB_SET_ERR(MB_FAILURE, "Trouble getting number of dims for variable " << data.varName);

    // Print debug info here so attribute info comes afterwards
    dbgOut.tprintf(2, "Variable %s: Id=%d, numAtts=%d, datatype=%d, num_dims=%u\n", data.varName.c_str(), data.varId, data.numAtts,
        data.varDataType, (unsigned int) data.varDims.size());

    ErrorCode rval = get_attributes(i, data.numAtts, data.varAtts, "   ");MB_CHK_SET_ERR(rval, "Trouble getting attributes for variable " << data.varName);
  }

  return MB_SUCCESS;
}

ErrorCode ReadNC::read_tag_values(const char*, const char*, const FileOptions&, std::vector<int>&, const SubsetList*)
{
  return MB_FAILURE;
}

} // namespace moab
