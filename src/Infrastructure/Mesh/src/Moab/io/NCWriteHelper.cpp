/*
 * NCWriteHelper.cpp
 *
 *  Created on: Mar 28, 2014
 *      Author: iulian
 */

#include "NCWriteHelper.hpp"
#include "NCWriteEuler.hpp"
#include "NCWriteFV.hpp"
#include "NCWriteHOMME.hpp"
#include "NCWriteMPAS.hpp"
#include "NCWriteGCRM.hpp"

#include "moab/WriteUtilIface.hpp"
#include "MBTagConventions.hpp"

#include <sstream>

#ifdef WIN32
#ifdef size_t
#undef size_t
#endif
#endif

namespace moab {

//! Get appropriate helper instance for WriteNC class; based on some info in the file set
NCWriteHelper* NCWriteHelper::get_nc_helper(WriteNC* writeNC, int fileId, const FileOptions& opts, EntityHandle fileSet)
{
  std::string& grid_type = writeNC->grid_type;
  if (grid_type == "CAM_EUL")
    return new (std::nothrow) NCWriteEuler(writeNC, fileId, opts, fileSet);
  else if (grid_type == "CAM_FV")
    return new (std::nothrow) NCWriteFV(writeNC, fileId, opts, fileSet);
  else if (grid_type == "CAM_SE")
    return new (std::nothrow) NCWriteHOMME(writeNC, fileId, opts, fileSet);
  else if (grid_type == "MPAS")
    return new (std::nothrow) NCWriteMPAS(writeNC, fileId, opts, fileSet);
  else if (grid_type == "GCRM")
    return new (std::nothrow) NCWriteGCRM(writeNC, fileId, opts, fileSet);

  // Unknown NetCDF grid
  return NULL;
}

ErrorCode NCWriteHelper::collect_variable_data(std::vector<std::string>& var_names, std::vector<int>& tstep_nums)
{
  Interface*& mbImpl = _writeNC->mbImpl;
  std::vector<std::string>& dimNames = _writeNC->dimNames;
  std::vector<int>& dimLens = _writeNC->dimLens;
  std::set<std::string>& usedCoordinates = _writeNC->usedCoordinates;
  std::set<std::string>& dummyVarNames = _writeNC->dummyVarNames;
  std::map<std::string, WriteNC::VarData>& varInfo = _writeNC->varInfo;
  DebugOutput& dbgOut = _writeNC->dbgOut;

  ErrorCode rval;

  usedCoordinates.clear();

  if (tstep_nums.empty() && nTimeSteps > 0) {
    // No timesteps input, get them all
    for (int i = 0; i < nTimeSteps; i++)
      tstep_nums.push_back(i);
  }

  for (size_t i = 0; i < var_names.size(); i++) {
    std::string varname = var_names[i];
    std::map<std::string, WriteNC::VarData>::iterator vit = varInfo.find(varname);
    if (vit == varInfo.end())
      MB_SET_ERR(MB_FAILURE, "Can't find variable " << varname);

    WriteNC::VarData& currentVarData = vit->second;

    dbgOut.tprintf(2, "    for variable %s varDims.size %d \n", varname.c_str(), (int)currentVarData.varDims.size());
    for (size_t j = 0; j < currentVarData.varDims.size(); j++) {
      std::string dimName = dimNames[currentVarData.varDims[j]];
      vit = varInfo.find(dimName);
      if (vit == varInfo.end())
        MB_SET_ERR(MB_FAILURE, "Can't find coordinate variable " << dimName);

      usedCoordinates.insert(dimName); // Collect those used, we will need to write them to the file
      dbgOut.tprintf(2, "    for variable %s need dimension %s with length %d\n", varname.c_str(), dimName.c_str(), dimLens[currentVarData.varDims[j]]);
    }

    // Process coordinate variables later
    if (usedCoordinates.find(varname) != usedCoordinates.end())
      continue;

    // Default has_tsteps is false
    if (std::find(currentVarData.varDims.begin(), currentVarData.varDims.end(), tDim) != currentVarData.varDims.end())
      currentVarData.has_tsteps = true;

    // Default numLev is 0
    if ((std::find(currentVarData.varDims.begin(), currentVarData.varDims.end(), levDim) != currentVarData.varDims.end()))
      currentVarData.numLev = nLevels;

    // Process set variables
    if (WriteNC::ENTLOCSET == currentVarData.entLoc) {
      if (currentVarData.has_tsteps) {
        // Set variables with timesteps, e.g. xtime(Time) or xtime(Time, StrLen)
        // TBD
        MB_SET_ERR(MB_NOT_IMPLEMENTED, "Writing set variables with timesteps is not implemented yet");
      }
      else {
        // Get the tag with varname
        Tag tag = 0;
        rval = mbImpl->tag_get_handle(varname.c_str(), tag);MB_CHK_SET_ERR(rval, "Can't find tag " << varname);
        currentVarData.varTags.push_back(tag); // Really, only one for these
        const void* data;
        int size;
        rval = mbImpl->tag_get_by_ptr(tag, &_fileSet, 1, &data, &size);MB_CHK_SET_ERR(rval, "Can't get data of tag " << varname);

        // Find the type of tag, and use it
        DataType type;
        rval = mbImpl->tag_get_data_type(tag, type);MB_CHK_SET_ERR(rval, "Can't get data type of tag " << varname);

        currentVarData.varDataType = NC_DOUBLE;
        if (MB_TYPE_INTEGER == type)
          currentVarData.varDataType = NC_INT;

        assert(0 == currentVarData.memoryHogs.size()); // Nothing so far
        currentVarData.memoryHogs.push_back((void*)data);

        if (currentVarData.varDims.empty()) {
          // Scalar variable
          currentVarData.writeStarts.push_back(0);
          currentVarData.writeCounts.push_back(1);
        }
        else {
          for (size_t j = 0; j < currentVarData.varDims.size(); j++) {
            currentVarData.writeStarts.push_back(0);
            currentVarData.writeCounts.push_back(dimLens[currentVarData.varDims[j]]);
          }
        }

        // Get variable size
        currentVarData.sz = 1;
        for (std::size_t idx = 0; idx != currentVarData.writeCounts.size(); idx++)
          currentVarData.sz *= currentVarData.writeCounts[idx];
      }
    } // if (WriteNC::ENTLOCSET == currentVarData.entLoc)
    // Process non-set variables
    else {
      Tag indexedTag = 0;

      if (currentVarData.has_tsteps) {
        for (unsigned int t = 0; t < tstep_nums.size(); t++) {
          std::stringstream ssTagNameWithIndex;
          ssTagNameWithIndex << varname << tstep_nums[t];
          rval = mbImpl->tag_get_handle(ssTagNameWithIndex.str().c_str(), indexedTag);MB_CHK_SET_ERR(rval, "Can't find tag " << ssTagNameWithIndex.str());
          dbgOut.tprintf(2, "    found indexed tag %d with name %s\n", tstep_nums[t], ssTagNameWithIndex.str().c_str());
          currentVarData.varTags.push_back(indexedTag);
        }
      }
      else {
        // This should be a user-created non-set variable without timesteps
        // Treat it like having one, 0th, timestep
        std::stringstream ssTagNameWithIndex;
        ssTagNameWithIndex << varname << 0;
        rval = mbImpl->tag_get_handle(ssTagNameWithIndex.str().c_str(), indexedTag);MB_CHK_SET_ERR(rval, "Can't find tag " << ssTagNameWithIndex.str() << " for a user-created variable");
        dbgOut.tprintf(2, "    found indexed tag 0 with name %s\n", ssTagNameWithIndex.str().c_str());
        currentVarData.varTags.push_back(indexedTag);
      }

      // The type of the tag is fixed though
      DataType type;
      rval = mbImpl->tag_get_data_type(indexedTag, type);MB_CHK_SET_ERR(rval, "Can't get data type of tag " << varname);

      currentVarData.varDataType = NC_DOUBLE;
      if (MB_TYPE_INTEGER == type)
        currentVarData.varDataType = NC_INT;
    }
  } // for (size_t i = 0; i < var_names.size(); i++)

  // Process coordinate variables here
  // Check that for used coordinates we have found the tags
  for (std::set<std::string>::iterator setIt = usedCoordinates.begin();
      setIt != usedCoordinates.end(); ++setIt) {
    const std::string& coordName = *setIt;

    std::map<std::string, WriteNC::VarData>::iterator vit = varInfo.find(coordName);
    if (vit == varInfo.end())
      MB_SET_ERR(MB_FAILURE, "Can't find coordinate variable " << coordName);

    WriteNC::VarData& varCoordData = vit->second;
    Tag coordTag = 0;
    rval = mbImpl->tag_get_handle(coordName.c_str(), coordTag);MB_CHK_SET_ERR(rval, "Can't find tag " << coordName);
    varCoordData.varTags.push_back(coordTag); // Really, only one for these

    const void* data;
    int sizeCoordinate;
    rval = mbImpl->tag_get_by_ptr(coordTag, &_fileSet, 1, &data, &sizeCoordinate);MB_CHK_SET_ERR(rval, "Can't get coordinate values of " << coordName);
    dbgOut.tprintf(2, "    found coordinate tag with name %s and length %d\n", coordName.c_str(),
        sizeCoordinate);

    // Find the type of tag, and use it
    DataType type;
    rval = mbImpl->tag_get_data_type(coordTag, type);MB_CHK_SET_ERR(rval, "Can't get data type of tag " << coordName);
    varCoordData.varDataType = NC_DOUBLE;
    if (MB_TYPE_INTEGER == type)
      varCoordData.varDataType = NC_INT;

    // Get dimension length (the only dimension of this coordinate variable, with the same name)
    assert(1 == varCoordData.varDims.size());
    int coordDimLen = dimLens[varCoordData.varDims[0]];

    if (dummyVarNames.find(coordName) != dummyVarNames.end()) {
      // For a dummy coordinate variable, the tag size is always 1
      // The number of coordinates should be set to dimension length, instead of 1
      assert(1 == sizeCoordinate);
      sizeCoordinate = coordDimLen;

      // No variable data to write
      data = NULL;
    }
    else {
      // The number of coordinates should be exactly the same as dimension length
      // However, if timesteps spread across files and time tag has been updated,
      // sizeCoordinate will be larger
      if (varCoordData.varDims[0] != tDim)
        assert(sizeCoordinate == coordDimLen);
    }

    // For time, the actual output size and values are determined by tstep_nums
    if (varCoordData.varDims[0] == tDim) {
      // Does not apply to dummy time tag (e.g. 'Time' tag of MPAS), when timesteps
      // spread across files
      if (NULL != data)
        assert(tstep_nums.size() > 0 && tstep_nums.size() <= (size_t)sizeCoordinate);

      sizeCoordinate = tstep_nums.size();

      if (NULL != data) {
        assert(NC_DOUBLE == varCoordData.varDataType);
        timeStepVals.resize(sizeCoordinate);
        for (unsigned int t = 0; t < tstep_nums.size(); t++)
          timeStepVals[t] = ((double*)data)[tstep_nums[t]];

        data = &timeStepVals[0];
      }
    }

    // This is the length
    varCoordData.sz = sizeCoordinate;
    varCoordData.writeStarts.resize(1);
    varCoordData.writeStarts[0] = 0;
    varCoordData.writeCounts.resize(1);
    varCoordData.writeCounts[0] = sizeCoordinate;

    assert(0 == varCoordData.memoryHogs.size()); // Nothing so far
    varCoordData.memoryHogs.push_back((void*)data);
  } // for (std::set<std::string>::iterator setIt ...

  return MB_SUCCESS;
}

ErrorCode NCWriteHelper::init_file(std::vector<std::string>& var_names, std::vector<std::string>& desired_names, bool append)
{
  std::vector<std::string>& dimNames = _writeNC->dimNames;
  std::set<std::string>& usedCoordinates = _writeNC->usedCoordinates;
  std::set<std::string>& dummyVarNames = _writeNC->dummyVarNames;
  std::map<std::string, WriteNC::VarData>& varInfo = _writeNC->varInfo;
  std::map<std::string, WriteNC::AttData>& globalAtts = _writeNC->globalAtts;
  DebugOutput& dbgOut = _writeNC->dbgOut;

  int tDim_in_dimNames = tDim;
  int levDim_in_dimNames = levDim;

  // If append mode, make sure we are in define mode; a simple open will not allow creation of new variables
  if (append) {
    int errcode = NCFUNC(redef)(_fileId);
    if (errcode != NC_NOERR)
      MB_SET_ERR(MB_FAILURE, "Can't open file in redefine mode");
  }

  // First initialize all coordinates, then fill VarData for actual variables (and dimensions)
  // Check that for used coordinates we have found the tags
  for (std::set<std::string>::iterator setIt = usedCoordinates.begin();
      setIt != usedCoordinates.end(); ++setIt) {
    const std::string& coordName = *setIt;

    std::map<std::string, WriteNC::VarData>::iterator vit = varInfo.find(coordName);
    if (vit == varInfo.end())
      MB_SET_ERR(MB_FAILURE, "Can't find coordinate variable " << coordName);

    WriteNC::VarData& varCoordData = vit->second;
    varCoordData.varDims.resize(1);

    // If not append, create it for sure
    // If append, we might already have it, including the tag / variable with the same name
    /*
     * int ncmpi_inq_dimid(int ncid, const char *name, int *idp);
     */
    if (append) {
      int dimId;
      if (NCFUNC(inq_dimid)(_fileId, coordName.c_str(), &dimId) == NC_NOERR) { // If not found, create it later
        varCoordData.varDims[0] = dimId;
        dbgOut.tprintf(2, "    file already has coordName %s dim id is %d \n", coordName.c_str(), (int)varCoordData.varDims[0]);

        // Update tDim and levDim to actual dimension id
        if (coordName == dimNames[tDim_in_dimNames])
          tDim = varCoordData.varDims[0];
        else if (coordName == dimNames[levDim_in_dimNames])
          levDim = varCoordData.varDims[0];

        // Skip dummy coordinate variables (e.g. ncol)
        if (dummyVarNames.find(coordName) != dummyVarNames.end())
          continue;

        // Check that the coordinate is a variable too
        // Inquire for a variable with the same name
        int varId;
        if (NCFUNC(inq_varid)(_fileId, coordName.c_str(), &varId) != NC_NOERR)
          MB_SET_ERR(MB_FAILURE, "We do not have a variable with the same name " << coordName);
        // We should also check that this variable has one dimension, and it is dimId
        varCoordData.varId = varId;
        dbgOut.tprintf(2, "    file already has coordinate %s and varId is %d \n", coordName.c_str(), varId);

        continue; // Maybe more checks are needed here
      }
    }

    /* int nc_def_dim (int ncid, const char *name, size_t len, int *dimidp);
       * example:  status = nc_def_dim(fileId, "lat", 18L, &latid);
    */

    // Actually define a dimension
    if (NCFUNC(def_dim)(_fileId, coordName.c_str(), (size_t)varCoordData.sz,
        &varCoordData.varDims[0]) != NC_NOERR)
      MB_SET_ERR(MB_FAILURE, "Failed to generate dimension " << coordName);
    dbgOut.tprintf(2, "    for coordName %s dim id is %d \n", coordName.c_str(), (int)varCoordData.varDims[0]);

    // Update tDim and levDim to actual dimension id
    if (coordName == dimNames[tDim_in_dimNames])
      tDim = varCoordData.varDims[0];
    else if (coordName == dimNames[levDim_in_dimNames])
      levDim = varCoordData.varDims[0];

    // Create a variable with the same name, and its only dimension the one we just defined
    /*
     * int nc_def_var (int ncid, const char *name, nc_type xtype,
                       int ndims, const int dimids[], int *varidp);
       example: http://www.unidata.ucar.edu/software/netcdf/docs/netcdf-c/nc_005fdef_005fvar.html#nc_005fdef_005fvar
     */

    // Skip dummy coordinate variables (e.g. ncol)
    if (dummyVarNames.find(coordName) != dummyVarNames.end())
      continue;

    // Define a coordinate variable
    if (NCFUNC(def_var)(_fileId, coordName.c_str(), varCoordData.varDataType,
        1, &(varCoordData.varDims[0]), &varCoordData.varId) != NC_NOERR)
      MB_SET_ERR(MB_FAILURE, "Failed to create coordinate variable " << coordName);

    dbgOut.tprintf(2, "    for coordName %s variable id is %d \n", coordName.c_str(), varCoordData.varId);
  }

  // Now look at requested variables, and update from the index in dimNames to the actual dimension id
  for (size_t i = 0; i < var_names.size(); i++) {
    std::map<std::string, WriteNC::VarData>::iterator vit = varInfo.find(var_names[i]);
    if (vit == varInfo.end())
      MB_SET_ERR(MB_FAILURE, "Can't find requested variable " << var_names[i]);

    // Skip coordinate variables
    if (usedCoordinates.find(var_names[i]) != usedCoordinates.end())
      continue;

    WriteNC::VarData& variableData = vit->second;

    // The index is for dimNames; we need to find out the actual dimension id (from above)
    int numDims = (int)variableData.varDims.size();
    for (int j = 0; j < numDims; j++) {
      std::string dimName = dimNames[variableData.varDims[j]];
      std::map<std::string, WriteNC::VarData>::iterator vit2 = varInfo.find(dimName);
      if (vit2 == varInfo.end())
        MB_SET_ERR(MB_FAILURE, "Can't find requested coordinate variable " << dimName);

      WriteNC::VarData& coordData = vit2->second;
      // Index in dimNames to actual dimension id
      variableData.varDims[j] = coordData.varDims[0]; // This one, being a coordinate, is the only one
      dbgOut.tprintf(2, "          dimension with index %d name %s has ID %d \n",
          j, dimName.c_str(), variableData.varDims[j]);
    }

    // Define the variable now:
    int errCode = NCFUNC(def_var)(_fileId, desired_names[i].c_str(), variableData.varDataType,
        (int)variableData.varDims.size(), &(variableData.varDims[0]),
        &variableData.varId);
    if (errCode != NC_NOERR)
      MB_SET_ERR(MB_FAILURE, "Failed to create requested variable " << desired_names[i]);

    dbgOut.tprintf(2, "    for variable %s with desired name %s variable id is %d \n", var_names[i].c_str(),
        desired_names[i].c_str(), variableData.varId);
    // Now define the variable, with all dimensions
  }

  // Define global attributes (exactly copied from the original file for the time being)
  // Should we modify some of them (e.g. revision_Id) later?
  std::map<std::string, WriteNC::AttData>::iterator attIt;
  for (attIt = globalAtts.begin(); attIt != globalAtts.end(); ++attIt) {
    const std::string& attName = attIt->first;
    WriteNC::AttData& attData = attIt->second;
    NCDF_SIZE& attLen = attData.attLen;
    nc_type& attDataType = attData.attDataType;
    const std::string& attValue = attData.attValue;

    switch (attDataType) {
      case NC_BYTE:
      case NC_CHAR:
        if (NC_NOERR != NCFUNC(put_att_text)(_fileId, NC_GLOBAL, attName.c_str(), attLen, attValue.c_str()))
          MB_SET_ERR(MB_FAILURE, "Failed to define text type attribute");
        break;
      case NC_DOUBLE:
        if (NC_NOERR != NCFUNC(put_att_double)(_fileId, NC_GLOBAL, attName.c_str(), NC_DOUBLE, 1, (double*)attValue.c_str()))
          MB_SET_ERR(MB_FAILURE, "Failed to define double type attribute");
        break;
      case NC_FLOAT:
        if (NC_NOERR != NCFUNC(put_att_float)(_fileId, NC_GLOBAL, attName.c_str(), NC_FLOAT, 1, (float*)attValue.c_str()))
          MB_SET_ERR(MB_FAILURE, "Failed to define float type attribute");
        break;
      case NC_INT:
        if (NC_NOERR != NCFUNC(put_att_int)(_fileId, NC_GLOBAL, attName.c_str(), NC_INT, 1, (int*)attValue.c_str()))
          MB_SET_ERR(MB_FAILURE, "Failed to define int type attribute");
        break;
      case NC_SHORT:
        if (NC_NOERR != NCFUNC(put_att_short)(_fileId, NC_GLOBAL, attName.c_str(), NC_SHORT, 1, (short*)attValue.c_str()))
          MB_SET_ERR(MB_FAILURE, "Failed to define short type attribute");
        break;
      default:
        MB_SET_ERR(MB_FAILURE, "Unknown attribute data type");
    }
  }

  // Take it out of define mode
  if (NC_NOERR != NCFUNC(enddef)(_fileId))
    MB_SET_ERR(MB_FAILURE, "Failed to close define mode");

  return MB_SUCCESS;
}

ErrorCode NCWriteHelper::write_values(std::vector<std::string>& var_names, std::vector<int>& tstep_nums)
{
  std::set<std::string>& usedCoordinates = _writeNC->usedCoordinates;
  std::set<std::string>& dummyVarNames = _writeNC->dummyVarNames;
  std::map<std::string, WriteNC::VarData>& varInfo = _writeNC->varInfo;

  std::vector<WriteNC::VarData> vdatas;
  std::vector<WriteNC::VarData> vsetdatas;

  // For set variables, include coordinates used by requested var_names
  for (std::set<std::string>::iterator setIt = usedCoordinates.begin();
      setIt != usedCoordinates.end(); ++setIt) {
    const std::string& coordName = *setIt;

    // Skip dummy coordinate variables (if any)
    if (dummyVarNames.find(coordName) != dummyVarNames.end())
      continue;

    std::map<std::string, WriteNC::VarData>::iterator vit = varInfo.find(coordName);
    if (vit == varInfo.end()) {
      MB_SET_ERR(MB_FAILURE, "Can't find coordinate variable " << coordName);
    }

     vsetdatas.push_back(vit->second);
  }

  // Collect non-set and set variables from requested var_names
  for (unsigned int i = 0; i < var_names.size(); i++) {
    std::map<std::string, WriteNC::VarData>::iterator vit = varInfo.find(var_names[i]);
    if (vit == varInfo.end()) {
      MB_SET_ERR(MB_FAILURE, "Can't find requested variable " << var_names[i]);
    }

    WriteNC::VarData& variableData = vit->second;
    if (WriteNC::ENTLOCSET == variableData.entLoc) {
      // Used coordinates has all ready been included
      if (usedCoordinates.find(var_names[i]) != usedCoordinates.end())
        continue;

      vsetdatas.push_back(variableData);
    }
    else
      vdatas.push_back(variableData);
  }

  // Assume that the data ranges do not overlap across processors
  // While overlapped writing might still work, we should better not take that risk
  write_nonset_variables(vdatas, tstep_nums);

  // Use independent I/O mode put, since this write is only for the root processor
  write_set_variables(vsetdatas, tstep_nums);

  return MB_SUCCESS;
}

ErrorCode NCWriteHelper::write_set_variables(std::vector<WriteNC::VarData>& vsetdatas, std::vector<int>& /* tstep_nums */)
{
  int success;

// CAUTION: if the NetCDF ID is from a previous call to ncmpi_create rather than ncmpi_open,
// all processors need to call ncmpi_begin_indep_data(). If only the root processor does so,
// ncmpi_begin_indep_data() call will be blocked forever :(
 #ifdef MOAB_HAVE_PNETCDF
   // Enter independent I/O mode
   success = NCFUNC(begin_indep_data)(_fileId);
   if (success)
     MB_SET_ERR(MB_FAILURE, "Failed to begin independent I/O mode");
 #endif

   int rank = 0;
 #ifdef MOAB_HAVE_MPI
   bool& isParallel = _writeNC->isParallel;
   if (isParallel) {
     ParallelComm*& myPcomm = _writeNC->myPcomm;
     rank = myPcomm->proc_config().proc_rank();
   }
 #endif
   if (0 == rank) {
     for (unsigned int i = 0; i < vsetdatas.size(); i++) {
       WriteNC::VarData& variableData = vsetdatas[i];

       // Set variables with timesteps, e.g. xtime(Time) or xtime(Time, StrLen)
       if (variableData.has_tsteps) {
         MB_SET_ERR(MB_NOT_IMPLEMENTED, "Writing set variables with timesteps is not implemented yet");
       }

       switch (variableData.varDataType) {
         case NC_DOUBLE:
           // Independent I/O mode put
           success = NCFUNCP(_vara_double)(_fileId, variableData.varId, &variableData.writeStarts[0],
                     &variableData.writeCounts[0], (double*)(variableData.memoryHogs[0]));
           if (success)
             MB_SET_ERR(MB_FAILURE, "Failed to write double data for variable " << variableData.varName);
           break;
         case NC_INT:
           // Independent I/O mode put
           success = NCFUNCP(_vara_int)(_fileId, variableData.varId, &variableData.writeStarts[0],
                     &variableData.writeCounts[0], (int*)(variableData.memoryHogs[0]));
           if (success)
             MB_SET_ERR(MB_FAILURE, "Failed to write int data for variable " << variableData.varName);
           break;
         default:
           MB_SET_ERR(MB_NOT_IMPLEMENTED, "Writing non-double or non-int data is not implemented yet");
       }
     }
   }

 #ifdef MOAB_HAVE_PNETCDF
   // End independent I/O mode
   success = NCFUNC(end_indep_data)(_fileId);
   if (success)
     MB_SET_ERR(MB_FAILURE, "Failed to end independent I/O mode");
 #endif

   return MB_SUCCESS;
}

ErrorCode ScdNCWriteHelper::collect_mesh_info()
{
  Interface*& mbImpl = _writeNC->mbImpl;
  std::vector<std::string>& dimNames = _writeNC->dimNames;
  std::vector<int>& dimLens = _writeNC->dimLens;

  ErrorCode rval;

  // Look for time dimension
  std::vector<std::string>::iterator vecIt;
  if ((vecIt = std::find(dimNames.begin(), dimNames.end(), "time")) != dimNames.end())
    tDim = vecIt - dimNames.begin();
  else if ((vecIt = std::find(dimNames.begin(), dimNames.end(), "t")) != dimNames.end())
    tDim = vecIt - dimNames.begin();
  else {
    MB_SET_ERR(MB_FAILURE, "Couldn't find 'time' or 't' dimension");
  }
  nTimeSteps = dimLens[tDim];

  // Get number of levels
  if ((vecIt = std::find(dimNames.begin(), dimNames.end(), "lev")) != dimNames.end())
    levDim = vecIt - dimNames.begin();
  else if ((vecIt = std::find(dimNames.begin(), dimNames.end(), "ilev")) != dimNames.end())
    levDim = vecIt - dimNames.begin();
  else {
    MB_SET_ERR(MB_FAILURE, "Couldn't find 'lev' or 'ilev' dimension");
  }
  nLevels = dimLens[levDim];

  // __<dim_name>_LOC_MINMAX (for slon, slat, lon and lat)
  Tag convTag = 0;
  rval = mbImpl->tag_get_handle("__slon_LOC_MINMAX", 0, MB_TYPE_INTEGER, convTag,
                                MB_TAG_ANY);MB_CHK_SET_ERR(rval, "Trouble getting conventional tag __slon_LOC_MINMAX");
  int val[2];
  rval = mbImpl->tag_get_data(convTag, &_fileSet, 1, val);MB_CHK_SET_ERR(rval, "Trouble getting data of conventional tag __slon_LOC_MINMAX");
  lDims[0] = val[0];
  lDims[3] = val[1];

  rval = mbImpl->tag_get_handle("__slat_LOC_MINMAX", 0, MB_TYPE_INTEGER, convTag,
                                MB_TAG_ANY);MB_CHK_SET_ERR(rval, "Trouble getting conventional tag __slat_LOC_MINMAX");
  rval = mbImpl->tag_get_data(convTag, &_fileSet, 1, val);MB_CHK_SET_ERR(rval, "Trouble getting data of conventional tag __slat_LOC_MINMAX");
  lDims[1] = val[0];
  lDims[4] = val[1];

  rval = mbImpl->tag_get_handle("__lon_LOC_MINMAX", 0, MB_TYPE_INTEGER, convTag,
                                MB_TAG_ANY);MB_CHK_SET_ERR(rval, "Trouble getting conventional tag __lon_LOC_MINMAX");
  rval = mbImpl->tag_get_data(convTag, &_fileSet, 1, val);MB_CHK_SET_ERR(rval, "Trouble getting data of conventional tag __lon_LOC_MINMAX");
  lCDims[0] = val[0];
  lCDims[3] = val[1];

  rval = mbImpl->tag_get_handle("__lat_LOC_MINMAX", 0, MB_TYPE_INTEGER, convTag,
                                MB_TAG_ANY);MB_CHK_SET_ERR(rval, "Trouble getting conventional tag __lat_LOC_MINMAX");
  rval = mbImpl->tag_get_data(convTag, &_fileSet, 1, val);MB_CHK_SET_ERR(rval, "Trouble getting data of conventional tag __lat_LOC_MINMAX");
  lCDims[1] = val[0];
  lCDims[4] = val[1];

  // Get local faces
  rval = mbImpl->get_entities_by_dimension(_fileSet, 2, localCellsOwned);MB_CHK_SET_ERR(rval, "Trouble getting local faces in current file set");
  assert(!localCellsOwned.empty());

#ifdef MOAB_HAVE_MPI
  bool& isParallel = _writeNC->isParallel;
  if (isParallel) {
    ParallelComm*& myPcomm = _writeNC->myPcomm;
    int procs = myPcomm->proc_config().proc_size();
    if (procs > 1) {
      rval = myPcomm->filter_pstatus(localCellsOwned, PSTATUS_NOT_OWNED, PSTATUS_NOT);MB_CHK_SET_ERR(rval, "Trouble getting owned faces in current file set");
    }
  }
#endif

  return MB_SUCCESS;
}

ErrorCode ScdNCWriteHelper::collect_variable_data(std::vector<std::string>& var_names, std::vector<int>& tstep_nums)
{
  NCWriteHelper::collect_variable_data(var_names, tstep_nums);

  std::map<std::string, WriteNC::VarData>& varInfo = _writeNC->varInfo;

  for (size_t i = 0; i < var_names.size(); i++) {
    std::string varname = var_names[i];
    std::map<std::string, WriteNC::VarData>::iterator vit = varInfo.find(varname);
    if (vit == varInfo.end())
      MB_SET_ERR(MB_FAILURE, "Can't find variable " << varname);

    WriteNC::VarData& currentVarData = vit->second;
#ifndef NDEBUG
    std::vector<int>& varDims = currentVarData.varDims;
#endif

    // Skip set variables, which were already processed in NCWriteHelper::collect_variable_data()
    if (WriteNC::ENTLOCSET == currentVarData.entLoc)
      continue;

    // Set up writeStarts and writeCounts (maximum number of dimensions is 4)
    currentVarData.writeStarts.resize(4);
    currentVarData.writeCounts.resize(4);
    unsigned int dim_idx = 0;

    // First: time
    if (currentVarData.has_tsteps) {
      // Non-set variables with timesteps
      // 4 dimensions like (time, lev, lat, lon)
      // 3 dimensions like (time, lat, lon)
      assert(4 == varDims.size() || 3 == varDims.size());

      // Time should be the first dimension
      assert(tDim == varDims[0]);

      currentVarData.writeStarts[dim_idx] = 0; // This value is timestep dependent, will be set later
      currentVarData.writeCounts[dim_idx] = 1;
      dim_idx++;
    }
    else {
      // Non-set variables without timesteps
      // 3 dimensions like (lev, lat, lon)
      // 2 dimensions like (lat, lon)
      assert(3 == varDims.size() || 2 == varDims.size());
    }

    // Next: lev
    if (currentVarData.numLev > 0) {
      // Non-set variables with levels
      // 4 dimensions like (time, lev, lat, lon)
      // 3 dimensions like (lev, lat, lon)
      assert(4 == varDims.size() || 3 == varDims.size());

      currentVarData.writeStarts[dim_idx] = 0;
      currentVarData.writeCounts[dim_idx] = currentVarData.numLev;
      dim_idx++;
    }
    else {
      // Non-set variables without levels
      // 3 dimensions like (time, lat, lon)
      // 2 dimensions like (lat, lon)
      assert(3 == varDims.size() || 2 == varDims.size());
    }

    // Finally: lat and lon
    switch (currentVarData.entLoc) {
      case WriteNC::ENTLOCFACE:
        // Faces
        currentVarData.writeStarts[dim_idx] = lCDims[1];
        currentVarData.writeCounts[dim_idx] = lCDims[4] - lCDims[1] + 1;
        currentVarData.writeStarts[dim_idx + 1] = lCDims[0];
        currentVarData.writeCounts[dim_idx + 1] = lCDims[3] - lCDims[0] + 1;
        break;
      default:
        MB_SET_ERR(MB_FAILURE, "Unexpected entity location type for variable " << varname);
    }
    dim_idx += 2;

    // Get variable size
    currentVarData.sz = 1;
    for (std::size_t idx = 0; idx < dim_idx; idx++)
      currentVarData.sz *= currentVarData.writeCounts[idx];
  } // for (size_t i = 0; i < var_names.size(); i++)

  return MB_SUCCESS;
}

// Write CAM-EUL and CAM-FV non-set variables on non-shared quads (e.g. T)
// We assume that there are no variables on vertices and we do not support
// variables on edges (e.g. US in CAM-FV) for the time being
ErrorCode ScdNCWriteHelper::write_nonset_variables(std::vector<WriteNC::VarData>& vdatas, std::vector<int>& tstep_nums)
{
  Interface*& mbImpl = _writeNC->mbImpl;

  int success;

  // For each indexed variable tag, write a time step data
  for (unsigned int i = 0; i < vdatas.size(); i++) {
    WriteNC::VarData& variableData = vdatas[i];

    // Assume this variable is on faces for the time being
    switch (variableData.entLoc) {
      case WriteNC::ENTLOCFACE:
        // Faces
        break;
      default:
        MB_SET_ERR(MB_FAILURE, "Unexpected entity location type for variable " << variableData.varName);
    }

    unsigned int num_timesteps;
    unsigned int lat_idx = 0;
    unsigned int lon_idx = 1;
    if (variableData.has_tsteps) {
      // Non-set variables with timesteps
      // 4 dimensions like (time, lev, lat, lon)
      // 3 dimensions like (time, lat, lon)
      num_timesteps = tstep_nums.size();
      lat_idx++;
      lon_idx++;
    }
    else {
      // Non-set variables without timesteps
      // 3 dimensions like (lev, lat, lon)
      // 2 dimensions like (lat, lon)
      num_timesteps = 1;
    }

    unsigned int num_lev;
    if (variableData.numLev > 0) {
      // Non-set variables with levels
      // 4 dimensions like (time, lev, lat, lon)
      // 3 dimensions like (lev, lat, lon)
      num_lev = variableData.numLev;
      lat_idx++;
      lon_idx++;
    }
    else {
      // Non-set variables without levels
      // 3 dimensions like (time, lat, lon)
      // 2 dimensions like (lat, lon)
      num_lev = 1;
    }

    size_t ni = variableData.writeCounts[lon_idx]; // lon
    size_t nj = variableData.writeCounts[lat_idx]; // lat

    // At each timestep, we need to transpose tag format (lat, lon, lev) back
    // to NC format (lev, lat, lon) for writing
    for (unsigned int t = 0; t < num_timesteps; t++) {
      // We will write one time step, and count will be one; start will be different
      // Use tag_iterate to get tag data (assume that localCellsOwned is contiguous)
      // We should also transpose for level so that means deep copy for transpose
      if (tDim == variableData.varDims[0])
        variableData.writeStarts[0] = t; // This is start for time
      int count;
      void* dataptr;
      ErrorCode rval = mbImpl->tag_iterate(variableData.varTags[t], localCellsOwned.begin(), localCellsOwned.end(),
                                           count, dataptr);MB_CHK_SET_ERR(rval, "Failed to iterate tag on owned faces");
      assert(count == (int)localCellsOwned.size());

      // Now transpose and write tag data
      // Use collective I/O mode put (synchronous write) for the time being, we can try
      // nonblocking put (request aggregation) later
      switch (variableData.varDataType) {
        case NC_DOUBLE: {
          std::vector<double> tmpdoubledata(ni*nj*num_lev);
          if (num_lev > 1)
            // Transpose (lat, lon, lev) back to (lev, lat, lon)
            jik_to_kji(ni, nj, num_lev, &tmpdoubledata[0], (double*)(dataptr));
          success = NCFUNCAP(_vara_double)(_fileId, variableData.varId,
                    &variableData.writeStarts[0], &variableData.writeCounts[0],
                    &tmpdoubledata[0]);
          if (success)
            MB_SET_ERR(MB_FAILURE, "Failed to write double data for variable " << variableData.varName);
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
