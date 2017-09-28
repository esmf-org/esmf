/*
 * NCWriteHelper.hpp
 *
 * Purpose       : Climate NC writer file helper; abstract, will be implemented for each type
 *
 *  Created on: Mar 28, 2014
 */

#ifndef NCWRITEHELPER_HPP_
#define NCWRITEHELPER_HPP_
#include "WriteNC.hpp"

#ifdef WIN32
#ifdef size_t
#undef size_t
#endif
#endif

namespace moab {

class NCWriteHelper
{
public:
  NCWriteHelper(WriteNC* writeNC, int fileId, const FileOptions& opts, EntityHandle fileSet)
: _writeNC(writeNC), _fileId(fileId), _opts(opts), _fileSet(fileSet),
  nTimeSteps(0), nLevels(1), tDim(-1), levDim(-1) {}
  virtual ~NCWriteHelper() {};

  //! Get appropriate helper instance for WriteNC class based on some info in the file set
  static NCWriteHelper* get_nc_helper(WriteNC* writeNC, int fileId, const FileOptions& opts, EntityHandle fileSet);

  //! Collect necessary info about local mesh (implemented in child classes)
  virtual ErrorCode collect_mesh_info() = 0;

  //! Collect data for specified variables (partially implemented in child classes)
  virtual ErrorCode collect_variable_data(std::vector<std::string>& var_names, std::vector<int>& tstep_nums);

  //! Initialize file: this is where all defines are done
  //! The VarData dimension ids are filled up after define
  ErrorCode init_file(std::vector<std::string>& var_names, std::vector<std::string>& desired_names, bool _append);

  //! Take the info from VarData and write first non-set variables, then set variables
  ErrorCode write_values(std::vector<std::string>& var_names, std::vector<int>& tstep_nums);

private:
  // Write set variables (common to scd mesh and ucd mesh)
  ErrorCode write_set_variables(std::vector<WriteNC::VarData>& vsetdatas, std::vector<int>& tstep_nums);

protected:
  // Write non-set variables (implemented in child classes)
  virtual ErrorCode write_nonset_variables(std::vector<WriteNC::VarData>& vdatas, std::vector<int>& tstep_nums) = 0;

  //! Allow NCWriteHelper to directly access members of WriteNC
  WriteNC* _writeNC;

  //! Cache some information from WriteNC
  int _fileId;
  const FileOptions& _opts;
  EntityHandle _fileSet;

  //! Dimensions of time and level
  int nTimeSteps, nLevels;

  //! Dimension numbers for time and level
  int tDim, levDim;

  //! Local owned cells, edges and vertices
  Range localCellsOwned, localEdgesOwned, localVertsOwned;

  //! Time values of output timesteps
  std::vector<double> timeStepVals;
};

//! Child helper class for scd mesh, e.g. CAM_EL or CAM_FV
class ScdNCWriteHelper : public NCWriteHelper
{
public:
  ScdNCWriteHelper(WriteNC* writeNC, int fileId, const FileOptions& opts, EntityHandle fileSet)
: NCWriteHelper(writeNC, fileId, opts, fileSet)
  {
    for (unsigned int i = 0; i < 6; i++) {
      lDims[i] = -1;
      lCDims[i] = -1;
    }
  }
  virtual ~ScdNCWriteHelper() {}

private:
  //! Implementation of NCWriteHelper::collect_mesh_info()
  virtual ErrorCode collect_mesh_info();

  //! Collect data for specified variables
  virtual ErrorCode collect_variable_data(std::vector<std::string>& var_names, std::vector<int>& tstep_nums);

  //! Implementation of NCWriteHelper::write_nonset_variables()
  virtual ErrorCode write_nonset_variables(std::vector<WriteNC::VarData>& vdatas, std::vector<int>& tstep_nums);

  template <typename T> void jik_to_kji(size_t ni, size_t nj, size_t nk, T* dest, T* source)
  {
    size_t nik = ni * nk, nij = ni * nj;
    for (std::size_t k = 0; k != nk; k++)
      for (std::size_t j = 0; j != nj; j++)
        for (std::size_t i = 0; i != ni; i++)
          dest[k*nij + j*ni + i] = source[j*nik + i*nk + k];
  }

protected:
  //! Dimensions of my local part of grid
  int lDims[6];

  //! Center dimensions of my local part of grid
  int lCDims[6];
};

//! Child helper class for ucd mesh, e.g. CAM_SE (HOMME) or MPAS
class UcdNCWriteHelper : public NCWriteHelper
{
public:
  UcdNCWriteHelper(WriteNC* writeNC, int fileId, const FileOptions& opts, EntityHandle fileSet)
: NCWriteHelper(writeNC, fileId, opts, fileSet),
  cDim(-1), eDim(-1), vDim(-1) {}
  virtual ~UcdNCWriteHelper() {}

protected:
  //! This version takes as input the moab range, from which we actually need just the
  //! size of each sequence, for a proper transpose of the data
  template <typename T> void jik_to_kji_stride(size_t , size_t nj, size_t nk, T* dest, T* source, Range& localGid)
  {
    std::size_t idxInSource = 0; // Position of the start of the stride
    // For each subrange, we will transpose a matrix of size
    // subrange*nj*nk (subrange takes the role of ni)
    for (Range::pair_iterator pair_iter = localGid.pair_begin();
        pair_iter != localGid.pair_end(); ++pair_iter) {
      std::size_t size_range = pair_iter->second - pair_iter->first + 1;
      std::size_t nik = size_range * nk, nij = size_range * nj;
      for (std::size_t k = 0; k != nk; k++)
        for (std::size_t j = 0; j != nj; j++)
          for (std::size_t i = 0; i != size_range; i++)
            dest[idxInSource + k*nij + j*size_range + i] = source[idxInSource + j*nik + i*nk + k];
      idxInSource += (size_range*nj*nk);
    }
  }

  //! Dimension numbers for nCells, nEdges and nVertices
  int cDim, eDim, vDim;

  //! Local global ID for owned cells, edges and vertices
  Range localGidCellsOwned, localGidEdgesOwned, localGidVertsOwned;
};

} // namespace moab

#endif
