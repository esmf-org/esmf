//-------------------------------------------------------------------------
// Filename      : NCHelper.hpp
//
// Purpose       : Climate NC file helper
//
// Creator       : Danqing Wu
//-------------------------------------------------------------------------

#ifndef NCHELPER_HPP
#define NCHELPER_HPP

#include "ReadNC.hpp"

#ifdef WIN32
#ifdef size_t
#undef size_t
#endif
#endif

namespace moab {

//! Helper class to isolate reading of several different nc file formats
class NCHelper
{
public:
  NCHelper(ReadNC* readNC, int fileId, const FileOptions& opts, EntityHandle fileSet)
:_readNC(readNC), _fileId(fileId), _opts(opts), _fileSet(fileSet),
  nTimeSteps(0), nLevels(1), tDim(-1), levDim(-1) {}
  virtual ~NCHelper() {}

  //! Get appropriate helper instance for ReadNC class
  static NCHelper* get_nc_helper(ReadNC* readNC, int fileId, const FileOptions& opts, EntityHandle fileSet);

  //! Interfaces to be implemented in child classes
  virtual ErrorCode init_mesh_vals() = 0;
  virtual ErrorCode check_existing_mesh() = 0;
  virtual ErrorCode create_mesh(Range& faces) = 0;
  virtual ErrorCode read_variables(std::vector<std::string>& var_names, std::vector<int>& tstep_nums) = 0;
  virtual std::string get_mesh_type_name() = 0;

  //! Create NC conventional tags
  ErrorCode create_conventional_tags(const std::vector<int>& tstep_nums);

  //! Update time tag values if timesteps spread across files
  ErrorCode update_time_tag_vals();

protected:
  //! Separate set and non-set variables (common to scd mesh and ucd mesh)
  ErrorCode read_variables_setup(std::vector<std::string>& var_names,
                                            std::vector<int>& tstep_nums,
                                            std::vector<ReadNC::VarData>& vdatas,
                                            std::vector<ReadNC::VarData>& vsetdatas);

  //! Read set variables (common to scd mesh and ucd mesh)
  ErrorCode read_variables_to_set(std::vector<ReadNC::VarData>& vdatas, std::vector<int>& tstep_nums);

  ErrorCode read_coordinate(const char* var_name, int lmin, int lmax,
                            std::vector<double>& cvals);

  ErrorCode get_tag_to_set(ReadNC::VarData& var_data, int tstep_num, Tag& tagh);

  ErrorCode get_tag_to_nonset(ReadNC::VarData& var_data, int tstep_num, Tag& tagh, int num_lev);

  //! Create a character string attString of attMap. with '\0'
  //! terminating each attribute name, ';' separating the data type
  //! and value, and ';' separating one name/data type/value from
  //! the next'. attLen stores the end position for each name/data
  //! type/ value.
  ErrorCode create_attrib_string(const std::map<std::string, ReadNC::AttData>& attMap,
                                 std::string& attString,
                                 std::vector<int>& attLen);

  //! For a dimension that does not have a corresponding coordinate variable (e.g. ncol for HOMME),
  //! create a dummy variable with a sparse tag to store the dimension length
  ErrorCode create_dummy_variables();

private:
  //! Used by read_variables_to_set()
  ErrorCode read_variables_to_set_allocate(std::vector<ReadNC::VarData>& vdatas, std::vector<int>& tstep_nums);

protected:
  //! Allow NCHelper to directly access members of ReadNC
  ReadNC* _readNC;

  //! Cache some information from ReadNC
  int _fileId;
  const FileOptions& _opts;
  EntityHandle _fileSet;

  //! Dimensions of time and level
  int nTimeSteps, nLevels;

  //! Values for time and level
  std::vector<double> tVals, levVals;

  //! Dimension numbers for time and level
  int tDim, levDim;

  //! Ignored variables
  std::set<std::string> ignoredVarNames;

  //! Dummy variables
  std::set<std::string> dummyVarNames;
};

//! Child helper class for scd mesh, e.g. CAM_EL or CAM_FV
class ScdNCHelper : public NCHelper
{
public:
  ScdNCHelper(ReadNC* readNC, int fileId, const FileOptions& opts, EntityHandle fileSet)
: NCHelper(readNC, fileId, opts, fileSet),
  iDim(-1), jDim(-1), iCDim(-1), jCDim(-1)
  {
    for (unsigned int i = 0; i < 6; i++) {
      gDims[i] = -1;
      lDims[i] = -1;
      gCDims[i] = -1;
      lCDims[i] = -1;
    }

    locallyPeriodic[0] = locallyPeriodic[1] = locallyPeriodic[2] = 0;
    globallyPeriodic[0] = globallyPeriodic[1] = globallyPeriodic[2] = 0;
  }
  virtual ~ScdNCHelper() {}

private:
  //! Implementation of NCHelper::check_existing_mesh()
  virtual ErrorCode check_existing_mesh();
  //! Implementation of NCHelper::create_mesh()
  virtual ErrorCode create_mesh(Range& faces);
  //! Implementation of NCHelper::read_variables()
  virtual ErrorCode read_variables(std::vector<std::string>& var_names, std::vector<int>& tstep_nums);

  //! Read non-set variables for scd mesh
  ErrorCode read_scd_variables_to_nonset_allocate(std::vector<ReadNC::VarData>& vdatas,
                                                 std::vector<int>& tstep_nums);
  ErrorCode read_scd_variables_to_nonset(std::vector<ReadNC::VarData>& vdatas,
                                        std::vector<int>& tstep_nums);

  //! Create COORDS tag for quads coordinate
  ErrorCode create_quad_coordinate_tag();

  template <typename T> void kji_to_jik(size_t ni, size_t nj, size_t nk, void* dest, T* source)
  {
    size_t nik = ni * nk, nij = ni * nj;
    T* tmp_data = reinterpret_cast<T*>(dest);
    for (std::size_t j = 0; j != nj; j++)
      for (std::size_t i = 0; i != ni; i++)
        for (std::size_t k = 0; k != nk; k++)
          tmp_data[j*nik + i*nk + k] = source[k*nij + j*ni + i];
  }

protected:
  //! Dimensions of global grid in file
  int gDims[6];

  //! Dimensions of my local part of grid
  int lDims[6];

  //! Center dimensions of global grid in file
  int gCDims[6];

  //! Center dimensions of my local part of grid
  int lCDims[6];

  //! Values for i/j
  std::vector<double> ilVals, jlVals;

  //! Center values for i/j
  std::vector<double> ilCVals, jlCVals;

  //! Dimension numbers for i/j
  int iDim, jDim;

  //! Center dimension numbers for i/j
  int iCDim, jCDim;

  //! Whether mesh is locally periodic in i or j or k
  int locallyPeriodic[3];

  //! Whether mesh is globally periodic in i or j or k
  int globallyPeriodic[3];
};

//! Child helper class for ucd mesh, e.g. CAM_SE (HOMME) or MPAS
class UcdNCHelper : public NCHelper
{
public:
  UcdNCHelper(ReadNC* readNC, int fileId, const FileOptions& opts, EntityHandle fileSet)
: NCHelper(readNC, fileId, opts, fileSet),
  nCells(0), nEdges(0), nVertices(0),
  nLocalCells(0), nLocalEdges(0), nLocalVertices(0),
  cDim(-1), eDim(-1), vDim(-1) {}
  virtual ~UcdNCHelper() {}

private:
  //! Implementation of NCHelper::read_variables()
  virtual ErrorCode read_variables(std::vector<std::string>& var_names,
                                   std::vector<int> &tstep_nums);

  //! Read non-set variables for ucd mesh (implemented differently in child classes)
  virtual ErrorCode read_ucd_variables_to_nonset_allocate(std::vector<ReadNC::VarData>& vdatas,
                                                         std::vector<int>& tstep_nums) = 0;
#ifdef MOAB_HAVE_PNETCDF
  virtual ErrorCode read_ucd_variables_to_nonset_async(std::vector<ReadNC::VarData>& vdatas,
                                                      std::vector<int>& tstep_nums) = 0;
#else
  virtual ErrorCode read_ucd_variables_to_nonset(std::vector<ReadNC::VarData>& vdatas,
                                                std::vector<int>& tstep_nums) = 0;
#endif

protected:
  //! This version takes as input the moab range, from which we actually need just the
  //! size of each sequence, for a proper transpose of the data
  template <typename T> void kji_to_jik_stride(size_t , size_t nj, size_t nk, void* dest, T* source, Range& localGid)
  {
    std::size_t idxInSource = 0; // Position of the start of the stride
    // For each subrange, we will transpose a matrix of size
    // subrange*nj*nk (subrange takes the role of ni)
    T* tmp_data = reinterpret_cast<T*>(dest);
    for (Range::pair_iterator pair_iter = localGid.pair_begin();
        pair_iter != localGid.pair_end(); ++pair_iter) {
      std::size_t size_range = pair_iter->second - pair_iter->first + 1;
      std::size_t nik = size_range * nk, nij = size_range * nj;
      for (std::size_t j = 0; j != nj; j++)
        for (std::size_t i = 0; i != size_range; i++)
          for (std::size_t k = 0; k != nk; k++)
            tmp_data[idxInSource + j*nik + i*nk + k] = source[idxInSource + k*nij + j*size_range + i];
      idxInSource += (size_range*nj*nk);
    }
  }

  //! Dimensions of global grid in file
  int nCells;
  int nEdges;
  int nVertices;

  //! Dimensions of my local part of grid
  int nLocalCells;
  int nLocalEdges;
  int nLocalVertices;

  //! Coordinate values for vertices
  std::vector<double> xVertVals, yVertVals, zVertVals;

  //! Dimension numbers for nCells, nEdges and nVertices
  int cDim, eDim, vDim;

  //! Local global ID for cells, edges and vertices
  Range localGidCells, localGidEdges, localGidVerts;
};

} // namespace moab

#endif
