//-------------------------------------------------------------------------
// Filename      : ReadNC.hpp
//
// Purpose       : Climate NC file reader
//
// Creator       : Tim Tautges
//-------------------------------------------------------------------------

#ifndef READNC_HPP
#define READNC_HPP

#ifndef IS_BUILDING_MB
#error "ReadNC.hpp isn't supposed to be included into an application"
#endif

#include <vector>
#include <map>
#include <set>
#include <string>

#include "moab/ReaderIface.hpp"
#include "moab/ScdInterface.hpp"
#include "DebugOutput.hpp"

#ifdef MOAB_HAVE_MPI
#include "moab_mpi.h"
#include "moab/ParallelComm.hpp"
#endif 

#ifdef MOAB_HAVE_PNETCDF
#include "pnetcdf.h"
#define NCFUNC(func) ncmpi_ ## func

//! Collective I/O mode get
#define NCFUNCAG(func) ncmpi_get ## func ## _all

//! Independent I/O mode get
#define NCFUNCG(func) ncmpi_get ## func

//! Nonblocking get (request aggregation), used so far only for ucd mesh
#define NCFUNCREQG(func) ncmpi_iget ## func

#define NCDF_SIZE MPI_Offset
#define NCDF_DIFF MPI_Offset
#else
#include "netcdf.h"
#define NCFUNC(func) nc_ ## func
#define NCFUNCAG(func) nc_get ## func
#define NCFUNCG(func) nc_get ## func
#define NCDF_SIZE size_t
#define NCDF_DIFF ptrdiff_t
#endif

namespace moab {

class ReadUtilIface;
class ScdInterface;
class NCHelper;

//! Output Exodus File for VERDE
class ReadNC : public ReaderIface
{
  friend class NCHelper;
  friend class ScdNCHelper;
  friend class UcdNCHelper;
  friend class NCHelperEuler;
  friend class NCHelperFV;
  friend class NCHelperHOMME;
  friend class NCHelperMPAS;
  friend class NCHelperGCRM;

public:

  static ReaderIface* factory(Interface*);

  //! Load an NC file
  ErrorCode load_file(const char* file_name,
                       const EntityHandle* file_set,
                       const FileOptions& opts,
                       const SubsetList* subset_list = 0,
                       const Tag* file_id_tag = 0);

  //! Constructor
  ReadNC(Interface* impl = NULL);

  //! Destructor
  virtual ~ReadNC();

  virtual ErrorCode read_tag_values(const char* file_name,
                                    const char* tag_name,
                                    const FileOptions& opts,
                                    std::vector<int>& tag_values_out,
                                    const SubsetList* subset_list = 0);

  //! ENTLOCNSEDGE for north/south edge
  //! ENTLOCWEEDGE for west/east edge
  enum EntityLocation {ENTLOCVERT = 0, ENTLOCNSEDGE, ENTLOCEWEDGE, ENTLOCFACE, ENTLOCSET, ENTLOCEDGE, ENTLOCREGION};

private:

  class AttData
  {
    public:
    AttData() : attId(-1), attLen(0), attVarId(-2) {}
    int attId;
    NCDF_SIZE attLen;
    int attVarId;
    nc_type attDataType;
    std::string attName;
  };

  class VarData
  {
    public:
    VarData() : varId(-1), numAtts(-1), entLoc(ENTLOCSET), numLev(0), sz(0), has_tsteps(false) {}
    int varId;
    int numAtts;
    nc_type varDataType;
    std::vector<int> varDims; // The dimension indices making up this multi-dimensional variable
    std::map<std::string, AttData> varAtts;
    std::string varName;
    std::vector<Tag> varTags; // Tags created for this variable, e.g. one tag per timestep
    std::vector<void*> varDatas;
    std::vector<NCDF_SIZE> readStarts; // Starting index for reading data values along each dimension
    std::vector<NCDF_SIZE> readCounts; // Number of data values to be read along each dimension
    int entLoc;
    int numLev;
    int sz;
    bool has_tsteps; // Indicate whether timestep numbers are appended to tag names
  };

  ReadUtilIface* readMeshIface;

  //! Read the header information
  ErrorCode read_header();

  //! Get all global attributes in the file
  ErrorCode get_attributes(int var_id, int num_atts, std::map<std::string, AttData>& atts,
                           const char* prefix = "");

  //! Get all dimensions in the file
  ErrorCode get_dimensions(int file_id, std::vector<std::string>& dim_names, std::vector<int>& dim_lens);

  //! Get the variable names and other info defined for this file
  ErrorCode get_variables();

  ErrorCode parse_options(const FileOptions& opts,
                          std::vector<std::string>& var_names,
                          std::vector<int>& tstep_nums,
                          std::vector<double>& tstep_vals);

//------------member variables ------------//

  //! Interface instance
  Interface* mbImpl;

  //! File name
  std::string fileName;

  //! File numbers assigned by netcdf
  int fileId;

  //! Dimension names
  std::vector<std::string> dimNames;

  //! Dimension lengths
  std::vector<int> dimLens;

  //! Global attribs
  std::map<std::string, AttData> globalAtts;

  //! Variable info
  std::map<std::string, VarData> varInfo;

  //! Cached tags for reading. Note that all these tags are defined when the
  //! core is initialized.
  Tag mGlobalIdTag;

  //! This is a pointer to the file id tag that is passed from ReadParallel
  //! it gets deleted at the end of resolve sharing, but it will have same data
  //! as the global id tag
  //! global id tag is preserved, and is needed later on.
  const Tag* mpFileIdTag;

  //! Debug stuff
  DebugOutput dbgOut;

  //! Are we reading in parallel?
  bool isParallel;

  //! Partitioning method
  int partMethod;

  //! Scd interface
  ScdInterface* scdi;

  //! Parallel data object, to be cached with ScdBox
  ScdParData parData;

#ifdef MOAB_HAVE_MPI
  ParallelComm* myPcomm;
#endif

  //! Read options
  bool noMesh;
  bool noVars;
  bool spectralMesh;
  bool noMixedElements;
  bool noEdges;
  int gatherSetRank;
  int tStepBase;
  int trivialPartitionShift;

  //! Helper class instance
  NCHelper* myHelper;
};

} // namespace moab

#endif
