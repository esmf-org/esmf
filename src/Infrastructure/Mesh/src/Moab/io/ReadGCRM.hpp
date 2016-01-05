#ifndef READGCRM_HPP
#define READGCRM_HPP

#ifndef IS_BUILDING_MB
//#error "ReadNC.hpp isn't supposed to be included into an application"
#endif

#include <vector>
#include <map>
#include <string>

#include "moab/Forward.hpp"
#include "moab/ReaderIface.hpp"
#include "moab/Range.hpp"
#include "moab/ScdInterface.hpp"
#include "DebugOutput.hpp"

#ifdef USE_MPI
#  include "moab_mpi.h"
#  include "moab/ParallelComm.hpp"
#endif 

#ifdef PNETCDF_FILE
#  include "pnetcdf.h"
#  define NCFUNC(func) ncmpi_ ## func
#  define NCFUNCA(func) ncmpi_ ## func ## _all
// keep it this way , introduce another macro, used so far only for ucd mesh
//#  define NCASYNCH
#  ifdef NCASYNCH
#    define NCREQ , &requests[j]
#    define NCFUNCAG(func) ncmpi_iget ## func 
#    define NCWAIT
#  else
#    define NCREQ2 , &requests[idxReq++]
#    define NCFUNCAG2(func) ncmpi_iget ## func
#    define NCREQ 
#    define NCFUNCAG(func) ncmpi_get ## func ## _all
#  endif
#  define NCDF_SIZE MPI_Offset
#  define NCDF_DIFF MPI_Offset
#else
#  include "netcdf.h"
#define NCREQ
#define NCGET get
#  define NCFUNC(func) nc_ ## func
#  define NCFUNCA(func) nc_ ## func
#  define NCFUNCAG(func) nc_get ## func
#  define NCDF_SIZE size_t
#  define NCDF_DIFF ptrdiff_t
#endif

namespace moab {

class ReadUtilIface;

/**
 * \brief GCRM for implementing new file readers in MOAB
 * This class is a GCRM for writing new file readers in MOAB.  This shows how to efficiently create
 * vertices and elements using the ReadUtilIface class, and to translate indices in connectivity lists
 * into vertex handles created during the read.
 *
 * After writing the new reader class, you should also modify src/ReaderWriterSet.cpp, to register the
 * new reader along with the file extensions that it reads.  This will turn on automatic creating of this
 * reader based on file extension, which is done in Core::serial_load_file.
 */
class ReadGCRM : public ReaderIface
{
   
public:

    //! factory method 
  static ReaderIface* factory( Interface* );

  virtual ErrorCode load_file( const char* file_name,
                               const EntityHandle* file_set,
                               const FileOptions& opts,
                               const SubsetList* subset_list = 0,
                               const Tag* file_id_tag = 0 );

  virtual ErrorCode read_tag_values( const char* file_name,
                                     const char* tag_name,
                                     const FileOptions& opts,
                                     std::vector<int>& tag_values_out,
                                     const SubsetList* subset_list = 0 );
  
    //! Constructor
  ReadGCRM(Interface* impl = NULL);

   //! Destructor
  virtual ~ReadGCRM();

  // ENTLOCNSEDGE for north/south edge
  // ENTLOCWEEDGE for west/east edge
  enum EntityLocation {ENTLOCNODE=0, ENTLOCNSEDGE, ENTLOCEWEDGE, ENTLOCQUAD, ENTLOCSET};

private:

    /** \brief Read vertex data and create vertices in MOAB database
     * \param num_verts Number of vertices to be read
     * \param start_vertex Starting vertex handle; used later to offset connectivity indices
     * \param read_ents Range storing all entities read from this file
     */
  ErrorCode read_vertices(int num_verts, EntityHandle &start_vertex, Range &read_ents);
  
    /** \brief Read element data and create elements in MOAB database
     * \param num_elems Number of elements to be read
     * \param start_vertex Starting vertex handle; used to offset connectivity indices
     * \param start_elem Starting element handle; may be used later to offset set entities
     * \param read_ents Range storing all entities read from this file
     */
  ErrorCode read_elements(int num_elems, EntityHandle start_vertex,
                          EntityHandle &start_elem, Range &read_ents);

    /** \brief Read entity set data and create/populate sets in MOAB database
     * \param num_sets Number of sets to be read
     * \param start_vertex Starting vertex handle
     * \param num_verts Total number of vertices read from file
     * \param start_elem Starting element handle
     * \param num_elems Total number of elements read from file
     * \param read_ents Range storing all entities read from this file
     */
  ErrorCode create_sets(int num_sets, EntityHandle start_vertex, int num_verts, 
                        EntityHandle start_elem, int num_elems, Range &read_ents);
  
  
    /** \brief Process options passed into the reader
     * \param opts Options passed into this read
     */
  ErrorCode process_options(const FileOptions &opts);
  
  void reset();

    //! read the header information
  ErrorCode read_header();

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
    VarData() : varId(-1), numAtts(-1), read(false), entLoc(ENTLOCSET), numLev(1), sz(0), has_t(false) {}
    int varId;
    int numAtts;
    nc_type varDataType;
    std::vector<int> varDims; // the dimension indices making up this multi-dimensional variable
    std::map<std::string,AttData> varAtts;
    std::string varName;
    bool read;
    std::vector<Tag> varTags; // one tag for each timestep, varTags[t]
    std::vector<void*> varDatas;
    std::vector<std::vector<NCDF_SIZE> > readDims; // start value for this [t][dim]
    std::vector<std::vector<NCDF_SIZE> > readCounts; // number of data values for this [t][dim]
    int entLoc;
    int numLev;
    int sz;
    bool has_t;
  };

    //! get all global attributes in the file
  ErrorCode get_attributes(int var_id, int num_atts, std::map<std::string,AttData> &atts,
                           const char *prefix="");
  
    //! get all dimensions in the file
  ErrorCode get_dimensions(int file_id, std::vector<std::string> &dim_names, std::vector<int> &dim_vals);

    //! get the variable names and other info defined for this file
  ErrorCode get_variables();
  
    //! number of dimensions in this nc file
  unsigned int number_dimensions();

  ErrorCode get_tag(VarData &var_data, int tstep_num, Tag &tagh, int num_lev);
  
    //! create a character string attString of attMap.  with '\0'
    //! terminating each attribute name, ';' separating the data type
    //! and value, and ';' separating one name/data type/value from
    //! the next'.  attLen stores the end postion for each name/data
    //! type/ value.
  ErrorCode create_attrib_string(const std::map<std::string, AttData>& attMap, 
				 std::string& attString,
				 std::vector<int>& attLen);

  ErrorCode create_tags(ScdInterface *scdi, EntityHandle file_set, const std::vector<int>& tstep_nums);
  
  ReadUtilIface* readMeshIface;

  int CPU_WORD_SIZE;
  int IO_WORD_SIZE;

    //! interface instance
  Interface* mbImpl;

  const char *fileName;
  
    //! file numbers assigned by netcdf
  int fileId, connectId;
  
    //! dimensions
  std::vector<std::string> dimNames;
  std::vector<int> dimVals;

    //! global attribs
  std::map<std::string,AttData> globalAtts;
  
    //! variable info
  std::map<std::string,VarData> varInfo;

  int tMin, tMax;
  
    //! values for t
  std::vector<double> tVals;

    //! dimension numbers for i, j, t
  int tDim;

    //! number of the dimension of unlimited dimension, if any
  int numUnLim;

    //! Meshset Handle for the mesh that is currently being read
  EntityHandle mCurrentMeshHandle;

    //! starting vertex and element handles for this read
  EntityHandle startVertex, startElem;

  //! Cached tags for reading.  Note that all these tags are defined when the
  //! core is initialized.
  Tag mGlobalIdTag;

  int max_line_length, max_str_length;

    //! range of entities in initial mesh, before this read
  Range initRange;

    //! offset of first vertex id
  int vertexOffset;

    //! debug stuff
  DebugOutput dbgOut;

    //! partitioning method
  int partMethod;

    //! are we reading in parallel?
  bool isParallel;

#ifdef USE_MPI
  ParallelComm *myPcomm;
#endif
  
};

inline unsigned int ReadGCRM::number_dimensions() 
{
  return dimVals.size();
}

} // namespace moab

#endif
