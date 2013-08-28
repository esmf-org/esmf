#ifndef READIDEAS_HPP
#define READIDEAS_HPP
                  
#ifndef IS_BUILDING_MB
  #error "ReadIDEAS.hpp isn't supposed to be included into an application"
#endif

#include <iostream>
#include <fstream>
#include <vector>

#include "moab/ReaderIface.hpp"
#include "moab/Interface.hpp"
#include "moab/RangeMap.hpp"

#define MAT_PROP_TABLE_TAG "mat_prop_table"
#define PHYS_PROP_TABLE_TAG "phys_prop_table"

namespace moab {

class ReadUtilIface;

class ReadIDEAS : public ReaderIface
{

public:

  static ReaderIface* factory( Interface* );

  ErrorCode load_file( const char* file_name,
                       const EntityHandle* file_set,
                       const FileOptions& opts,
                       const SubsetList* subset_list = 0,
                       const Tag* file_id_tag = 0 );

  ErrorCode read_tag_values( const char* file_name,
                             const char* tag_name,
                             const FileOptions& opts,
                             std::vector<int>& tag_values_out,
                             const SubsetList* subset_list = 0 );
  
  //! Constructor
  ReadIDEAS(Interface* impl = NULL);
  
  //! Destructor
  virtual ~ReadIDEAS() {}

protected:
  
  ErrorCode skip_header();
  ErrorCode create_vertices(EntityHandle& first_vertex, const Tag* file_id_tag);
  ErrorCode create_elements(EntityHandle first_vertex, const Tag* file_id_tag);
  
private:
  
  std::ifstream file;
  RangeMap<int, EntityHandle> nodeIdMap;
  
  // Read mesh interface
  ReadUtilIface* readMeshIface;
  
  // MOAB Interface
  Interface* MBI;


  /* Universal dataset numbers
     An integer describes a chunk of information. These chunks include headers,
     units, nodes, elements, patches, etc... described in the OpenFOAM IDEAS 
     reader.

     1) http://amira.zib.de/usersguide31/hxideas/HxFileFormat_IDEAS.html
     55,2414 data at nodes

     2) http://www.sdrl.uc.edu/universal-file-formats-for-modal-analysis-testing-1
     /file-format-storehouse/unv_0015.htm/
     15 nodes with single precision coordinates
     line1 (4I10,1P3E13.5): node_label coord_sys_num displacement_sys_num 
     color x y z */
  static const unsigned SINGLE_PRECISION_NODES = 15;

  /* 3) http://www.sdrl.uc.edu/pdf/test_universal_file_formats.pdf
     781,2411 nodes with double precision coordinates
     line1 (4I10):       node_label coord_sys_num displacement_sys_num color
     line2 (1P3D25.16):  x y z */
  static const unsigned DOUBLE_PRECISION_NODES0 = 781;
  static const unsigned DOUBLE_PRECISION_NODES1 = 2411;

  /* 4) http://www.sdrl.uc.edu/universal-file-formats-for-modal-analysis-testing-1
     /file-format-storehouse/unv_0780.htm/
     71, 780, 2412 element definitions
     line1 (8I10): element_label fe_id phys_prop_bin_num phys_prop_num 
     mat_prop_bin_num mat_prop_num color num_of_nodes
     line2 (8I10): connectivity_node_labels */
  static const unsigned ELEMENTS0 = 71;
  static const unsigned ELEMENTS1 = 780;
  static const unsigned ELEMENTS2 = 2412;

  /* Mesh elements exist inside chunks 71, 780, and 2412. Each element definition 
     includes the finite element id that describes the element type. These are
     used in the OpenFOAM IDEAS reader. The canonical node ordering matches that
     of MBCN, as suggested by the Gmsh 2.2.3 source code.*/
  static const int ROD0  = 11;
  static const int ROD1  = 171;
  static const int TRI0  = 41;
  static const int TRI1  = 91;
  static const int QUAD0 = 44;
  static const int QUAD1 = 94;
  static const int TET   = 111;
  static const int WEDGE = 112;     
  static const int HEX   = 115;

};

} // namespace moab
#endif
