#ifndef READ_TEMPLATE_HPP
#define READ_TEMPLATE_HPP

#include "moab/ReaderIface.hpp"
#include "moab/Range.hpp"

namespace moab {

class ReadUtilIface;
class Interface;

/**
 * \brief Template for implementing new file readers in MOAB
 * This class is a template for writing new file readers in MOAB.  This shows how to efficiently create
 * vertices and elements using the ReadUtilIface class, and to translate indices in connectivity lists
 * into vertex handles created during the read.
 *
 * After writing the new reader class, you should also modify src/ReaderWriterSet.cpp, to register the
 * new reader along with the file extensions that it reads.  This will turn on automatic creating of this
 * reader based on file extension, which is done in Core::serial_load_file.
 */
class ReadTemplate : public ReaderIface
{
   
public:

    //! factory method 
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
  ReadTemplate(Interface* impl = NULL);

   //! Destructor
  virtual ~ReadTemplate();

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
  
  ReadUtilIface* readMeshIface;

    //! interface instance
  Interface* mbImpl;

  const char *fileName;
  
};

} // namespace moab

#endif
