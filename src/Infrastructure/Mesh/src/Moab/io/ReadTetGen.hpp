#ifndef READ_TET_GEN_HPP
#define READ_TET_GEN_HPP

#include <iosfwd>
#include "moab/Forward.hpp"
#include "moab/ReaderIface.hpp"
#include <string>

namespace moab {

class ReadUtilIface;

/* TetGen mesh data is typically split into two or three files:
 * name.node : node data
 * name.ele  : tet data
 * name.face : tri data
 * The reader will attempt to guess the correct file names from
 * the single input file name unless explicit file names are
 * specified using file options as described below.
 *
 * File options:
 *   NODE_FILE=filename   : node file name
 *   ELE_FILE[=filename]  : require tet file and optionally specify file name
 *   FACE_FILE[=filename] : reequire tri file and optionally specify file name
 *   EDGE_FILE[=filename] : reequire edge file and optionally specify file name
 *   NODE_ATTR_LIST=name[,name[,...]] : List of tag names in which to store
 *                                     attribute values.  If the same name
 *                                     is repeated multiple times, multiple
 *                                     attribute values will be stored in the
 *                                     same tag as array values in the order
 *                                     they are read from the file.  If a 
 *                                     name is zero-length, the attribute data
 *                                     will be disgarded.  
 */
class ReadTetGen : public ReaderIface
{
   
public:

  static ReaderIface* factory( Interface* );

    //! load a file
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
  ReadTetGen(Interface* impl = NULL);

   //! Destructor
  virtual ~ReadTetGen();

private:

  Interface* mbIface;
  ReadUtilIface* readTool;

  /**\brief Try to open one of several input files
   *
   *\param input_file_name  The file name as passed in by the application
   *\param input_name_base  If the input file name ends with a known suffix,
   *                        the portition of the input file without the suffix.
   *                        Otherwise equal to input_file_name.
   *\param input_file_suffix If the input file name ends with a known suffix,
   *                        the suffix.  Otherwise empty.
   *\param file_type_suffix The suffix for the file type that is to be opened.
   *\param file_name_option The FileOptions option name specifying the file
   *                        name to open.
   *\param opts             Input options list.
   *\param file_stream      The stream to open for the file.
   */
  ErrorCode open_file( const std::string& input_file_name,
                         const std::string& input_name_base,
                         const std::string& input_name_suffix,
                         const char* file_type_suffix,
                         const char* file_name_option,
                         const FileOptions& opts,
                         std::ifstream& file_stream,
                         bool file_required = false );

  /**\brief Read a line from a file
   * 
   * Read the next non-empty line.  Strips comments.
   *\param file   The stream to read from
   *\param line   Output: the line read from the stream
   *\param lineno Incremented for each real line read from the stream
   *              (including disgarded empty and comment lines.)
   */
  ErrorCode read_line( std::istream& file, std::string& line, int& lineno );

  /**\brief Read a line of double values from a file.
   */
  ErrorCode read_line( std::istream& file, 
                         double* values_out, int num_values,
                         int& lineno );

  /**\brief Parse option string specifying mapping from 
   *        attributes to tags.
   *
   * Given a file option string describing the mapping from tetgen
   * attributes to MOAB tags, parse it and populate the passed vectors.
   * \param option_str  Input: The option string to parse.
   * \param tag_list    Output: A list tag handles, one for each attribute.
   *                    Tag handle value will be zero if the attribute
   *                    is to be interpreted as a group id.
   * \param index_list  Output: Which array index to store the attribute
   *                    value at for a multi-valued tag.  Zero for single-
   *                    valued tags.  -1 if the corresponding attribute value
   *                    is to be interpreted as a group ID.
   * \param group_designator Input: special tag name used to designate an
   *                    attribute as the group (surface or volume) ID.
   */
  ErrorCode parse_attr_list( const std::string& option_str,
                               std::vector<Tag>& tag_list,
                               std::vector<int>& index_list,
                               const char* group_designator = 0 );

  ErrorCode read_node_file( std::istream& file, 
                              const Tag* attr_tag_list,
                              const int* attr_tag_index,
                              int attr_tag_list_len,
                              std::vector<EntityHandle>& nodes );

  ErrorCode read_elem_file( EntityType type,
                              std::istream& file, 
                              const std::vector<EntityHandle>& nodes,
                              Range& elems );
};

} // namespace moab

#endif // defined(READ_TET_GEN_HPP)
