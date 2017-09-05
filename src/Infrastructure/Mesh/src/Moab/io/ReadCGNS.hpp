#ifndef READ_CGNS_HPP
#define READ_CGNS_HPP

#include "moab/ReaderIface.hpp"
#include "moab/Range.hpp"

#include "cgnslib.h"

namespace moab
{

class ReadUtilIface;
class Interface;

/**
 * \brief Export CGNS files.
 * \author Carlos Breviglieri, Carlos Junqueira Junior
 */

class ReadCGNS : public ReaderIface
{

public:

    //! factory method
    static ReaderIface* factory(Interface*);

    ErrorCode load_file(const char* file_name,
                        const EntityHandle* file_set,
                        const FileOptions& opts,
                        const SubsetList* subset_list = 0,
                        const Tag* file_id_tag = 0);

    ErrorCode read_tag_values(const char* file_name,
                              const char* tag_name,
                              const FileOptions& opts,
                              std::vector<int>& tag_values_out,
                              const SubsetList* subset_list = 0);

    //! Constructor
    ReadCGNS(Interface* impl = NULL);

    //! Destructor
    virtual ~ReadCGNS();

private:

    ErrorCode create_elements(char *sectionName,
                              const Tag* file_id_tag,
                              const EntityType& ent_type,
                              const int& verts_per_elem,
                              long& section_offset,
                              int elems_count,
                              const std::vector<cgsize_t>& elemsConn);

    ErrorCode create_sets(char* sectionName,
                          const Tag* file_id_tag,
                          EntityType element_type,
                          const Range& elements,
                          const std::vector<int>& set_ids,
                          int set_type);

    ErrorCode create_geometric_topology();

    /** \brief Process options passed into the reader
     * \param opts Options passed into this read
     */
    ErrorCode process_options(const FileOptions &opts);

    const char *fileName;

    short mesh_dim;

    ReadUtilIface* readMeshIface;

    //! interface instance
    Interface* mbImpl;

    Tag globalId;
    Tag boundary;
    Range geomSets;

};

} // namespace moab

#endif

