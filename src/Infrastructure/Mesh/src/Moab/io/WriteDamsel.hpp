//-------------------------------------------------------------------------
// Filename      : WriteDamsel.hpp
//
// Purpose       : ExodusII writer
//
// Special Notes : Lots of code taken from verde implementation
//
// Creator       : Corey Ernst 
//
// Date          : 8/02
//
// Owner         : Corey Ernst 
//-------------------------------------------------------------------------

#ifndef WRITEDAMSEL_HPP
#define WRITEDAMSEL_HPP

#ifndef IS_BUILDING_MB
#error "WriteDamsel.hpp isn't supposed to be included into an application"
#endif

#include <vector>
#include <string>

#include "moab/Interface.hpp"
#include "moab/Range.hpp"
#include "moab/WriterIface.hpp"
#include "RangeSeqIntersectIter.hpp"
#include "FileOptions.hpp"
#include "DamselUtil.hpp"

#include "damsel.h"

namespace moab {

class WriteUtilIface;
class SequenceManager;
class Error;

class WriteDamsel : public WriterIface
{
 
public:
    //! factory function, for ReaderWriter
  static WriterIface* factory( Interface* iface );

    //! Constructor
  WriteDamsel(Interface *impl);

    //! Destructor
  virtual ~WriteDamsel();

    //! Primary interface function
    //! \param file_name Filename being written
    //! \param overwrite If true and the file exists, an error is returned
    //! \param opts File options, e.g. whether and how to write in parallel
    //! \param meshset_list If non-NULL, a vector of sets defining what is to be written
    //! \param num_sets The number of sets to be written, only used if meshset_list is non-NULL
    //! \param qa_records Strings defining provenance information
    //! \param tag_list If non-NULL, only these tags should be written
    //! \param num_tags The number of tag handles in tag_list, used only if tag_list is non-NULL
    //! \param requested_output_dimension Dimension used for coordinates
  ErrorCode write_file(const char *file_name, 
                       const bool /* overwrite */,
                       const FileOptions& opts,
                       const EntityHandle *meshset_list,
                       const int num_sets,
                       const std::vector<std::string>& /* qa_records */,
                       const Tag* /* tag_list */ = NULL,
                       int /* num_tags */ = 0,
                       int /* requested_output_dimension */ = 3);

  enum {DAMSEL_IS_TRACKING = 0x1
  } DAMSEL_FLAGS;
  
private:

    //! Initialize global information about dense/sparse/conventional tags, once for entire write_file call
  ErrorCode init_tag_info();

    //! write a subrange of entities/sets; just a wrapper to write_[vertices, entities, sets]
  ErrorCode write_subrange(RangeSeqIntersectIter &rsi);
  
    //! Write the vertices in the model, for the handles in the specified RangeSeqIntersectIter
    //! \param rsi Range sequence iterator defining range of entities/sets to be written
  ErrorCode write_vertices(RangeSeqIntersectIter &rsi);
  
    //! Write the entities in the model, for the handles in the specified RangeSeqIntersectIter
    //! \param rsi Range sequence iterator defining range of entities/sets to be written
  ErrorCode write_entities(RangeSeqIntersectIter &rsi);
  
    //! Write the sets in the model, for the handles in the specified RangeSeqIntersectIter
    //! \param rsi Range sequence iterator defining range of entities/sets to be written
  ErrorCode write_sets(RangeSeqIntersectIter &rsi);
  
    //! Map dense tags for the specified entities, using the specified damsel entity container
  ErrorCode map_dense_tags(RangeSeqIntersectIter &rsi, damsel_container &ent_cont);

    //! Map sparse tags for all entities
  ErrorCode map_sparse_tags();

    //! interface instance
  Interface *mbImpl;

    //! WriteUtil object used in this writer
  WriteUtilIface* mWriteIface;

    //! Error object used to register errors
  Error *mError;
  
    //! Used to initialize the RangeSeqIntersectIter
  SequenceManager *sequenceManager;

    //! file name
  std::string fileName;

    //! utility for storing damsel-specific stuff
  DamselUtil dU;
};

inline ErrorCode WriteDamsel::write_subrange(RangeSeqIntersectIter &rsi) 
{
  ErrorCode rval = MB_SUCCESS;

  if (MBVERTEX == mbImpl->type_from_handle(rsi.get_start_handle())) 
    rval = write_vertices(rsi);

  else if (MBENTITYSET > mbImpl->type_from_handle(rsi.get_start_handle())) 
    rval = write_entities(rsi);

//  else
//    rval = write_sets(rsi);

  return rval;
}

} // namespace moab

#endif
