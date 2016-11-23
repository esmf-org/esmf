/**
 * MOAB, a Mesh-Oriented datABase, is a software component for creating,
 * storing and accessing finite element mesh data.
 *
 * Copyright 2004 Sandia Corporation.  Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S. Government
 * retains certain rights in this software.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 */

#ifdef WIN32
// turn off warnings that say they debugging identifier has been truncated
// this warning comes up when using some STL containers
#pragma warning(disable : 4786)
#endif

#define MOAB_MPE_LOG "moab.mpe"

#include <iostream>
#include <sstream>
#include <vector>
#include <string>
#include <algorithm>
#include "moab/Core.hpp"
#include "MeshSetSequence.hpp"
#include "ElementSequence.hpp"
#include "VertexSequence.hpp"
#include "assert.h"
#include "AEntityFactory.hpp"
#include "ReadUtil.hpp"
#include "WriteUtil.hpp"
#include "moab/CN.hpp"
#include "moab/HigherOrderFactory.hpp"
#include "SequenceManager.hpp"
#include "moab/Error.hpp"
#include "moab/ReaderWriterSet.hpp"
#include "moab/ReaderIface.hpp"
#include "moab/WriterIface.hpp"
#include "moab/ScdInterface.hpp"
#include "moab/SetIterator.hpp"

#include "BitTag.hpp"
#include "DenseTag.hpp"
#include "MeshTag.hpp"
#include "SparseTag.hpp"
#include "VarLenDenseTag.hpp"
#include "VarLenSparseTag.hpp"

#include <sys/stat.h>
#include <errno.h>
#include <string.h>

#ifdef MOAB_HAVE_AHF
#include "moab/HalfFacetRep.hpp"
#endif

#ifdef MOAB_HAVE_MPI
/* Leave ParallelComm.hpp before mpi.h or MPICH2 will fail
 * because its C++ headers do not like SEEK_* macros.
 */
#include "moab/ParallelComm.hpp"
#include "moab_mpi.h"
#include "ReadParallel.hpp"
#endif

#ifdef MOAB_HAVE_HDF5
#  ifdef MOAB_HAVE_HDF5_PARALLEL
#    include "WriteHDF5Parallel.hpp"
     typedef moab::WriteHDF5Parallel DefaultWriter;
#    define DefaultWriterName "WriteHDF5Parallel"
#  else
#    include "WriteHDF5.hpp"
     typedef moab::WriteHDF5 DefaultWriter;
#    define DefaultWriterName "WriteHDF5"
#  endif
#elif defined(MOAB_HAVE_NETCDF)
#  include "WriteNCDF.hpp"
   typedef moab::WriteNCDF DefaultWriter;
#  define DefaultWriterName "WriteNCDF"
#else
#  include "WriteVtk.hpp"
   typedef moab::WriteVtk DefaultWriter;
#  define DefaultWriterName "WriteVtk"
#endif
#include "MBTagConventions.hpp"
#include "ExoIIUtil.hpp"
#include "EntitySequence.hpp"
#include "moab/FileOptions.hpp"
#ifdef LINUX
# include <dlfcn.h>
# include <dirent.h>
#endif


#ifdef XPCOM_MB
#include "nsMemory.h"
#endif

#ifdef MOAB_HAVE_MPI
#include "moab_mpe.h"
#endif

// MOAB used to use a NULL handle list and a zero handle count
// to indicate that tag functions are to operate on the global/mesh
// value of the tag.  For the 3.0 release MOAB also accepted a handle
// with a value of 0 to indicate the same.  Now we want to drop the
// old NULL list method because a) it is one less special case that
// must be handled in the tag get/set paths and b) it aviods unexpected
// segfaults when applications requested tag data with an empty list
// of handles.
//
// Define this constant to revert to the old behavior, but also print
// a warning.
#define ALLOW_NULL_FOR_MESH_TAG
//
// Define this to print an error and abort() when a NULL handle list
// is passed.  This is intended as an interim solution to help catch
// spots in code that haven't been updated for the change.
#undef DISALLOW_EMPTY_HANDLE_LIST_FOR_TAGS
//
// The eventual goal is to define neither of the above, eliminating the
// check and allowing applications to safely request tag data for no
// entities.

static void warn_null_array_mesh_tag()
{
  std::cerr << "WARNING: Accepting empty array to indicate mesh tag" << std::endl; \
}

#ifdef ALLOW_NULL_FOR_MESH_TAG
# define CHECK_MESH_NULL \
  EntityHandle root = 0; \
  if (NULL == entity_handles && 0 == num_entities) { \
    entity_handles = &root; \
    num_entities = 1; \
    warn_null_array_mesh_tag(); \
  }
#elif defined(DISALLOW_EMPTY_HANDLE_LIST_FOR_TAGS)
# define CHECK_MESH_NULL \
  if (NULL == entity_handles) { \
    std::cerr << "ERROR: Deprecated NULL handle list at " __FILE__ ":" << __LINE__ << std::endl; \
    abort(); \
  }
#else
# define CHECK_MESH_NULL
#endif

namespace moab {

using namespace std;

static inline const MeshSet* get_mesh_set( const SequenceManager* sm,
                                    EntityHandle h )
{
  const EntitySequence* seq;
  if (MBENTITYSET != TYPE_FROM_HANDLE(h) || MB_SUCCESS != sm->find( h, seq ))
    return 0;
  return reinterpret_cast<const MeshSetSequence*>(seq)->get_set(h);
}

static inline MeshSet* get_mesh_set( SequenceManager* sm,
                                     EntityHandle h )
{
  EntitySequence* seq;
  if (MBENTITYSET != TYPE_FROM_HANDLE(h) || MB_SUCCESS != sm->find( h, seq ))
    return 0;
  return reinterpret_cast<MeshSetSequence*>(seq)->get_set(h);
}

//! Constructor
Core::Core()
{
#ifdef XPCOM_MB
  NS_INIT_ISUPPORTS();
#endif

  if (initialize() != MB_SUCCESS)
  {
    printf("Error initializing moab::Core\n");
    exit(1);
  }
}


//! Constructor
Core::Core( int , int  )
{
  std::cerr << "Using depricated construtor: Core::Core(rank,size)" << std::endl;

#ifdef XPCOM_MB
  NS_INIT_ISUPPORTS();
#endif

  if (initialize() != MB_SUCCESS)
  {
    printf("Error initializing moab::Core\n");
    exit(1);
  }
}

//! destructor
Core::~Core()
{
  if(mMBWriteUtil)
    delete mMBWriteUtil;
  if(mMBReadUtil)
    delete mMBReadUtil;
  if (scdInterface)
    delete scdInterface;

  mMBWriteUtil = NULL;
  mMBReadUtil = NULL;
  scdInterface = NULL;

  deinitialize();
}


ErrorCode Core::initialize()
{
#ifdef MOAB_HAVE_MPI
  int flag;
  if (MPI_SUCCESS == MPI_Initialized(&flag)) {
    mpiFinalize = !flag;
    if (mpiFinalize) {
      char argv0[] = "MOAB";
      int one = 1;
      char* argv[] = { argv0, 0 };
      char** ptr = argv;
      MPI_Init( &one, &ptr );
    }
    writeMPELog = ! MPE_Initialized_logging();
    if (writeMPELog)
      (void)MPE_Init_log();
  }
#endif

  initErrorHandlerInCore = false;
  if (!MBErrorHandler_Initialized()) {
    MBErrorHandler_Init();
    initErrorHandlerInCore = true;
  }

  geometricDimension = 3;
  materialTag      = 0;
  neumannBCTag     = 0;
  dirichletBCTag   = 0;
  geomDimensionTag = 0;
  globalIdTag      = 0;

  sequenceManager = new (std::nothrow) SequenceManager;
  if (!sequenceManager)
    return MB_MEMORY_ALLOCATION_FAILED;

  aEntityFactory = new (std::nothrow) AEntityFactory(this);
  if (!aEntityFactory)
    return MB_MEMORY_ALLOCATION_FAILED;

  mError = new (std::nothrow) Error;
  if (!mError)
    return MB_MEMORY_ALLOCATION_FAILED;

  mMBWriteUtil = NULL;
  mMBReadUtil = NULL;
  scdInterface = NULL;

    // Readers and writers try to get pointers to above utils.
    // Do this after pointers are initialized. (Pointers should
    // really be initialized in constructor to avoid this kind
    // of thing -- j.kraftcheck.)
  readerWriterSet = new (std::nothrow) ReaderWriterSet(this);
  if (!readerWriterSet)
    return MB_MEMORY_ALLOCATION_FAILED;

  material_tag();
  neumannBC_tag();
  dirichletBC_tag();
  geom_dimension_tag();
  globalId_tag();

#ifdef MOAB_HAVE_AHF
  ahfRep = new HalfFacetRep(this);
  if (!ahfRep)
    return MB_MEMORY_ALLOCATION_FAILED;
  mesh_modified = false;
#endif

  return MB_SUCCESS;
}

EntityHandle Core::get_root_set()
{
  return 0;
}

void Core::deinitialize()
{

#ifdef MOAB_HAVE_MPI
  std::vector<ParallelComm*> pc_list;
  ParallelComm::get_all_pcomm(this, pc_list);
  for (std::vector<ParallelComm*>::iterator vit = pc_list.begin();
       vit != pc_list.end(); ++vit)
    delete *vit;
#endif

#ifdef MOAB_HAVE_AHF
  delete ahfRep;
  ahfRep = 0;
#endif

  if (aEntityFactory)
    delete aEntityFactory;

  aEntityFactory = 0;

  while (!tagList.empty())
    tag_delete( tagList.front() );

  if (sequenceManager)
    delete sequenceManager;

  sequenceManager = 0;

  delete readerWriterSet;
  readerWriterSet = 0;

  if(mError)
    delete mError;
  mError = 0;

#ifdef MOAB_HAVE_MPI
  if (writeMPELog) {
    const char* default_log = MOAB_MPE_LOG;
    const char* logfile = getenv("MPE_LOG_FILE");
    if (!logfile)
      logfile = default_log;
    MPE_Finish_log( logfile );
  }
  if (mpiFinalize)
    MPI_Finalize();
#endif

  if (initErrorHandlerInCore)
    MBErrorHandler_Finalize();
}

ErrorCode Core::query_interface_type( const std::type_info& type, void*& ptr )
{
  if (type == typeid(ReadUtilIface)) {
    if (!mMBReadUtil)
      mMBReadUtil = new ReadUtil( this, mError );
    ptr = static_cast<ReadUtilIface*>(mMBReadUtil);
  }
  else if (type == typeid(WriteUtilIface)) {
    if(!mMBWriteUtil)
      mMBWriteUtil = new WriteUtil(this, mError);
    ptr = static_cast<WriteUtilIface*>(mMBWriteUtil);
  }
  else if (type == typeid(ReaderWriterSet)) {
    ptr = reader_writer_set();
  }
  else if (type == typeid(Error)) {
    ptr = mError;
  }
  else if (type == typeid(ExoIIInterface)) {
    ptr = static_cast<ExoIIInterface*>(new ExoIIUtil(this));
  }
  else if (type == typeid(ScdInterface)) {
    if(!scdInterface)
      scdInterface = new ScdInterface(this);
    ptr = scdInterface;
  }
  else {
    ptr = 0;
    return MB_FAILURE;
  }
  return MB_SUCCESS;
}


ErrorCode Core::release_interface_type(const std::type_info& type, void* iface)
{
  if (type == typeid(ExoIIInterface))
    delete static_cast<ExoIIInterface*>(iface);
  else if (type != typeid(ReadUtilIface) &&
           type != typeid(WriteUtilIface) &&
           type != typeid(ReaderWriterSet) &&
           type != typeid(Error) &&
           type != typeid(ScdInterface))
    return MB_FAILURE;

  return MB_SUCCESS;
}

#ifdef XPCOM_MB
// provides basic implementation of nsISupports methods
NS_IMPL_ISUPPORTS1_CI(Core, Interface);
#endif

int Core::QueryInterface(const MBuuid& uuid, UnknownInterface** iface)
{
  *iface = 0;
  if(uuid == IDD_MBUnknown)
    *iface = this;
  if(uuid == IDD_MBCore)
    *iface = this;
  else
    return 0;
  return 1;
}

float Core::impl_version( std::string *version_string )
{
  if (version_string)
    *version_string = MOAB_VERSION_STRING;

  return MOAB_VERSION_MAJOR + MOAB_VERSION_MINOR / 100.0f;
}

//! get the type from a handle, returns type
EntityType Core::type_from_handle(const EntityHandle handle) const
{
  if (!handle) // root set
    return MBENTITYSET;
  else
    return TYPE_FROM_HANDLE(handle);
}

//! get the id from a handle, returns id
EntityID Core::id_from_handle(const EntityHandle handle) const
{
  return ID_FROM_HANDLE(handle);
}

//! get a handle from an id and type
ErrorCode Core::handle_from_id( const EntityType type,
                                    const EntityID id,
                                    EntityHandle& handle) const
{
  int err;
  handle = CREATE_HANDLE(type, id, err);

    //check to see if handle exists
  const EntitySequence *dummy_seq = 0;
  ErrorCode error_code = sequence_manager()->find(handle, dummy_seq);
  return error_code;
}

int Core::dimension_from_handle(const EntityHandle handle) const
{
  if (!handle) // root set
    return 4;
  else
    return CN::Dimension(TYPE_FROM_HANDLE(handle));
}

//! load mesh from data in file
//! NOTE: if there is mesh already present, the new mesh will be added
ErrorCode  Core::load_mesh( const char *file_name,
                                const int* block_id_list,
                                const int num_blocks )
{
  const char* name = block_id_list ? MATERIAL_SET_TAG_NAME : 0;
  return load_file( file_name, 0, 0, name, block_id_list, num_blocks );
}

ErrorCode Core::load_file( const char* file_name,
                               const EntityHandle* file_set,
                               const char* setoptions,
                               const char* set_tag_name,
                               const int* set_tag_vals,
                               int num_set_tag_vals )
{
  FileOptions opts(setoptions);
  ErrorCode rval;
  ReaderIface::IDTag t = { set_tag_name, set_tag_vals, num_set_tag_vals };
  ReaderIface::SubsetList sl = { &t, 1, 0, 0 };

  assert(!file_set || (*file_set && is_valid(*file_set)));
  if (file_set && !*file_set) {
    MB_SET_GLB_ERR(MB_FAILURE, "Non-NULL file set pointer should point to non-NULL set");
  }

    // if reading in parallel, call a different reader
  std::string parallel_opt;
  rval = opts.get_option( "PARALLEL", parallel_opt);
  if (MB_SUCCESS == rval) {
#ifdef MOAB_HAVE_MPI
    ParallelComm* pcomm = 0;
    int pcomm_id;
    rval = opts.get_int_option( "PARALLEL_COMM", pcomm_id );
    if (MB_ENTITY_NOT_FOUND == rval)
      rval = opts.get_int_option( "PCOMM", pcomm_id );
    if (rval == MB_SUCCESS) {
      pcomm = ParallelComm::get_pcomm( this, pcomm_id );
      if (!pcomm)
        return MB_ENTITY_NOT_FOUND;
    }
    else if (rval != MB_ENTITY_NOT_FOUND)
      return rval;
    if (set_tag_name && num_set_tag_vals) {
      rval = ReadParallel(this,pcomm).load_file( file_name, file_set, opts, &sl );MB_CHK_ERR(rval);
    }
    else {
      rval = ReadParallel(this,pcomm).load_file( file_name, file_set, opts );MB_CHK_ERR(rval);
    }
#else
    MB_SET_GLB_ERR(MB_FAILURE, "PARALLEL option not valid, this instance compiled for serial execution");
#endif
  }
  else {
    if (set_tag_name && num_set_tag_vals) {
      rval = serial_load_file( file_name, file_set, opts, &sl );MB_CHK_ERR(rval);
    }
    else {
      rval = serial_load_file( file_name, file_set, opts );MB_CHK_ERR(rval);
    }
  }

  if (MB_SUCCESS == rval && !opts.all_seen()) {
    std::string bad_opt;
    if (MB_SUCCESS == opts.get_unseen_option( bad_opt )) {
      MB_SET_ERR(MB_UNHANDLED_OPTION, "Unrecognized option: \"" << bad_opt << "\"");
    }
    else {
      MB_SET_ERR(MB_UNHANDLED_OPTION, "Unrecognized option");
    }
  }

  return MB_SUCCESS;
}

void Core::clean_up_failed_read( const Range& initial_ents,
                                   std::vector<Tag> initial_tags )
{
  Range new_ents;
  get_entities_by_handle( 0, new_ents );
  new_ents = subtract( new_ents, initial_ents );
  delete_entities( new_ents );

  std::vector<Tag> all_tags, new_tags;
  tag_get_tags( all_tags );
  std::sort( initial_tags.begin(), initial_tags.end() );
  std::sort( all_tags.begin(), all_tags.end() );
  std::set_difference( all_tags.begin(), all_tags.end(),
                       initial_tags.begin(), initial_tags.end(),
                       std::back_inserter( new_tags ) );
  while (!new_tags.empty()) {
    tag_delete( new_tags.back() );
    new_tags.pop_back();
  }
}

ErrorCode Core::serial_load_file( const char* file_name,
                                  const EntityHandle* file_set,
                                  const FileOptions& opts,
                                  const ReaderIface::SubsetList* subsets,
                                  const Tag* id_tag  )
{
  int status;
#if defined(WIN32) || defined(WIN64) || defined(MSC_VER)
  struct _stat stat_data;
  status = _stat(file_name, &stat_data);
#else
  struct stat stat_data;
  status = stat(file_name, &stat_data);
#endif
  if (status) {
    MB_SET_GLB_ERR(MB_FILE_DOES_NOT_EXIST, file_name << ": " << strerror(errno));
  }
#if defined(WIN32) || defined(WIN64) || defined(MSC_VER)
  else if (stat_data.st_mode & _S_IFDIR) {
#else
  else if (S_ISDIR(stat_data.st_mode)) {
#endif
    MB_SET_GLB_ERR(MB_FILE_DOES_NOT_EXIST, file_name << ": Cannot read directory/folder");
  }

  const ReaderWriterSet* set = reader_writer_set();

  Range initial_ents;
  ErrorCode rval = get_entities_by_handle( 0, initial_ents );MB_CHK_ERR(rval);

  std::vector<Tag> initial_tags;
  rval = tag_get_tags( initial_tags );MB_CHK_ERR(rval);

    // otherwise try using the file extension to select a reader
  std::string ext = set->extension_from_filename( file_name );

    // Try all the readers
  ReaderWriterSet::iterator iter;
  rval = MB_FAILURE;
  bool tried_one = false;
  for (iter = set->begin(); iter != set->end(); ++iter)
  {
    if (!iter->reads_extension(ext.c_str())) continue;

    ReaderIface *reader = iter->make_reader( this );
    if (NULL != reader)
    {
      tried_one = true;
      rval = reader->load_file( file_name, file_set, opts, subsets, id_tag );
      delete reader;
      if (MB_SUCCESS == rval)
        break;
      clean_up_failed_read( initial_ents, initial_tags );
    }
  }

  if (MB_SUCCESS != rval && !tried_one) {
      // didn't recognize the extension; try all of them now
    for (iter = set->begin(); iter != set->end(); ++iter)
    {
      ReaderIface *reader = iter->make_reader( this );
      if (!reader) continue;
      rval = reader->load_file( file_name, file_set, opts, subsets, id_tag );
      delete reader;
      if (MB_SUCCESS == rval) break;
      else clean_up_failed_read(initial_ents, initial_tags);
    }
  }

  if (MB_SUCCESS != rval) {
    clean_up_failed_read( initial_ents, initial_tags );
    MB_SET_ERR(rval, "Failed to load file after trying all possible readers");
  }
  else if (file_set) {
    Range new_ents;
    get_entities_by_handle( 0, new_ents );
    new_ents = subtract( new_ents, initial_ents );

    // Check if gather set exists
    EntityHandle gather_set;
    rval = mMBReadUtil->get_gather_set(gather_set);
    if (MB_SUCCESS == rval) {
      // Exclude gather set itself
      new_ents.erase(gather_set);

      // Exclude gather set entities
      Range gather_ents;
      rval = get_entities_by_handle(gather_set, gather_ents);
      if (MB_SUCCESS == rval)
        new_ents = subtract(new_ents, gather_ents);
    }

    rval = add_entities( *file_set, new_ents );
  }

  return rval;
}

ErrorCode Core::serial_read_tag( const char* file_name,
                                 const char* tag_name,
                                 const FileOptions& opts,
                                 std::vector<int>& vals,
                                 const ReaderIface::SubsetList* subsets )
{
  ErrorCode rval = MB_FAILURE;
  const ReaderWriterSet* set = reader_writer_set();

    // otherwise try using the file extension to select a reader
  ReaderIface* reader = set->get_file_extension_reader( file_name );
  if (reader)
  {
    rval = reader->read_tag_values( file_name, tag_name, opts, vals, subsets );
    delete reader;
  }
  else
  {
      // Try all the readers
    ReaderWriterSet::iterator iter;
    for (iter = set->begin(); iter != set->end(); ++iter)
    {
      reader = iter->make_reader( this );
      if (NULL != reader)
      {
        rval = reader->read_tag_values( file_name, tag_name, opts, vals, subsets );
        delete reader;
        if (MB_SUCCESS == rval)
          break;
      }
    }
  }

  return rval;
}

ErrorCode  Core::write_mesh(const char *file_name,
                                  const EntityHandle *output_list,
                                  const int num_sets)
{
  return write_file( file_name, 0, 0, output_list, num_sets );
}

ErrorCode Core::write_file( const char* file_name,
                                const char* file_type,
                                const char* options_string,
                                const EntityHandle* output_sets,
                                int num_output_sets,
                                const Tag* tag_list,
                                int num_tags )
{
  Range range;
  std::copy( output_sets, output_sets+num_output_sets, range_inserter(range) );
  return write_file( file_name, file_type, options_string, range, tag_list, num_tags );
}

ErrorCode Core::write_file( const char* file_name,
                                const char* file_type,
                                const char* options_string,
                                const Range& output_sets,
                                const Tag* tag_list,
                                int num_tags )
{
    // convert range to vector
  std::vector<EntityHandle> list( output_sets.size() );
  std::copy( output_sets.begin(), output_sets.end(), list.begin() );

    // parse some options
  FileOptions opts( options_string );
  ErrorCode rval;

  rval = opts.get_null_option( "CREATE" );
  if (rval == MB_TYPE_OUT_OF_RANGE) {
    MB_SET_GLB_ERR(MB_FAILURE, "Unexpected value for CREATE option");
  }
  bool overwrite = (rval == MB_ENTITY_NOT_FOUND);

    // Get the file writer
  std::string ext = ReaderWriterSet::extension_from_filename( file_name );
  std::vector<std::string> qa_records;
  const EntityHandle* list_ptr = list.empty() ? (EntityHandle*)0 : &list[0];

  rval = MB_TYPE_OUT_OF_RANGE;

  // Try all possible writers
  for (ReaderWriterSet::iterator i = reader_writer_set()->begin();
       i != reader_writer_set()->end(); ++i) {

    if ((file_type && !i->name().compare(file_type)) ||
        i->writes_extension(ext.c_str())) {

      WriterIface *writer = i->make_writer(this);

    // write the file
      rval = writer->write_file(file_name, overwrite, opts, list_ptr, list.size(), qa_records,
                                tag_list, num_tags );
      delete writer;
      if (MB_SUCCESS == rval)
        break;
      printf("Writer with name %s for file %s using extension %s (file type \"%s\") was unsuccessful\n",
            i->name().c_str(), file_name, ext.c_str(), file_type);
    }
  }

  if (file_type && rval == MB_TYPE_OUT_OF_RANGE) {
    MB_SET_ERR(rval, "Unrecognized file type \"" << file_type << "\"");
  }
  // Should we use default writer (e.g. HDF5)?
  else if (MB_SUCCESS != rval) {
    DefaultWriter writer(this);
    printf("Using default writer %s for file %s \n", DefaultWriterName, file_name);
    rval = writer.write_file(file_name, overwrite, opts, list_ptr, list.size(), qa_records,
                             tag_list, num_tags );
  }

  if (MB_SUCCESS == rval && !opts.all_seen()) {
    std::string bad_opt;
    if (MB_SUCCESS == opts.get_unseen_option( bad_opt )) {
      MB_SET_ERR(MB_UNHANDLED_OPTION, "Unrecognized option: \"" << bad_opt << "\"");
    }
    else {
      MB_SET_ERR(MB_UNHANDLED_OPTION, "Unrecognized option");
    }
  }

  return MB_SUCCESS;
}



//! deletes all mesh entities from this datastore
ErrorCode Core::delete_mesh()
{

  ErrorCode result = MB_SUCCESS;

    // perform all deinitialization procedures to clean up
  if (aEntityFactory)
    delete aEntityFactory;
  aEntityFactory = new AEntityFactory(this);

  for (std::list<TagInfo*>::iterator i = tagList.begin(); i != tagList.end(); ++i) {
    result = (*i)->release_all_data( sequenceManager, mError, false );MB_CHK_ERR(result);
  }

  sequenceManager->clear();

  return MB_SUCCESS;
}

  //! get overall geometric dimension
ErrorCode Core::get_dimension(int &dim) const
{
  dim = geometricDimension;
  return MB_SUCCESS;
}

  //! set overall geometric dimension
  /** Returns error if setting to 3 dimensions, mesh has been created, and
   *  there are only 2 dimensions on that mesh
   */
ErrorCode Core::set_dimension(const int dim)
{
    // check to see if current dimension is smaller
  if (geometricDimension < dim)
  {
      // need to check the number of entities
    int num;
    /*ErrorCode result = */ get_number_entities_by_dimension(0, geometricDimension, num);

      // test written to be more readable but possibly less efficient
      //if (MB_SUCCESS != result) return MB_FAILURE;
      //else if (0 != num && dim == 2 && ycoordTag == 0) return MB_FAILURE;
      //else if (0 != num && dim == 3 && (ycoordTag == 0 || zcoordTag == 0)) return MB_FAILURE;
      //TODO -- replace this with not using xcoordTag, etc...
  }

    // if we got here, it's ok to set dimension
  geometricDimension = dim;
  return MB_SUCCESS;
}

  //! get blocked vertex coordinates for all vertices
  /** Blocked = all x, then all y, etc.
   */
ErrorCode Core::get_vertex_coordinates(std::vector<double> &coords) const
{
    // INEFFICIENT implementation for now, until we get blocked tag access
  Range vertices;
  ErrorCode result = get_entities_by_type(0, MBVERTEX, vertices);MB_CHK_ERR(result);

    // the least we can do is resize the vector and only go through the
    // vertex list once
  int num_verts = vertices.size();
  int vec_pos = 0;
  double xyz[3];
  coords.resize(geometricDimension*num_verts);
  for (Range::iterator it = vertices.begin(); it != vertices.end(); ++it)
  {
    result = get_coords(&(*it), 1, xyz);MB_CHK_ERR(result);

    coords[vec_pos] = xyz[0];
    coords[num_verts+vec_pos] = xyz[1];
    coords[2*num_verts+vec_pos] = xyz[2];

    vec_pos++;
  }

  return MB_SUCCESS;
}

ErrorCode Core::coords_iterate(Range::const_iterator iter,
                               Range::const_iterator end,
                               double*& xcoords_ptr,
                               double*& ycoords_ptr,
                               double*& zcoords_ptr,
                               int& count)
{
  EntitySequence *seq;
  ErrorCode rval = sequence_manager()->find(*iter, seq);
  if (MB_SUCCESS != rval) {
    xcoords_ptr = ycoords_ptr = zcoords_ptr = NULL;
    MB_SET_ERR(rval, "Couldn't find sequence for start handle");
  }
  VertexSequence *vseq = dynamic_cast<VertexSequence*>(seq);
  if (!vseq) {
    MB_SET_ERR(MB_ENTITY_NOT_FOUND, "Couldn't find sequence for start handle");
  }

  unsigned int offset = *iter - vseq->data()->start_handle();
  xcoords_ptr = reinterpret_cast<double*>(vseq->data()->get_sequence_data(0)) + offset;
  ycoords_ptr = reinterpret_cast<double*>(vseq->data()->get_sequence_data(1)) + offset;
  zcoords_ptr = reinterpret_cast<double*>(vseq->data()->get_sequence_data(2)) + offset;

  EntityHandle real_end = std::min(seq->end_handle(), *(iter.end_of_block()));
  if (*end) real_end = std::min(real_end, *end);
  count = real_end - *iter + 1;

  return MB_SUCCESS;
}

ErrorCode  Core::get_coords(const Range& entities, double *coords) const
{
  const TypeSequenceManager& vert_data = sequence_manager()->entity_map( MBVERTEX );
  TypeSequenceManager::const_iterator seq_iter;

  Range::const_pair_iterator i = entities.const_pair_begin();
  EntityHandle first = i->first;
  while (i != entities.const_pair_end() && TYPE_FROM_HANDLE(i->first) == MBVERTEX) {

    seq_iter = vert_data.lower_bound( first );
    if (seq_iter == vert_data.end() || first < (*seq_iter)->start_handle())
      return MB_ENTITY_NOT_FOUND;
    const VertexSequence* vseq = reinterpret_cast<const VertexSequence*>(*seq_iter);

    EntityID offset = first - vseq->start_handle();
    EntityID count;
    if (i->second <= vseq->end_handle()) {
      count = i->second - first + 1;
      ++i;
      if (i != entities.const_pair_end())
        first = i->first;
    }
    else {
      count = vseq->end_handle() - first + 1;
      first = vseq->end_handle()+1;
    }

    double const *x, *y, *z;
    ErrorCode rval = vseq->get_coordinate_arrays( x, y, z );MB_CHK_ERR(rval);
    x += offset;
    y += offset;
    z += offset;
    for (EntityID j = 0; j < count; ++j) {
      coords[3*j] = x[j];
      coords[3*j+1] = y[j];
      coords[3*j+2] = z[j];
    }
    coords=&coords[ 3*count ];
  }

    // for non-vertices...
  ErrorCode rval = MB_SUCCESS;
  for (Range::const_iterator rit(&(*i), i->first); rit != entities.end(); ++rit) {
    rval = get_coords(&(*rit), 1, coords);MB_CHK_ERR(rval);
    coords += 3;
  }

  return rval;
}

/**\author Jason Kraftcheck <kraftche@cae.wisc.edu> - 2007-5-15 */
ErrorCode Core::get_coords( const Range& entities,
                                double *x_coords,
                                double *y_coords,
                                double *z_coords ) const
{
  const TypeSequenceManager& vert_data = sequence_manager()->entity_map( MBVERTEX );
  TypeSequenceManager::const_iterator seq_iter;

  Range::const_pair_iterator i = entities.const_pair_begin();
  EntityHandle first = i->first;
  while (i != entities.const_pair_end() && TYPE_FROM_HANDLE(i->first) == MBVERTEX) {

    seq_iter = vert_data.lower_bound( first );
    if (seq_iter == vert_data.end() || first < (*seq_iter)->start_handle())
      return MB_ENTITY_NOT_FOUND;
    const VertexSequence* vseq = reinterpret_cast<const VertexSequence*>(*seq_iter);

    EntityID offset = first - vseq->start_handle();
    EntityID count;
    if (i->second <= vseq->end_handle()) {
      count = i->second - first + 1;
      ++i;
      if (i != entities.const_pair_end())
        first = i->first;
    }
    else {
      count = vseq->end_handle() - first + 1;
      first = vseq->end_handle()+1;
    }

    double const *x, *y, *z;
    ErrorCode rval = vseq->get_coordinate_arrays( x, y, z );MB_CHK_ERR(rval);
    if (x_coords) {
      memcpy( x_coords, x + offset, count * sizeof(double ) );
      x_coords += count;
    }
    if (y_coords) {
      memcpy( y_coords, y + offset, count * sizeof(double ) );
      y_coords += count;
    }
    if (z_coords) {
      memcpy( z_coords, z + offset, count * sizeof(double ) );
      z_coords += count;
    }
  }

    // for non-vertices...
  ErrorCode rval = MB_SUCCESS;
  double xyz[3];
  for (Range::const_iterator rit(&(*i), i->first); rit != entities.end(); ++rit) {
    rval = get_coords(&(*rit), 1, xyz);MB_CHK_ERR(rval);
    *x_coords++ = xyz[0];
    *y_coords++ = xyz[1];
    *z_coords++ = xyz[2];
  }

  return rval;
}

ErrorCode  Core::get_coords(const EntityHandle* entities,
                                  const int num_entities,
                                  double *coords) const
{
  const EntitySequence* seq = NULL;
  const VertexSequence* vseq = NULL;
  const EntityHandle* const end = entities + num_entities;
  const EntityHandle* iter = entities;
  ErrorCode status = MB_SUCCESS;

  while (iter != end) {
    if (TYPE_FROM_HANDLE(*iter) == MBVERTEX) {
      if (!seq) {
        seq = sequence_manager()->get_last_accessed_sequence( MBVERTEX );
        vseq = static_cast<const VertexSequence*>(seq);
      }
      if (!vseq) return MB_ENTITY_NOT_FOUND;
      else if (vseq->start_handle() > *iter || vseq->end_handle() < *iter) {
        if (MB_SUCCESS != sequence_manager()->find(*iter, seq))
          return MB_ENTITY_NOT_FOUND;
        vseq = static_cast<const VertexSequence*>(seq);
      }
      vseq->get_coordinates( *iter, coords );
    }
    else {
      static std::vector<EntityHandle> dum_conn(CN::MAX_NODES_PER_ELEMENT);
      static std::vector<double> dum_pos(3*CN::MAX_NODES_PER_ELEMENT);
      static const EntityHandle *conn;
      static int num_conn;
      status = get_connectivity(*iter, conn, num_conn, false, &dum_conn);MB_CHK_ERR(status);
      status = get_coords(conn, num_conn, &dum_pos[0]);MB_CHK_ERR(status);
      coords[0] = coords[1] = coords[2] = 0.0;
      for (int i = 0; i < num_conn; i++) {
        coords[0] += dum_pos[3*i];
        coords[1] += dum_pos[3*i+1];
        coords[2] += dum_pos[3*i+2];
      }
      coords[0] /= num_conn;
      coords[1] /= num_conn;
      coords[2] /= num_conn;
    }
    coords += 3;
    ++iter;
  }

  return status;
}


ErrorCode  Core::get_coords(const EntityHandle entity_handle,
                                  const double *& x, const double *& y, const double *& z) const
{
  ErrorCode status = MB_TYPE_OUT_OF_RANGE;

  if ( TYPE_FROM_HANDLE(entity_handle) == MBVERTEX )
  {
    const EntitySequence* seq = 0;
    status = sequence_manager()->find(entity_handle, seq);

    if (seq == 0 || status != MB_SUCCESS)
      return MB_ENTITY_NOT_FOUND;

    status = static_cast<const VertexSequence*>(seq)->get_coordinates_ref(entity_handle,
                                                                          x, y, z);

  }

  return status;
}

//! set the coordinate information for this handle if it is of type Vertex
//! otherwise, return an error
ErrorCode  Core::set_coords( const EntityHandle *entity_handles,
                                 const int num_entities,
                                 const double *coords)
{

  ErrorCode status = MB_SUCCESS;

  int i, j = 0;

  for (i = 0; i < num_entities; i++) {
    if ( TYPE_FROM_HANDLE(entity_handles[i]) == MBVERTEX )
    {
      EntitySequence* seq = 0;
      status = sequence_manager()->find(entity_handles[i], seq);

      if (seq != 0 && status == MB_SUCCESS) {
        status = static_cast<VertexSequence*>(seq)->set_coordinates(entity_handles[i], coords[j], coords[j+1], coords[j+2]);
        j += 3;
      }
    }
    else if (status == MB_SUCCESS)
      status = MB_TYPE_OUT_OF_RANGE;
  }

  return status;

}

//! set the coordinate information for this handle if it is of type Vertex
//! otherwise, return an error
ErrorCode  Core::set_coords(Range entity_handles, const double *coords)
{

  ErrorCode status = MB_SUCCESS;

  int j = 0;

  for (Range::iterator rit = entity_handles.begin(); rit != entity_handles.end(); ++rit) {
    if ( TYPE_FROM_HANDLE(*rit) == MBVERTEX )
    {
      EntitySequence* seq = 0;
      status = sequence_manager()->find(*rit, seq);

      if (seq != 0 && status == MB_SUCCESS) {
        status = static_cast<VertexSequence*>(seq)->set_coordinates(*rit, coords[j], coords[j+1], coords[j+2]);
        j += 3;
      }
    }
    else if (status == MB_SUCCESS)
      status = MB_TYPE_OUT_OF_RANGE;
  }

  return status;

}

double Core::get_sequence_multiplier() const
{
  return sequenceManager->get_sequence_multiplier();
}

void Core::set_sequence_multiplier(double factor)
{
  assert(factor >= 1.0);
  sequenceManager->set_sequence_multiplier(factor);
}

  //! get global connectivity array for specified entity type
  /**  Assumes just vertices, no higher order nodes
   */
ErrorCode Core::get_connectivity_by_type(const EntityType type,
                                               std::vector<EntityHandle> &connect) const
{
    // inefficient implementation until we get blocked tag access

    // get the range of entities of this type
  Range this_range;
  ErrorCode result = get_entities_by_type(0, type, this_range);

  int num_ents = this_range.size();
  connect.reserve(num_ents*CN::VerticesPerEntity(type));

    // now loop over these entities, getting connectivity for each
  for (Range::iterator this_it = this_range.begin();
       this_it != this_range.end();
       ++this_it)
  {
    const EntityHandle *connect_vec = NULL;
    result = get_connectivity(*this_it, connect_vec, num_ents, true);MB_CHK_ERR(result);
    connect.insert(connect.end(), &connect_vec[0], &connect_vec[num_ents]);
  }

  return MB_SUCCESS;
}


//! get the connectivity for element /handles.  For non-element handles, return an error
ErrorCode  Core::get_connectivity(const EntityHandle *entity_handles,
                                      const int num_handles,
                                      Range &connectivity,
                                      bool corners_only) const
{
  std::vector<EntityHandle> tmp_connect;
  ErrorCode result = get_connectivity(entity_handles, num_handles, tmp_connect,
                                        corners_only);MB_CHK_ERR(result);

  std::sort( tmp_connect.begin(), tmp_connect.end() );
  std::copy(tmp_connect.rbegin(), tmp_connect.rend(), range_inserter(connectivity));
  return result;
}

//! get the connectivity for element /handles.  For non-element handles, return an error
ErrorCode  Core::get_connectivity(const EntityHandle *entity_handles,
                                  const int num_handles,
                                  std::vector<EntityHandle> &connectivity,
                                  bool corners_only,
                                  std::vector<int> *offsets) const
{
  connectivity.clear(); // this seems wrong as compared to other API functions,
                        // but changing it breaks lost of code, so I'm leaving
                        // it in.  - j.kraftcheck 2009-11-06

  ErrorCode rval;
  std::vector<EntityHandle> tmp_storage; // used only for structured mesh
  const EntityHandle* conn;
  int len;
  if (offsets) offsets->push_back(0);
  for (int i = 0; i < num_handles; ++i) {
    rval = get_connectivity( entity_handles[i], conn, len, corners_only, &tmp_storage );MB_CHK_ERR(rval);
    connectivity.insert( connectivity.end(), conn, conn + len );
    if (offsets) offsets->push_back(connectivity.size());
  }
  return MB_SUCCESS;
}

//! get the connectivity for element handles.  For non-element handles, return an error
ErrorCode Core::get_connectivity(const EntityHandle entity_handle,
                                     const EntityHandle*& connectivity,
                                     int& number_nodes,
                                     bool corners_only,
                                     std::vector<EntityHandle>* storage) const
{
  ErrorCode status;

    // Make sure the entity should have a connectivity.
  EntityType type = TYPE_FROM_HANDLE(entity_handle);

    // WARNING: This is very dependent on the ordering of the EntityType enum
  if(type < MBVERTEX || type >= MBENTITYSET)
    return MB_TYPE_OUT_OF_RANGE;

  else if (type == MBVERTEX) {
    return MB_FAILURE;
  }

  const EntitySequence* seq = 0;

    // We know that connectivity is stored in an EntitySequence so jump straight
    // to the entity sequence
  status = sequence_manager()->find(entity_handle, seq);
  if (seq == 0 || status != MB_SUCCESS)
    return MB_ENTITY_NOT_FOUND;

  return static_cast<const ElementSequence*>(seq)->get_connectivity(entity_handle,
                                                              connectivity,
                                                              number_nodes,
                                                              corners_only,
                                                              storage);
}

//! set the connectivity for element handles.  For non-element handles, return an error
ErrorCode  Core::set_connectivity(const EntityHandle entity_handle,
                                      EntityHandle *connect,
                                      const int num_connect)
{
  ErrorCode status = MB_FAILURE;

    // Make sure the entity should have a connectivity.
    // WARNING: This is very dependent on the ordering of the EntityType enum
  EntityType type = TYPE_FROM_HANDLE(entity_handle);

  EntitySequence* seq = 0;

  if (type < MBVERTEX || type > MBENTITYSET)
    return MB_TYPE_OUT_OF_RANGE;

  status = sequence_manager()->find(entity_handle, seq);
  if (seq == 0 || status != MB_SUCCESS)
    return (status != MB_SUCCESS ? status : MB_ENTITY_NOT_FOUND);

  const EntityHandle* old_conn;
  int len;
  status = static_cast<ElementSequence*>(seq)->get_connectivity(entity_handle, old_conn, len);MB_CHK_ERR(status);

  aEntityFactory->notify_change_connectivity(
    entity_handle, old_conn, connect, num_connect);

  status = static_cast<ElementSequence*>(seq)->set_connectivity(entity_handle,
                                                                connect, num_connect);
  if (status != MB_SUCCESS)
    aEntityFactory->notify_change_connectivity(
      entity_handle, connect, old_conn, num_connect);

  return status;
}


template <typename ITER> static inline
ErrorCode get_adjacencies_union( Core* gMB,
                                   ITER begin, ITER end,
                                   int to_dimension,
                                   bool create_if_missing,
                                   Range& adj_entities )
{
  const size_t DEFAULT_MAX_BLOCKS_SIZE = 4000;
  const size_t MAX_OUTER_ITERATIONS = 100;

  std::vector<EntityHandle> temp_vec, storage;
  std::vector<EntityHandle>::const_iterator ti;
  ErrorCode result = MB_SUCCESS, tmp_result;
  ITER i = begin;
  Range::iterator ins;
  const EntityHandle* conn;
  int conn_len;

    // Just copy any vertices from the input range into the output
  size_t remaining = end - begin;
  assert(begin + remaining == end);

    // How many entities to work with at once? 2000 or so shouldn't require
    // too much memory, but don't iterate in outer loop more than a
    // 1000 times (make it bigger if many input entiites.)
  const size_t block_size = std::max( DEFAULT_MAX_BLOCKS_SIZE, remaining/MAX_OUTER_ITERATIONS );
  while (remaining > 0) {
    const size_t count = remaining > block_size ? block_size : remaining;
    remaining -= count;
    temp_vec.clear();
    for (size_t j = 0; j < count; ++i, ++j) {
      if (CN::Dimension(TYPE_FROM_HANDLE(*i)) == to_dimension) {
        temp_vec.push_back(*i);
      }
      else if (to_dimension == 0 && TYPE_FROM_HANDLE(*i) != MBPOLYHEDRON) {
        tmp_result = gMB->get_connectivity( *i, conn, conn_len, false, &storage );
        if (MB_SUCCESS != tmp_result) {
          result = tmp_result;
          continue;
        }
        temp_vec.insert( temp_vec.end(), conn, conn + conn_len );
      }
      else {
        tmp_result = gMB->a_entity_factory()->get_adjacencies( *i, to_dimension,
                                                   create_if_missing, temp_vec);
        if (MB_SUCCESS != tmp_result) {
          result = tmp_result;
          continue;
        }
      }
    }

    std::sort( temp_vec.begin(), temp_vec.end() );
    ins = adj_entities.begin();
    ti = temp_vec.begin();
    while (ti != temp_vec.end()) {
      EntityHandle first = *ti;
      EntityHandle second = *ti;
      for (++ti; ti != temp_vec.end() && (*ti - second <= 1); ++ti)
        second = *ti;
      ins = adj_entities.insert( ins, first, second );
    }
  }
  return result;
}

template <typename ITER> static inline
ErrorCode get_adjacencies_intersection( Core* mb,
                             ITER begin, ITER end,
                             const int to_dimension,
                             const bool create_if_missing,
                             std::vector<EntityHandle>& adj_entities )
{
  const size_t SORT_THRESHOLD = 200;
  std::vector<EntityHandle> temp_vec;
  std::vector<EntityHandle>::iterator adj_it, w_it;
  ErrorCode result = MB_SUCCESS;

  if (begin == end) {
    adj_entities.clear(); // intersection
    return MB_SUCCESS;
  }

    // First iteration is a special case if input list is empty.
    // Rather than returning nothing (intersecting with empty
    // input list), we begin with the adjacencies for the first entity.
  if (adj_entities.empty()) {
    EntityType type = TYPE_FROM_HANDLE(*begin);
    if (to_dimension == CN::Dimension(type))
      adj_entities.push_back(*begin);
    else if (to_dimension == 0 && type != MBPOLYHEDRON) {
      result = mb->get_connectivity(&(*begin), 1, adj_entities);MB_CHK_ERR(result);
    }
    else {
      result = mb->a_entity_factory()->get_adjacencies(*begin, to_dimension,
                                                   create_if_missing, adj_entities);MB_CHK_ERR(result);
    }
    ++begin;
  }

  for (ITER from_it = begin; from_it != end; ++from_it)
  {
      // running results kept in adj_entities; clear temp_vec, which is working space
    temp_vec.clear();

      // get the next set of adjacencies
    EntityType type = TYPE_FROM_HANDLE(*from_it);
    if (to_dimension == CN::Dimension(type))
      temp_vec.push_back(*from_it);
    else if (to_dimension == 0 && type != MBPOLYHEDRON) {
      result = mb->get_connectivity(&(*from_it), 1, temp_vec);MB_CHK_ERR(result);
    }
    else {
      result = mb->a_entity_factory()->get_adjacencies(*from_it, to_dimension,
                                                   create_if_missing, temp_vec);MB_CHK_ERR(result);
    }

      // otherwise intersect with the current set of results
    w_it = adj_it = adj_entities.begin();
    if (temp_vec.size()*adj_entities.size() < SORT_THRESHOLD) {
      for (; adj_it != adj_entities.end(); ++adj_it)
        if (std::find(temp_vec.begin(), temp_vec.end(), *adj_it) != temp_vec.end())
          { *w_it = *adj_it; ++w_it; }
    }
    else {
      std::sort( temp_vec.begin(), temp_vec.end() );
      for (; adj_it != adj_entities.end(); ++adj_it)
        if (std::binary_search(temp_vec.begin(), temp_vec.end(), *adj_it))
          { *w_it = *adj_it; ++w_it; }
    }
    adj_entities.erase( w_it, adj_entities.end() );

      // we're intersecting, so if there are no more results, we're done
    if (adj_entities.empty())
      break;
  }

  return MB_SUCCESS;
}

template <typename ITER> static inline
ErrorCode get_adjacencies_intersection( Core* mb,
                             ITER begin, ITER end,
                             const int to_dimension,
                             const bool create_if_missing,
                             Range& adj_entities )
{
  std::vector<EntityHandle> results;
  ErrorCode rval = moab::get_adjacencies_intersection( mb, begin, end, to_dimension,
						       create_if_missing, results );MB_CHK_ERR(rval);

  if (adj_entities.empty()) {
    std::copy( results.begin(), results.end(), range_inserter(adj_entities) );
    return MB_SUCCESS;
  }

  Range::iterator it = adj_entities.begin();
  while (it != adj_entities.end()) {
    if (std::find( results.begin(), results.end(), *it) == results.end())
      it = adj_entities.erase( it );
    else
      ++it;
  }
  return MB_SUCCESS;
}

///////////////////////////////////////////////////////////////////
//////////////////////////////////////////
#ifdef MOAB_HAVE_AHF

template <typename ITER> static inline
ErrorCode get_adjacencies_intersection_ahf(Core *mb,
                             ITER begin, ITER end,
                             const int to_dimension,
                             std::vector<EntityHandle>& adj_entities )
{
  const size_t SORT_THRESHOLD = 200;
  std::vector<EntityHandle> temp_vec;
  std::vector<EntityHandle>::iterator adj_it, w_it;
  ErrorCode result = MB_SUCCESS;

  if (begin == end) {
    adj_entities.clear(); // intersection
    return MB_SUCCESS;
  }

    // First iteration is a special case if input list is empty.
    // Rather than returning nothing (intersecting with empty
    // input list), we begin with the adjacencies for the first entity.
  if (adj_entities.empty()) {
    EntityType type = TYPE_FROM_HANDLE(*begin);

    if(to_dimension == 0 && type != MBPOLYHEDRON)
      result = mb->get_connectivity(&(*begin), 1, adj_entities);
    else
      result = mb->a_half_facet_rep()->get_adjacencies(*begin, to_dimension, adj_entities);
    if (MB_SUCCESS != result)
      return result;
    ++begin;
  }

  for (ITER from_it = begin; from_it != end; ++from_it)
  {
      // running results kept in adj_entities; clear temp_vec, which is working space
    temp_vec.clear();

      // get the next set of adjacencies
    EntityType type = TYPE_FROM_HANDLE(*from_it);
    if(to_dimension == 0 && type != MBPOLYHEDRON)
      result = mb->get_connectivity(&(*from_it), 1, temp_vec);
    else
      result = mb->a_half_facet_rep()->get_adjacencies(*from_it, to_dimension, temp_vec);
    if (MB_SUCCESS != result)
      return result;

      // otherwise intersect with the current set of results
    w_it = adj_it = adj_entities.begin();
    if (temp_vec.size()*adj_entities.size() < SORT_THRESHOLD) {
      for (; adj_it != adj_entities.end(); ++adj_it)
        if (std::find(temp_vec.begin(), temp_vec.end(), *adj_it) != temp_vec.end())
          { *w_it = *adj_it; ++w_it; }
    }
    else {
      std::sort( temp_vec.begin(), temp_vec.end() );
      for (; adj_it != adj_entities.end(); ++adj_it)
        if (std::binary_search(temp_vec.begin(), temp_vec.end(), *adj_it))
          { *w_it = *adj_it; ++w_it; }
    }
    adj_entities.erase( w_it, adj_entities.end() );

      // we're intersecting, so if there are no more results, we're done
    if (adj_entities.empty())
      break;
  }

  return MB_SUCCESS;
}
#endif

///////////////////////////////////////////

ErrorCode Core::get_adjacencies( const EntityHandle *from_entities,
                                     const int num_entities,
                                     const int to_dimension,
                                     const bool create_if_missing,
                                     std::vector<EntityHandle> &adj_entities,
                                     const int operation_type)
{

#ifdef MOAB_HAVE_AHF
    bool can_handle = true;

    if (to_dimension == 4)
        can_handle = false; // NOT SUPPORTED: meshsets
    else if (create_if_missing)
        can_handle = false;//NOT SUPPORTED: create_if_missing

    bool mixed = ahfRep->check_mixed_entity_type(); //NOT SUPPORTED: mixed entity types or polygonal/hedrals types
    if (mixed)
        can_handle = false;

    if (mesh_modified) //NOT SUPPORTED: modified mesh
        can_handle = false;

    if (can_handle)
    {
        ErrorCode result;
        if (operation_type == Interface::INTERSECT)
            return get_adjacencies_intersection_ahf(this, from_entities, from_entities+num_entities,
                                                    to_dimension, adj_entities );
        else if (operation_type != Interface::UNION)
            return MB_FAILURE;

        // do union

        std::vector<EntityHandle> tmp_storage;
        const EntityHandle* conn;
        int len;
        for (int i = 0; i < num_entities; ++i) {
            if(to_dimension == 0 && TYPE_FROM_HANDLE(from_entities[0]) != MBPOLYHEDRON) {
                result = get_connectivity(from_entities[i], conn, len, false, &tmp_storage);
                adj_entities.insert( adj_entities.end(), conn, conn+len );
                if (MB_SUCCESS != result)
                    return result;
            }
            else {
                result = ahfRep->get_adjacencies(from_entities[i], to_dimension, adj_entities);
                if (MB_SUCCESS != result)
                    return result;
            }
        }
        std::sort( adj_entities.begin(), adj_entities.end() );
        adj_entities.erase( std::unique( adj_entities.begin(), adj_entities.end() ), adj_entities.end() );

    }
    else
    {

#endif

        if (operation_type == Interface::INTERSECT)
            return get_adjacencies_intersection( this, from_entities, from_entities+num_entities,
                                                 to_dimension, create_if_missing, adj_entities );
        else if (operation_type != Interface::UNION)
            return MB_FAILURE;

        // do union
        ErrorCode result;
        std::vector<EntityHandle> tmp_storage;
        const EntityHandle* conn;
        int len;
        for (int i = 0; i < num_entities; ++i) {
            if(to_dimension == 0 && TYPE_FROM_HANDLE(from_entities[0]) != MBPOLYHEDRON) {
                result = get_connectivity(from_entities[i], conn, len, false, &tmp_storage);MB_CHK_ERR(result);
                adj_entities.insert( adj_entities.end(), conn, conn+len );
            }
            else {
                result = aEntityFactory->get_adjacencies(from_entities[i], to_dimension,
                                                         create_if_missing, adj_entities);MB_CHK_ERR(result);
            }
        }
        std::sort( adj_entities.begin(), adj_entities.end() );
        adj_entities.erase( std::unique( adj_entities.begin(), adj_entities.end() ), adj_entities.end() );

        //return MB_SUCCESS;


#ifdef MOAB_HAVE_AHF
    }
#endif

return MB_SUCCESS;

}


ErrorCode Core::get_adjacencies( const EntityHandle *from_entities,
                                     const int num_entities,
                                     const int to_dimension,
                                     const bool create_if_missing,
                                     Range &adj_entities,
                                     const int operation_type )
{
    if (operation_type == Interface::INTERSECT)
        return get_adjacencies_intersection( this, from_entities, from_entities + num_entities,
                                             to_dimension, create_if_missing, adj_entities );
    else if (operation_type == Interface::UNION)
        return get_adjacencies_union( this, from_entities, from_entities + num_entities,
                                      to_dimension, create_if_missing, adj_entities );
    else
        return MB_FAILURE;
}

ErrorCode Core::get_connectivity( const Range& from_entities,
                                      Range& adj_entities,
                                      bool corners_only ) const
{
  const size_t DEFAULT_MAX_BLOCKS_SIZE = 4000;
  const size_t MAX_OUTER_ITERATIONS = 100;

  std::vector<EntityHandle> temp_vec, storage;
  std::vector<EntityHandle>::const_iterator ti;
  ErrorCode result = MB_SUCCESS, tmp_result;
  Range::const_iterator i = from_entities.begin();
  Range::iterator ins;
  const EntityHandle* conn;
  int conn_len;

    // Just copy any vertices from the input range into the output
  size_t remaining = from_entities.size();
  for (; i != from_entities.end() && TYPE_FROM_HANDLE(*i) == MBVERTEX; ++i)
    --remaining;
  adj_entities.merge( from_entities.begin(), i );

    // How many entities to work with at once? 2000 or so shouldn't require
    // too much memory, but don't iterate in outer loop more than a
    // 1000 times (make it bigger if many input entiites.)
  const size_t block_size = std::max( DEFAULT_MAX_BLOCKS_SIZE, remaining/MAX_OUTER_ITERATIONS );
  while (remaining > 0) {
    const size_t count = remaining > block_size ? block_size : remaining;
    remaining -= count;
    temp_vec.clear();
    for (size_t j = 0; j < count; ++i, ++j) {
      tmp_result = get_connectivity( *i, conn, conn_len, corners_only, &storage );
      if (MB_SUCCESS != tmp_result) {
        result = tmp_result;
        continue;
      }

      const size_t oldsize = temp_vec.size();
      temp_vec.resize( oldsize + conn_len );
      memcpy( &temp_vec[oldsize], conn, sizeof(EntityHandle)*conn_len );
    }

    std::sort( temp_vec.begin(), temp_vec.end() );
    ins = adj_entities.begin();
    ti = temp_vec.begin();
    while (ti != temp_vec.end()) {
      EntityHandle first = *ti;
      EntityHandle second = *ti;
      for (++ti; ti != temp_vec.end() && (*ti - second <= 1); ++ti)
        second = *ti;
      ins = adj_entities.insert( ins, first, second );
    }
  }
  return result;
}

ErrorCode Core::connect_iterate(Range::const_iterator iter,
                                Range::const_iterator end,
                                EntityHandle *&connect,
                                int &verts_per_entity,
                                int& count)
{
    // Make sure the entity should have a connectivity.
  EntityType type = TYPE_FROM_HANDLE(*iter);

    // WARNING: This is very dependent on the ordering of the EntityType enum
  if(type <= MBVERTEX || type >= MBENTITYSET)
    return MB_TYPE_OUT_OF_RANGE;

  EntitySequence* seq = NULL;

    // We know that connectivity is stored in an EntitySequence so jump straight
    // to the entity sequence
  ErrorCode rval = sequence_manager()->find(*iter, seq);
  if (!seq || rval != MB_SUCCESS)
    return MB_ENTITY_NOT_FOUND;

  ElementSequence *eseq = dynamic_cast<ElementSequence*>(seq);
  assert(eseq != NULL);

  connect = eseq->get_connectivity_array();
  if (!connect) {
    MB_SET_ERR(MB_FAILURE, "Couldn't find connectivity array for start handle");
  }

  connect += eseq->nodes_per_element() * (*iter - eseq->start_handle());

  EntityHandle real_end = std::min(eseq->end_handle(), *(iter.end_of_block()));
  if (*end) real_end = std::min(real_end, *end);
  count = real_end - *iter + 1;

  verts_per_entity = eseq->nodes_per_element();

  return MB_SUCCESS;
}

ErrorCode Core::get_vertices( const Range& from_entities,
                                  Range& vertices )
{
  Range range;
  ErrorCode rval = get_connectivity( from_entities, range );MB_CHK_ERR(rval);

    // If input contained polyhedra, connectivity will contain faces.
    // Get vertices from faces.
  if (!range.all_of_dimension(0)) {
    Range::iterator it = range.upper_bound(MBVERTEX);
    Range polygons;
    polygons.merge( it, range.end() );
    range.erase( it, range.end() );
    rval = get_connectivity( polygons, range );MB_CHK_ERR(rval);
  }

  if (vertices.empty())
    vertices.swap( range );
  else
    vertices.merge( range );
  return MB_SUCCESS;
}

ErrorCode Core::get_adjacencies(const Range &from_entities,
                                      const int to_dimension,
                                      const bool create_if_missing,
                                      Range &adj_entities,
                                      const int operation_type)
{
  if (operation_type == Interface::INTERSECT)
    return get_adjacencies_intersection( this, from_entities.begin(), from_entities.end(),
                                         to_dimension, create_if_missing, adj_entities );
  else if (operation_type != Interface::UNION)
    return MB_FAILURE;
  else if (to_dimension == 0)
    return get_vertices( from_entities, adj_entities );
  else
    return get_adjacencies_union( this, from_entities.begin(), from_entities.end(),
                                  to_dimension, create_if_missing, adj_entities );
}


ErrorCode Core::add_adjacencies(const EntityHandle entity_handle,
                                    const EntityHandle *adjacencies,
                                    const int num_handles,
                                    bool both_ways)
{
  ErrorCode result = MB_SUCCESS;

  for (const EntityHandle *it = adjacencies;
       it != adjacencies+num_handles; it++) {
    result = aEntityFactory->add_adjacency(entity_handle, *it, both_ways);MB_CHK_ERR(result);
  }

  return MB_SUCCESS;
}

ErrorCode Core::add_adjacencies(const EntityHandle entity_handle,
                                    Range &adjacencies,
                                    bool both_ways)
{
  ErrorCode result = MB_SUCCESS;

  for (Range::iterator rit = adjacencies.begin(); rit != adjacencies.end(); ++rit) {
    result = aEntityFactory->add_adjacency(entity_handle, *rit, both_ways);MB_CHK_ERR(result);
  }

  return MB_SUCCESS;
}

ErrorCode Core::remove_adjacencies(const EntityHandle entity_handle,
                                       const EntityHandle *adjacencies,
                                       const int num_handles)
{
  ErrorCode result = MB_SUCCESS;

  for (const EntityHandle *it = adjacencies;
       it != adjacencies+num_handles; it++) {
    result = aEntityFactory->remove_adjacency(entity_handle, *it);MB_CHK_ERR(result);
    result = aEntityFactory->remove_adjacency(*it, entity_handle);MB_CHK_ERR(result);
  }

  return MB_SUCCESS;
}

ErrorCode Core::adjacencies_iterate(Range::const_iterator iter,
                                    Range::const_iterator end,
                                    const std::vector<EntityHandle> **& adjs_ptr,
                                    int& count)
{
    // Make sure the entity should have a connectivity.
  EntityType type = TYPE_FROM_HANDLE(*iter);

    // WARNING: This is very dependent on the ordering of the EntityType enum
  if(type < MBVERTEX || type > MBENTITYSET)
    return MB_TYPE_OUT_OF_RANGE;

  EntitySequence* seq = NULL;

    // We know that connectivity is stored in an EntitySequence so jump straight
    // to the entity sequence
  ErrorCode rval = sequence_manager()->find(*iter, seq);
  if (!seq || rval != MB_SUCCESS)
    return MB_ENTITY_NOT_FOUND;

  adjs_ptr = const_cast<const std::vector<EntityHandle>**>(seq->data()->get_adjacency_data());
  if (!adjs_ptr)
    return rval;

  adjs_ptr += *iter - seq->data()->start_handle();

  EntityHandle real_end = *(iter.end_of_block());
  if (*end) real_end = std::min(real_end, *end);
  count = real_end - *iter + 1;

  return MB_SUCCESS;
}

ErrorCode Core::get_entities_by_dimension(const EntityHandle meshset,
                                                const int dimension,
                                                Range &entities,
                                                const bool recursive) const
{
  ErrorCode result = MB_SUCCESS;
  if (meshset) {
    const EntitySequence* seq;
    result = sequence_manager()->find( meshset, seq );MB_CHK_ERR(result);
    const MeshSetSequence* mseq = reinterpret_cast<const MeshSetSequence*>(seq);
    result = mseq->get_dimension( sequence_manager(), meshset, dimension, entities, recursive );MB_CHK_ERR(result);
  }
  else if (dimension > 3) {
    sequence_manager()->get_entities( MBENTITYSET, entities );
  }
  else {
    for (EntityType this_type = CN::TypeDimensionMap[dimension].first;
         this_type <= CN::TypeDimensionMap[dimension].second;
         this_type++) {
      sequence_manager()->get_entities( this_type, entities );
    }
  }

  return MB_SUCCESS;
}

ErrorCode Core::get_entities_by_dimension(const EntityHandle meshset,
                                                const int dimension,
                                                std::vector<EntityHandle> &entities,
                                                const bool recursive) const
{
  ErrorCode result = MB_SUCCESS;
  if (meshset) {
    const EntitySequence* seq;
    result = sequence_manager()->find( meshset, seq );MB_CHK_ERR(result);
    const MeshSetSequence* mseq = reinterpret_cast<const MeshSetSequence*>(seq);
    result = mseq->get_dimension( sequence_manager(), meshset, dimension, entities, recursive );MB_CHK_ERR(result);
  }
  else if (dimension > 3) {
    sequence_manager()->get_entities( MBENTITYSET, entities );
  }
  else {
    for (EntityType this_type = CN::TypeDimensionMap[dimension].first;
         this_type <= CN::TypeDimensionMap[dimension].second;
         this_type++) {
      sequence_manager()->get_entities( this_type, entities );
    }
  }

  return MB_SUCCESS;
}

ErrorCode Core::get_entities_by_type( const EntityHandle meshset,
                                          const EntityType type,
                                          Range &entities,
                                          const bool recursive) const
{
  ErrorCode result = MB_SUCCESS;
  if (meshset) {
    const EntitySequence* seq;
    result = sequence_manager()->find( meshset, seq );MB_CHK_ERR(result);
    const MeshSetSequence* mseq = reinterpret_cast<const MeshSetSequence*>(seq);
    result = mseq->get_type( sequence_manager(), meshset, type, entities, recursive );MB_CHK_ERR(result);
  }
  else {
    sequence_manager()->get_entities( type, entities );
  }

  return MB_SUCCESS;
}

ErrorCode Core::get_entities_by_type( const EntityHandle meshset,
                                          const EntityType type,
                                          std::vector<EntityHandle> &entities,
                                          const bool recursive) const
{
  ErrorCode result = MB_SUCCESS;
  if (meshset) {
    const EntitySequence* seq;
    result = sequence_manager()->find( meshset, seq );MB_CHK_ERR(result);
    const MeshSetSequence* mseq = reinterpret_cast<const MeshSetSequence*>(seq);
    result = mseq->get_type( sequence_manager(), meshset, type, entities, recursive );MB_CHK_ERR(result);
  }
  else {
    sequence_manager()->get_entities( type, entities );
  }

  return MB_SUCCESS;
}

ErrorCode Core::get_entities_by_type_and_tag(const EntityHandle meshset,
                                                   const EntityType type,
                                                   const Tag *tags,
                                                   const void* const* values,
                                                   const int num_tags,
                                                   Range &entities,
                                                   const int condition,
                                                   const bool recursive) const
{
  ErrorCode result;
  Range range;

  result = get_entities_by_type( meshset, type, range, recursive );MB_CHK_ERR(result);
  if (!entities.empty() && Interface::INTERSECT == condition)
    range = intersect( entities, range);

    // For each tag:
    //  if operation is INTERSECT remove from 'range' any non-tagged entities
    //  if operation is UNION add to 'entities' any tagged entities
  for (int it = 0; it < num_tags && !range.empty(); it++) {
    if (!valid_tag_handle( tags[it] ))
      return MB_TAG_NOT_FOUND;

      // Of the entities in 'range', put in 'tmp_range' the subset
      // that are tagged as requested for this tag.
    Range tmp_range;

      // get the entities with this tag/value combo
    if (NULL == values || NULL == values[it]) {
      result = tags[it]->get_tagged_entities( sequenceManager, tmp_range, type, &range );MB_CHK_ERR(result);
    }
    else {
      result = tags[it]->find_entities_with_value( sequenceManager, mError, tmp_range,
                                                  values[it], 0, type, &range );MB_CHK_ERR(result);
        // if there is a default value, then we should return all entities
        // that are untagged
      if (tags[it]->equals_default_value( values[it] )) {
        Range all_tagged, untagged;
        result = tags[it]->get_tagged_entities( sequenceManager, all_tagged, type, &range );MB_CHK_ERR(result);
          // add to 'tmp_range' any untagged entities in 'range'
        tmp_range.merge( subtract( range, all_tagged ) );
      }
    }

      // The above calls should have already done the intersect for us.
    if (Interface::INTERSECT == condition)
      range.swap( tmp_range );
    else
      entities.merge( tmp_range );
  }

  if (Interface::INTERSECT == condition)
    entities.swap( range );

  return MB_SUCCESS;
}

ErrorCode Core::get_entities_by_handle(const EntityHandle meshset,
                                             Range &entities,
                                             const bool recursive) const
{
  ErrorCode result = MB_SUCCESS;
  if (meshset) {
    const EntitySequence* seq;
    result = sequence_manager()->find( meshset, seq );MB_CHK_ERR(result);
    const MeshSetSequence* mseq = reinterpret_cast<const MeshSetSequence*>(seq);
    result = mseq->get_entities( sequence_manager(), meshset, entities, recursive );MB_CHK_ERR(result);
  }
  else {
    // iterate backwards so range insertion is quicker
    for (EntityType type = MBENTITYSET; type >= MBVERTEX; --type)
      sequence_manager()->get_entities( type, entities );
  }

  return MB_SUCCESS;
}


ErrorCode Core::get_entities_by_handle(const EntityHandle meshset,
                                   std::vector<EntityHandle> &entities,
                                   const bool recursive) const
{
  ErrorCode result;
  if (recursive || !meshset) {
    Range tmp_range;
    result = get_entities_by_handle( meshset, tmp_range, recursive);
    size_t offset = entities.size();
    entities.resize( offset + tmp_range.size() );
    std::copy( tmp_range.begin(), tmp_range.end(), entities.begin() + offset );
  }
  else {
    const EntitySequence* seq;
    result = sequence_manager()->find( meshset, seq );MB_CHK_ERR(result);
    const MeshSetSequence* mseq = reinterpret_cast<const MeshSetSequence*>(seq);
    result = mseq->get_entities( meshset, entities );MB_CHK_ERR(result);
  }
  return MB_SUCCESS;
}

  //! get # entities of a given dimension
ErrorCode Core::get_number_entities_by_dimension(const EntityHandle meshset,
                                                       const int dim,
                                                       int &number,
                                                       const bool recursive) const
{
  ErrorCode result = MB_SUCCESS;

  if (!meshset) {
    number = 0;
    for (EntityType this_type = CN::TypeDimensionMap[dim].first;
         this_type <= CN::TypeDimensionMap[dim].second;
         this_type++) {
      number += sequence_manager()->get_number_entities( this_type );
    }
  }
  else {
    const EntitySequence* seq;
    result = sequence_manager()->find( meshset, seq );MB_CHK_ERR(result);
    const MeshSetSequence* mseq = reinterpret_cast<const MeshSetSequence*>(seq);
    result = mseq->num_dimension( sequence_manager(), meshset, dim, number, recursive );MB_CHK_ERR(result);
  }

  return MB_SUCCESS;
}

//! returns the number of entities with a given type and tag
ErrorCode Core::get_number_entities_by_type(const EntityHandle meshset,
                                                  const EntityType type,
                                                  int& num_ent,
                                                  const bool recursive) const
{
  ErrorCode result = MB_SUCCESS;

  if (recursive && type == MBENTITYSET)  // will never return anything
    return MB_TYPE_OUT_OF_RANGE;

  if (meshset) {
    const EntitySequence* seq;
    result = sequence_manager()->find( meshset, seq );MB_CHK_ERR(result);
    const MeshSetSequence* mseq = reinterpret_cast<const MeshSetSequence*>(seq);
    result = mseq->num_type( sequence_manager(), meshset, type, num_ent, recursive );MB_CHK_ERR(result);
  }
  else {
    num_ent = sequence_manager()->get_number_entities( type );
  }

  return MB_SUCCESS;
}

ErrorCode Core::get_number_entities_by_type_and_tag(const EntityHandle meshset,
                                                          const EntityType type,
                                                          const Tag *tag_handles,
                                                          const void* const* values,
                                                          const int num_tags,
                                                          int &num_entities,
                                                          int condition,
                                                          const bool recursive) const
{
  Range dum_ents;
  ErrorCode result = get_entities_by_type_and_tag(meshset, type, tag_handles, values, num_tags,
                                                     dum_ents, condition, recursive);
  num_entities = dum_ents.size();
  return result;
}

ErrorCode Core::get_number_entities_by_handle(const EntityHandle meshset,
                                          int& num_ent,
                                          const bool recursive) const
{
  ErrorCode result;
  if (meshset) {
    const EntitySequence* seq;
    result = sequence_manager()->find( meshset, seq );MB_CHK_ERR(result);
    const MeshSetSequence* mseq = reinterpret_cast<const MeshSetSequence*>(seq);
    return mseq->num_entities( sequence_manager(), meshset, num_ent, recursive );
  }

  num_ent = 0;
  for (EntityType this_type = MBVERTEX;
       this_type < MBMAXTYPE;
       this_type++) {
    int dummy = 0;
    result = get_number_entities_by_type(0, this_type, dummy);
    if (result != MB_SUCCESS) {
      num_ent = 0;
      return result;
    }
    num_ent += dummy;
  }

  return MB_SUCCESS;
}

//! return the tag data for a given EntityHandle and Tag
ErrorCode Core::tag_get_data( const Tag tag_handle,
                              const EntityHandle* entity_handles,
                              int num_entities,
                              void *tag_data) const
{
  assert(valid_tag_handle( tag_handle ));
  CHECK_MESH_NULL
  return tag_handle->get_data( sequenceManager, mError, entity_handles, num_entities, tag_data );
}

//! return the tag data for a given EntityHandle and Tag
ErrorCode  Core::tag_get_data( const Tag tag_handle,
                               const Range& entity_handles,
                               void *tag_data) const
{
  assert(valid_tag_handle( tag_handle ));
  return tag_handle->get_data( sequenceManager, mError, entity_handles, tag_data );
}

//! set the data  for given EntityHandles and Tag
ErrorCode  Core::tag_set_data( Tag tag_handle,
                               const EntityHandle* entity_handles,
                               int num_entities,
                               const void *tag_data)
{
  assert(valid_tag_handle( tag_handle ));
  CHECK_MESH_NULL
  return tag_handle->set_data( sequenceManager, mError, entity_handles, num_entities, tag_data );
}

//! set the data  for given EntityHandles and Tag
ErrorCode  Core::tag_set_data( Tag tag_handle,
                               const Range& entity_handles,
                               const void *tag_data)
{
  assert(valid_tag_handle( tag_handle ));
  return tag_handle->set_data( sequenceManager, mError, entity_handles, tag_data );
}


//! return the tag data for a given EntityHandle and Tag
ErrorCode  Core::tag_get_by_ptr( const Tag tag_handle,
                               const EntityHandle* entity_handles,
                               int num_entities,
                               const void** tag_data,
                               int* tag_sizes ) const
{
  assert(valid_tag_handle( tag_handle ));
  CHECK_MESH_NULL
  ErrorCode result = tag_handle->get_data( sequenceManager, mError, entity_handles, num_entities, tag_data, tag_sizes );
  int typesize = TagInfo::size_from_data_type( tag_handle->get_data_type() );
  if (tag_sizes && typesize != 1)
    for (int i = 0; i < num_entities; ++i)
      tag_sizes[i] /= typesize;
  return result;
}

//! return the tag data for a given EntityHandle and Tag
ErrorCode  Core::tag_get_by_ptr( const Tag tag_handle,
                               const Range& entity_handles,
                               const void** tag_data,
                               int* tag_sizes ) const
{
  assert(valid_tag_handle( tag_handle ));
  ErrorCode result = tag_handle->get_data( sequenceManager, mError, entity_handles, tag_data, tag_sizes );
  int typesize = TagInfo::size_from_data_type( tag_handle->get_data_type() );
  if (tag_sizes && typesize != 1) {
    int num_entities = entity_handles.size();
    for (int i = 0; i < num_entities; ++i)
      tag_sizes[i] /= typesize;
  }
  return result;
}

//! set the data  for given EntityHandles and Tag
ErrorCode  Core::tag_set_by_ptr( Tag tag_handle,
                               const EntityHandle* entity_handles,
                               int num_entities,
                               void const* const* tag_data,
                               const int* tag_sizes )
{
  assert(valid_tag_handle( tag_handle ));
  CHECK_MESH_NULL
  std::vector<int> tmp_sizes;
  int typesize = TagInfo::size_from_data_type( tag_handle->get_data_type() );
  if (typesize != 1 && tag_sizes) {
    tmp_sizes.resize(num_entities);
    for (int i = 0; i < num_entities; ++i)
      tmp_sizes[i] = tag_sizes[i] * typesize;
    tag_sizes = &tmp_sizes[0];
  }
  return tag_handle->set_data( sequenceManager, mError, entity_handles, num_entities, tag_data, tag_sizes );
}

//! set the data  for given EntityHandles and Tag
ErrorCode  Core::tag_set_by_ptr( Tag tag_handle,
                               const Range& entity_handles,
                               void const* const* tag_data,
                               const int* tag_sizes )
{
  assert(valid_tag_handle( tag_handle ));
  std::vector<int> tmp_sizes;
  int typesize = TagInfo::size_from_data_type( tag_handle->get_data_type() );
  if (typesize != 1 && tag_sizes) {
    int num_entities = entity_handles.size();
    tmp_sizes.resize(num_entities);
    for (int i = 0; i < num_entities; ++i)
      tmp_sizes[i] = tag_sizes[i] * typesize;
    tag_sizes = &tmp_sizes[0];
  }
  return tag_handle->set_data(sequenceManager, mError, entity_handles, tag_data, tag_sizes);
}

//! set the data  for given EntityHandles and Tag
ErrorCode  Core::tag_clear_data( Tag tag_handle,
                                 const EntityHandle* entity_handles,
                                 int num_entities,
                                 const void* tag_data,
                                 int tag_size )
{
  assert(valid_tag_handle( tag_handle ));
  CHECK_MESH_NULL
  return tag_handle->clear_data( sequenceManager, mError, entity_handles, num_entities, tag_data,
                                 tag_size * TagInfo::size_from_data_type( tag_handle->get_data_type() ) );
}

//! set the data  for given EntityHandles and Tag
ErrorCode  Core::tag_clear_data( Tag tag_handle,
                                 const Range& entity_handles,
                                 const void* tag_data,
                                 int tag_size )
{
  assert(valid_tag_handle( tag_handle ));
  return tag_handle->clear_data( sequenceManager, mError, entity_handles, tag_data,
                                 tag_size * TagInfo::size_from_data_type( tag_handle->get_data_type() ) );
}

static bool is_zero_bytes( const void* mem, size_t size )
{
  const char* iter = reinterpret_cast<const char*>(mem);
  const char* const end = iter + size;
  for (; iter != end; ++iter)
    if (*iter)
      return false;
  return true;
}

ErrorCode Core::tag_get_handle( const char* name,
                                int size,
                                DataType type,
                                Tag& tag_handle,
                                unsigned flags,
                                const void* default_value,
                                bool* created )
{
  if (created) *created = false;

  // we always work with sizes in bytes internally
  if (!((flags & MB_TAG_VARLEN) && size == MB_VARIABLE_LENGTH)) {
    if (flags & MB_TAG_BYTES) {
      if (size % TagInfo::size_from_data_type( type ))
        return MB_INVALID_SIZE;
    }
    else {
      size *= TagInfo::size_from_data_type( type );
    }
  }

  const TagType storage = static_cast<TagType>(flags & 3);

  // search for an existing tag
  tag_handle = 0;
  if (name && *name) { // not anonymous tag
    for (std::list<Tag>::iterator i = tagList.begin(); i != tagList.end(); ++i) {
      if ((*i)->get_name() == name) {
        tag_handle = *i;
        break;
      }
    }
  }

  if (tag_handle) {
    if (flags & MB_TAG_EXCL)
      return MB_ALREADY_ALLOCATED;
    // user asked that we not check anything
    if (flags & MB_TAG_ANY)
      return MB_SUCCESS;
    // user asked that we also match storage types
    if ((flags & MB_TAG_STORE) && tag_handle->get_storage_type() != storage)
      return MB_TYPE_OUT_OF_RANGE;
    // check if data type matches
    const DataType extype = tag_handle->get_data_type();
    if (extype != type) {
      if (flags & MB_TAG_NOOPQ)
        return MB_TYPE_OUT_OF_RANGE;
      else if (extype != MB_TYPE_OPAQUE && type != MB_TYPE_OPAQUE)
        return MB_TYPE_OUT_OF_RANGE;
    }

    // Require that the size value be zero or MB_VARIABLE_LENGTH
    // for variable length tags.  The caller passing such a size
    // value is sufficient to indicate that the caller is aware
    // that it is requesting a variable-length tag, so no need
    // to also require/check the MB_TAG_VARLEN bit in the flags.
    if (tag_handle->variable_length()) {
      if (size != 0 && size != MB_VARIABLE_LENGTH && !(flags & MB_TAG_VARLEN))
        return MB_INVALID_SIZE;
    }
    // But /do/ fail if MB_TAG_VARLEN flag is set and tag is
    // not variable length.
    else if (flags & MB_TAG_VARLEN)
      return MB_TYPE_OUT_OF_RANGE;
    // check size for fixed-length tag
    else if (tag_handle->get_size() != size)
      return MB_INVALID_SIZE;

    // If user passed a default value, check that it matches.
    // If user did not pass a default value, assume they're OK
    // with the existing one.
    // If tag does not have a default value but the user passed
    // one, allow it only if the tag is dense and the passed value
    // is all zero bytes because dense tags have an implicit default
    // value of all zeros in some cases.
    if (default_value && !(flags & MB_TAG_DFTOK) &&
        !(tag_handle->equals_default_value(default_value,size) ||
         (!tag_handle->get_default_value() &&
          tag_handle->get_storage_type() == MB_TAG_DENSE &&
          is_zero_bytes(default_value,size))))
      return MB_ALREADY_ALLOCATED;

    return MB_SUCCESS;
  }

    // MB_TAG_EXCL implies MB_TAG_CREAT
  if (!(flags & (MB_TAG_CREAT|MB_TAG_EXCL)))
    return MB_TAG_NOT_FOUND;

    // if a non-opaque non-bit type was specified, then the size
    // must be multiple of the size of the type
  if ((!(flags & MB_TAG_VARLEN) || default_value) &&
      (size <= 0 || (size % TagInfo::size_from_data_type(type)) != 0))
    return MB_INVALID_SIZE;

    // if MB_TYPE_BIT may be used only with MB_TAG_BIT
  //if (storage != MB_TAG_BIT && type == MB_TYPE_BIT)
  //    return MB_INVALID_ARG;
  if (type == MB_TYPE_BIT)
    flags &= ~(unsigned)(MB_TAG_DENSE|MB_TAG_SPARSE);

    // create the tag
  switch (flags & (MB_TAG_DENSE|MB_TAG_SPARSE|MB_TAG_MESH|MB_TAG_VARLEN)) {
    case MB_TAG_DENSE|MB_TAG_VARLEN:
      tag_handle = VarLenDenseTag::create_tag( sequenceManager, mError, name, type, default_value, size );
      break;
    case MB_TAG_DENSE:
      tag_handle = DenseTag::create_tag( sequenceManager, mError, name, size, type, default_value );
      break;
    case MB_TAG_SPARSE|MB_TAG_VARLEN:
      tag_handle = new VarLenSparseTag( name, type, default_value, size );
      break;
    case MB_TAG_SPARSE:
      tag_handle = new SparseTag( name, size, type, default_value );
      break;
    case MB_TAG_MESH|MB_TAG_VARLEN:
      tag_handle = new MeshTag( name, MB_VARIABLE_LENGTH, type, default_value, size );
      break;
    case MB_TAG_MESH:
      tag_handle = new MeshTag( name, size, type, default_value, size );
      break;
    case MB_TAG_BIT:
      if (MB_TYPE_BIT != type && MB_TYPE_OPAQUE != type)
        return MB_TYPE_OUT_OF_RANGE;
      tag_handle = BitTag::create_tag( name, size, default_value );
      break;
    default: // some illegal combination (multiple storage types, variable-length bit tag, etc.)
      return MB_TYPE_OUT_OF_RANGE;
  }

  if (!tag_handle)
    return MB_INVALID_SIZE;

  if (created) *created = true;
  tagList.push_back( tag_handle );
  return MB_SUCCESS;
}

ErrorCode Core::tag_get_handle( const char* name,
                                int size,
                                DataType type,
                                Tag& tag_handle,
                                unsigned flags,
                                const void* default_value ) const
{
    // If caller specified MB_TAG_EXCL, then we must fail because
    // this const function can never create a tag.  We need to test
    // this here because the non-const version of this function
    // assumes MB_TAG_CREAT if MB_TAG_EXCL is specified.
  if (flags & MB_TAG_EXCL) {
    // anonymous tag?
    if (!name || !*name)
      return MB_TAG_NOT_FOUND;

    // search for an existing tag
    tag_handle = 0;
    for (std::list<Tag>::const_iterator i = tagList.begin(); i != tagList.end(); ++i) {
      if ((*i)->get_name() == name) {
        tag_handle = *i;
        return MB_ALREADY_ALLOCATED;
      }
    }

    return MB_TAG_NOT_FOUND;
  }

  return const_cast<Core*>(this)->tag_get_handle(
                   name, size, type, tag_handle,
                   flags & ~(unsigned)MB_TAG_CREAT,
                   default_value );
}

//! removes the tag from the entity
ErrorCode  Core::tag_delete_data( Tag tag_handle,
                                  const EntityHandle *entity_handles,
                                  int num_entities )
{
  assert(valid_tag_handle( tag_handle ));
  CHECK_MESH_NULL
  return tag_handle->remove_data( sequenceManager, mError, entity_handles, num_entities );
}

//! removes the tag from the entity
ErrorCode  Core::tag_delete_data( Tag tag_handle,
                                  const Range &entity_handles )
{
  assert(valid_tag_handle( tag_handle ));
  return tag_handle->remove_data( sequenceManager, mError, entity_handles );
}

//! removes the tag from MB
ErrorCode Core::tag_delete(Tag tag_handle)
{
  std::list<TagInfo*>::iterator i = std::find( tagList.begin(), tagList.end(), tag_handle );
  if (i == tagList.end())
    return MB_TAG_NOT_FOUND;

  ErrorCode rval = tag_handle->release_all_data( sequenceManager, mError, true );MB_CHK_ERR(rval);

  tagList.erase( i );
  delete tag_handle;
  return MB_SUCCESS;
}

ErrorCode Core::tag_iterate( Tag tag_handle,
                             Range::const_iterator iter,
                             Range::const_iterator end,
                             int& count,
                             void*& data_ptr,
                             bool allocate)
{
  Range::const_iterator init = iter;
  assert(valid_tag_handle( tag_handle ));
  ErrorCode result = tag_handle->tag_iterate( sequenceManager, mError, iter, end, data_ptr,
                                              allocate);
  if (MB_SUCCESS == result)
    count = iter - init;
  return result;
}


//! gets the tag name string for the tag_handle
ErrorCode Core::tag_get_name( const Tag tag_handle,
                              std::string& tag_name ) const
{
  if (!valid_tag_handle( tag_handle ))
    return MB_TAG_NOT_FOUND;
  tag_name = tag_handle->get_name();
  return MB_SUCCESS;

}

ErrorCode Core::tag_get_handle( const char *tag_name, Tag &tag_handle ) const
{ return tag_get_handle( tag_name, 0, MB_TYPE_OPAQUE, tag_handle, MB_TAG_ANY ); }

  //! get size of tag in bytes
ErrorCode Core::tag_get_bytes(const Tag tag_handle, int &tag_size) const
{
  if (!valid_tag_handle( tag_handle ))
    return MB_TAG_NOT_FOUND;

  if (tag_handle->variable_length()) {
    tag_size = MB_VARIABLE_LENGTH;
    return MB_VARIABLE_DATA_LENGTH;
  }
  else if (tag_handle->get_storage_type() == MB_TAG_BIT) {
    tag_size = 1;
    return MB_SUCCESS;
  }
  else {
    tag_size = tag_handle->get_size();
    return MB_SUCCESS;
  }
}

  //! get size of tag in $values
ErrorCode Core::tag_get_length(const Tag tag_handle, int &tag_size) const
{
  if (!valid_tag_handle( tag_handle ))
    return MB_TAG_NOT_FOUND;

  if (tag_handle->variable_length()) {
    tag_size = MB_VARIABLE_LENGTH;
    return MB_VARIABLE_DATA_LENGTH;
  }
  else {
    tag_size = tag_handle->get_size()
      / TagInfo::size_from_data_type( tag_handle->get_data_type() );
    return MB_SUCCESS;
  }
}

ErrorCode Core::tag_get_data_type( const Tag handle, DataType& type ) const
{
  if (!valid_tag_handle( handle ))
    return MB_TAG_NOT_FOUND;

  type = handle->get_data_type();
  return MB_SUCCESS;
}

  //! get default value of the tag
ErrorCode Core::tag_get_default_value( const Tag tag_handle, void *def_value ) const
{
  if (!valid_tag_handle( tag_handle ))
    return MB_TAG_NOT_FOUND;

  if (tag_handle->variable_length())
    return MB_VARIABLE_DATA_LENGTH;

  if (!tag_handle->get_default_value())
    return MB_ENTITY_NOT_FOUND;

  memcpy( def_value, tag_handle->get_default_value(), tag_handle->get_default_value_size() );
  return MB_SUCCESS;
}

ErrorCode Core::tag_get_default_value( Tag tag, const void*& ptr, int& size ) const
{
  if (!valid_tag_handle( tag ))
    return MB_ENTITY_NOT_FOUND;

  if (!tag->get_default_value())
    return MB_ENTITY_NOT_FOUND;

  ptr = tag->get_default_value();
  size = tag->get_default_value_size() / TagInfo::size_from_data_type( tag->get_data_type() );
  return MB_SUCCESS;
}

  //! get type of tag (sparse, dense, etc.; 0 = dense, 1 = sparse, 2 = bit, 3 = static)
ErrorCode Core::tag_get_type(const Tag tag_handle, TagType &tag_type) const
{
  assert( valid_tag_handle( tag_handle ) );
  tag_type = tag_handle->get_storage_type();
  return MB_SUCCESS;
}

  //! get handles for all tags defined
ErrorCode Core::tag_get_tags(std::vector<Tag> &tag_handles) const
{
  std::copy( tagList.begin(), tagList.end(), std::back_inserter(tag_handles) );
  return MB_SUCCESS;
}

  //! Get handles for all tags defined on this entity
ErrorCode Core::tag_get_tags_on_entity(const EntityHandle entity,
                                       std::vector<Tag> &tag_handles) const
{
  for (std::list<TagInfo*>::const_iterator i = tagList.begin(); i != tagList.end(); ++i)
    if ((*i)->is_tagged( sequenceManager, entity ))
      tag_handles.push_back( *i );
  return MB_SUCCESS;
}

Tag Core::material_tag()
{
  const int negone = -1;
  if (0 == materialTag)
    tag_get_handle(MATERIAL_SET_TAG_NAME, 1,
                   MB_TYPE_INTEGER, materialTag,MB_TAG_CREAT|MB_TAG_SPARSE, &negone);
  return materialTag;
}

Tag Core::neumannBC_tag()
{
  const int negone = -1;
  if (0 == neumannBCTag)
    tag_get_handle(NEUMANN_SET_TAG_NAME, 1,
                   MB_TYPE_INTEGER, neumannBCTag,MB_TAG_CREAT|MB_TAG_SPARSE, &negone);
  return neumannBCTag;
}

Tag Core::dirichletBC_tag()
{
  const int negone = -1;
  if (0 == dirichletBCTag)
    tag_get_handle(DIRICHLET_SET_TAG_NAME, 1,
                   MB_TYPE_INTEGER, dirichletBCTag,MB_TAG_CREAT|MB_TAG_SPARSE, &negone);
  return dirichletBCTag;
}

Tag Core::globalId_tag()
{
  const int zero = 0;
  if (0 == globalIdTag)
    tag_get_handle(GLOBAL_ID_TAG_NAME, 1,
                   MB_TYPE_INTEGER, globalIdTag,MB_TAG_CREAT|MB_TAG_DENSE, &zero);
  return globalIdTag;
}

Tag Core::geom_dimension_tag()
{
  const int negone = -1;
  if (0 == geomDimensionTag)
    tag_get_handle(GEOM_DIMENSION_TAG_NAME, 1,
                   MB_TYPE_INTEGER, geomDimensionTag,MB_TAG_CREAT|MB_TAG_SPARSE, &negone);
  return geomDimensionTag;
}

//! creates an element based on the type and connectivity.  returns a handle and error code
ErrorCode Core::create_element(const EntityType type,
                                   const EntityHandle *connectivity,
                                   const int num_nodes,
                                   EntityHandle &handle)
{
    // make sure we have enough vertices for this entity type
  if(num_nodes < CN::VerticesPerEntity(type))
    return MB_FAILURE;

  ErrorCode status = sequence_manager()->create_element(type, connectivity, num_nodes, handle);
  if (MB_SUCCESS == status)
    status = aEntityFactory->notify_create_entity( handle, connectivity, num_nodes);

#ifdef MOAB_HAVE_AHF
  mesh_modified = true;
#endif


  return status;
}

//! creates a vertex based on coordinates, returns a handle and error code
ErrorCode Core::create_vertex(const double coords[3], EntityHandle &handle )
{
    // get an available vertex handle
  return sequence_manager()->create_vertex( coords, handle );
}

ErrorCode Core::create_vertices(const double *coordinates,
                                    const int nverts,
                                    Range &entity_handles )
{
    // Create vertices
  ReadUtilIface *read_iface;
  ErrorCode result = Interface::query_interface(read_iface);MB_CHK_ERR(result);

  std::vector<double*> arrays;
  EntityHandle start_handle_out = 0;
  result = read_iface->get_node_coords( 3, nverts, MB_START_ID,
                                        start_handle_out, arrays);
  Interface::release_interface(read_iface);MB_CHK_ERR(result);
  // Cppcheck warning (false positive): variable arrays is assigned a value that is never used
  for (int i = 0; i < nverts; i++) {
    arrays[0][i] = coordinates[3*i];
    arrays[1][i] = coordinates[3*i+1];
    arrays[2][i] = coordinates[3*i+2];
  }

  entity_handles.clear();
  entity_handles.insert(start_handle_out, start_handle_out+nverts-1);

  return MB_SUCCESS;
}


//! merges two  entities
ErrorCode Core::merge_entities( EntityHandle entity_to_keep,
                                      EntityHandle entity_to_remove,
                                      bool auto_merge,
                                      bool delete_removed_entity)
{
  if (auto_merge) return MB_FAILURE;

    // The two entities to merge must not be the same entity.
  if (entity_to_keep == entity_to_remove)
    return MB_FAILURE;

    // The two entities to merge must be of the same type
  EntityType type_to_keep = TYPE_FROM_HANDLE(entity_to_keep);

  if (type_to_keep != TYPE_FROM_HANDLE(entity_to_remove))
    return MB_TYPE_OUT_OF_RANGE;

    // Make sure both entities exist before trying to merge.
  EntitySequence* seq = 0;
  ErrorCode result, status;
  status = sequence_manager()->find(entity_to_keep, seq);
  if(seq == 0 || status != MB_SUCCESS)
    return MB_ENTITY_NOT_FOUND;
  status = sequence_manager()->find(entity_to_remove, seq);
  if(seq == 0 || status != MB_SUCCESS)
    return MB_ENTITY_NOT_FOUND;

    // If auto_merge is not set, all sub-entities should
    // be merged if the entities are to be merged.
  int ent_dim = CN::Dimension(type_to_keep);
  if(ent_dim > 0)
  {
    std::vector<EntityHandle> conn, conn2;

    result = get_connectivity(&entity_to_keep, 1, conn);MB_CHK_ERR(result);
    result = get_connectivity(&entity_to_remove, 1, conn2);MB_CHK_ERR(result);

      // Check to see if we can merge before pulling adjacencies.
    int dum1, dum2;
    if(!auto_merge &&
       (conn.size() != conn2.size() ||
        !CN::ConnectivityMatch(&conn[0], &conn2[0], conn.size(), dum1, dum2)))
      return MB_FAILURE;
  }

  result = aEntityFactory->merge_adjust_adjacencies(entity_to_keep, entity_to_remove);

  if (MB_SUCCESS == result && delete_removed_entity)
    result = delete_entities(&entity_to_remove, 1);

  return result;
}


//! deletes an entity range
ErrorCode Core::delete_entities(const Range &range)
{
  ErrorCode result = MB_SUCCESS, temp_result;
  Range failed_ents;

  for (std::list<TagInfo*>::iterator i = tagList.begin(); i != tagList.end(); ++i) {
    temp_result = (*i)->remove_data( sequenceManager, mError, range );
      // ok if the error is tag_not_found, some ents may not have every tag on them
    if (MB_SUCCESS != temp_result && MB_TAG_NOT_FOUND != temp_result)
      result = temp_result;
  }

  for (Range::const_reverse_iterator rit = range.rbegin(); rit != range.rend(); ++rit) {

      // tell AEntityFactory that this element is going away
    temp_result = aEntityFactory->notify_delete_entity(*rit);
    if (MB_SUCCESS != temp_result) {
      result = temp_result;
      failed_ents.insert(*rit);
      continue;
    }

    if (TYPE_FROM_HANDLE(*rit) == MBENTITYSET) {
      if (MeshSet* ptr = get_mesh_set( sequence_manager(), *rit )) {
        int j, count;
        const EntityHandle* rel;
        ptr->clear( *rit, a_entity_factory() );
        rel = ptr->get_parents( count );
        for (j = 0; j < count; ++j)
          remove_child_meshset( rel[j], *rit );
        rel = ptr->get_children( count );
        for (j = 0; j < count; ++j)
          remove_parent_meshset( rel[j], *rit );
      }
    }
  }

  if (!failed_ents.empty()) {
    Range dum_range = subtract(range, failed_ents);
      // don't test for success, since we'll return failure in this case
    sequence_manager()->delete_entities(mError,dum_range);
  }
  else
      // now delete the entities
    result = sequence_manager()->delete_entities(mError,range);

  return result;
}


//! deletes an entity vector
ErrorCode Core::delete_entities(const EntityHandle *entities,
                                    const int num_entities)
{
  ErrorCode result = MB_SUCCESS, temp_result;
  Range failed_ents;

  for (std::list<TagInfo*>::iterator i = tagList.begin(); i != tagList.end(); ++i) {
    temp_result = (*i)->remove_data( sequenceManager, mError, entities, num_entities);
      // ok if the error is tag_not_found, some ents may not have every tag on them
    if (MB_SUCCESS != temp_result && MB_TAG_NOT_FOUND != temp_result)
      result = temp_result;
  }

  for (int i = 0; i < num_entities; i++) {

      // tell AEntityFactory that this element is going away
    bool failed = false;
    temp_result = aEntityFactory->notify_delete_entity(entities[i]);
    if (MB_SUCCESS != temp_result) {
      result = temp_result;
      failed = true;
    }

    if (TYPE_FROM_HANDLE(entities[i]) == MBENTITYSET) {
      if (MeshSet* ptr = get_mesh_set( sequence_manager(), entities[i] )) {
        int j, count;
        const EntityHandle* rel;
        ptr->clear( entities[i], a_entity_factory() );
        rel = ptr->get_parents( count );
        for (j = 0; j < count; ++j)
          remove_child_meshset( rel[j], entities[i] );
        rel = ptr->get_children( count );
        for (j = 0; j < count; ++j)
          remove_parent_meshset( rel[j], entities[i] );
      }
    }

    if (failed)
        // don't test for success, since we'll return failure in this case
      sequence_manager()->delete_entity(mError, entities[i]);
    else {
        // now delete the entity
      temp_result = sequence_manager()->delete_entity(mError, entities[i]);
      if (MB_SUCCESS != temp_result) result = temp_result;
    }
  }

  return result;
}

ErrorCode Core::list_entities(const EntityHandle *entities,
                                    const int num_entities) const
{
  Range temp_range;
  ErrorCode result = MB_SUCCESS;
  if (NULL == entities && num_entities == 0) {
      // just list the numbers of each entity type
    int num_ents;
    std::cout << std::endl;
    std::cout << "Number of entities per type: " << std::endl;
    for (EntityType this_type = MBVERTEX; this_type < MBMAXTYPE; this_type++) {
      result = get_number_entities_by_type(0, this_type, num_ents);
      std::cout << CN::EntityTypeName(this_type) << ": " << num_ents << std::endl;
    }
    std::cout << std::endl;

    return MB_SUCCESS;
  }
  else if (NULL == entities && num_entities < 0) {

      // list all entities of all types
    std::cout << std::endl;
    for (EntityType this_type = MBVERTEX; this_type < MBMAXTYPE; this_type++) {
      result = get_entities_by_type(0, this_type, temp_range);
    }

    return list_entities(temp_range);
  }
  else if (NULL == entities && num_entities > 0) {

      // list all entities of type == num_entities
    std::cout << std::endl;
    result = get_entities_by_type(0, (EntityType)num_entities, temp_range);

    return list_entities(temp_range);
  }
  else {
    ErrorCode tmp_result;
    for (int i = 0; i < num_entities; i++) {
      EntityType this_type = TYPE_FROM_HANDLE(entities[i]);
      std::cout << CN::EntityTypeName(this_type) << " "
                << ID_FROM_HANDLE(entities[i]) << ":" << endl;

      tmp_result = (const_cast<Core*>(this))->list_entity(entities[i]);
      if (MB_SUCCESS != tmp_result) result = tmp_result;
    }
  }

  return result;
}

ErrorCode Core::list_entities(const Range &temp_range) const
{
  ErrorCode result = MB_SUCCESS, tmp_result;

  for (Range::const_iterator rit = temp_range.begin(); rit != temp_range.end(); ++rit) {
    EntityType this_type = TYPE_FROM_HANDLE(*rit);
    std::cout << CN::EntityTypeName(this_type) << " " << ID_FROM_HANDLE(*rit) << ":" << endl;

    tmp_result = (const_cast<Core*>(this))->list_entity(*rit);
    if (MB_SUCCESS != tmp_result) result = tmp_result;
  }

  return result;
}

ErrorCode Core::list_entity(const EntityHandle entity) const
{
  ErrorCode result;
  HandleVec adj_vec;

  if (!is_valid(entity)) {
    std::cout << "(invalid)" << std::endl;
    return MB_SUCCESS;
  }

  if (0 != globalIdTag) {
    int dum;
    result = tag_get_data(globalIdTag, &entity, 1, &dum);
    if (MB_SUCCESS == result)
      std::cout << "Global id = " << dum << std::endl;
  }

    // list entity
  EntityType this_type = TYPE_FROM_HANDLE(entity);
  if (this_type == MBVERTEX) {
    double coords[3];
    result = get_coords(&(entity), 1, coords);MB_CHK_ERR(result);
    std::cout << "Coordinates: (" << coords[0] << ", " << coords[1] << ", " << coords[2]
              << ")" << std::endl;
  }
  else if (this_type == MBENTITYSET)
    this->print(entity, "");

  std::cout << "  Adjacencies:" << std::endl;
  bool some = false;
  int multiple = 0;
  for (int dim = 0; dim <= 3; dim++) {
    if (dim == CN::Dimension(this_type)) continue;
    adj_vec.clear();
      // use const_cast here 'cuz we're in a const function and we're passing 'false' for
      // create_if_missing, so we know we won't change anything
    result = (const_cast<Core*>(this))->get_adjacencies(&(entity), 1, dim, false, adj_vec);
    if (MB_FAILURE == result) continue;
    for (HandleVec::iterator adj_it = adj_vec.begin(); adj_it != adj_vec.end(); ++adj_it) {
      if (adj_it != adj_vec.begin()) std::cout << ", ";
      else std::cout << "   ";
      std::cout << CN::EntityTypeName(TYPE_FROM_HANDLE(*adj_it)) << " " << ID_FROM_HANDLE(*adj_it);
    }
    if (!adj_vec.empty()) {
      std::cout << std::endl;
      some = true;
    }
    if (MB_MULTIPLE_ENTITIES_FOUND == result)
      multiple += dim;
  }
  if (!some) std::cout << "(none)" << std::endl;
  const EntityHandle *explicit_adjs;
  int num_exp;
  aEntityFactory->get_adjacencies(entity, explicit_adjs, num_exp);
  if (NULL != explicit_adjs && 0 != num_exp) {
    std::cout << "  Explicit adjacencies: ";
    for (int i = 0; i < num_exp; i++) {
      if (i != 0) std::cout << ", ";
      std::cout << CN::EntityTypeName(TYPE_FROM_HANDLE(explicit_adjs[i])) << " "
                << ID_FROM_HANDLE(explicit_adjs[i]);
    }
    std::cout << std::endl;
  }
  if (multiple != 0)
    std::cout << "   (MULTIPLE = " << multiple << ")" << std::endl;

  result = print_entity_tags(std::string(), entity, MB_TAG_DENSE);

  std::cout << std::endl;

  return result;
}

ErrorCode Core::convert_entities( const EntityHandle meshset,
                                  const bool mid_side, const bool mid_face, const bool mid_volume,
                                  Interface::HONodeAddedRemoved* function_object )
{
  HigherOrderFactory fact(this, function_object);
  return fact.convert(meshset, mid_side, mid_face, mid_volume);
}

  //! function to get the side number given two elements; returns
  //! MB_FAILURE if child not related to parent; does *not* create adjacencies
  //! between parent and child
ErrorCode Core::side_number(const EntityHandle parent,
                                  const EntityHandle child,
                                  int &sd_number,
                                  int &sense,
                                  int &offset) const
{
    // get the connectivity of parent and child
  const EntityHandle *parent_conn = NULL, *child_conn = NULL;
  int num_parent_vertices = 0, num_child_vertices = 0;
  ErrorCode result = get_connectivity(parent, parent_conn, num_parent_vertices, true);
  if (MB_NOT_IMPLEMENTED == result) {
    static std::vector<EntityHandle> tmp_connect(CN::MAX_NODES_PER_ELEMENT);
    result = get_connectivity(parent, parent_conn, num_parent_vertices, true, &tmp_connect);
  }
  if (MB_SUCCESS != result) return result;

  if (TYPE_FROM_HANDLE(child) == MBVERTEX) {
    int child_index = std::find(parent_conn, parent_conn+num_parent_vertices,
                                child) - parent_conn;
    if (child_index == num_parent_vertices) {
      sd_number = -1;
      sense = 0;
      return MB_FAILURE;
    }
    else {
      sd_number = child_index;
      sense = 1;
      return MB_SUCCESS;
    }
  }

  if (TYPE_FROM_HANDLE(parent) == MBPOLYHEDRON ) {
    // find the child in the parent_conn connectivity list, and call it a day ..
    // it should work only for faces within a conn list
    for (int i=0; i< num_parent_vertices; i++)
      if (child == parent_conn[i])
      {
        sd_number = i;
        sense =1 ; // always
        offset =0;
        return MB_SUCCESS;
      }
    return MB_FAILURE;
  }
  result = get_connectivity(child, child_conn, num_child_vertices, true);MB_CHK_ERR(result);

    // call handle vector-based function
  if (TYPE_FROM_HANDLE(parent) != MBPOLYGON &&
      TYPE_FROM_HANDLE(parent) != MBPOLYHEDRON) {

      // find indices into parent_conn for each entry in child_conn
    int child_conn_indices[10];
    assert((unsigned)num_child_vertices <= sizeof(child_conn_indices)/sizeof(child_conn_indices[0]));
    for (int i = 0; i < num_child_vertices; ++i) {
      child_conn_indices[i] = std::find( parent_conn,
        parent_conn + num_parent_vertices, child_conn[i] ) - parent_conn;
      if (child_conn_indices[i] >= num_parent_vertices) {
        sd_number = -1;
        return MB_FAILURE;
      }
    }

    int temp_result = CN::SideNumber(TYPE_FROM_HANDLE(parent),
                                       child_conn_indices, num_child_vertices,
                                       CN::Dimension(TYPE_FROM_HANDLE(child)),
                                       sd_number, sense, offset);
    return (0 == temp_result ? MB_SUCCESS : MB_FAILURE);
  }
  else if (TYPE_FROM_HANDLE(parent) == MBPOLYGON) {
      // find location of 1st vertex; this works even for padded vertices
    const EntityHandle *first_v = std::find(parent_conn, parent_conn+num_parent_vertices,
                                              child_conn[0]);
    if (first_v == parent_conn+num_parent_vertices) return MB_ENTITY_NOT_FOUND;
    sd_number = first_v - parent_conn;
    offset = sd_number;
    if (TYPE_FROM_HANDLE(child) == MBVERTEX) {
      sense = 0;
      return MB_SUCCESS;
    }
    else if (TYPE_FROM_HANDLE(child) == MBPOLYGON) {
      bool match = CN::ConnectivityMatch(parent_conn, child_conn,
                                           num_parent_vertices,
                                           sense, offset);
      sd_number = 0;
      if (match) return MB_SUCCESS;
      else return MB_ENTITY_NOT_FOUND;
    }
    else if (TYPE_FROM_HANDLE(child) == MBEDGE) {
      // determine the actual number of vertices, for the padded case
      // the padded case could be like ABCDEFFF; num_parent_vertices=8, actual_num_parent_vertices=6
      int actual_num_parent_vertices = num_parent_vertices;
      while(actual_num_parent_vertices>=3 &&
          (parent_conn[actual_num_parent_vertices-2] ==parent_conn[actual_num_parent_vertices-1] ) )
        actual_num_parent_vertices--;

      if (parent_conn[(sd_number+1)%num_parent_vertices] == child_conn[1])
        sense = 1;
      else if (parent_conn[(sd_number+num_parent_vertices-1)%num_parent_vertices] ==
               child_conn[1]) // this will also cover edge AF for padded case, side will be 0, sense -1
        sense = -1;
      // if edge FA in above example, we should return sd_number = 5, sense 1
      else if ((sd_number==actual_num_parent_vertices-1) && (child_conn[1]==parent_conn[0]))
        sense =1;
      else
        return MB_ENTITY_NOT_FOUND;
      return MB_SUCCESS;
    }
  }

  return MB_FAILURE;
}

  //! given an entity and the connectivity and type of one of its subfacets, find the
  //! high order node on that subfacet, if any
ErrorCode Core::high_order_node(const EntityHandle parent_handle,
                                      const EntityHandle *subfacet_conn,
                                      const EntityType subfacet_type,
                                      EntityHandle &hon) const
{
  hon = 0;

  EntityType parent_type = TYPE_FROM_HANDLE(parent_handle);

    // get the parent's connectivity
  const EntityHandle *parent_conn = NULL;
  int num_parent_vertices = 0;
  ErrorCode result = get_connectivity(parent_handle, parent_conn,
                                         num_parent_vertices, false);MB_CHK_ERR(result);

    // find whether this entity has ho nodes
  int mid_nodes[4];
  CN::HasMidNodes(parent_type, num_parent_vertices, mid_nodes);

    // check whether this entity has mid nodes on this dimension subfacet;
    // use dimension-1 because vertices don't have mid nodes
  if (!mid_nodes[CN::Dimension(subfacet_type)]) return MB_SUCCESS;

    // ok, we have mid nodes; now must compute expected index in connectivity array;
    // ho nodes stored for edges, faces then entity

    // offset starts with # corner vertices
  int offset = CN::VerticesPerEntity(parent_type);
  int i;

  for (i = 0; i < CN::Dimension(subfacet_type)-1; i++)
      // for each dimension lower than that of the subfacet we're looking for,
      // if this entity has midnodes in that dimension, increment offset by #
      // of subfacets of that dimension; use dimension-1 in loop because
      // canon numbering table only has 2 positions, for edges and faces;
    if (mid_nodes[i+1]) offset += CN::mConnectivityMap[parent_type][i].num_sub_elements;

    // now add the index of this subfacet; only need to if it's not the highest dimension
  if (subfacet_type != parent_type) {

      // find indices into parent_conn for each entry in child_conn
    unsigned subfacet_size = CN::VerticesPerEntity(subfacet_type);
    int subfacet_indices[10];
    assert(subfacet_size <= sizeof(subfacet_indices)/sizeof(subfacet_indices[0]));
    for (unsigned j = 0; j < subfacet_size; ++j) {
      subfacet_indices[j] = std::find( parent_conn,
        parent_conn + num_parent_vertices, subfacet_conn[j] ) - parent_conn;
      if (subfacet_indices[j] >= num_parent_vertices) {
        return MB_FAILURE;
      }
    }

    int dum, side_no, temp_offset;
    int temp_result =
      CN::SideNumber(  parent_type, subfacet_indices,
                         subfacet_size, subfacet_type,
                         side_no, dum, temp_offset);
    if(temp_result != 0) return MB_FAILURE;

    offset += side_no;
  }

    // offset shouldn't be off the end of the connectivity vector
  if (offset >= num_parent_vertices) return MB_INDEX_OUT_OF_RANGE;

  hon = parent_conn[offset];

  return MB_SUCCESS;
}

  //! given an entity and a target dimension & side number, get that entity
ErrorCode Core::side_element(const EntityHandle source_entity,
                                   const int dim,
                                   const int sd_number,
                                   EntityHandle &target_entity) const
{
    // get a handle on the connectivity
  const EntityHandle *verts;
  int num_verts;
  ErrorCode result = get_connectivity(source_entity, verts, num_verts);MB_CHK_ERR(result);

    // special case for vertices
  if (dim == 0) {
    if (sd_number < num_verts) {
      target_entity = verts[sd_number];
      return MB_SUCCESS;
    }

    else return MB_INDEX_OUT_OF_RANGE;
  }

    // get the vertices comprising the target entity
  Range side_verts, target_ents;
  const EntityType source_type = TYPE_FROM_HANDLE(source_entity);
    // first get the indices
  std::vector<int> vertex_indices;

  int temp_result =
    CN::AdjacentSubEntities(source_type, &sd_number, 1, dim, 0, vertex_indices);
  if (0 != temp_result) return MB_FAILURE;
    // now get the actual vertices
  for (unsigned int i = 0; i < vertex_indices.size(); i++)
    side_verts.insert(verts[vertex_indices[i]]);

    // now look for an entity of the correct type
    // use const_cast here 'cuz we're in a const function and we're passing 'false' for
    // create_if_missing, so we know we won't change anything
  result = (const_cast<Core*>(this))->get_adjacencies(side_verts, dim, false, target_ents);
  if (MB_SUCCESS != result && MB_MULTIPLE_ENTITIES_FOUND != result) return result;

  if (!target_ents.empty() &&
      TYPE_FROM_HANDLE(*(target_ents.begin())) != MBVERTEX &&
      TYPE_FROM_HANDLE(*(target_ents.begin())) !=
      CN::mConnectivityMap[source_type][dim-1].target_type[sd_number])
    return MB_ENTITY_NOT_FOUND;

  if (!target_ents.empty()) target_entity = *(target_ents.begin());

  return result;
}

//-------------------------Set Functions---------------------//

ErrorCode Core::create_meshset(const unsigned int setoptions,
                                   EntityHandle &ms_handle,
                                   int )
{
  return sequence_manager()->create_mesh_set( setoptions, ms_handle );
}

ErrorCode Core::get_meshset_options( const EntityHandle ms_handle,
                                          unsigned int& setoptions) const
{
  if (!ms_handle) { // root set
    setoptions = MESHSET_SET|MESHSET_TRACK_OWNER;
    return MB_SUCCESS;
  }

  const MeshSet* set = get_mesh_set( sequence_manager(), ms_handle );
  if (!set)
    return MB_ENTITY_NOT_FOUND;

  setoptions = set->flags();
  return MB_SUCCESS;
}

ErrorCode Core::set_meshset_options( const EntityHandle ms_handle,
                                         const unsigned int setoptions)
{
  MeshSet* set = get_mesh_set( sequence_manager(), ms_handle );
  if (!set)
    return MB_ENTITY_NOT_FOUND;

  return set->set_flags(setoptions, ms_handle, a_entity_factory());
}


ErrorCode Core::clear_meshset( const EntityHandle *ms_handles,
                                    const int num_meshsets)
{
  ErrorCode result = MB_SUCCESS;
  for (int i = 0; i < num_meshsets; ++i) {
    MeshSet* set = get_mesh_set( sequence_manager(), ms_handles[i]);
    if (set)
      set->clear(ms_handles[i], a_entity_factory());
    else
      result = MB_ENTITY_NOT_FOUND;
  }

  return result;
}

ErrorCode Core::clear_meshset(const Range &ms_handles)
{
  ErrorCode result = MB_SUCCESS;
  for (Range::iterator i = ms_handles.begin(); i != ms_handles.end(); ++i) {
    MeshSet* set = get_mesh_set( sequence_manager(), *i);
    if (set)
      set->clear(*i, a_entity_factory());
    else
      result = MB_ENTITY_NOT_FOUND;
  }

  return result;
}

ErrorCode Core::subtract_meshset(EntityHandle meshset1, const EntityHandle meshset2)
{
  MeshSet *set1 = get_mesh_set( sequence_manager(), meshset1 );
  MeshSet *set2 = get_mesh_set( sequence_manager(), meshset2 );
  if (!set1 || !set2)
    return MB_ENTITY_NOT_FOUND;

  return set1->subtract( set2, meshset1, a_entity_factory() );
}


ErrorCode Core::intersect_meshset(EntityHandle meshset1, const EntityHandle meshset2)
{
  MeshSet *set1 = get_mesh_set( sequence_manager(), meshset1 );
  MeshSet *set2 = get_mesh_set( sequence_manager(), meshset2 );
  if (!set1 || !set2)
    return MB_ENTITY_NOT_FOUND;

  return set1->intersect( set2, meshset1, a_entity_factory() );
}

ErrorCode Core::unite_meshset(EntityHandle meshset1, const EntityHandle meshset2)
{
  MeshSet *set1 = get_mesh_set( sequence_manager(), meshset1 );
  MeshSet *set2 = get_mesh_set( sequence_manager(), meshset2 );
  if (!set1 || !set2)
    return MB_ENTITY_NOT_FOUND;

  return set1->unite( set2, meshset1, a_entity_factory() );
}

ErrorCode Core::add_entities(EntityHandle meshset,
                                   const Range &entities)
{
  MeshSet* set = get_mesh_set( sequence_manager(), meshset );
  if (set)
    return set->add_entities( entities, meshset, a_entity_factory() );
  else
    return MB_ENTITY_NOT_FOUND;
}

ErrorCode Core::add_entities(EntityHandle meshset,
                                   const EntityHandle *entities,
                                   const int num_entities)
{
  MeshSet* set = get_mesh_set( sequence_manager(), meshset );
  if (set)
    return set->add_entities( entities, num_entities, meshset, a_entity_factory() );
  else
    return MB_ENTITY_NOT_FOUND;
}


//! remove a range of entities from a meshset
ErrorCode Core::remove_entities(EntityHandle meshset,
                                      const Range &entities)
{
  MeshSet* set = get_mesh_set( sequence_manager(), meshset );
  if (set)
    return set->remove_entities( entities, meshset, a_entity_factory() );
  else
    return MB_ENTITY_NOT_FOUND;
}

//! remove a vector of entities from a meshset
ErrorCode Core::remove_entities( EntityHandle meshset,
                                       const EntityHandle *entities,
                                       const int num_entities)
{
  MeshSet* set = get_mesh_set( sequence_manager(), meshset );
  if (set)
    return set->remove_entities( entities, num_entities, meshset, a_entity_factory() );
  else
    return MB_ENTITY_NOT_FOUND;
}

    //! return true if all entities are contained in set
bool Core::contains_entities(EntityHandle meshset,
                               const EntityHandle *entities,
                               int num_entities,
                               const int operation_type)
{
  if (!meshset) // root
    return true;
  else if (MeshSet* set = get_mesh_set( sequence_manager(), meshset ))
    return set->contains_entities(entities, num_entities, operation_type);
  else
    return false;
}

// replace entities in a meshset
ErrorCode Core::replace_entities(EntityHandle meshset,
                                     const EntityHandle *old_entities,
                                     const EntityHandle *new_entities,
                                     int num_entities)
{
  MeshSet* set = get_mesh_set( sequence_manager(), meshset );
  if (set)
    return set->replace_entities( meshset, old_entities, new_entities,
                                  num_entities, a_entity_factory());
  else
    return MB_ENTITY_NOT_FOUND;
}

ErrorCode Core::get_parent_meshsets(const EntityHandle meshset,
                                          std::vector<EntityHandle> &parents,
                                          const int num_hops) const
{
  if (0 == meshset) return MB_ENTITY_NOT_FOUND;

  const EntitySequence *seq;
  ErrorCode rval = sequence_manager()->find( meshset, seq );
  if (MB_SUCCESS != rval)
    return MB_ENTITY_NOT_FOUND;
  const MeshSetSequence* mseq = reinterpret_cast<const MeshSetSequence*>(seq);

  return mseq->get_parents( sequence_manager(), meshset, parents, num_hops );
}

ErrorCode Core::get_parent_meshsets(const EntityHandle meshset,
                                        Range &parents,
                                          const int num_hops) const
{
  if (0 == meshset) return MB_ENTITY_NOT_FOUND;

  std::vector<EntityHandle> parent_vec;
  ErrorCode result = get_parent_meshsets(meshset, parent_vec, num_hops);MB_CHK_ERR(result);
  std::sort( parent_vec.begin(), parent_vec.end() );
  std::copy(parent_vec.rbegin(), parent_vec.rend(), range_inserter(parents));
  return MB_SUCCESS;
}

ErrorCode Core::get_child_meshsets(const EntityHandle meshset,
                                         std::vector<EntityHandle> &children,
                                         const int num_hops) const
{
  if (0 == meshset) return MB_ENTITY_NOT_FOUND;

  const EntitySequence *seq;
  ErrorCode rval = sequence_manager()->find( meshset, seq );
  if (MB_SUCCESS != rval)
    return MB_ENTITY_NOT_FOUND;
  const MeshSetSequence* mseq = reinterpret_cast<const MeshSetSequence*>(seq);

  return mseq->get_children( sequence_manager(), meshset, children, num_hops );
}

ErrorCode Core::get_child_meshsets(const EntityHandle meshset,
                                        Range &children,
                                          const int num_hops) const
{
  if (0 == meshset) return MB_ENTITY_NOT_FOUND;

  std::vector<EntityHandle> child_vec;
  ErrorCode result = get_child_meshsets(meshset, child_vec, num_hops);MB_CHK_ERR(result);
  std::sort( child_vec.begin(), child_vec.end() );
  std::copy(child_vec.rbegin(), child_vec.rend(), range_inserter(children));
  return MB_SUCCESS;
}

ErrorCode Core::get_contained_meshsets( const EntityHandle meshset,
                                            std::vector<EntityHandle> &children,
                                            const int num_hops) const
{
  if (0 == meshset) {
    return get_entities_by_type( meshset, MBENTITYSET, children );
  }

  const EntitySequence *seq;
  ErrorCode rval = sequence_manager()->find( meshset, seq );
  if (MB_SUCCESS != rval)
    return MB_ENTITY_NOT_FOUND;
  const MeshSetSequence* mseq = reinterpret_cast<const MeshSetSequence*>(seq);

  return mseq->get_contained_sets( sequence_manager(), meshset, children, num_hops );
}

ErrorCode Core::get_contained_meshsets( const EntityHandle meshset,
                                            Range &children,
                                            const int num_hops) const
{
  if (0 == meshset) {
    return get_entities_by_type( meshset, MBENTITYSET, children );
  }

  std::vector<EntityHandle> child_vec;
  ErrorCode result = get_contained_meshsets(meshset, child_vec, num_hops);MB_CHK_ERR(result);
  std::sort( child_vec.begin(), child_vec.end() );
  std::copy(child_vec.rbegin(), child_vec.rend(), range_inserter(children));
  return MB_SUCCESS;
}

ErrorCode Core::num_parent_meshsets(const EntityHandle meshset, int* number,
                                        const int num_hops) const
{
  if (0 == meshset) return MB_ENTITY_NOT_FOUND;

  const EntitySequence *seq;
  ErrorCode rval = sequence_manager()->find( meshset, seq );
  if (MB_SUCCESS != rval)
    return MB_ENTITY_NOT_FOUND;
  const MeshSetSequence* mseq = reinterpret_cast<const MeshSetSequence*>(seq);

  return mseq->num_parents( sequence_manager(), meshset, *number, num_hops );
}

ErrorCode Core::num_child_meshsets(const EntityHandle meshset, int* number,
                                       const int num_hops) const
{
  if (0 == meshset) return MB_ENTITY_NOT_FOUND;

  const EntitySequence *seq;
  ErrorCode rval = sequence_manager()->find( meshset, seq );
  if (MB_SUCCESS != rval)
    return MB_ENTITY_NOT_FOUND;
  const MeshSetSequence* mseq = reinterpret_cast<const MeshSetSequence*>(seq);

  return mseq->num_children( sequence_manager(), meshset, *number, num_hops );
}

ErrorCode Core::num_contained_meshsets(const EntityHandle meshset, int* number,
                                       const int num_hops) const
{
  if (0 == meshset) {
    return get_number_entities_by_type( 0, MBENTITYSET, *number );
  }

  const EntitySequence *seq;
  ErrorCode rval = sequence_manager()->find( meshset, seq );
  if (MB_SUCCESS != rval)
    return MB_ENTITY_NOT_FOUND;
  const MeshSetSequence* mseq = reinterpret_cast<const MeshSetSequence*>(seq);

  return mseq->num_contained_sets( sequence_manager(), meshset, *number, num_hops );
}


ErrorCode Core::add_parent_meshset( EntityHandle meshset,
                                        const EntityHandle parent_meshset)
{
  MeshSet* set_ptr = get_mesh_set( sequence_manager(), meshset );
  MeshSet* parent_ptr = get_mesh_set( sequence_manager(), parent_meshset );
  if (!set_ptr || !parent_ptr)
    return MB_ENTITY_NOT_FOUND;

  set_ptr->add_parent( parent_meshset );
  return MB_SUCCESS;
}

ErrorCode Core::add_parent_meshsets( EntityHandle meshset,
                                         const EntityHandle* parents,
                                         int count )
{
  MeshSet* set_ptr = get_mesh_set( sequence_manager(), meshset );
  if (!set_ptr)
    return MB_ENTITY_NOT_FOUND;

  for (int i = 0; i < count; ++i)
    if (!get_mesh_set( sequence_manager(), parents[i] ))
      return MB_ENTITY_NOT_FOUND;

  for (int i = 0; i < count; ++i)
    set_ptr->add_parent( parents[i] );
  return MB_SUCCESS;
}

ErrorCode Core::add_child_meshset(EntityHandle meshset,
                                        const EntityHandle child_meshset)
{
  MeshSet* set_ptr = get_mesh_set( sequence_manager(), meshset );
  MeshSet* child_ptr = get_mesh_set( sequence_manager(), child_meshset );
  if (!set_ptr || !child_ptr)
    return MB_ENTITY_NOT_FOUND;

  set_ptr->add_child( child_meshset );
  return MB_SUCCESS;
}

ErrorCode Core::add_child_meshsets( EntityHandle meshset,
                                        const EntityHandle* children,
                                        int count )
{
  MeshSet* set_ptr = get_mesh_set( sequence_manager(), meshset );
  if (!set_ptr)
    return MB_ENTITY_NOT_FOUND;

  for (int i = 0; i < count; ++i)
    if (!get_mesh_set( sequence_manager(), children[i] ))
      return MB_ENTITY_NOT_FOUND;

  for (int i = 0; i < count; ++i)
    set_ptr->add_child( children[i] );
  return MB_SUCCESS;
}


ErrorCode Core::add_parent_child(EntityHandle parent,
                                       EntityHandle child)
{
  MeshSet* parent_ptr = get_mesh_set( sequence_manager(), parent );
  MeshSet* child_ptr = get_mesh_set( sequence_manager(), child );
  if (!parent_ptr || !child_ptr)
    return MB_ENTITY_NOT_FOUND;

  parent_ptr->add_child( child );
  child_ptr->add_parent( parent );
  return MB_SUCCESS;
}

ErrorCode Core::remove_parent_child(EntityHandle parent,
                                          EntityHandle child)
{
  MeshSet* parent_ptr = get_mesh_set( sequence_manager(), parent );
  MeshSet* child_ptr = get_mesh_set( sequence_manager(), child );
  if (!parent_ptr || !child_ptr)
    return MB_ENTITY_NOT_FOUND;

  parent_ptr->remove_child( child );
  child_ptr->remove_parent( parent );
  return MB_SUCCESS;
}


ErrorCode Core::remove_parent_meshset(EntityHandle meshset,
                                            const EntityHandle parent_meshset)
{
  MeshSet* set_ptr = get_mesh_set( sequence_manager(), meshset );
  if (!set_ptr)
    return MB_ENTITY_NOT_FOUND;
  set_ptr->remove_parent( parent_meshset );
  return MB_SUCCESS;
}

ErrorCode Core::remove_child_meshset(EntityHandle meshset,
                                           const EntityHandle child_meshset)
{
  MeshSet* set_ptr = get_mesh_set( sequence_manager(), meshset );
  if (!set_ptr)
    return MB_ENTITY_NOT_FOUND;
  set_ptr->remove_child( child_meshset );
  return MB_SUCCESS;
}

ErrorCode Core::get_last_error(std::string& info) const
{
  MBErrorHandler_GetLastError(info);
  return MB_SUCCESS;
}

std::string Core::get_error_string(const ErrorCode code) const
{
  return (unsigned)code <= (unsigned)MB_FAILURE ? ErrorCodeStr[code] : "INVALID ERROR CODE";
}

void Core::print(const EntityHandle ms_handle, const char *prefix,
                   bool first_call) const
{
    // get the entities
  Range entities;

  if (0 != ms_handle) {
    get_entities_by_handle( ms_handle, entities );
    std::cout << prefix << "MBENTITYSET " << ID_FROM_HANDLE(ms_handle)
              << std::endl;
  }
  else {
    get_entities_by_dimension(0, 3, entities);
    if (entities.empty()) get_entities_by_dimension(0, 2, entities);
    if (entities.empty()) get_entities_by_dimension(0, 1, entities);
    get_entities_by_dimension(0, 0, entities);
    get_entities_by_type(0, MBENTITYSET, entities);
    std::cout << prefix << "--: " << std::endl;
  }

  std::string indent_prefix = prefix;
  indent_prefix += "  ";
  entities.print(indent_prefix.c_str());

  if (!first_call || !ms_handle) return;

    // print parent/children
  Range temp;
  this->get_parent_meshsets(ms_handle, temp);
  std::cout << "  Parent sets: ";
  if (temp.empty()) std::cout << "(none)" << std::endl;
  else {
    for (Range::iterator rit = temp.begin(); rit != temp.end(); ++rit) {
      if (rit != temp.begin()) std::cout << ", ";
      std::cout << ID_FROM_HANDLE(*rit);
    }
    std::cout << std::endl;
  }

  temp.clear();
  this->get_child_meshsets(ms_handle, temp);
  std::cout << "  Child sets: ";
  if (temp.empty()) std::cout << "(none)" << std::endl;
  else {
    for (Range::iterator rit = temp.begin(); rit != temp.end(); ++rit) {
      if (rit != temp.begin()) std::cout << ", ";
      std::cout << ID_FROM_HANDLE(*rit);
    }
    std::cout << std::endl;
  }

    // print all sparse tags
  print_entity_tags(indent_prefix, ms_handle, MB_TAG_SPARSE);
}

ErrorCode Core::print_entity_tags(std::string indent_prefix, const EntityHandle handle, TagType tp) const
{
  std::vector<Tag> set_tags;
  ErrorCode result = this->tag_get_tags_on_entity(handle, set_tags);
  std::cout << indent_prefix << (tp == MB_TAG_SPARSE ? "Sparse tags:" : "Dense tags:") << std::endl;
  indent_prefix += "  ";

  for (std::vector<Tag>::iterator vit = set_tags.begin();
       vit != set_tags.end(); ++vit) {
    TagType this_type;
    result = this->tag_get_type(*vit, this_type);
    if (MB_SUCCESS != result || tp != this_type) continue;
    DataType this_data_type;
    result = this->tag_get_data_type(*vit, this_data_type);
    if (MB_SUCCESS != result) continue;
    int this_size;
    result = this->tag_get_length(*vit, this_size);
    if (MB_SUCCESS != result) continue;
      // use double since this is largest single-valued tag
    std::vector<double> dbl_vals(this_size);
    std::vector<int> int_vals(this_size);
    std::vector<EntityHandle> hdl_vals(this_size);
    std::string tag_name;
    result = this->tag_get_name(*vit, tag_name);
    if (MB_SUCCESS != result) continue;
    switch (this_data_type) {
      case MB_TYPE_INTEGER:
        result = this->tag_get_data(*vit, &handle, 1, &int_vals[0]);
        if (MB_SUCCESS != result) continue;
        std::cout << indent_prefix << tag_name << " = ";
        if (this_size < 10)
          for (int i = 0; i < this_size; i++) std::cout << int_vals[i] << " ";
        else std::cout << int_vals[0] << "... (mult values)";
        std::cout << std::endl;
        break;
      case MB_TYPE_DOUBLE:
        result = this->tag_get_data(*vit, &handle, 1, &dbl_vals[0]);
        if (MB_SUCCESS != result) continue;
        std::cout << indent_prefix << tag_name << " = ";
        if (this_size < 10)
          for (int i = 0; i < this_size; i++) std::cout << dbl_vals[i] << " ";
        else std::cout << dbl_vals[0] << "... (mult values)";
        std::cout << std::endl;
        break;
      case MB_TYPE_HANDLE:
        result = this->tag_get_data(*vit, &handle, 1, &hdl_vals[0]);
        if (MB_SUCCESS != result) continue;
        std::cout << indent_prefix << tag_name << " = ";
        if (this_size < 10)
          for (int i = 0; i < this_size; i++) std::cout << hdl_vals[i] << " ";
        else std::cout << hdl_vals[0] << "... (mult values)";
        std::cout << std::endl;
        break;
      case MB_TYPE_OPAQUE:
        if (NAME_TAG_SIZE == this_size) {
          char dum_tag[NAME_TAG_SIZE];
          result = this->tag_get_data(*vit, &handle, 1, &dum_tag);
          if (MB_SUCCESS != result) continue;
            // insert NULL just in case there isn't one
          dum_tag[NAME_TAG_SIZE-1] = '\0';
          std::cout << indent_prefix << tag_name << " = " << dum_tag << std::endl;
        }
        break;
      case MB_TYPE_BIT:
        break;
    }
  }

  return MB_SUCCESS;
}

ErrorCode Core::check_adjacencies()
{
    // run through all entities, checking adjacencies and reverse-evaluating them
  Range all_ents;
  ErrorCode result = get_entities_by_handle(0, all_ents);MB_CHK_ERR(result);

  for (Range::iterator rit = all_ents.begin(); rit != all_ents.end(); ++rit) {
    result = check_adjacencies(&(*rit), 1);MB_CHK_ERR(result);
  }

  return MB_SUCCESS;
}

ErrorCode Core::check_adjacencies(const EntityHandle *ents, int num_ents)
{

  ErrorCode result = MB_SUCCESS, tmp_result;
  std::ostringstream oss;

  for (int i = 0; i < num_ents; i++) {
    EntityHandle this_ent = ents[i];
    std::ostringstream ent_str;
    ent_str << CN::EntityTypeName(TYPE_FROM_HANDLE(this_ent)) << " "
            << ID_FROM_HANDLE(this_ent) << ": ";
    int this_dim = dimension_from_handle(this_ent);

    if (!is_valid(this_ent)) {
      std::cerr << ent_str.str()
                << "Not a valid entity." << std::endl;
      result = MB_FAILURE;
    }

    else {
      if (TYPE_FROM_HANDLE(this_ent) == MBENTITYSET) continue;

        // get adjacencies for this entity
      Range adjs;
      for (int dim = 0; dim <= 3; dim++) {
        if (dim == this_dim) continue;
        tmp_result = get_adjacencies(&this_ent, 1, dim, false, adjs, Interface::UNION);
        if (MB_SUCCESS != tmp_result) {
          oss << ent_str.str()
              << "Failed to get adjacencies for dimension " << dim << "." << std::endl;
          result = tmp_result;
        }
      }
      if (!oss.str().empty()) {
        std::cerr << oss.str();
        oss.str("");
      }

        // now check and reverse-evaluate them
      for (Range::iterator rit = adjs.begin(); rit != adjs.end(); ++rit) {
        EntitySequence* seq = 0;
        tmp_result = sequence_manager()->find(*rit, seq);
        if(seq == 0 || tmp_result != MB_SUCCESS) {
          oss << ent_str.str() <<
            "Adjacent entity " << CN::EntityTypeName(TYPE_FROM_HANDLE(*rit)) << " "
              << ID_FROM_HANDLE(*rit) << " is invalid." << std::endl;
          result = tmp_result;
        }
        else {
          Range rev_adjs;
          tmp_result = get_adjacencies(&(*rit), 1, this_dim, false, rev_adjs);
          if (MB_SUCCESS != tmp_result) {
            oss << ent_str.str()
                << "Failed to get reverse adjacency from "
                << CN::EntityTypeName(TYPE_FROM_HANDLE(*rit)) << " "
                << ID_FROM_HANDLE(*rit);
            if (MB_MULTIPLE_ENTITIES_FOUND == tmp_result)
              oss << " (MULTIPLE)" << std::endl;
            else oss << " (" << tmp_result << ")" << std::endl;
            result = tmp_result;
          }
          else if (rev_adjs.find(this_ent) == rev_adjs.end()) {
            oss << ent_str.str()
                << "Failed to find adjacency to this entity from "
                << CN::EntityTypeName(TYPE_FROM_HANDLE(*rit)) << " "
                << ID_FROM_HANDLE(*rit) << "." << std::endl;
            result = tmp_result;
          }
        }
        if (!oss.str().empty()) {
          std::cerr << oss.str();
          oss.str("");
        }
      }
    }
  }

  return result;
}

bool Core::is_valid(const EntityHandle this_ent) const
{
  const EntitySequence* seq = 0;
  ErrorCode result = sequence_manager()->find(this_ent, seq);
  return seq != 0 && result == MB_SUCCESS;
}

ErrorCode Core::create_set_iterator(EntityHandle meshset,
                                    EntityType ent_type,
                                    int ent_dim,
                                    int chunk_size,
                                    bool check_valid,
                                    SetIterator *&set_iter)
{
    // check the type of set
  unsigned int setoptions;
  ErrorCode rval = MB_SUCCESS;
  if (meshset) {
    rval = get_meshset_options(meshset, setoptions);MB_CHK_ERR(rval);
  }

  if (!meshset || (setoptions & MESHSET_SET))
    set_iter = new (std::nothrow) RangeSetIterator(this, meshset, chunk_size, ent_type, ent_dim, check_valid);
  else
    set_iter = new (std::nothrow) VectorSetIterator(this, meshset, chunk_size, ent_type, ent_dim, check_valid);

  setIterators.push_back(set_iter);
  return MB_SUCCESS;
}

    /** \brief Remove the set iterator from the instance's list
     * This function is called from the SetIterator destructor, and should not be called directly
     * from anywhere else.
     * \param set_iter Set iterator being removed
     */
ErrorCode Core::remove_set_iterator(SetIterator *set_iter)
{
  std::vector<SetIterator*>::iterator vit = std::find(setIterators.begin(), setIterators.end(), set_iter);
  if (vit == setIterators.end()) {
    MB_SET_ERR(MB_FAILURE, "Didn't find that iterator");
  }

  setIterators.erase(vit);

  return MB_SUCCESS;
}

  /** \brief Get all set iterators associated with the set passed in
   * \param meshset Meshset for which iterators are requested
   * \param set_iters Set iterators for the set
   */
ErrorCode Core::get_set_iterators(EntityHandle meshset,
                                  std::vector<SetIterator *> &set_iters)
{
  for (std::vector<SetIterator*>::const_iterator vit = setIterators.begin(); vit != setIterators.end(); ++vit)
    if ((*vit)->ent_set() == meshset) set_iters.push_back(*vit);
  return MB_SUCCESS;
}

void Core::estimated_memory_use_internal( const Range* ents,
                                  type_memstorage* total_storage,
                                  type_memstorage* total_amortized_storage,
                                  type_memstorage* entity_storage,
                                  type_memstorage* amortized_entity_storage,
                                  type_memstorage* adjacency_storage,
                                  type_memstorage* amortized_adjacency_storage,
                                  const Tag* tag_array,
                                  unsigned num_tags,
                                  type_memstorage* tag_storage,
                                  type_memstorage* amortized_tag_storage )
{
    // Figure out which values we need to calculate
  type_memstorage i_entity_storage,    ia_entity_storage,
                i_adjacency_storage, ia_adjacency_storage,
                i_tag_storage,       ia_tag_storage;
  type_memstorage *total_tag_storage = 0,
                *amortized_total_tag_storage =0;
  if (!tag_array) {
    total_tag_storage = tag_storage;
    amortized_total_tag_storage = amortized_tag_storage;
  }
  if (total_storage || total_amortized_storage) {
    if (!entity_storage)
      entity_storage = &i_entity_storage;
    if (!amortized_entity_storage)
      amortized_entity_storage = &ia_entity_storage;
    if (!adjacency_storage)
      adjacency_storage = &i_adjacency_storage;
    if (!amortized_adjacency_storage)
      amortized_adjacency_storage = &ia_adjacency_storage;
  }
  else {
    if (entity_storage || amortized_entity_storage) {
      if (!amortized_entity_storage)
        amortized_entity_storage = &ia_entity_storage;
      else if (!entity_storage)
        entity_storage = &i_entity_storage;
    }
    if (adjacency_storage || amortized_adjacency_storage) {
      if (!amortized_adjacency_storage)
        amortized_adjacency_storage = &ia_adjacency_storage;
      else if (!adjacency_storage)
        adjacency_storage = &i_adjacency_storage;
    }
  }
  if (!total_tag_storage && total_storage)
    total_tag_storage = &i_tag_storage;
  if (!amortized_total_tag_storage && total_amortized_storage)
    amortized_total_tag_storage = &ia_tag_storage;

    // get entity storage
  if (amortized_entity_storage) {
    if (ents)
      sequenceManager->get_memory_use( *ents, *entity_storage, *amortized_entity_storage );
    else
      sequenceManager->get_memory_use( *entity_storage, *amortized_entity_storage );
  }

    // get adjacency storage
  if (amortized_adjacency_storage) {
    if (ents)
      aEntityFactory->get_memory_use( *ents, *adjacency_storage, *amortized_adjacency_storage );
    else
#ifdef MOAB_HAVE_AHF
      ahfRep->get_memory_use(*adjacency_storage, *amortized_adjacency_storage);
#else
      aEntityFactory->get_memory_use( *adjacency_storage, *amortized_adjacency_storage );
#endif
  }

    // get storage for requested list of tags
  if (tag_array) {
    for (unsigned i = 0; i < num_tags; ++i) {
      if (!valid_tag_handle(tag_array[i]))
        continue;

      unsigned long total = 0, per_ent = 0;
      tag_array[i]->get_memory_use( sequenceManager, total, per_ent );

      if (ents) {
        size_t count = 0, count2 = 0;
        tag_array[i]->num_tagged_entities( sequenceManager, count, MBMAXTYPE, ents );
        if (tag_storage)
          tag_storage[i] = count * per_ent;
        if (amortized_tag_storage) {
          tag_array[i]->num_tagged_entities( sequenceManager, count2 );
          if (count2)
            amortized_tag_storage[i] = static_cast<type_memstorage>(total * count * 1.0/ count2);
        }
      }
      else {
        size_t count = 0;
        if (tag_storage) {
          tag_array[i]->num_tagged_entities( sequenceManager, count );
          tag_storage[i] = count * per_ent;
        }
        if (amortized_tag_storage)
          amortized_tag_storage[i] = total;
      }
    }
  }

    // get storage for all tags
  if (total_tag_storage || amortized_total_tag_storage) {
    if (amortized_total_tag_storage)
      *amortized_total_tag_storage = 0;
    if (total_tag_storage)
      *total_tag_storage =0;

    std::vector<Tag> tags;
    tag_get_tags( tags );
    for (std::list<TagInfo*>::const_iterator i = tagList.begin(); i != tagList.end(); ++i) {
      unsigned long total = 0, per_ent = 0;
      (*i)->get_memory_use( sequenceManager, total, per_ent );

      if (ents) {
        size_t count = 0, count2 = 0;
        (*i)->num_tagged_entities( sequenceManager, count, MBMAXTYPE, ents );
        if (total_tag_storage)
          *total_tag_storage += count * per_ent;
        if (amortized_total_tag_storage) {
          (*i)->num_tagged_entities( sequenceManager, count2 );
          if (count2)
            *amortized_total_tag_storage += static_cast<type_memstorage>(total * count * 1.0/ count2);
        }
      }
      else {
        size_t count = 0;
        if (total_tag_storage) {
          (*i)->num_tagged_entities( sequenceManager, count );
          *total_tag_storage += count * per_ent;
        }
        if (amortized_total_tag_storage)
          *amortized_total_tag_storage += total;
      }
    }
  }

    // calculate totals
  if (total_storage)
    *total_storage = *entity_storage + *adjacency_storage + *total_tag_storage;

  if (total_amortized_storage)
    *total_amortized_storage = *amortized_entity_storage
                             + *amortized_adjacency_storage
                             + *amortized_total_tag_storage;
}


void  Core::estimated_memory_use( const EntityHandle* ent_array,
                                    unsigned long num_ents,
                                    type_memstorage* total_storage,
                                    type_memstorage* total_amortized_storage,
                                    type_memstorage* entity_storage,
                                    type_memstorage* amortized_entity_storage,
                                    type_memstorage* adjacency_storage,
                                    type_memstorage* amortized_adjacency_storage,
                                    const Tag* tag_array,
                                    unsigned num_tags,
                                    type_memstorage* tag_storage,
                                    type_memstorage* amortized_tag_storage )
{
  Range range;

    // If non-empty entity list, call range version of function
  if (ent_array) {
    if (num_ents > 20) {
      std::vector<EntityHandle> list(num_ents);
      std::copy(ent_array, ent_array+num_ents, list.begin());
      std::sort( list.begin(), list.end() );
      Range::iterator j = range.begin();
      for (std::vector<EntityHandle>::reverse_iterator i = list.rbegin(); i != list.rend(); ++i)
        j = range.insert( j, *i, *i );
    }
    else {
      std::copy( ent_array, ent_array + num_ents, range_inserter(range) );
    }
  }

  estimated_memory_use_internal( ent_array ? &range : 0,
                         total_storage,     total_amortized_storage,
                         entity_storage,    amortized_entity_storage,
                         adjacency_storage, amortized_adjacency_storage,
                         tag_array,         num_tags,
                         tag_storage,       amortized_tag_storage );
}

void Core::estimated_memory_use( const Range& ents,
                                   type_memstorage* total_storage,
                                   type_memstorage* total_amortized_storage,
                                   type_memstorage* entity_storage,
                                   type_memstorage* amortized_entity_storage,
                                   type_memstorage* adjacency_storage,
                                   type_memstorage* amortized_adjacency_storage,
                                   const Tag* tag_array,
                                   unsigned num_tags,
                                   type_memstorage* tag_storage,
                                   type_memstorage* amortized_tag_storage )
{
  estimated_memory_use_internal( &ents,
                         total_storage,     total_amortized_storage,
                         entity_storage,    amortized_entity_storage,
                         adjacency_storage, amortized_adjacency_storage,
                         tag_array,         num_tags,
                         tag_storage,       amortized_tag_storage );
}

void Core::print_database() const
{
  ErrorCode rval;
  TypeSequenceManager::const_iterator i;
  const TypeSequenceManager& verts = sequence_manager()->entity_map(MBVERTEX);
  if (!verts.empty())
    printf("  Vertex ID  X        Y        Z        Adjacencies   \n"
           "  ---------- -------- -------- -------- -----------...\n");
  const EntityHandle* adj;
  int nadj;
  for (i = verts.begin(); i != verts.end(); ++i) {
    const VertexSequence* seq = static_cast<const VertexSequence* >(*i);
    printf("(Sequence [%d,%d] in SequenceData [%d,%d])\n",
      (int)ID_FROM_HANDLE(seq->start_handle()),
      (int)ID_FROM_HANDLE(seq->end_handle()),
      (int)ID_FROM_HANDLE(seq->data()->start_handle()),
      (int)ID_FROM_HANDLE(seq->data()->end_handle()));

    double c[3];
    for (EntityHandle h = seq->start_handle(); h <= seq->end_handle(); ++h) {
      rval = seq->get_coordinates( h, c );
      if (MB_SUCCESS == rval)
        printf("  %10d %8g %8g %8g", (int)ID_FROM_HANDLE(h), c[0], c[1], c[2] );
      else
        printf("  %10d <       ERROR %4d       >", (int)ID_FROM_HANDLE(h), (int)rval );

      rval = a_entity_factory()->get_adjacencies( h, adj, nadj );
      if (MB_SUCCESS != rval) {
        printf(" <ERROR %d>\n", (int)rval );
        continue;
      }
      EntityType pt = MBMAXTYPE;
      for (int j = 0; j < nadj; ++j) {
        if (TYPE_FROM_HANDLE(adj[j]) != pt) {
          pt = TYPE_FROM_HANDLE(adj[j]);
          printf("  %s", pt >= MBMAXTYPE ? "INVALID TYPE" : CN::EntityTypeName(pt) );
        }
        printf(" %d", (int)ID_FROM_HANDLE(adj[j]));
      }
      printf("\n");
    }
  }

  for (EntityType t = MBEDGE; t < MBENTITYSET; ++t) {
    const TypeSequenceManager& elems = sequence_manager()->entity_map(t);
    if (elems.empty())
      continue;

    int clen = 0;
    for (i = elems.begin(); i != elems.end(); ++i) {
      int n = static_cast<const ElementSequence*>(*i)->nodes_per_element();
      if (n > clen)
        clen = n;
    }

    clen *= 5;
    if (clen < (int)strlen("Connectivity"))
      clen = strlen("Connectivity");
    std::vector<char> dashes( clen, '-' );
    dashes.push_back( '\0' );
    printf( "  %7s ID %-*s Adjacencies\n", CN::EntityTypeName(t), clen, "Connectivity" );
    printf( "  ---------- %s -----------...\n", &dashes[0] );

    std::vector<EntityHandle> storage;
    const EntityHandle* conn;
    int nconn;
    for (i = elems.begin(); i != elems.end(); ++i) {
      const ElementSequence* seq = static_cast<const ElementSequence*>(*i);
      printf("(Sequence [%d,%d] in SequenceData [%d,%d])\n",
        (int)ID_FROM_HANDLE(seq->start_handle()),
        (int)ID_FROM_HANDLE(seq->end_handle()),
        (int)ID_FROM_HANDLE(seq->data()->start_handle()),
        (int)ID_FROM_HANDLE(seq->data()->end_handle()));

      for (EntityHandle h = seq->start_handle(); h <= seq->end_handle(); ++h) {
        printf( "  %10d", (int)ID_FROM_HANDLE(h) );
        rval = get_connectivity( h, conn, nconn, false, &storage );
        if (MB_SUCCESS != rval)
          printf( "  <ERROR %2d>%*s", (int)rval, clen-10, "" );
        else {
          for (int j = 0; j < nconn; ++j)
            printf(" %4d", (int)ID_FROM_HANDLE(conn[j]));
          printf("%*s", clen - 5*nconn, "" );
        }

        rval = a_entity_factory()->get_adjacencies( h, adj, nadj );
        if (MB_SUCCESS != rval) {
          printf(" <ERROR %d>\n", (int)rval );
          continue;
        }
        EntityType pt = MBMAXTYPE;
        for (int j = 0; j < nadj; ++j) {
          if (TYPE_FROM_HANDLE(adj[j]) != pt) {
            pt = TYPE_FROM_HANDLE(adj[j]);
            printf("  %s", pt >= MBMAXTYPE ? "INVALID TYPE" : CN::EntityTypeName(pt) );
          }
          printf(" %d", (int)ID_FROM_HANDLE(adj[j]));
        }
        printf("\n");
      }
    }
  }
}

ErrorCode Core::create_scd_sequence(const HomCoord & coord_min,
					const HomCoord &  coord_max,
					EntityType  type,
					EntityID  start_id_hint,
					EntityHandle &  first_handle_out,
					EntitySequence *&  sequence_out )
{
  //NR: Previously, the structured element sequences were created via direct call to
  //the sequence manager instead of using the same from the ScdInterface which
  //creates the associated scd bounding box after element sequence creation.

  if(!scdInterface)
    scdInterface = new ScdInterface(this);
  ScdBox * newBox = NULL;
  ErrorCode rval = scdInterface->create_scd_sequence(coord_min, coord_max, type,
                              /*starting_id*/    (int)start_id_hint, newBox); MB_CHK_ERR(rval);

  if (MBVERTEX==type)
    first_handle_out = newBox->get_vertex(coord_min);
  else
    first_handle_out = newBox->get_element(coord_min);
  return  sequence_manager()->find( first_handle_out, sequence_out );
}

ErrorCode Core::add_vsequence(EntitySequence *    vert_seq,
				  EntitySequence *  elem_seq,
				  const HomCoord &  p1,
				  const HomCoord &  q1,
				  const HomCoord &  p2,
				  const HomCoord &  q2,
				  const HomCoord &  p3,
				  const HomCoord &  q3,
				  bool  bb_input,
				  const HomCoord *  bb_min,
				  const HomCoord *  bb_max )
{
  return sequence_manager()->add_vsequence(vert_seq, elem_seq,
					   p1, q1, p2, q2, p3, q3,
					   bb_input, bb_min, bb_max);

}

} // namespace moab
