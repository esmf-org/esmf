/* WriteDamsel.cpp
 * The Damsel library provides mesh-aware parallel I/O; see http://cucis.ece.northwestern.edu/projects/DAMSEL/ for details,
 * though for now that site is restricted to project participants.  Damsel uses a data model that's very similar to
 * that used in MOAB and ITAPS.  It uses the same basic data model concepts of entities, sets, tags, and interface.
 * In theory, we should be able to completely save/restore to/from Damsel any data that can be saved/restored to/from
 * our native HDF5-based reader/writer.
 *
 * Mapping between MOAB-Damsel data models
 * =======================================
 * Basic data model entities, MOAB <--> Damsel:
 *        Entity <--> Entity
 *     EntitySet <--> Collection
 *           Tag <--> Tag
 * API/data strutures:
 *          Range (n1) <--> Sequence container
 *         std::vector <--> Vector container
 *          Range (n2) <--> Tree container
 *
 * n1: single contiguous subrange
 * n2: multiple subranges
 *
 * Conventions
 * ===========
 * There are parts of MOAB data structures that need to be stored to Damsel that aren't represented in the Damsel data model,
 * e.g. dense vs. sparse storage type, set tracking flags.  
 * - We need to store these as tags in Damsel.  
 * - Since Damsel tags need to have a MOAB counterpart, we have to create those as tag data in MOAB too 
 *   (duplicating the data in the data structures, bummer).
 * - Because we may want to use these tags for multiple Damsel writes/models, we create the MOAB-side tags in the WriteDamsel
 *   constructor, not in the init_tags function that's called for every write
 * - Conventional tags have names prefixed with mbdmsl_ to avoid name conflicts with other MOAB tags.
 * Here we list the conventional tags used by MOAB's Damsel reader/writer.
 * 
 * Tag name                   Tag char's (storage type, data type, length, def val)       Values, used for what
 * --------                   -----------------------------------------------------       --------------------
 * mbdmsl_XCOORDS             dense; double[1]; 0.0                                       MOAB vertex x coordinate
 * mbdmsl_YCOORDS             dense; double[1]; 0.0                                       MOAB vertex y coordinate
 * mbdmsl_ZCOORDS             dense; double[1]; 0.0                                       MOAB vertex z coordinate
 * mbdmsl_COLL_FLAGS          sparse; char; 1; 0x0                                        bit 0: 0=set-type, 1=vector-type
 *                                                                                            1: 1=tracking, 0=not tracking
 * mbdmsl_PARENTS      sparse; handle; var;                                        (list of parent sets)
 * mbdmsl_CHILDS       sparse; handle; var;                                        (list of child sets)
 *
 * 
 * 
 */

#include "WriteDamsel.hpp"

#include "DamselUtil.hpp"
#include "damsel.h"
#include "assert.h"
#include "moab/Interface.hpp"
#include "moab/Core.hpp"
#include "moab/Range.hpp"
#include "moab/Error.hpp"
#include "moab/WriteUtilIface.hpp"
#include "MBTagConventions.hpp"
#include "EntitySequence.hpp"
#include "Internals.hpp"
#include "DenseTag.hpp"
#include "SparseTag.hpp"

namespace moab {

WriterIface* WriteDamsel::factory(Interface* iface)
{
  return new WriteDamsel(iface);
}

WriteDamsel::WriteDamsel(Interface *impl)
  : mbImpl(impl), mWriteIface(NULL), sequenceManager(NULL), dU(),
    DAMSEL_FLAGS(DAMSEL_IS_TRACKING)
{
  assert(impl != NULL);

  impl->query_interface(mWriteIface);
  assert(mWriteIface);

  sequenceManager = dynamic_cast<Core*>(impl)->sequence_manager();
  assert(sequenceManager);

  ErrorCode rval = mbImpl->tag_get_handle("mbdmsl_XCOORDS", 1, MB_TYPE_DOUBLE,
                                         dU.xcoordsTag.mTagh, MB_TAG_DENSE | MB_TAG_CREAT);MB_CHK_SET_ERR_CONT(rval, "Failed to create_tag mbdmsl_XCOORDS");
  dU.xcoordsTag.tagType = MB_TAG_ANY;
  dU.tagMap.push_back(dU.xcoordsTag);
  rval = mbImpl->tag_get_handle("mbdmsl_YCOORDS", 1, MB_TYPE_DOUBLE,
                               dU.ycoordsTag.mTagh, MB_TAG_DENSE | MB_TAG_CREAT);MB_CHK_SET_ERR_CONT(rval, "Failed to create_tag mbdmsl_YCOORDS");
  dU.ycoordsTag.tagType = MB_TAG_ANY;
  dU.tagMap.push_back(dU.ycoordsTag);

  rval = mbImpl->tag_get_handle("mbdmsl_ZCOORDS", 1, MB_TYPE_DOUBLE,
                               dU.zcoordsTag.mTagh, MB_TAG_DENSE | MB_TAG_CREAT);MB_CHK_SET_ERR_CONT(rval, "Failed to create_tag mbdmsl_ZCOORDS");
  dU.zcoordsTag.tagType = MB_TAG_ANY;
  dU.tagMap.push_back(dU.zcoordsTag);

  rval = mbImpl->tag_get_handle("mbdmsl_COLL_FLAGS", 1, MB_TYPE_INTEGER,
                               dU.collFlagsTag.mTagh, MB_TAG_DENSE | MB_TAG_CREAT);MB_CHK_SET_ERR_CONT(rval, "Failed to create_tag mbdmsl_COLL_FLAGS");
  dU.collFlagsTag.tagType = MB_TAG_ANY;
  dU.tagMap.push_back(dU.collFlagsTag);

/*
  rval = mbImpl->tag_get_handle("mbdmsl_PARENTS", 1, MB_TYPE_HANDLE,
                               dU.parentsTag.mTagh, MB_TAG_DENSE | MB_TAG_CREAT | MB_TAG_VARLEN);MB_CHK_SET_ERR_CONT(rval, "Failed to create_tag mbdmsl_PARENTS");
  dU.parentsTag.tagType = MB_TAG_DENSE;
  dU.tagMap.push_back(dU.parentsTag);

  rval = mbImpl->tag_get_handle("mbdmsl_CHILDREN", 1, MB_TYPE_HANDLE,
                               dU.childrenTag.mTagh, MB_TAG_DENSE | MB_TAG_CREAT | MB_TAG_VARLEN);MB_CHK_SET_ERR_CONT(rval, "Failed to create_tag mbdmsl_CHILDREN");
  dU.childrenTag.tagType = MB_TAG_DENSE;
  dU.tagMap.push_back(dU.childrenTag);
*/

  dU.moabHandleType = (sizeof(EntityHandle) == 64 ? DAMSEL_HANDLE_TYPE_HANDLE64 :
                       DAMSEL_HANDLE_TYPE_HANDLE32);
}

WriteDamsel::~WriteDamsel()
{
  if (mWriteIface)
    mbImpl->release_interface(mWriteIface);
}

ErrorCode WriteDamsel::write_file(const char *file_name,
                                  const bool /* overwrite */,
                                  const FileOptions& opts,
                                  const EntityHandle *meshset_list,
                                  const int num_sets,
                                  const std::vector<std::string>& /* qa_records */,
                                  const Tag* /* tag_list */,
                                  int /* num_tags */,
                                  int /* requested_output_dimension */)
{
  // Gather all entities into one big range
  Range all_ents;
  ErrorCode rval;
  damsel_err_t err;

  dU.dmslLib = DMSLlib_init();

  // Create a damsel model
  dU.dmslModel = DMSLmodel_create(sizeof(EntityHandle) == 8 ? DAMSEL_HANDLE_TYPE_HANDLE64 :
                               DAMSEL_HANDLE_TYPE_HANDLE32);

  // Attach to a file, since we need it for creating containers
  MPI_Comm comm = MPI_COMM_WORLD;
  unlink(file_name);
  err = DMSLmodel_attach(dU.dmslModel, file_name, comm, NULL);CHK_DMSL_ERR(err, "DMSLmodel_attach failed");

  rval = mWriteIface->gather_entities(all_ents, meshset_list, num_sets);MB_CHK_SET_ERR(rval, "Gather entities failed in WriteDamsel");

  if (all_ents.empty())
    return MB_SUCCESS;

  // Create damsel tags for MOAB dense, sparse, and conventional tags
  rval = init_tag_info();MB_CHK_ERR(rval);

  // Iterate through the groups of contiguous sequences of handles
  RangeSeqIntersectIter rsi(sequenceManager);
  rval = rsi.init(all_ents.begin(), all_ents.end());

  while (MB_SUCCESS == rval) {
    // Write subrange of things to damsel: map handles, map entity definition data (connectivity/coords/set contents),
    // map dense tags
    rval = write_subrange(rsi);MB_CHK_SET_ERR(rval, "Failed to write subrange");

    rval = rsi.step();
    while (MB_ENTITY_NOT_FOUND == rval)
      rval = rsi.step();
  }

  // Write sparse tags
  rval = map_sparse_tags();MB_CHK_SET_ERR(rval, "Failed to write sparse tags");

  //damsel_request_t request;
  //err = DMSLmodel_transfer_async(dU.dmslModel, DAMSEL_TRANSFER_TYPE_WRITE, &request);
  err = DMSLmodel_transfer_sync(dU.dmslModel, DAMSEL_TRANSFER_TYPE_WRITE);CHK_DMSL_ERR(err, "DMSLmodel_transfer_asynch failed");

  //damsel_status_t status;
  //err = DMSLmodel_wait(request, &status);CHK_DMSL_ERR(err, "DMSLmodel_wait failed");

  DMSLmodel_close(dU.dmslModel);

  DMSLlib_finalize(dU.dmslLib);

  // We should be done
  return MB_SUCCESS;
}

ErrorCode WriteDamsel::init_tag_info()
{
  // Initialize allTags and tagIndices
  std::vector<Tag> tmp_mtags;
  ErrorCode rval = mbImpl->tag_get_tags(tmp_mtags);MB_CHK_SET_ERR(rval, "Failed to get all tag handles.");
  int dum_size;
  damsel_err_t err;

  // Define damsel tag handles for all dense/sparse tags
  for (std::vector<Tag>::iterator vit = tmp_mtags.begin(); vit != tmp_mtags.end(); ++vit) {
    if (((*vit)->get_storage_type() != MB_TAG_DENSE && (*vit)->get_storage_type() != MB_TAG_SPARSE) ||
        mbImpl->tag_get_length(*vit, dum_size) == MB_VARIABLE_DATA_LENGTH ||
        dum_size != 1) {
      std::cerr << "Warning: tag " << (*vit)->get_name()
                << "is not of type dense or sparse, and is not currently supported by the damsel writer." 
                << std::endl;
      continue;
    }

    std::vector<DamselUtil::tinfo>::iterator vit2 =
        std::find_if(dU.tagMap.begin(), dU.tagMap.end(), DamselUtil::MtagP<DamselUtil::tinfo>(*vit));

    if (vit2 != dU.tagMap.end() && (*vit2).tagType == MB_TAG_ANY) 
      // Conventional tag - skip
      continue;

    else if (vit2 == dU.tagMap.end()) {
      // Create a damsel counterpart for this tag
      Tag thandle = *vit;
      err = DMSLtag_define(dU.dmslModel, (damsel_handle_ptr)&thandle,
                                        DamselUtil::mtod_data_type[(*vit)->get_data_type()],
                            (*vit)->get_name().c_str());CHK_DMSL_ERR(err, "Failure to get Damsel tag for MOAB tag");
      dU.tagMap.push_back(DamselUtil::tinfo(thandle, 0, (*vit)->get_storage_type()));
    }
    else {
      // Assert there's a corresponding moab tag handle
      assert((*vit2).mTagh);
    }
  }

  // Do the same for conventional tags:
  // XCOORDS
  err = DMSLtag_define(dU.dmslModel, (damsel_handle_ptr)&(dU.xcoordsTag.mTagh),
                                       DamselUtil::mtod_data_type[MB_TYPE_DOUBLE],
                                       dU.xcoordsTag.mTagh->get_name().c_str());
  dU.tagMap.push_back(dU.xcoordsTag);CHK_DMSL_ERR(err, "Failure to get Damsel tag for MOAB tag");

  // YCOORDS
  err = DMSLtag_define(dU.dmslModel, (damsel_handle_ptr)&(dU.ycoordsTag.mTagh),
                                       DamselUtil::mtod_data_type[MB_TYPE_DOUBLE],
                                       dU.ycoordsTag.mTagh->get_name().c_str());
  dU.tagMap.push_back(dU.ycoordsTag);CHK_DMSL_ERR(err, "Failure to get Damsel tag for MOAB tag");

  // ZCOORDS
  err = DMSLtag_define(dU.dmslModel, (damsel_handle_ptr)&(dU.zcoordsTag.mTagh),
                                       DamselUtil::mtod_data_type[MB_TYPE_DOUBLE],
                                       dU.zcoordsTag.mTagh->get_name().c_str());
  dU.tagMap.push_back(dU.zcoordsTag);CHK_DMSL_ERR(err, "Failure to get Damsel tag for MOAB tag");

  // COLL_FLAGS
  err = DMSLtag_define(dU.dmslModel, (damsel_handle_ptr)&(dU.collFlagsTag.mTagh),
                                       DamselUtil::mtod_data_type[MB_TYPE_INTEGER],
                                       dU.collFlagsTag.mTagh->get_name().c_str());
  dU.tagMap.push_back(dU.collFlagsTag);CHK_DMSL_ERR(err, "Failure to get Damsel tag for MOAB tag");

/*
  SKIP PARENTS/CHILDREN FOR NOW, UNTIL WE HAVE VAR LENGTH TAGS IN DAMSEL

  // PARENTS
  dU.parentsTagPair.second = DMSLtag_define(dU.dmslModel, (damsel_handle_ptr)&(dU.collFlagsTagPair.first), 
                                           DamselUtil::mtod_data_type[(dU.collFlagsTagPair.first)->get_data_type()],
                                           (dU.parentsTagPair.first)->get_name().c_str());
  if (DAMSEL_TAG_INVALID == dtag)
    MB_SET_ERR(MB_FAILURE, "Failure to get Damsel tag for MOAB tag " << (dU.parentsTagPair.first)->get_name());

  // CHILDREN
  dU.childrenTagPair.second = DMSLtag_define(dU.dmslModel, (damsel_handle_ptr)&(dU.collFlagsTagPair.first), 
                                           DamselUtil::mtod_data_type[(dU.collFlagsTagPair.first)->get_data_type()],
                                           (dU.childrenTagPair.first)->get_name().c_str());
  if (DAMSEL_TAG_INVALID == dtag)
    MB_SET_ERR(MB_FAILURE, "Failure to get Damsel tag for MOAB tag " << (dU.childrenTagPair.first)->get_name());
*/

  // Map the tag handles in one big call
  int num_tags = dU.tagMap.size();
  std::vector<Tag> moab_taghs;
  moab_taghs.reserve(num_tags);
  for (std::vector<DamselUtil::tinfo>::iterator vit = dU.tagMap.begin(); vit != dU.tagMap.end(); ++vit) {
    moab_taghs.push_back((*vit).mTagh);
  }

  damsel_container mtags = DMSLcontainer_create_vector(dU.dmslModel, (damsel_handle_ptr)&moab_taghs[0], moab_taghs.size());
  std::cerr << "MOAB: created model container: mtags = " << mtags <<std::endl;

  err = DMSLmodel_map_handles_inventing_file_handles(mtags);CHK_DMSL_ERR(err, "Failed to map tag handles");

  err = DMSLcontainer_release(mtags);CHK_DMSL_ERR(err, "Problem releasing tag handle container");

  return MB_SUCCESS;
}

ErrorCode WriteDamsel::write_vertices(RangeSeqIntersectIter &rsi)
{
  // Write the vertices; these vertices will be in the same sequence and will be contiguous, guaranteed
  EntityHandle start_vert = rsi.get_start_handle(), end_vert = rsi.get_end_handle();

  // Create a damsel container for these vertex handles
  damsel_container vertex_cont = DMSLcontainer_create_sequence(dU.dmslModel, start_vert, (int)(end_vert-start_vert+1), 1);
  std::cerr << "MOAB: created model container: vertex_cont = " << vertex_cont <<std::endl;
  if (DAMSEL_CONTAINER_INVALID == vertex_cont)
    MB_SET_ERR(MB_FAILURE, "Failed to create vertex sequence for vertices starting with handle " << rsi.get_start_handle());

  damsel_err_t err = DMSLmodel_map_handles_inventing_file_handles(vertex_cont);CHK_DMSL_ERR(err, "Failed to map handles");

  // Define the entities to damsel
  err = DMSLentity_define(vertex_cont, DAMSEL_ENTITY_TYPE_VERTEX, 1, vertex_cont);CHK_DMSL_ERR(err, "Failure in DMSLentity_define for vertices starting with handle " << rsi.get_start_handle());

  // Get the vertex coordinates storage locations and pass to damsel
  Range vert_range(start_vert, end_vert);
  double *xcoords = NULL, *ycoords = NULL, *zcoords = NULL;
  int count;
  ErrorCode rval = mbImpl->coords_iterate(vert_range.begin(), vert_range.end(),
                                          xcoords, ycoords, zcoords, count);MB_CHK_SET_ERR(rval, "Failed to get coordinate iterator for vertices starting with handle " << rsi.get_start_handle());
  if (count != (int)vert_range.size()) {
    MB_SET_ERR(MB_FAILURE, "Vertex subrange not in the same sequence for vertices starting with handle " << rsi.get_start_handle());
  }

  if (xcoords && !ycoords && !zcoords) {
    // Interleaved

    // Map the data to damsel
    err = DMSLmodel_map_tag(xcoords, vertex_cont, (damsel_handle_ptr)&dU.xcoordsTag.mTagh);CHK_DMSL_ERR(err, "Failed to assign vertex coordinates tag for vertices starting with handle " << rsi.get_start_handle());
  }
  else {
    // Map the data to damsel
    err = DMSLmodel_map_tag(xcoords, vertex_cont, (damsel_handle_ptr)&dU.xcoordsTag.mTagh);CHK_DMSL_ERR(err, "Failed to assign vertex x coordinates tag for vertices starting with handle " << rsi.get_start_handle());
    err = DMSLmodel_map_tag(ycoords, vertex_cont, (damsel_handle_ptr)&dU.ycoordsTag.mTagh);CHK_DMSL_ERR(err, "Failed to assign vertex y coordinates tag for vertices starting with handle " << rsi.get_start_handle());
    err = DMSLmodel_map_tag(zcoords, vertex_cont, (damsel_handle_ptr)&dU.zcoordsTag.mTagh);CHK_DMSL_ERR(err, "Failed to assign vertex z coordinates tag for vertices starting with handle " << rsi.get_start_handle());
  }

  // Write/map dense tags
  rval = map_dense_tags(rsi, vertex_cont);MB_CHK_ERR(rval);

  err = DMSLcontainer_release(vertex_cont);CHK_DMSL_ERR(err, "Problem releasing vertex handle container");

  return MB_SUCCESS;
}

ErrorCode WriteDamsel::write_entities(RangeSeqIntersectIter &rsi)
{
  // Write the entities; these entities will be in the same sequence and will be contiguous, guaranteed
  EntityHandle start_ent = rsi.get_start_handle(), end_ent = rsi.get_end_handle();

  // Create a damsel container for these entity handles
  damsel_container ent_cont;
  ent_cont = DMSLcontainer_create_sequence(dU.dmslModel, start_ent, (int)(end_ent-start_ent+1), 1);
  std::cerr << "MOAB: created model container: ent_cont = " << ent_cont <<std::endl;
  if (DAMSEL_CONTAINER_INVALID == ent_cont)
    MB_SET_ERR(MB_FAILURE, "Bad sequence returned by Damsel");

  damsel_err_t err = DMSLmodel_map_handles_inventing_file_handles(ent_cont);CHK_DMSL_ERR(err, "Failed to map handles");

  // Get # verts per entity and entity type
  EntityType etype = mbImpl->type_from_handle(start_ent);
  assert(MBMAXTYPE != etype);
  int num_connect = rsi.get_sequence()->values_per_entity();
  assert(0 < num_connect);

  // Get the connectivity storage location and pass to damsel
  Range ent_range(start_ent, end_ent);
  int count;
  EntityHandle *connect;
  int verts_per_ent;
  ErrorCode rval = mbImpl->connect_iterate(ent_range.begin(), ent_range.end(), connect, verts_per_ent, count);MB_CHK_SET_ERR(rval, "Failed to get connect iterator for entities starting with handle " << rsi.get_start_handle());
  if (count != (int)ent_range.size())
    MB_SET_ERR(MB_FAILURE, "Entity subrange not in the same sequence for entities starting with handle " << rsi.get_start_handle());

  // Define the entities to damsel
  err = DMSLentity_define_fast(ent_cont, DamselUtil::mtod_entity_type[etype], num_connect, (damsel_handle*)connect);CHK_DMSL_ERR(err, "DMSLentity_define failed for entities starting with handle " << rsi.get_start_handle());

  // Write dense tags
  rval = map_dense_tags(rsi, ent_cont);MB_CHK_ERR(rval);

  err = DMSLcontainer_release(ent_cont);CHK_DMSL_ERR(err, "Problem releasing entity handle container");

  return MB_SUCCESS;
}

ErrorCode WriteDamsel::map_dense_tags(RangeSeqIntersectIter &rsi, damsel_container &ent_cont)
{
  // All dense_tags have been initialized before this, so here we just go through
  // them and map data if there is any
  const unsigned char *val_ptr;
  ErrorCode rval = MB_SUCCESS;
  std::vector<DamselUtil::tinfo>::iterator tagit;
  damsel_err_t err;
  for (tagit = dU.tagMap.begin(); tagit != dU.tagMap.end(); ++tagit) {
    if ((*tagit).tagType != MB_TAG_DENSE)
      continue;

    // Get a ptr to memory for this tag/sequence
    DenseTag *dtag = dynamic_cast<DenseTag*>((*tagit).mTagh);
    assert(dtag);
    rval = dtag->get_array(rsi.get_sequence(), val_ptr);
    MB_CHK_SET_ERR(rval, "Failed to get tag coordinates pointer for vertices starting with handle " << rsi.get_start_handle());

    // If ptr is NULL, no data for this tag in this sequence
    if (!val_ptr)
      continue;

    // Else, register with damsel
    err = DMSLmodel_map_tag((void*)val_ptr, ent_cont, (damsel_handle_ptr)&dtag);CHK_DMSL_ERR(err, "Failed to write coordinates tag for vertices starting with handle " << rsi.get_start_handle());
  }

  return rval;
}

ErrorCode WriteDamsel::map_sparse_tags()
{
  // All sparse_tags have been initialized before this, so here we just go through
  // them and map data if there is any
  ErrorCode rval = MB_SUCCESS;
  damsel_err_t err;
  std::vector<DamselUtil::tinfo>::iterator tagit;
  std::vector<unsigned char> tag_values;
  std::vector<EntityHandle> tagged_ents;
  damsel_container ent_cont;
  for (tagit = dU.tagMap.begin(); tagit != dU.tagMap.end(); ++tagit) {
    if ((*tagit).tagType != MB_TAG_SPARSE)
      continue;
    // Get a ptr to memory for this tag/sequence
    SparseTag *stag = dynamic_cast<SparseTag*>((*tagit).mTagh);
    assert(stag);
    Range output_ents;
    rval = stag->get_tagged_entities(sequenceManager, output_ents);MB_CHK_SET_ERR(rval, "Trouble getting tagged entities for tag " << stag->get_name());

    // If no entities have this tag set, don't map it
    if (output_ents.empty())
      continue;

    // Else, register with damsel
    // Allocate space for and get values
    tag_values.resize(stag->get_size() * output_ents.size());
    rval = mbImpl->tag_get_data(stag, output_ents, &tag_values[0]);MB_CHK_SET_ERR(rval, "Trouble getting tag values for tag " << stag->get_name());

    // Build a vector of entity handles from the range, and a container from that
    tagged_ents.resize(output_ents.size());
    std::copy(output_ents.begin(), output_ents.end(), tagged_ents.begin());
    ent_cont = DMSLcontainer_create_vector(dU.dmslModel, (damsel_handle_ptr)&tagged_ents[0], tagged_ents.size());
    std::cerr << "MOAB: created model container: sparse_tag_ent_cont = " << ent_cont <<std::endl;
    if (DAMSEL_CONTAINER_INVALID == ent_cont)
      MB_SET_ERR(MB_FAILURE, "Trouble creating entity handle container for tag " << stag->get_name());

    // Now map it
    err = DMSLmodel_map_tag((void*)&tag_values[0], ent_cont, (damsel_handle_ptr)&stag);CHK_DMSL_ERR(err, "Failed to write tag " << stag->get_name());

    err = DMSLcontainer_release(ent_cont);CHK_DMSL_ERR(err, "Problem releasing entity handle container");
  }

  return rval;
}

ErrorCode WriteDamsel::write_sets(RangeSeqIntersectIter &rsi)
{
  // Write the sets
  ErrorCode rval = MB_SUCCESS;
  std::vector<EntityHandle> ents;
  damsel_container mcont;
  damsel_err_t err;
  unsigned int i, num_sets = rsi.get_end_handle() - rsi.get_start_handle() + 1;
  std::vector<unsigned int> set_flags(num_sets, 0);
  EntityHandle seth;
  for (seth = rsi.get_start_handle(), i = 0; seth <= rsi.get_end_handle(); seth++, i++) {
    // Get all the entities in the set
    ents.clear();
    rval = mbImpl->get_entities_by_handle(seth, ents);MB_CHK_SET_ERR(rval, "get_entities_by_handle failed for set " << seth);
    if (!ents.empty()) {
      mcont = DMSLcontainer_create_vector(dU.dmslModel, (damsel_handle*)&ents[0], ents.size());
    }
    else {
      mcont = DMSLcontainer_create_vector(dU.dmslModel, (damsel_handle*)NULL, 0);
    }
    std::cerr << "MOAB: created model container: sets_cont = " << mcont <<std::endl;

    // Get the set type (range or set)
    unsigned int opts;
    rval = mbImpl->get_meshset_options(seth, opts);MB_CHK_SET_ERR(rval, "Failed to get options for meshset " << seth);
    damsel_collection_type coll_type = (opts&MESHSET_SET ? DAMSEL_HANDLE_COLLECTION_TYPE_SET :
                     DAMSEL_HANDLE_COLLECTION_TYPE_VECTOR);

    // Parents/children...

    // Set flags
    if (opts & MESHSET_TRACK_OWNER)
      set_flags[i] |= MESHSET_TRACK_OWNER;
    else
      set_flags[i] &= !MESHSET_TRACK_OWNER;

    // Create the collection
    DMSLcoll_create(dU.dmslModel, (damsel_handle_ptr)&seth, mcont, coll_type);

    // Release the container
    err = DMSLcontainer_release(mcont);CHK_DMSL_ERR(err, "Problem releasing set entity handle container");
  }

  // Set the COLL_FLAGS tag, using assign (direct)
  // Make a container of set handles...
  mcont = DMSLcontainer_create_sequence(dU.dmslModel, rsi.get_start_handle(), num_sets, 1);
  std::cerr << "MOAB: created model container: sets_cont = " << mcont <<std::endl;

  // Assign the tags on them
  err = DMSLmodel_map_tag(&set_flags[0], mcont, (damsel_handle_ptr)&(dU.collFlagsTag.mTagh));CHK_DMSL_ERR(err, "Failed to assign COLL_FLAGS tag for sets");

  // Map set handles
  err = DMSLmodel_map_handles_inventing_file_handles(mcont);CHK_DMSL_ERR(err, "Failed to map set handles");

  // Map other dense tags
  rval = map_dense_tags(rsi, mcont);MB_CHK_SET_ERR(rval, "Failed to map dense tags for sets");

  return rval;
}

} // namespace moab
