#include <stdlib.h> // For exit()
#include <vector>
#include <map>
#include <iostream>
#include <string>
#include <algorithm>

#include "moab/CN.hpp"
#include "moab/Range.hpp"
#include "moab/Interface.hpp"
#include "MBTagConventions.hpp"
#include "Internals.hpp"
#include "moab/ReadUtilIface.hpp"
#include "moab/FileOptions.hpp"
#include "ReadCCMIO.hpp"
#include "moab/MeshTopoUtil.hpp"

#include "ccmio.h"

/*
 * CCMIO file structure
 *
 * Root
 *   State(kCCMIOState)
 *     Processor*
 *       Vertices
 *         ->ReadVerticesx, ReadMap
 *       Topology
 *         Boundary faces*(kCCMIOBoundaryFaces)
 *            ->ReadFaces, ReadFaceCells, ReadMap
 *         Internal faces(kCCMIOInternalFaces)
 *         Cells (kCCMIOCells)
 *            ->ReadCells (mapID), ReadMap, ReadCells (cellTypes)
 *       Solution
 *         Phase
 *           Field
 *             FieldData
 *   Problem(kCCMIOProblemDescription)
 *     CellType* (kCCMIOCellType)
 *       Index (GetEntityIndex), MaterialId(ReadOpti), MaterialType(ReadOptstr),
 *         PorosityId(ReadOpti), SpinId(ReadOpti), GroupId(ReadOpti)
 *
 * MaterialType (CCMIOReadOptstr in readexample)
 * constants (see readexample)
 * lagrangian data (CCMIOReadLagrangianData)
 * vertices label (CCMIOEntityDescription)
 * restart info: char solver[], iteratoins, time, char timeUnits[], angle
 *      (CCMIOReadRestartInfo, kCCMIORestartData), reference data?
 * phase:
 *   field: char name[], dims, CCMIODataType datatype, char units[]
 *       dims = kCCMIOScalar (CCMIOReadFieldDataf), 
 *              kCCMIOVector (CCMIOReadMultiDimensionalFieldData),
 *              kCCMIOTensor
 * MonitoringSets: num, name (CellSet, VertexSet, BoundarySet, BlockSet, SplineSet, CoupleSet)
 *      CCMIOGetProstarSet, CCMIOReadOpt1i,
 */

enum DataType {kScalar, kVector, kVertex, kCell, kInternalFace, kBoundaryFace,
               kBoundaryData, kBoundaryFaceData, kCellType};

namespace moab
{

static int const kNValues = 10; // Number of values of each element to print
static char const kDefaultState[] = "default";
static char const kUnitsName[] = "Units";
static int const kVertOffset = 2;
static int const kCellInc = 4;

#define CHK_SET_CCMERR(ccm_err_code, ccm_err_msg) \
  { \
    if (kCCMIONoErr != ccm_err_code && kCCMIONoFileErr != ccm_err_code && kCCMIONoNodeErr != ccm_err_code) \
      MB_SET_ERR(MB_FAILURE, ccm_err_msg); \
  }

ReaderIface* ReadCCMIO::factory(Interface* iface)
{
  return new ReadCCMIO(iface);
}

ReadCCMIO::ReadCCMIO(Interface* impl)
        : mMaterialIdTag(0), mMaterialTypeTag(0), mRadiationTag(0), mPorosityIdTag(0),
          mSpinIdTag(0), mGroupIdTag(0), mColorIdxTag(0), mProcessorIdTag(0),
          mLightMaterialTag(0), mFreeSurfaceMaterialTag(0), mThicknessTag(0),
          mProstarRegionNumberTag(0), mBoundaryTypeTag(0), mCreatingProgramTag(0),
          mbImpl(impl), hasSolution(false)
{
  assert(impl != NULL);

  impl->query_interface(readMeshIface);

  // Initialize in case tag_get_handle fails below
  mMaterialSetTag  = 0;
  mDirichletSetTag = 0;
  mNeumannSetTag   = 0;
  mHasMidNodesTag  = 0;
  mGlobalIdTag     = 0;
  mNameTag         = 0;

  //! Get and cache predefined tag handles
  const int negone = -1;
  ErrorCode result = impl->tag_get_handle(MATERIAL_SET_TAG_NAME,  1, MB_TYPE_INTEGER,
                                          mMaterialSetTag, MB_TAG_CREAT | MB_TAG_SPARSE, &negone);MB_CHK_SET_ERR_RET(result, "Failed to get MATERIAL_SET tag");

  result = impl->tag_get_handle(DIRICHLET_SET_TAG_NAME, 1, MB_TYPE_INTEGER,
                                mDirichletSetTag, MB_TAG_CREAT | MB_TAG_SPARSE, &negone);MB_CHK_SET_ERR_RET(result, "Failed to get DIRICHLET_SET tag");

  result = impl->tag_get_handle(NEUMANN_SET_TAG_NAME, 1, MB_TYPE_INTEGER,
                                mNeumannSetTag, MB_TAG_CREAT | MB_TAG_SPARSE, &negone);MB_CHK_SET_ERR_RET(result, "Failed to get NEUMANN_SET tag");

  const int negonearr[] = {-1, -1, -1, -1};
  result = impl->tag_get_handle(HAS_MID_NODES_TAG_NAME, 4, MB_TYPE_INTEGER,
                                mHasMidNodesTag, MB_TAG_CREAT | MB_TAG_SPARSE, negonearr);MB_CHK_SET_ERR_RET(result, "Failed to get HAS_MID_NODES tag");

  const int zero = 0;
  result = impl->tag_get_handle(GLOBAL_ID_TAG_NAME, 1, MB_TYPE_INTEGER,
                                mGlobalIdTag, MB_TAG_CREAT | MB_TAG_SPARSE, &zero);MB_CHK_SET_ERR_RET(result, "Failed to get GLOBAL_ID tag");

  result = impl->tag_get_handle(NAME_TAG_NAME, NAME_TAG_SIZE, MB_TYPE_OPAQUE,
                                mNameTag, MB_TAG_CREAT | MB_TAG_SPARSE);MB_CHK_SET_ERR_RET(result, "Failed to get NAME tag");
}

ReadCCMIO::~ReadCCMIO()
{
  mbImpl->release_interface(readMeshIface);
}

ErrorCode ReadCCMIO::load_file(const char *file_name,
                               const EntityHandle* file_set,
                               const FileOptions& /* opts */,
                               const ReaderIface::SubsetList* subset_list,
                               const Tag* /* file_id_tag */)
{
  CCMIOID rootID, problemID, stateID, processorID,
          verticesID, topologyID, solutionID;
  CCMIOError error = kCCMIONoErr;

  if (subset_list) {
    MB_SET_ERR(MB_UNSUPPORTED_OPERATION, "Reading subset of files not supported for CCMOI data");
  }

  CCMIOOpenFile(&error, file_name, kCCMIORead, &rootID);CHK_SET_CCMERR(error, "Problem opening file");

  // Get the file state
  ErrorCode rval = get_state(rootID, problemID, stateID);MB_CHK_SET_ERR(rval, "Failed to get state");

  // Get processors
  std::vector<CCMIOSize_t> procs;
  bool has_solution = false;
  rval = get_processors(stateID, processorID, verticesID, topologyID, solutionID,
                        procs, has_solution);MB_CHK_SET_ERR(rval, "Failed to get processors");

  std::vector<CCMIOSize_t>::iterator vit;
  Range new_ents, *new_ents_ptr = NULL;
  if (file_set)
    new_ents_ptr = &new_ents;

  for (vit = procs.begin(); vit != procs.end(); ++vit) {
    rval = read_processor(stateID, problemID, processorID, verticesID, topologyID,
                          *vit, new_ents_ptr);MB_CHK_SET_ERR(rval, "Failed to read processors");
  }

  // Load some meta-data
  rval = load_metadata(rootID, problemID, stateID, processorID, file_set);MB_CHK_SET_ERR(rval, "Failed to load some meta-data");

  // Now, put all this into the file set, if there is one
  if (file_set) {
    rval = mbImpl->add_entities(*file_set, new_ents);MB_CHK_SET_ERR(rval, "Failed to add new entities to file set");
  }

  return rval;
}

ErrorCode ReadCCMIO::get_state(CCMIOID rootID, CCMIOID &problemID, CCMIOID &stateID)
{
  CCMIOError error = kCCMIONoErr;

  // First try default
  CCMIOGetState(&error, rootID, "default", &problemID, &stateID);
  if (kCCMIONoErr != error) {
    CCMIOSize_t i = CCMIOSIZEC(0);
    CCMIOError tmp_error = kCCMIONoErr;
    CCMIONextEntity(&tmp_error, rootID, kCCMIOState, &i, &stateID);
    if (kCCMIONoErr == tmp_error)
      CCMIONextEntity(&error, rootID, kCCMIOProblemDescription,
                      &i, &problemID);
  }
  CHK_SET_CCMERR(error, "Couldn't find state");

  return MB_SUCCESS;
}

ErrorCode ReadCCMIO::load_metadata(CCMIOID rootID, CCMIOID problemID,
                                   CCMIOID /* stateID */, CCMIOID processorID,
                                   const EntityHandle *file_set)
{
  // Read the simulation title.
  CCMIOError error = kCCMIONoErr;
  ErrorCode rval = MB_SUCCESS;
  CCMIONode rootNode;
  if (kCCMIONoErr == CCMIOGetEntityNode(&error, rootID, &rootNode)) {
    char *name = NULL;
    CCMIOGetTitle(&error, rootNode, &name);

    if (NULL != name && strlen(name) != 0) {
      // Make a tag for it and tag the read set
      Tag simname;
      rval = mbImpl->tag_get_handle("Title", strlen(name), MB_TYPE_OPAQUE,
                                    simname, MB_TAG_CREAT | MB_TAG_SPARSE);MB_CHK_SET_ERR(rval, "Simulation name tag not found or created");
      EntityHandle set = file_set ? *file_set : 0;
      rval = mbImpl->tag_set_data(simname, &set, 1, name);MB_CHK_SET_ERR(rval, "Problem setting simulation name tag");
    }
    if (name)
      free(name);
  }

  // Creating program
  EntityHandle dumh = (file_set ? *file_set : 0);
  rval = get_str_option("CreatingProgram", dumh, mCreatingProgramTag, processorID);MB_CHK_SET_ERR(rval, "Trouble getting CreatingProgram tag");

  rval = load_matset_data(problemID);MB_CHK_SET_ERR(rval, "Failure loading matset data");

  rval = load_neuset_data(problemID);MB_CHK_SET_ERR(rval, "Failure loading neuset data");

  return rval;
}

ErrorCode ReadCCMIO::load_matset_data(CCMIOID problemID)
{
  // Make sure there are matsets
  if (newMatsets.empty())
    return MB_SUCCESS;

  // ... walk through each cell type
  CCMIOSize_t i = CCMIOSIZEC(0);
  CCMIOID next;
  CCMIOError error = kCCMIONoErr;

  while (CCMIONextEntity(NULL, problemID, kCCMIOCellType, &i, &next)
         == kCCMIONoErr) {
    // Get index, corresponding set, and label with material set tag
    int mindex;
    CCMIOGetEntityIndex(&error, next, &mindex);
    std::map<int, EntityHandle>::iterator mit = newMatsets.find(mindex);
    if (mit == newMatsets.end())
      // No actual faces for this matset; continue to next
      continue;

    EntityHandle dum_ent = mit->second;
    ErrorCode rval = mbImpl->tag_set_data(mMaterialSetTag, &dum_ent, 1, &mindex);MB_CHK_SET_ERR(rval, "Trouble setting material set tag");

    // Set name
    CCMIOSize_t len;
    CCMIOEntityLabel(&error, next, &len, NULL);
    std::vector<char> opt_string2(GETINT32(len) + 1, '\0');
    CCMIOEntityLabel(&error, next, NULL, &opt_string2[0]);
    if (opt_string2.size() >= NAME_TAG_SIZE)
      opt_string2[NAME_TAG_SIZE - 1] = '\0';
    else
      (opt_string2.resize(NAME_TAG_SIZE, '\0'));
    rval = mbImpl->tag_set_data(mNameTag, &dum_ent, 1, &opt_string2[0]);MB_CHK_SET_ERR(rval, "Trouble setting name tag for material set");

    // Material id
    rval = get_int_option("MaterialId", dum_ent, mMaterialIdTag, next);MB_CHK_SET_ERR(rval, "Trouble getting MaterialId tag");
    
    rval = get_str_option("MaterialType", dum_ent, mMaterialTypeTag, next);MB_CHK_SET_ERR(rval, "Trouble getting MaterialType tag");
    
    rval = get_int_option("Radiation", dum_ent, mRadiationTag, next);MB_CHK_SET_ERR(rval, "Trouble getting Radiation option");

    rval = get_int_option("PorosityId", dum_ent, mPorosityIdTag, next);MB_CHK_SET_ERR(rval, "Trouble getting PorosityId option");

    rval = get_int_option("SpinId", dum_ent, mSpinIdTag, next);MB_CHK_SET_ERR(rval, "Trouble getting SpinId option");

    rval = get_int_option("GroupId", dum_ent, mGroupIdTag, next);MB_CHK_SET_ERR(rval, "Trouble getting GroupId option");

    rval = get_int_option("ColorIdx", dum_ent, mColorIdxTag, next);MB_CHK_SET_ERR(rval, "Trouble getting ColorIdx option");

    rval = get_int_option("ProcessorId", dum_ent, mProcessorIdTag, next);MB_CHK_SET_ERR(rval, "Trouble getting ProcessorId option");

    rval = get_int_option("LightMaterial", dum_ent, mLightMaterialTag, next);MB_CHK_SET_ERR(rval, "Trouble getting LightMaterial option");

    rval = get_int_option("FreeSurfaceMaterial", dum_ent, mFreeSurfaceMaterialTag, next);MB_CHK_SET_ERR(rval, "Trouble getting FreeSurfaceMaterial option");

    rval = get_dbl_option("Thickness", dum_ent, mThicknessTag, next);MB_CHK_SET_ERR(rval, "Trouble getting Thickness option");
  }

  return MB_SUCCESS;
}

ErrorCode ReadCCMIO::get_int_option(const char *opt_str, EntityHandle seth,
                                    Tag &tag, CCMIOID node)
{
  int idum;
  ErrorCode rval;
  if (kCCMIONoErr == CCMIOReadOpti(NULL, node, opt_str, &idum)) {
    if (!tag) {
      rval = mbImpl->tag_get_handle(opt_str, 1, MB_TYPE_INTEGER,
                                    tag, MB_TAG_SPARSE | MB_TAG_CREAT);MB_CHK_SET_ERR(rval, "Failed to get tag handle");
    }

    rval = mbImpl->tag_set_data(tag, &seth, 1, &idum);MB_CHK_SET_ERR(rval, "Failed to set tag data");
  }

  return MB_SUCCESS;
}

ErrorCode ReadCCMIO::get_dbl_option(const char *opt_str, EntityHandle seth,
                                    Tag &tag, CCMIOID node)
{
  float fdum;
  if (kCCMIONoErr == CCMIOReadOptf(NULL, node, opt_str, &fdum)) {
    ErrorCode rval;
    if (!tag) {
      rval = mbImpl->tag_get_handle(opt_str, 1, MB_TYPE_DOUBLE, 
                                    tag, MB_TAG_SPARSE | MB_TAG_CREAT);MB_CHK_SET_ERR(rval, "Failed to get tag handle");
    }

    double dum_dbl = fdum;
    rval = mbImpl->tag_set_data(tag, &seth, 1, &dum_dbl);MB_CHK_SET_ERR(rval, "Failed to set tag data");
  }

  return MB_SUCCESS;
}

ErrorCode ReadCCMIO::get_str_option(const char *opt_str, EntityHandle seth, Tag &tag,
                                    CCMIOID node, const char *other_tag_name)
{
  int len;
  CCMIOError error = kCCMIONoErr;
  std::vector<char> opt_string;
  if (kCCMIONoErr != CCMIOReadOptstr(NULL, node, opt_str, &len, NULL))
    return MB_SUCCESS;

  opt_string.resize(len);
  CCMIOReadOptstr(&error, node, opt_str, &len, &opt_string[0]);
  ErrorCode rval = MB_SUCCESS;
  if (!tag) {
    rval = mbImpl->tag_get_handle(other_tag_name ? other_tag_name : opt_str,
                                  NAME_TAG_SIZE, MB_TYPE_OPAQUE, tag,
                                  MB_TAG_SPARSE | MB_TAG_CREAT);MB_CHK_SET_ERR(rval, "Failed to get tag handle");
  }

  if (opt_string.size() > NAME_TAG_SIZE)
    opt_string[NAME_TAG_SIZE - 1] = '\0';
  else
    (opt_string.resize(NAME_TAG_SIZE, '\0'));

  rval = mbImpl->tag_set_data(tag, &seth, 1, &opt_string[0]);MB_CHK_SET_ERR(rval, "Failed to set tag data");

  return MB_SUCCESS;
}

ErrorCode ReadCCMIO::load_neuset_data(CCMIOID problemID)
{
  CCMIOSize_t i = CCMIOSIZEC(0);
  CCMIOID next;

  // Make sure there are matsets
  if (newNeusets.empty())
    return MB_SUCCESS;

  while (CCMIONextEntity(NULL, problemID, kCCMIOBoundaryRegion, &i, &next)
         == kCCMIONoErr) {
    // Get index, corresponding set, and label with neumann set tag
    int mindex;
    CCMIOError error = kCCMIONoErr;
    CCMIOGetEntityIndex(&error, next, &mindex);
    std::map<int, EntityHandle>::iterator mit = newNeusets.find(mindex);
    if (mit == newNeusets.end())
      // No actual faces for this neuset; continue to next
      continue;

    EntityHandle dum_ent = mit->second;
    ErrorCode rval = mbImpl->tag_set_data(mNeumannSetTag, &dum_ent, 1, &mindex);MB_CHK_SET_ERR(rval, "Trouble setting neumann set tag");

    // Set name
    rval = get_str_option("BoundaryName", dum_ent, mNameTag, next, NAME_TAG_NAME);MB_CHK_SET_ERR(rval, "Trouble creating BoundaryName tag");

    // BoundaryType
    rval = get_str_option("BoundaryType", dum_ent, mBoundaryTypeTag, next);MB_CHK_SET_ERR(rval, "Trouble creating BoundaryType tag");

    // ProstarRegionNumber
    rval = get_int_option("ProstarRegionNumber", dum_ent, mProstarRegionNumberTag, next);MB_CHK_SET_ERR(rval, "Trouble creating ProstarRegionNumber tag");
  }

  return MB_SUCCESS;
}

ErrorCode ReadCCMIO::read_processor(CCMIOID /* stateID */, CCMIOID problemID,
                                    CCMIOID processorID, CCMIOID verticesID, CCMIOID topologyID,
                                    CCMIOSize_t proc, Range *new_ents)
{
  ErrorCode rval;

  // vert_map fields: s: none, i: gid, ul: vert handle, r: none
  //TupleList vert_map(0, 1, 1, 0, 0);
  TupleList vert_map;
  rval = read_vertices(proc, processorID, verticesID, topologyID,
                       new_ents, vert_map);MB_CHK_SET_ERR(rval, "Failed to read vertices");

  rval = read_cells(proc, problemID, verticesID, topologyID,
                    vert_map, new_ents);MB_CHK_SET_ERR(rval, "Failed to read cells");

  return rval;
}

ErrorCode ReadCCMIO::read_cells(CCMIOSize_t /* proc */, CCMIOID problemID,
                                CCMIOID /* verticesID */, CCMIOID topologyID,
                                TupleList &vert_map, Range *new_ents)
{
  // Read the faces.
  // face_map fields: s:forward/reverse, i: cell id, ul: face handle, r: none
  ErrorCode rval;
#ifdef TUPLE_LIST
  TupleList face_map(1, 1, 1, 0, 0);
#else
  TupleList face_map;
  SenseList sense_map;
#endif
  rval = read_all_faces(topologyID, vert_map, face_map,
#ifndef TUPLE_LIST
                        sense_map,
#endif
                        new_ents);MB_CHK_SET_ERR(rval, "Failed to read all cells");

  // Read the cell topology types, if any exist in the file
  std::map<int, int> cell_topo_types;
  rval = read_topology_types(topologyID, cell_topo_types);MB_CHK_SET_ERR(rval, "Problem reading cell topo types");

  // Now construct the cells; sort the face map by cell ids first
#ifdef TUPLE_LIST  
  rval = face_map.sort(1);MB_CHK_SET_ERR(rval, "Couldn't sort face map by cell id");
#endif
  std::vector<EntityHandle> new_cells;
  rval = construct_cells(face_map,
#ifndef TUPLE_LIST
                         sense_map,
#endif
                         vert_map, cell_topo_types, new_cells);MB_CHK_SET_ERR(rval, "Failed to construct cells");
  if (new_ents) {
    Range::iterator rit = new_ents->end();
    std::vector<EntityHandle>::reverse_iterator vit;
    for (vit = new_cells.rbegin(); vit != new_cells.rend(); ++vit)
      rit = new_ents->insert(rit, *vit);
  }

  rval = read_gids_and_types(problemID, topologyID, new_cells);MB_CHK_SET_ERR(rval, "Failed to read gids and types");

  return MB_SUCCESS;
}

ErrorCode ReadCCMIO::read_topology_types(CCMIOID &topologyID,
                                         std::map<int,int> &cell_topo_types)
{
  CCMIOError error = kCCMIONoErr;
  CCMIOID cellID, mapID;
  CCMIOSize_t ncells;
  CCMIOGetEntity(&error, topologyID, kCCMIOCells, 0, &cellID);
  CCMIOEntitySize(&error, cellID, &ncells, NULL);
  int num_cells = GETINT32(ncells);

  // First, do a dummy read to see if we even have topo types in this mesh
  int dum_int;
  CCMIOReadOpt1i(&error, cellID, "CellTopologyType", &dum_int,
                 CCMIOINDEXC(kCCMIOStart), CCMIOINDEXC(kCCMIOStart) + 1);
  if (kCCMIONoErr != error)
    return MB_SUCCESS;

  // OK, we have topo types; first get the map node
  std::vector<int> dum_ints(num_cells);
  CCMIOReadCells(&error, cellID, &mapID, &dum_ints[0],
                 CCMIOINDEXC(kCCMIOStart), CCMIOINDEXC(kCCMIOStart) + 1);CHK_SET_CCMERR(error, "Failed to get the map node");

  // Now read the map
  CCMIOReadMap(&error, mapID, &dum_ints[0],
               CCMIOINDEXC(kCCMIOStart), CCMIOINDEXC(kCCMIOEnd));CHK_SET_CCMERR(error, "Failed to get cell ids");
  int i;
  for (i = 0; i < num_cells; i++)
    cell_topo_types[dum_ints[i]] = 0;

  // Now read the cell topo types for real, reusing cell_topo_types
  std::vector<int> topo_types(num_cells);
  CCMIOReadOpt1i(&error, cellID, "CellTopologyType", &topo_types[0],
                 CCMIOINDEXC(kCCMIOStart), CCMIOINDEXC(kCCMIOEnd));CHK_SET_CCMERR(error, "Failed to get cell topo types");
  for (i = 0; i < num_cells; i++)
    cell_topo_types[dum_ints[i]] = topo_types[i];

  return MB_SUCCESS;
}

ErrorCode ReadCCMIO::read_gids_and_types(CCMIOID /* problemID */,
                                         CCMIOID topologyID,
                                         std::vector<EntityHandle> &cells)
{
  // Get the cells entity and number of cells
  CCMIOSize_t dum_cells;
  int num_cells;
  CCMIOError error = kCCMIONoErr;
  CCMIOID cellsID, mapID;
  CCMIOGetEntity(&error, topologyID, kCCMIOCells, 0, &cellsID);
  CCMIOEntitySize(&error, cellsID, &dum_cells, NULL);
  num_cells = GETINT32(dum_cells);

  // Check the number of cells against how many are in the cell array
  if (num_cells != (int)cells.size())
    MB_SET_ERR(MB_FAILURE, "Number of cells doesn't agree");

  // Read the gid map and set global ids
  std::vector<int> cell_gids(num_cells);
  CCMIOReadCells(&error, cellsID, &mapID, NULL,
                 CCMIOINDEXC(kCCMIOStart), CCMIOINDEXC(kCCMIOEnd));CHK_SET_CCMERR(error, "Couldn't read cells");
  CCMIOReadMap(&error, mapID, &cell_gids[0], 
               CCMIOINDEXC(kCCMIOStart), CCMIOINDEXC(kCCMIOEnd));CHK_SET_CCMERR(error, "Couldn't read cell id map");

  ErrorCode rval = mbImpl->tag_set_data(mGlobalIdTag, &cells[0], 
                                          cells.size(), &cell_gids[0]);MB_CHK_SET_ERR(rval, "Couldn't set gids tag");

  // Now read cell material types; reuse cell_gids
  CCMIOReadCells(&error, cellsID, NULL, &cell_gids[0],
                 CCMIOINDEXC(kCCMIOStart), CCMIOINDEXC(kCCMIOEnd));CHK_SET_CCMERR(error, "Trouble reading cell types");

  // Create the matsets
  std::map<int, Range> matset_ents;
  for (int i = 0; i < num_cells; i++)
    matset_ents[cell_gids[i]].insert(cells[i]);

  for (std::map<int, Range>::iterator mit = matset_ents.begin(); mit != matset_ents.end(); ++mit) {
    EntityHandle matset;
    rval = mbImpl->create_meshset(MESHSET_SET, matset);MB_CHK_SET_ERR(rval, "Couldn't create material set");
    newMatsets[mit->first] = matset;

    rval = mbImpl->add_entities(matset, mit->second);MB_CHK_SET_ERR(rval, "Couldn't add entities to material set");
  }

  return MB_SUCCESS;
}

ErrorCode ReadCCMIO::construct_cells(TupleList &face_map,
#ifndef TUPLE_LIST
                                     SenseList &sense_map,
#endif
                                     TupleList & /* vert_map */,
                                     std::map<int, int> &cell_topo_types,
                                     std::vector<EntityHandle> &new_cells)
{
  std::vector<EntityHandle> facehs;
  std::vector<int> senses;
  EntityHandle cell;
  ErrorCode tmp_rval, rval = MB_SUCCESS;
  EntityType this_type = MBMAXTYPE;
  bool has_mid_nodes = false;
#ifdef TUPLE_LIST
  unsigned int i = 0;
  while (i < face_map.n) {
    // Pull out face handles bounding the same cell
    facehs.clear();
    int this_id = face_map.get_int(i);
    unsigned int inext = i;
    while (face_map.get_int(inext) == this_id && inext <= face_map.n) {
      inext++;
      EntityHandle face = face_map.get_ulong(inext);
      facehs.push_back(face);
      senses.push_back(face_map.get_short(inext));
    }
    this_type = MBMAXTYPE;
    has_mid_nodes = false;
#else
  std::map<int, std::vector<EntityHandle> >::iterator fmit;
  std::map<int, std::vector<int> >::iterator smit;
  std::map<int, int>::iterator typeit;
  for (fmit = face_map.begin(), smit = sense_map.begin();
       fmit != face_map.end(); ++fmit, ++smit) {
    // Pull out face handles bounding the same cell
    facehs.clear();
    int this_id = (*fmit).first;
    facehs = (*fmit).second;
    senses.clear();
    senses = (*smit).second;
    typeit = cell_topo_types.find(this_id);
    if (typeit != cell_topo_types.end()) {
      rval = ccmio_to_moab_type(typeit->second, this_type, has_mid_nodes);
    }
    else {
      this_type = MBMAXTYPE;
      has_mid_nodes = false;
    }
#endif
    tmp_rval = create_cell_from_faces(facehs, senses, this_type, has_mid_nodes, cell);
    if (MB_SUCCESS != tmp_rval)
      rval = tmp_rval;
    else {
      new_cells.push_back(cell);
      // Tag cell with global id
      tmp_rval = mbImpl->tag_set_data(mGlobalIdTag, &cell, 1, &this_id);
      if (MB_SUCCESS != tmp_rval)
        rval = tmp_rval;
    }
  }

  return rval;
}

ErrorCode ReadCCMIO::ccmio_to_moab_type(int ccm_type, EntityType &moab_type, bool &has_mid_nodes)
{
  switch (ccm_type) {
    case 1:
        moab_type = MBVERTEX;
        break;
    case 2:
    case 28:
        moab_type = MBEDGE;
        break;
    case 29:
        moab_type = MBMAXTYPE;
        break;
    case 3:
    case 4:
        moab_type = MBQUAD;
        break;
    case 11:
    case 21:
        moab_type = MBHEX;
        break;
    case 12:
    case 22:
        moab_type = MBPRISM;
        break;
    case 13:
    case 23:
        moab_type = MBTET;
        break;
    case 14:
    case 24:
        moab_type = MBPYRAMID;
        break;
    case 255:
        moab_type = MBPOLYHEDRON;
        break;
    default:
        moab_type = MBMAXTYPE;
  }

  switch (ccm_type) {
    case 28:
    case 4:
    case 21:
    case 22:
    case 23:
    case 24:
        has_mid_nodes = true;
        break;
    default:
        break;
  }

  return MB_SUCCESS;
}

ErrorCode ReadCCMIO::create_cell_from_faces(std::vector<EntityHandle> &facehs,
                                            std::vector<int> &senses,
                                            EntityType this_type,
                                            bool /* has_mid_nodes */,
                                            EntityHandle &cell)
{
  ErrorCode rval;

  // Test up front to see if they're one type
  EntityType face_type = mbImpl->type_from_handle(facehs[0]);
  bool same_type = true;
  for (std::vector<EntityHandle>::iterator vit = facehs.begin(); vit != facehs.end(); ++vit) {
    if (face_type != mbImpl->type_from_handle(*vit)) {
      same_type = false;
      break;
    }
  }

  std::vector<EntityHandle> verts;
  EntityType input_type = this_type;
  std::vector<EntityHandle> storage;
  MeshTopoUtil mtu(mbImpl);

  // Preset this to maxtype, so we get an affirmative choice in loop
  this_type = MBMAXTYPE;

  if ((MBTET == input_type || MBMAXTYPE == input_type) && same_type &&
      face_type == MBTRI && facehs.size() == 4) {
    // Try to get proper connectivity for tet

    // Get connectivity of first face, and reverse it if sense is forward, since
    // base face always points into entity
    rval = mbImpl->get_connectivity(&facehs[0], 1, verts);MB_CHK_SET_ERR(rval, "Couldn't get connectivity");
    if (senses[0] > 0)
      std::reverse(verts.begin(), verts.end());

    // Get the 4th vertex through the next tri
    const EntityHandle *conn; int conn_size;
    rval = mbImpl->get_connectivity(facehs[1], conn, conn_size, true, &storage);MB_CHK_SET_ERR(rval, "Couldn't get connectivity");
    int i = 0;
    while (std::find(verts.begin(), verts.end(), conn[i]) != verts.end() && i < conn_size) i++;

    // If i is not at the end of the verts, found the apex; otherwise fall back to polyhedron
    if (conn_size != i) {
      this_type = MBTET;
      verts.push_back(conn[i]);
    }
  }
  else if ((MBHEX == input_type || MBMAXTYPE == input_type) && same_type &&
           MBQUAD == face_type && facehs.size() == 6) {
    // Build hex from quads
    // Algorithm:
    // - verts = vertices from 1st quad
    // - Find quad q1 sharing verts[0] and verts[1]
    // - Find quad q2 sharing other 2 verts in q1
    // - Find v1 = opposite vert from verts[1] in q1 , v2 = opposite from verts[0]
    // - Get i = offset of v1 in verts2 of q2, rotate verts2 by i
    // - If verts2[(i + 1) % 4] != v2, flip verts2 by switching verts2[1] and verts2[3]
    // - append verts2 to verts

    // Get the other vertices for this hex; need to find the quad with no common vertices
    Range tmp_faces, tmp_verts;
    // Get connectivity of first face, and reverse it if sense is forward, since
    // base face always points into entity
    rval = mbImpl->get_connectivity(&facehs[0], 1, verts);MB_CHK_SET_ERR(rval, "Couldn't get connectivity");
    if (senses[0] > 0)
      std::reverse(verts.begin(), verts.end());

    // Get q1, which shares 2 vertices with q0
    std::copy(facehs.begin(), facehs.end(), range_inserter(tmp_faces));
    rval = mbImpl->get_adjacencies(&verts[0], 2, 2, false, tmp_faces);
    if (MB_SUCCESS != rval || tmp_faces.size() != 2)
      MB_SET_ERR(MB_FAILURE, "Couldn't get adj face");
    tmp_faces.erase(facehs[0]);
    EntityHandle q1 = *tmp_faces.begin();
    // Get other 2 verts of q1
    rval = mbImpl->get_connectivity(&q1, 1, tmp_verts);MB_CHK_SET_ERR(rval, "Couldn't get adj verts");
    tmp_verts.erase(verts[0]); tmp_verts.erase(verts[1]);
    // Get q2
    std::copy(facehs.begin(), facehs.end(), range_inserter(tmp_faces));
    rval = mbImpl->get_adjacencies(tmp_verts, 2, false, tmp_faces);
    if (MB_SUCCESS != rval || tmp_faces.size() != 2)
      MB_SET_ERR(MB_FAILURE, "Couldn't get adj face");
    tmp_faces.erase(q1);
    EntityHandle q2 = *tmp_faces.begin();
    // Get verts in q2
    rval = mbImpl->get_connectivity(&q2, 1, storage);MB_CHK_SET_ERR(rval, "Couldn't get adj vertices");

    // Get verts in q1 opposite from v[1] and v[0] in q0
    EntityHandle v0 = 0, v1 = 0;
    rval = mtu.opposite_entity(q1, verts[1], v0);MB_CHK_SET_ERR(rval, "Couldn't get the opposite side entity");
    rval = mtu.opposite_entity(q1, verts[0], v1);MB_CHK_SET_ERR(rval, "Couldn't get the opposite side entity");
    if (v0 && v1) {
      // Offset of v0 in q2, then rotate and flip
      unsigned int ioff = std::find(storage.begin(), storage.end(), v0) - storage.begin();
      if (4 == ioff)
        MB_SET_ERR(MB_FAILURE, "Trouble finding offset");

      if (storage[(ioff + 1) % 4] != v1) {
        std::reverse(storage.begin(), storage.end());
        ioff = std::find(storage.begin(), storage.end(), v0) - storage.begin();
      }
      if (0 != ioff)
        std::rotate(storage.begin(), storage.begin() + ioff, storage.end());

      // Copy into verts, and make hex
      std::copy(storage.begin(), storage.end(), std::back_inserter(verts));
      this_type = MBHEX;
    }
  }

  if (MBMAXTYPE == this_type && facehs.size() == 5) {
    // Some preliminaries
    std::vector<EntityHandle> tris, quads;
    for (unsigned int i = 0; i < 5; i++) {
      if (MBTRI == mbImpl->type_from_handle(facehs[i]))
        tris.push_back(facehs[i]);
      else if (MBQUAD == mbImpl->type_from_handle(facehs[i]))
        quads.push_back(facehs[i]);
    }

    // Check for prisms
    if (2 == tris.size() && 3 == quads.size()) {
      // OK, we have the right number of tris and quads; try to find the proper verts

      // Get connectivity of first tri, and reverse if necessary
      int index = std::find(facehs.begin(), facehs.end(), tris[0]) - facehs.begin();
      rval = mbImpl->get_connectivity(&tris[0], 1, verts);MB_CHK_SET_ERR(rval, "Couldn't get connectivity");
      if (senses[index] > 0)
        std::reverse(verts.begin(), verts.end());

      // Now align vertices of other tri, through a quad, similar to how we did hexes
      // Get q1, which shares 2 vertices with t0
      Range tmp_faces, tmp_verts;
      std::copy(facehs.begin(), facehs.end(), range_inserter(tmp_faces));
      rval = mbImpl->get_adjacencies(&verts[0], 2, 2, false, tmp_faces);
      if (MB_SUCCESS != rval || tmp_faces.size() != 2)
      MB_SET_ERR(MB_FAILURE, "Couldn't get adj face");
      tmp_faces.erase(tris[0]);
      EntityHandle q1 = *tmp_faces.begin();
      // Get verts in q1
      rval = mbImpl->get_connectivity(&q1, 1, storage);MB_CHK_SET_ERR(rval, "Couldn't get adj vertices");

      // Get verts in q1 opposite from v[1] and v[0] in q0
      EntityHandle v0 = 0, v1 = 0;
      rval = mtu.opposite_entity(q1, verts[1], v0);MB_CHK_SET_ERR(rval, "Couldn't get the opposite side entity");
      rval = mtu.opposite_entity(q1, verts[0], v1);MB_CHK_SET_ERR(rval, "Couldn't get the opposite side entity");
      if (v0 && v1) {
        // Offset of v0 in t2, then rotate and flip
        storage.clear();
        rval = mbImpl->get_connectivity(&tris[1], 1, storage);MB_CHK_SET_ERR(rval, "Couldn't get connectivity");

        index = std::find(facehs.begin(), facehs.end(), tris[1]) - facehs.begin();
        if (senses[index] < 0)
          std::reverse(storage.begin(), storage.end());
        unsigned int ioff = std::find(storage.begin(), storage.end(), v0) - storage.begin();
        if (3 == ioff)
          MB_SET_ERR(MB_FAILURE, "Trouble finding offset");
        for (unsigned int i = 0; i < 3; i++)
          verts.push_back(storage[(ioff + i) % 3]);

        this_type = MBPRISM;
      }
    }
    else if (tris.size() == 4 && quads.size() == 1) {
      // Check for pyramid
      // Get connectivity of first tri, and reverse if necessary
      int index = std::find(facehs.begin(), facehs.end(), quads[0]) - facehs.begin();
      rval = mbImpl->get_connectivity(&quads[0], 1, verts);MB_CHK_SET_ERR(rval, "Couldn't get connectivity");
      if (senses[index] > 0)
        std::reverse(verts.begin(), verts.end());

      // Get apex node
      rval = mbImpl->get_connectivity(&tris[0], 1, storage);MB_CHK_SET_ERR(rval, "Couldn't get connectivity");
      for (unsigned int i = 0; i < 3; i++) {
        if (std::find(verts.begin(), verts.end(), storage[i]) == verts.end()) {
          verts.push_back(storage[i]);
          break;
        }
      }

      if (5 == verts.size())
        this_type = MBPYRAMID;
    }
    else {
      // Dummy else clause to stop in debugger
      this_type = MBMAXTYPE;
    }
  }

  if (MBMAXTYPE != input_type && input_type != this_type && this_type != MBMAXTYPE)
    std::cerr << "Warning: types disagree (cell_topo_type = " << CN::EntityTypeName(input_type)
              << ", faces indicate type " << CN::EntityTypeName(this_type) << std::endl;

  if (MBMAXTYPE != input_type && this_type == MBMAXTYPE && input_type != MBPOLYHEDRON)
    std::cerr << "Warning: couldn't find proper connectivity for specified topo_type = "
              << CN::EntityTypeName(input_type) << std::endl;

  // Now make the element; if we fell back to polyhedron, use faces, otherwise use verts
  if (MBPOLYHEDRON == this_type || MBMAXTYPE == this_type) {
    rval = mbImpl->create_element(MBPOLYHEDRON, &facehs[0], facehs.size(), cell);MB_CHK_SET_ERR(rval, "create_element failed");
  }
  else {
    rval = mbImpl->create_element(this_type, &verts[0], verts.size(), cell);MB_CHK_SET_ERR(rval, "create_element failed");
  }

  return MB_SUCCESS;
}

ErrorCode ReadCCMIO::read_all_faces(CCMIOID topologyID, TupleList &vert_map,
                                    TupleList &face_map,
#ifndef TUPLE_LIST
                                    SenseList &sense_map,
#endif
                                    Range *new_faces)
{
  CCMIOSize_t index;
  CCMIOID faceID;
  ErrorCode rval;
  CCMIOError error=kCCMIONoErr;

  // Get total # internal/bdy faces, size the face map accordingly
#ifdef TUPLE_LIST
  index = CCMIOSIZEC(0);
  int nbdy_faces = 0;
  CCMIOSize_t nf;
  error = kCCMIONoErr;
  while (kCCMIONoErr == CCMIONextEntity(NULL, topologyID, kCCMIOBoundaryFaces, &index,
                                        &faceID)) {
    CCMIOEntitySize(&error, faceID, &nf, NULL);
    nbdy_faces += nf;
  }
  CCMIOGetEntity(&error, topologyID, kCCMIOInternalFaces, 0, &faceID);
  CCMIOEntitySize(&error, faceID, &nf, NULL);

  int nint_faces = nf;
  face_map.resize(2*nint_faces + nbdy_faces);
#endif

  // Get multiple blocks of bdy faces
  index = CCMIOSIZEC(0);
  while (kCCMIONoErr == CCMIONextEntity(NULL, topologyID, kCCMIOBoundaryFaces, &index,
                                        &faceID)) {
    rval = read_faces(faceID, kCCMIOBoundaryFaces, vert_map, face_map,
#ifndef TUPLE_LIST
                      sense_map,
#endif
                      new_faces);MB_CHK_SET_ERR(rval, "Trouble reading boundary faces");
  }

  // Now get internal faces
  CCMIOGetEntity(&error, topologyID, kCCMIOInternalFaces, 0, &faceID);CHK_SET_CCMERR(error, "Couldn't get internal faces");

  rval = read_faces(faceID, kCCMIOInternalFaces, vert_map, face_map,
#ifndef TUPLE_LIST
                    sense_map,
#endif
                    new_faces);MB_CHK_SET_ERR(rval, "Trouble reading internal faces");

  return rval;
}

ErrorCode ReadCCMIO::read_faces(CCMIOID faceID,
                                CCMIOEntity bdy_or_int,
                                TupleList &vert_map,
                                TupleList &face_map,
#ifndef TUPLE_LIST
                                SenseList &sense_map,
#endif
                                Range *new_faces)
{
  if (kCCMIOInternalFaces != bdy_or_int && kCCMIOBoundaryFaces != bdy_or_int)
    MB_SET_ERR(MB_FAILURE, "Face type isn't boundary or internal");

  CCMIOSize_t dum_faces;
  CCMIOError error = kCCMIONoErr;
  CCMIOEntitySize(&error, faceID, &dum_faces, NULL);
  int num_faces = GETINT32(dum_faces);

  // Get the size of the face connectivity array (not really a straight connect
  // array, has n, connect(n), ...)
  CCMIOSize_t farray_size = CCMIOSIZEC(0);
  CCMIOReadFaces(&error, faceID, bdy_or_int, NULL, &farray_size, NULL,
                 CCMIOINDEXC(kCCMIOStart), CCMIOINDEXC(kCCMIOEnd));CHK_SET_CCMERR(error, "Trouble reading face connectivity length");

  // Allocate vectors for holding farray and cells for each face; use new for finer
  // control of de-allocation
  int num_sides = (kCCMIOInternalFaces == bdy_or_int ? 2 : 1);
  int *farray = new int[GETINT32(farray_size)];

  // Read farray and make the faces
  CCMIOID mapID;
  CCMIOReadFaces(&error, faceID, bdy_or_int, &mapID, NULL,
                 farray, CCMIOINDEXC(kCCMIOStart), CCMIOINDEXC(kCCMIOEnd));CHK_SET_CCMERR(error, "Trouble reading face connectivity");

  std::vector<EntityHandle> face_handles;
  ErrorCode rval = make_faces(farray, vert_map, face_handles, num_faces);MB_CHK_SET_ERR(rval, "Failed to make the faces");

  // Read face cells and make tuples
  int *face_cells;
  if (num_sides*num_faces < farray_size)
    face_cells = new int[num_sides*num_faces];
  else
    face_cells = farray;
  CCMIOReadFaceCells(&error, faceID, bdy_or_int, face_cells,
                     CCMIOINDEXC(kCCMIOStart), CCMIOINDEXC(kCCMIOEnd));CHK_SET_CCMERR(error, "Trouble reading face cells");

  int *tmp_ptr = face_cells;
  for (unsigned int i = 0; i < face_handles.size(); i++) {
#ifdef TUPLE_LIST
    short forward = 1, reverse = -1;
    face_map.push_back(&forward, tmp_ptr++, &face_handles[i], NULL);
    if (2 == num_sides)
      face_map.push_back(&reverse, tmp_ptr++, &face_handles[i], NULL);
#else
    face_map[*tmp_ptr].push_back(face_handles[i]);
    sense_map[*tmp_ptr++].push_back(1);
    if (2 == num_sides) {
      face_map[*tmp_ptr].push_back(face_handles[i]);
      sense_map[*tmp_ptr++].push_back(-1);
    }
#endif
  }

  // Now read & set face gids, reuse face_cells 'cuz we know it's big enough
  CCMIOReadMap(&error, mapID, face_cells, CCMIOINDEXC(kCCMIOStart), CCMIOINDEXC(kCCMIOEnd));CHK_SET_CCMERR(error, "Trouble reading face gids");

  rval = mbImpl->tag_set_data(mGlobalIdTag, &face_handles[0], face_handles.size(), face_cells);MB_CHK_SET_ERR(rval, "Couldn't set face global ids");

  // Make a neumann set for these faces if they're all in a boundary face set
  if (kCCMIOBoundaryFaces == bdy_or_int) {
    EntityHandle neuset;
    rval = mbImpl->create_meshset(MESHSET_SET, neuset);MB_CHK_SET_ERR(rval, "Failed to create neumann set");

    // Don't trust entity index passed in
    int index;
    CCMIOGetEntityIndex(&error, faceID, &index);
    newNeusets[index] = neuset;

    rval = mbImpl->add_entities(neuset, &face_handles[0], face_handles.size());MB_CHK_SET_ERR(rval, "Failed to add faces to neumann set");

    // Now tag as neumann set; will add id later
    int dum_val = 0;
    rval = mbImpl->tag_set_data(mNeumannSetTag, &neuset, 1, &dum_val);MB_CHK_SET_ERR(rval, "Failed to tag neumann set");
  }

  if (new_faces) {
    std::sort(face_handles.begin(), face_handles.end());
    std::copy(face_handles.rbegin(), face_handles.rend(), range_inserter(*new_faces));
  }

  return MB_SUCCESS;
}

ErrorCode ReadCCMIO::make_faces(int *farray,
                                TupleList &vert_map,
                                std::vector<EntityHandle> &new_faces, int num_faces)
{
  std::vector<EntityHandle> verts;
  ErrorCode tmp_rval = MB_SUCCESS, rval = MB_SUCCESS;

  for (int i = 0; i < num_faces; i++) {
    int num_verts = *farray++;
    verts.resize(num_verts);

    // Fill in connectivity by looking up by gid in vert tuple_list
    for (int j = 0; j < num_verts; j++) {
#ifdef TUPLE_LIST
      int tindex = vert_map.find(1, farray[j]);
      if (-1 == tindex) {
        tmp_rval = MB_FAILURE;
        break;
      }
      verts[j] = vert_map.get_ulong(tindex, 0);
#else
      verts[j] = (vert_map[farray[j]])[0];
#endif
    }
    farray += num_verts;

    if (MB_SUCCESS == tmp_rval) {
      // Make face
      EntityType ftype = (3 == num_verts ? MBTRI :
                         (4 == num_verts ? MBQUAD : MBPOLYGON));
      EntityHandle faceh;
      tmp_rval = mbImpl->create_element(ftype, &verts[0], num_verts, faceh);
      if (faceh)
        new_faces.push_back(faceh);
    }

    if (MB_SUCCESS != tmp_rval)
      rval = tmp_rval;
  }

  return rval;
}

ErrorCode ReadCCMIO::read_vertices(CCMIOSize_t /* proc */, CCMIOID /* processorID */, CCMIOID verticesID,
                                   CCMIOID /* topologyID */,
                                   Range *verts, TupleList &vert_map)
{
  CCMIOError error = kCCMIONoErr;

  // Pre-read the number of vertices, so we can pre-allocate & read directly in
  CCMIOSize_t nverts = CCMIOSIZEC(0);
  CCMIOEntitySize(&error, verticesID, &nverts, NULL);CHK_SET_CCMERR(error, "Couldn't get number of vertices");

  // Get # dimensions
  CCMIOSize_t dims;
  float scale;
  CCMIOReadVerticesf(&error, verticesID, &dims, NULL, NULL, NULL,
                     CCMIOINDEXC(0), CCMIOINDEXC(1));CHK_SET_CCMERR(error, "Couldn't get number of dimensions");

  // Allocate vertex space
  EntityHandle node_handle = 0;
  std::vector<double*> arrays;
  readMeshIface->get_node_coords(3, GETINT32(nverts), MB_START_ID, node_handle, arrays);

  // Read vertex coords
  CCMIOID mapID;
  std::vector<double> tmp_coords(GETINT32(dims)*GETINT32(nverts));
  CCMIOReadVerticesd(&error, verticesID, &dims, &scale, &mapID, &tmp_coords[0],
                     CCMIOINDEXC(0), CCMIOINDEXC(0 + nverts));CHK_SET_CCMERR(error, "Trouble reading vertex coordinates");

  // Copy interleaved coords into moab blocked coordinate space
  int i = 0, threei = 0;
  for ( ; i < nverts; i++) {
    arrays[0][i] = tmp_coords[threei++];
    arrays[1][i] = tmp_coords[threei++];
    if (3 == GETINT32(dims))
      arrays[2][i] = tmp_coords[threei++];
    else
      arrays[2][i] = 0.0;
  }

  // Scale, if necessary
  if (1.0 != scale) {
    for(i = 0; i < nverts; i++) {
      arrays[0][i] *= scale;
      arrays[1][i] *= scale;
      if (3 == GETINT32(dims))
        arrays[2][i] *= scale;
    }
  }

  // Read gids for vertices
  std::vector<int> gids(GETINT32(nverts));
  CCMIOReadMap(&error, mapID, &gids[0], CCMIOINDEXC(kCCMIOStart), CCMIOINDEXC(kCCMIOEnd));CHK_SET_CCMERR(error, "Trouble reading vertex global ids");

  // Put new vertex handles into range, and set gids for them
  Range new_verts(node_handle, node_handle + nverts - 1);
  ErrorCode rval = mbImpl->tag_set_data(mGlobalIdTag, new_verts, &gids[0]);MB_CHK_SET_ERR(rval, "Couldn't set gids on vertices");

  // Pack vert_map with global ids and handles for these vertices
#ifdef TUPLE_LIST
  vert_map.resize(GETINT32(nverts));
  for (i = 0; i < GETINT32(nverts); i++) {
    vert_map.push_back(NULL, &gids[i], &node_handle, NULL);
#else
  for (i = 0; i < GETINT32(nverts); i++) {
    (vert_map[gids[i]]).push_back(node_handle);
#endif
    node_handle += 1;
  }

  if (verts)
    verts->merge(new_verts);

  return MB_SUCCESS;
}
  
ErrorCode ReadCCMIO::get_processors(CCMIOID stateID,
                                    CCMIOID &processorID, CCMIOID &verticesID,
                                    CCMIOID &topologyID, CCMIOID &solutionID,
                                    std::vector<CCMIOSize_t> &procs,
                                    bool & /* has_solution */)
{
  CCMIOSize_t proc = CCMIOSIZEC(0);
  CCMIOError error = kCCMIONoErr;

  CCMIONextEntity(&error, stateID, kCCMIOProcessor, &proc, &processorID);CHK_SET_CCMERR(error, "CCMIONextEntity() failed");
  if (CCMIOReadProcessor(NULL, processorID, &verticesID,
                         &topologyID, NULL, &solutionID) != kCCMIONoErr) {
    // Maybe no solution; try again
    CCMIOReadProcessor(&error, processorID, &verticesID,
                       &topologyID, NULL, NULL);CHK_SET_CCMERR(error, "CCMIOReadProcessor() failed");
    hasSolution = false;
  }

  procs.push_back(proc);

  return MB_SUCCESS;
}

ErrorCode ReadCCMIO::read_tag_values(const char* /* file_name */,
                                     const char* /* tag_name */,
                                     const FileOptions& /* opts */,
                                     std::vector<int>& /* tag_values_out */,
                                     const SubsetList* /* subset_list */)
{
  return MB_FAILURE;
}

} // namespace moab
