/*
 * CCMIO file structure
 *
 * Root
 *   State(kCCMIOState)
 *     Processor*
 *       VerticesID
 *       TopologyID
 *       InitialID
 *       SolutionID
 *   Vertices*
 *     ->WriteVerticesx, WriteMap
 *   Topology*
 *     Boundary faces*(kCCMIOBoundaryFaces)
 *        ->WriteFaces, WriteFaceCells, WriteMap
 *     Internal faces(kCCMIOInternalFaces)
 *     Cells (kCCMIOCells)
 *        ->WriteCells (mapID), WriteMap, WriteCells
 *   Solution
 *     Phase
 *       Field
 *         FieldData
 *   Problem(kCCMIOProblemDescription)
 *     CellType* (kCCMIOCellType)
 *       Index (GetEntityIndex), MaterialId(WriteOpti), MaterialType(WriteOptstr),
 *         PorosityId(WriteOpti), SpinId(WriteOpti), GroupId(WriteOpti)
 *
 * MaterialType (CCMIOWriteOptstr in readexample)
 * constants (see readexample)
 * lagrangian data (CCMIOWriteLagrangianData)
 * vertices label (CCMIOEntityDescription)
 * restart info: char solver[], iteratoins, time, char timeUnits[], angle
 *      (CCMIOWriteRestartInfo, kCCMIORestartData), reference data?
 * phase:
 *   field: char name[], dims, CCMIODataType datatype, char units[]
 *       dims = kCCMIOScalar (CCMIOWriteFieldDataf), 
 *              kCCMIOVector (CCMIOWriteMultiDimensionalFieldData),
 *              kCCMIOTensor
 * MonitoringSets: num, name (CellSet, VertexSet, BoundarySet, BlockSet, SplineSet, CoupleSet)
 *      CCMIOGetProstarSet, CCMIOWriteOpt1i,
 */

#ifdef WIN32
#ifdef _DEBUG
// turn off warnings that say they debugging identifier has been truncated
// this warning comes up when using some STL containers
#pragma warning(disable : 4786)
#endif
#endif


#include "WriteCCMIO.hpp"
#include "ccmio.h"
#include "ccmioutility.h"
#include "ccmiocore.h"
#include <utility>
#include <algorithm>
#include <time.h>
#include <string>
#include <vector>
#include <stdio.h>
#include <iostream>
#include <algorithm>
#include <sstream>

#include "moab/Interface.hpp"
#include "moab/Range.hpp"
#include "moab/CN.hpp"
#include "moab/Skinner.hpp"
#include "assert.h"
#include "Internals.hpp"
#include "ExoIIUtil.hpp"
#include "MBTagConventions.hpp"
#ifdef USE_MPI  
#include "MBParallelConventions.h"
#endif
#include "moab/WriteUtilIface.hpp"

namespace moab {

  static char const kStateName[] = "default";

  static const int ccm_types[] = {
    1,   // MBVERTEX
    2,   // MBEDGE      
    -1,  // MBTRI
    -1,  // MBQUAD
    -1,  // MBPOLYGON
    13,  // MBTET
    14,  // MBPYRAMID
    12,  // MBPRISM
    -1,  // MBKNIFE
    11,  // MBHEX
    255  // MBPOLYHEDRON
  };

#define INS_ID(stringvar, prefix, id)           \
  sprintf(stringvar, prefix, id)

#define CHKERR(a, b)							\
  {if (MB_SUCCESS != a) {if (b) mWriteIface->report_error(b); return a;}}

#define CHKCCMERR(a, b)							\
  {if (kCCMIONoErr != a) {if (b) mWriteIface->report_error(b); return MB_FAILURE;}}
  
  WriterIface* WriteCCMIO::factory( Interface* iface )
  { return new WriteCCMIO( iface ); }

  WriteCCMIO::WriteCCMIO(Interface *impl) 
    : mbImpl(impl), mCurrentMeshHandle(0), mNameTag(0), mMaterialIdTag(0), 
      mMaterialTypeTag(0), 
      mRadiationTag(0), mPorosityIdTag(0), mSpinIdTag(0), mGroupIdTag(0), mColorIdxTag(0),
      mProcessorIdTag(0), mLightMaterialTag(0), mFreeSurfaceMaterialTag(0), 
      mThicknessTag(0), mProstarRegionNumberTag(0), mBoundaryTypeTag(0), mCreatingProgramTag(0),
      mWholeMesh(false)
  {
    assert(impl != NULL);

    impl->query_interface( mWriteIface );

    // initialize in case tag_get_handle fails below
    //! get and cache predefined tag handles
    int zero = 0, negone = -1;
    impl->tag_get_handle(MATERIAL_SET_TAG_NAME, 1, MB_TYPE_INTEGER,
                         mMaterialSetTag, MB_TAG_SPARSE|MB_TAG_CREAT, &negone);

    impl->tag_get_handle(DIRICHLET_SET_TAG_NAME, 1, MB_TYPE_INTEGER,
                         mDirichletSetTag, MB_TAG_SPARSE|MB_TAG_CREAT, &negone);

    impl->tag_get_handle(NEUMANN_SET_TAG_NAME, 1, MB_TYPE_INTEGER,
                         mNeumannSetTag, MB_TAG_SPARSE|MB_TAG_CREAT, &negone);

    impl->tag_get_handle(GLOBAL_ID_TAG_NAME, 1, MB_TYPE_INTEGER,
                         mGlobalIdTag, MB_TAG_SPARSE|MB_TAG_CREAT, &zero);

#ifdef USE_MPI  
    impl->tag_get_handle(PARALLEL_PARTITION_TAG_NAME, 
                         1, MB_TYPE_INTEGER, mPartitionSetTag,
                         MB_TAG_SPARSE);
    // no need to check result, if it's not there, we don't create one
#endif
  
    int dum_val_array[] = {-1, -1, -1, -1};
    impl->tag_get_handle(HAS_MID_NODES_TAG_NAME, 4, MB_TYPE_INTEGER,
                         mHasMidNodesTag, MB_TAG_SPARSE|MB_TAG_CREAT, dum_val_array);
  
    impl->tag_get_handle("__WriteCCMIO element mark", 1, MB_TYPE_BIT, mEntityMark, MB_TAG_CREAT);
  
    // don't need to check return of following, since it doesn't matter if there isn't one
    mbImpl->tag_get_handle(NAME_TAG_NAME, NAME_TAG_SIZE, MB_TYPE_OPAQUE, mNameTag);
  }

  WriteCCMIO::~WriteCCMIO() 
  {
    mbImpl->release_interface(mWriteIface);

    mbImpl->tag_delete(mEntityMark);

  }

  ErrorCode WriteCCMIO::write_file(const char *file_name, 
				   const bool overwrite,
				   const FileOptions& ,
				   const EntityHandle *ent_handles,
				   const int num_sets,
				   const std::vector<std::string>&,
				   const Tag* ,
				   int ,
				   int )
  {
    assert(0 != mMaterialSetTag &&
	   0 != mNeumannSetTag &&
	   0 != mDirichletSetTag);

    ErrorCode result;
  
    // check overwrite flag and file existence
    if (!overwrite) {
      FILE *file = fopen(file_name, "r");
      if (file) {
	fclose(file);
	result = MB_FILE_WRITE_ERROR;
	CHKERR(result, "File exists but overwrite set to false.");
      }
    }
  
    mDimension = 3;

    std::vector<EntityHandle> matsets, dirsets, neusets, partsets, entities;

    // separate into material, dirichlet, neumann, partition sets
    result = get_sets(ent_handles, num_sets, matsets, 
		      dirsets, neusets, partsets);
    CHKERR(result, "Failed to get material/etc. sets.");

    // if entity handles were input but didn't contain matsets, return error
    if (ent_handles && matsets.empty()) {
      result = MB_FILE_WRITE_ERROR;
      CHKERR(result, "Sets input to write but no material sets found.");
    }

    // otherwise, if no matsets, use root set
    if (matsets.empty()) matsets.push_back(0);

    std::vector<MaterialSetData> matset_info;
    Range all_verts;
    result = gather_matset_info(matsets, matset_info, all_verts);
    CHKERR(result, "gathering matset info failed.");

    // assign vertex gids
    result = mWriteIface->assign_ids(all_verts, mGlobalIdTag, 1);
    CHKERR(result, "Failed to assign vertex global ids.");

    // some CCMIO descriptors
    CCMIOID rootID, topologyID, stateID, problemID, verticesID, processorID;

    // try to open the file and establish state
    result = open_file(file_name, overwrite, rootID);
    CHKERR(result, "Couldn't open file or create state.");

    result = create_ccmio_structure(rootID, stateID, processorID);
    CHKERR(result, "Problem creating CCMIO file structure.");
  
    result = write_nodes(rootID, all_verts, mDimension, verticesID);
    CHKERR(result, "write_nodes failed.");

    std::vector<NeumannSetData> neuset_info;
    result = gather_neuset_info(neusets, neuset_info);
    CHKERR(result, "Failed to get neumann set info.");

    result = write_cells_and_faces(rootID, matset_info, neuset_info, all_verts, topologyID);
    CHKERR(result, "write_cells_and_faces failed.");

    result = write_problem_description(rootID, stateID, problemID, processorID,
				       matset_info, neuset_info);
    CHKERR(result, "write_problem_description failed.");

    result = write_solution_data();
    CHKERR(result, "trouble writing solution data.");
  
    result = write_processor(processorID, verticesID, topologyID);
    CHKERR(result, "trouble writing processor.");
  
    result = close_and_compress(file_name, rootID);
    CHKERR(result, "close or compress failed.");

    return MB_SUCCESS;
  }

  ErrorCode WriteCCMIO::write_solution_data() 
  {
    // for now, no solution (tag) data
    return MB_SUCCESS;
  }

  ErrorCode WriteCCMIO::write_processor(CCMIOID processorID, CCMIOID verticesID, CCMIOID topologyID) 
  {
    CCMIOError error = kCCMIONoErr;
  
    // Now we have the mesh (vertices and topology) and the post data written.
    // Since we now have their IDs, we can write out the processor information.
    CCMIOWriteProcessor(&error, processorID, NULL, &verticesID, NULL, &topologyID,
			NULL, NULL, NULL, NULL);
    CHKCCMERR(error, "Problem writing CCMIO processor.");
  
    return MB_SUCCESS;
  }

  ErrorCode WriteCCMIO::create_ccmio_structure(CCMIOID rootID, CCMIOID &stateID,
					       CCMIOID &processorID) 
  {
    // create problem state and other CCMIO nodes under it
    CCMIOError error = kCCMIONoErr;
  
    // Create a new state (or re-use an existing one).
    if (CCMIOGetState(NULL, rootID, kStateName, NULL, &stateID) != kCCMIONoErr)
      CCMIONewState(&error, rootID, kStateName, NULL, NULL, &stateID);
    CHKCCMERR(error, "Trouble creating state.");

    // Create or get an old processor for this state
    CCMIOSize_t i = CCMIOSIZEC(0);
    if (CCMIONextEntity(NULL, stateID, kCCMIOProcessor, &i, &processorID) != kCCMIONoErr)
      CCMIONewEntity(&error, stateID, kCCMIOProcessor, NULL, &processorID);

    // Get rid of any data that may be in this processor (if the state was
    // not new).
    else
      CCMIOClearProcessor(&error, stateID, processorID, TRUE, TRUE, TRUE, TRUE, TRUE);

    /*


    //  for (; i < CCMIOSIZEC(partsets.size()); i++) {
    CCMIOSize_t id = CCMIOSIZEC(0);
    if (CCMIONextEntity(NULL, stateID, kCCMIOProcessor, &id, &processorID) != kCCMIONoErr)
    CCMIONewEntity(&error, stateID, kCCMIOProcessor, NULL, &processorID);
    CHKCCMERR(error, "Trouble creating processor node.");
    */
    return MB_SUCCESS;
  }

  ErrorCode WriteCCMIO::close_and_compress(const char *, CCMIOID rootID)
  {
    CCMIOError error = kCCMIONoErr;
    CCMIOCloseFile(&error, rootID);
    CHKCCMERR(error, "File close failed.");

    // The CCMIO library uses ADF to store the actual data.  Unfortunately,
    // ADF leaks disk space;  deleting a node does not recover all the disk
    // space.  Now that everything is successfully written it might be useful
    // to call CCMIOCompress() here to ensure that the file is as small as
    // possible.  Please see the Core API documentation for caveats on its
    // usage.
    // CCMIOCompress(&error, const_cast<char*>(filename));
    // CHKCCMERR(error, "Error compressing file.");

    return MB_SUCCESS;
  }

  ErrorCode WriteCCMIO::open_file(const char *filename, bool , CCMIOID &rootID) 
  {
    CCMIOError error = kCCMIONoErr;
    CCMIOOpenFile(&error, filename, kCCMIOWrite, &rootID);
    CHKCCMERR(error, "Cannot open file.");

    return MB_SUCCESS;
  }

  ErrorCode WriteCCMIO::get_sets(const EntityHandle *ent_handles,
				 int num_sets,
				 std::vector<EntityHandle> &matsets,
				 std::vector<EntityHandle> &dirsets,
				 std::vector<EntityHandle> &neusets,
				 std::vector<EntityHandle> &partsets) 
  {
    if (num_sets == 0) {
      // default to all defined sets
      Range this_range;
      mbImpl->get_entities_by_type_and_tag(0, MBENTITYSET, &mMaterialSetTag, NULL, 1, this_range);
      std::copy(this_range.begin(), this_range.end(), std::back_inserter(matsets));
      this_range.clear();
      mbImpl->get_entities_by_type_and_tag(0, MBENTITYSET, &mDirichletSetTag, NULL, 1, this_range);
      std::copy(this_range.begin(), this_range.end(), std::back_inserter(dirsets));
      this_range.clear();
      mbImpl->get_entities_by_type_and_tag(0, MBENTITYSET, &mNeumannSetTag, NULL, 1, this_range);
      std::copy(this_range.begin(), this_range.end(), std::back_inserter(neusets));
      if (mPartitionSetTag) {
	this_range.clear();
	mbImpl->get_entities_by_type_and_tag(0, MBENTITYSET, &mPartitionSetTag, NULL, 1, this_range);
	std::copy(this_range.begin(), this_range.end(), std::back_inserter(partsets));
      }
    }
  
    else {
      int dummy;
      for (const EntityHandle *iter = ent_handles; iter < ent_handles+num_sets; iter++) 
	{
	  if (MB_SUCCESS == mbImpl->tag_get_data(mMaterialSetTag, &(*iter), 1, &dummy))
	    matsets.push_back(*iter);
	  else if (MB_SUCCESS == mbImpl->tag_get_data(mDirichletSetTag, &(*iter), 1, &dummy))
	    dirsets.push_back(*iter);
	  else if (MB_SUCCESS == mbImpl->tag_get_data(mNeumannSetTag, &(*iter), 1, &dummy))
	    neusets.push_back(*iter);
	  else if (mPartitionSetTag &&
		   MB_SUCCESS == mbImpl->tag_get_data(mPartitionSetTag, &(*iter), 1, &dummy))
	    partsets.push_back(*iter);
	}
    }

    return MB_SUCCESS;
  }
      
  ErrorCode WriteCCMIO::write_problem_description(CCMIOID rootID, CCMIOID stateID, CCMIOID &problemID, 
						  CCMIOID processorID, 
						  std::vector<WriteCCMIO::MaterialSetData> &matset_data,
						  std::vector<WriteCCMIO::NeumannSetData> &neuset_data) 
  {
    // Write out a dummy problem description.  If we happen to know that
    // there already is a problem description previously recorded that
    // is valid we could skip this step.
    CCMIOID id;
    CCMIOError error = kCCMIONoErr;
    ErrorCode rval;
    const EntityHandle mesh = 0;

    bool root_tagged = false, other_set_tagged = false;
    Tag simname;
    Range dum_sets;
    rval = mbImpl->tag_get_handle("Title", 0, MB_TYPE_OPAQUE, simname, MB_TAG_ANY);
    if (MB_SUCCESS == rval) {
      int tag_size;
      rval = mbImpl->tag_get_bytes(simname, tag_size);
      if (MB_SUCCESS == rval) {
	std::vector<char> title_tag(tag_size+1);
	rval = mbImpl->get_entities_by_type_and_tag(0, MBENTITYSET, &simname, NULL, 1, dum_sets);
	if (MB_SUCCESS == rval && !dum_sets.empty()) {
	  rval = mbImpl->tag_get_data(simname, &(*dum_sets.begin()), 1, &title_tag[0]);
	  CHKERR(rval, "Problem getting simulation name tag.");
	  other_set_tagged = true;
	}
	else if (MB_SUCCESS == rval) {
          // check to see if interface was tagged
	  rval = mbImpl->tag_get_data(simname, &mesh, 1, &title_tag[0]);
	  if (MB_SUCCESS == rval) root_tagged = true;
	  else rval = MB_SUCCESS;
	}
	*title_tag.rbegin() = '\0';
	if (root_tagged || other_set_tagged) {
	  CCMIONode rootNode;
	  if (kCCMIONoErr == CCMIOGetEntityNode(&error, rootID, &rootNode)) {
	    CCMIOSetTitle(&error, rootNode, &title_tag[0]);
	    CHKCCMERR(error, "Trouble setting title.");
	  }
	}
      }
    }
  
    rval = mbImpl->tag_get_handle("CreatingProgram", 0, MB_TYPE_OPAQUE, mCreatingProgramTag, MB_TAG_ANY);
    if (MB_SUCCESS == rval) {
      int tag_size;
      rval = mbImpl->tag_get_bytes(mCreatingProgramTag, tag_size);
      if (MB_SUCCESS == rval) {
	std::vector<char> cp_tag(tag_size+1);
	rval = mbImpl->get_entities_by_type_and_tag(0, MBENTITYSET, &mCreatingProgramTag, NULL, 1, dum_sets);
	if (MB_SUCCESS == rval && !dum_sets.empty()) {
	  rval = mbImpl->tag_get_data(mCreatingProgramTag, &(*dum_sets.begin()), 1, &cp_tag[0]);
	  CHKERR(rval, "Problem getting creating program tag.");
	  other_set_tagged = true;
	}
	else if (MB_SUCCESS == rval) {
          // check to see if interface was tagged
	  rval = mbImpl->tag_get_data(mCreatingProgramTag, &mesh, 1, &cp_tag[0]);
	  if (MB_SUCCESS == rval) root_tagged = true;
	  else rval = MB_SUCCESS;
	}
	*cp_tag.rbegin() = '\0';
	if (root_tagged || other_set_tagged) {
	  CCMIONode rootNode;
	  if (kCCMIONoErr == CCMIOGetEntityNode(&error, rootID, &rootNode)) {
	    CCMIOWriteOptstr(&error, processorID, "CreatingProgram", &cp_tag[0]);
	    CHKCCMERR(error, "Trouble setting creating program.");
	  }
	}
      }
    }
  
    CCMIONewEntity(&error, rootID, kCCMIOProblemDescription, NULL,
		   &problemID);
    CHKCCMERR(error, "Trouble creating problem node.");

    // write material types and other info
    for (unsigned int i = 0; i < matset_data.size(); i++) {
      if (!matset_data[i].setName.empty()){
	CCMIONewIndexedEntity(&error, problemID, kCCMIOCellType, matset_data[i].matsetId, 
			      matset_data[i].setName.c_str(), &id);
	CHKCCMERR(error, "Failure creating celltype node."); 

	CCMIOWriteOptstr(&error, id, "MaterialType", matset_data[i].setName.c_str());
	CHKCCMERR(error, "Error assigning material name.");
      }
      else{
	char dum_name[NAME_TAG_SIZE]; 
	std::ostringstream os;
	std::string mat_name = "Material", temp_str;
	os << mat_name << (i+1);
	temp_str = os.str();
	strcpy(dum_name,temp_str.c_str());
	CCMIONewIndexedEntity(&error, problemID, kCCMIOCellType, matset_data[i].matsetId, 
			      dum_name, &id);
	CHKCCMERR(error, "Failure creating celltype node."); 
  
	CCMIOWriteOptstr(&error, id, "MaterialType", dum_name);
	CHKCCMERR(error, "Error assigning material name.");
    
	os.str("");
      }
      rval = write_int_option("MaterialId", matset_data[i].setHandle, mMaterialIdTag, id);
      CHKERR(rval, "Trouble writing MaterialId option.");

      rval = write_int_option("Radiation", matset_data[i].setHandle, mRadiationTag, id);
      CHKERR(rval, "Trouble writing Radiation option.");

      rval = write_int_option("PorosityId", matset_data[i].setHandle, mPorosityIdTag, id);
      CHKERR(rval, "Trouble writing PorosityId option.");

      rval = write_int_option("SpinId", matset_data[i].setHandle, mSpinIdTag, id);
      CHKERR(rval, "Trouble writing SpinId option.");

      rval = write_int_option("GroupId", matset_data[i].setHandle, mGroupIdTag, id);
      CHKERR(rval, "Trouble writing GroupId option.");

      rval = write_int_option("ColorIdx", matset_data[i].setHandle, mColorIdxTag, id);
      CHKERR(rval, "Trouble writing ColorIdx option.");

      rval = write_int_option("ProcessorId", matset_data[i].setHandle, mProcessorIdTag, id);
      CHKERR(rval, "Trouble writing ProcessorId option.");

      rval = write_int_option("LightMaterial", matset_data[i].setHandle, mLightMaterialTag, id);
      CHKERR(rval, "Trouble writing LightMaterial option.");

      rval = write_int_option("FreeSurfaceMaterial", matset_data[i].setHandle, mFreeSurfaceMaterialTag, id);
      CHKERR(rval, "Trouble writing FreeSurfaceMaterial option.");

      rval = write_dbl_option("Thickness", matset_data[i].setHandle, mThicknessTag, id);
      CHKERR(rval, "Trouble writing Thickness option.");

      rval = write_str_option("MaterialType", matset_data[i].setHandle, mMaterialTypeTag, id);
      CHKERR(rval, "Trouble writing MaterialType option.");
    }
  
    // write neumann set info
    for (unsigned int i = 0; i < neuset_data.size(); i++) {
      // use the label to encode the id
      std::ostringstream dum_id;
      dum_id << neuset_data[i].neusetId;
      CCMIONewIndexedEntity(&error, problemID, kCCMIOBoundaryRegion, neuset_data[i].neusetId, 
			    dum_id.str().c_str(), &id);
      CHKCCMERR(error, "Failure creating BoundaryRegion node.");

      rval = write_str_option("BoundaryName", neuset_data[i].setHandle, mNameTag,
			      id);
      CHKERR(rval, "Trouble writing boundary type number.");

      rval = write_str_option("BoundaryType", neuset_data[i].setHandle, mBoundaryTypeTag,
			      id);
      CHKERR(rval, "Trouble writing boundary type number.");

      rval = write_int_option("ProstarRegionNumber", neuset_data[i].setHandle, mProstarRegionNumberTag,
			      id);
      CHKERR(rval, "Trouble writing prostar region number.");
    }
  
    CCMIOWriteState(&error, stateID, problemID, "Example state");
    CHKCCMERR(error, "Failure writing problem state.");

    // get cell types; reuse cell ids array
    //  for (i = 0, rit = all_elems.begin(); i < num_elems; i++, rit++) {
    //    egids[i] = ccm_types[mbImpl->type_from_handle(*rit)];
    //    assert(-1 != egids[i]);
    //  }

    return MB_SUCCESS;
  }

  ErrorCode WriteCCMIO::write_int_option(const char *opt_name,
					 EntityHandle seth,
					 Tag &tag, CCMIOID &node) 
  {
    ErrorCode rval;

    if (!tag) {
      rval = mbImpl->tag_get_handle(opt_name, 1, MB_TYPE_INTEGER, tag);
      // return success since that just means we don't have to write this option
      if (MB_SUCCESS != rval) return MB_SUCCESS;
    }
  
    int dum_val;
    rval = mbImpl->tag_get_data(tag, &seth, 1, &dum_val);
    // return success since that just means we don't have to write this option
    if (MB_SUCCESS != rval) return MB_SUCCESS;
  
    CCMIOError error = kCCMIONoErr;
    CCMIOWriteOpti(&error, node, opt_name, dum_val);
    CHKCCMERR(error, "Trouble writing int option.");
  
    return MB_SUCCESS;
  }

  ErrorCode WriteCCMIO::write_dbl_option(const char *opt_name,
					 EntityHandle seth,
					 Tag &tag, CCMIOID &node) 
  {
    ErrorCode rval;

    if (!tag) {
      rval = mbImpl->tag_get_handle(opt_name, 1, MB_TYPE_DOUBLE, tag);
      // return success since that just means we don't have to write this option
      if (MB_SUCCESS != rval) return MB_SUCCESS;
    }
  
    double dum_val;
    rval = mbImpl->tag_get_data(tag, &seth, 1, &dum_val);
    // return success since that just means we don't have to write this option
    if (MB_SUCCESS != rval) return MB_SUCCESS;
  
    CCMIOError error = kCCMIONoErr;
    CCMIOWriteOptf(&error, node, opt_name, dum_val);
    CHKCCMERR(error, "Trouble writing int option.");
  
    return MB_SUCCESS;
  }

  ErrorCode WriteCCMIO::write_str_option(const char *opt_name,
					 EntityHandle seth,
					 Tag &tag, CCMIOID &node,
					 const char *other_name) 
  {
    int tag_size;
    ErrorCode rval;

    if (!tag) {
      rval = mbImpl->tag_get_handle(opt_name, 0, MB_TYPE_OPAQUE, tag, MB_TAG_ANY);
      // return success since that just means we don't have to write this option
      if (MB_SUCCESS != rval) return MB_SUCCESS;
    }
  
    rval = mbImpl->tag_get_bytes(tag, tag_size);
    if (MB_SUCCESS != rval) return MB_SUCCESS;
    std::vector<char> opt_val(tag_size+1);

    rval = mbImpl->tag_get_data(tag, &seth, 1, &opt_val[0]);
    if (MB_SUCCESS != rval) return MB_SUCCESS;

    // null-terminate if necessary
    if (std::find(opt_val.begin(), opt_val.end(), '\0') == opt_val.end())
      *opt_val.rbegin() = '\0';

    CCMIOError error = kCCMIONoErr;
    if (other_name) 
      CCMIOWriteOptstr(&error, node, other_name, &opt_val[0]);
    else
      CCMIOWriteOptstr(&error, node, opt_name, &opt_val[0]);
    CHKCCMERR(error, "Failure writing an option string MaterialType.");
  
    return MB_SUCCESS;
  }

  ErrorCode WriteCCMIO::gather_matset_info(std::vector<EntityHandle> &matsets,
					   std::vector<MaterialSetData> &matset_data,
					   Range &all_verts)
  {
    ErrorCode result;
    matset_data.resize(matsets.size());
    if (1 == matsets.size() && 0 == matsets[0]) {
      // whole mesh
      mWholeMesh = true;
    
      result = mbImpl->get_entities_by_dimension(0, mDimension, matset_data[0].elems);
      CHKERR(result, "Trouble getting all elements in mesh.");
      result = mWriteIface->gather_nodes_from_elements(matset_data[0].elems,
						       mEntityMark, all_verts);
      CHKERR(result, "Trouble gathering nodes from elements.");

      return result;
    }

    std::vector<unsigned char> marks;
    for(unsigned int i = 0; i < matsets.size(); i++)
      {
	EntityHandle this_set = matset_data[i].setHandle = matsets[i];
    
	// get all Entity Handles in the set
	result = mbImpl->get_entities_by_dimension(this_set, mDimension, matset_data[i].elems, true);
	CHKERR(result, "Trouble getting m-dimensional ents.");

	// get all connected vertices
	result = mWriteIface->gather_nodes_from_elements(matset_data[i].elems,
							 mEntityMark, all_verts);
	CHKERR(result, "Trouble getting vertices for a matset.");

	// check for consistent entity type
	EntityType start_type = mbImpl->type_from_handle(*matset_data[i].elems.begin());
	if (start_type == mbImpl->type_from_handle(*matset_data[i].elems.rbegin()))
	  matset_data[i].entityType = start_type;

	// mark elements in this matset
	marks.resize(matset_data[i].elems.size(), 0x1);
	result = mbImpl->tag_set_data(mEntityMark, matset_data[i].elems, &marks[0]);
	CHKERR(result, "Couln't mark entities being output.");

	// get id for this matset
	result = mbImpl->tag_get_data(mMaterialSetTag, &this_set, 1, &matset_data[i].matsetId);
	CHKERR(result, "Couln't get global id for material set.");

	// get name for this matset
	if (mNameTag) {
	  char dum_name[NAME_TAG_SIZE];
	  result = mbImpl->tag_get_data(mNameTag, &this_set, 1, dum_name);
	  if (MB_SUCCESS == result) matset_data[i].setName = dum_name;

	  // reset success, so later checks don't fail
	  result = MB_SUCCESS;
	}
      }
  
  
    if (all_verts.empty()) {
      result = MB_FILE_WRITE_ERROR;
      CHKERR(result, "No vertices from elements.");
    }
    
    return MB_SUCCESS;
  }

  ErrorCode WriteCCMIO::gather_neuset_info(std::vector<EntityHandle> &neusets,
					   std::vector<NeumannSetData> &neuset_info)
  {
    ErrorCode result;

    std::vector<unsigned char> marks;
    neuset_info.resize(neusets.size());
    for(unsigned int i = 0; i < neusets.size(); i++)
      {
	EntityHandle this_set = neuset_info[i].setHandle = neusets[i];
    
	// get all Entity Handles of one less dimension than that being output 
	result = mbImpl->get_entities_by_dimension(this_set, mDimension-1, neuset_info[i].elems, true);
	CHKERR(result, "Trouble getting (m-1)-dimensional ents for neuset.");
    
	result = mbImpl->tag_get_data(mGlobalIdTag, &this_set, 1, &neuset_info[i].neusetId);
	if (MB_TAG_NOT_FOUND == result) {
	  result = mbImpl->tag_get_data(mNeumannSetTag, &this_set, 1, &neuset_info[i].neusetId);
	  if (MB_SUCCESS != result) 
	    // need some id; use the loop iteration number
	    neuset_info[i].neusetId = i;
	}

	// get name for this neuset
	if (mNameTag) {
	  char dum_name[NAME_TAG_SIZE];
	  result = mbImpl->tag_get_data(mNameTag, &this_set, 1, dum_name);
	  if (MB_SUCCESS == result) neuset_info[i].setName = dum_name;

	  // reset success, so later checks don't fail
	  result = MB_SUCCESS;
	}
      }

    return MB_SUCCESS;
  }

  ErrorCode WriteCCMIO::get_gids(const Range &ents, int *&gids,
				 int &minid, int &maxid) 
  {
    int num_ents = ents.size();
    gids = new int[num_ents];
    ErrorCode result = mbImpl->tag_get_data(mGlobalIdTag, ents, &gids[0]);
    if (MB_SUCCESS != result) {
      mWriteIface->report_error("Couldn't get global id data.");
      return result;
    }
    minid = *std::min_element(gids, gids+num_ents);
    maxid = *std::max_element(gids, gids+num_ents);
    if (0 == minid) {
      // gids need to be assigned
      for (int i = 1; i <= num_ents; i++) gids[i] = i;
      result = mbImpl->tag_set_data(mGlobalIdTag, ents, &gids[0]);
      if (MB_SUCCESS != result) {
	mWriteIface->report_error("Couldn't set global id data.");
	return result;
      }
      maxid = num_ents;
    }
    return MB_SUCCESS;
  }

  ErrorCode WriteCCMIO::write_nodes(CCMIOID rootID,
				    const Range &verts, 
				    const int dimension,
				    CCMIOID &verticesID)
  {
    // get/write map (global ids) first (gids already assigned)
    unsigned int num_verts = verts.size();
    std::vector<int> vgids(num_verts);
    ErrorCode result = mbImpl->tag_get_data(mGlobalIdTag, verts, &vgids[0]);
    CHKERR(result, "Failed to get global ids for vertices.");

    // create the map node for vertex ids, and write them to that node
    CCMIOID mapID;
    CCMIOError error = kCCMIONoErr;
    CCMIONewEntity(&error, rootID, kCCMIOMap, "Vertex map", &mapID);
    CHKCCMERR(error, "Failure creating Vertex map node.");

    int maxid = *std::max_element(vgids.begin(), vgids.end());
  
    CCMIOWriteMap(&error, mapID, CCMIOSIZEC(num_verts),
		  CCMIOSIZEC(maxid), &vgids[0],
		  CCMIOINDEXC(kCCMIOStart), CCMIOINDEXC(kCCMIOEnd));
    CHKCCMERR(error, "Problem writing node map.");

    // create the vertex coordinate node, and write it
    CCMIONewEntity(&error, rootID, kCCMIOVertices, "Vertices", &verticesID);
    CHKCCMERR(error, "Trouble creating vertices node.");

    // get the vertex locations
    double *coords = new double[3*num_verts];
    std::vector<double*> coord_arrays(3);
    coord_arrays[0] = coords;
    coord_arrays[1] = coords+num_verts;
    coord_arrays[2] = (dimension == 3 ? coords+2*num_verts : NULL);
    result = mWriteIface->get_node_coords(-1, verts.begin(), verts.end(),
					  3*num_verts, coords);
    if(result != MB_SUCCESS)
      {
	delete [] coords;
	return result;
      }
  
    // transform coordinates, if necessary
    result = transform_coords(dimension, num_verts, coords);
    if(result != MB_SUCCESS)
      {
	delete [] coords;
	CHKERR(result, "Trouble transforming vertex coordinates.");
      }
  
    // write the vertices
    CCMIOWriteVerticesd(&error, verticesID,
			CCMIOSIZEC(dimension), 1.0, mapID, coords,
			CCMIOINDEXC(kCCMIOStart), CCMIOINDEXC(kCCMIOEnd));
    CHKCCMERR(error, "CCMIOWriteVertices failed.");

    // clean up
    delete [] coords;

    return MB_SUCCESS;
  }

  ErrorCode WriteCCMIO::transform_coords(const int dimension, const int num_nodes, double *coords) 
  {
    Tag trans_tag;
    ErrorCode result = mbImpl->tag_get_handle( MESH_TRANSFORM_TAG_NAME, 16, MB_TYPE_DOUBLE, trans_tag);
    if( result == MB_TAG_NOT_FOUND ) return MB_SUCCESS;
    else if (MB_SUCCESS != result) return result;
    double trans_matrix[16]; 
    const EntityHandle mesh = 0;
    result = mbImpl->tag_get_data( trans_tag, &mesh, 1, trans_matrix ); 
    if (MB_SUCCESS != result) {
      mWriteIface->report_error("Couldn't get transform data.");
      return result;
    }
  
    double *tmp_coords = coords;
    for( int i=0; i<num_nodes; i++, tmp_coords += 1) {
      double vec1[3] = {0.0, 0.0, 0.0};
      for( int row=0; row<3; row++ ) {
	vec1[row] += ( trans_matrix[ (row*4)+0 ] * coords[0]);
	vec1[row] += ( trans_matrix[ (row*4)+1 ] * coords[num_nodes]);
	if (3 == dimension) vec1[row] += ( trans_matrix[ (row*4)+2 ] * coords[2*num_nodes]);
      }

      coords[0] = vec1[0];
      coords[num_nodes] = vec1[1];
      coords[2*num_nodes] = vec1[2];
    }

    return MB_SUCCESS;
  }

  ErrorCode WriteCCMIO::write_cells_and_faces(CCMIOID rootID,
					      std::vector<MaterialSetData> &matset_data,
					      std::vector<NeumannSetData> &neuset_data,
					      Range &,
					      CCMIOID &topologyID)
  {
    std::vector<int> connect;
    ErrorCode result;
    CCMIOID cellMapID, cells;
    CCMIOError error = kCCMIONoErr;
  
    // don't usually have anywhere near 31 nodes per element
    connect.reserve(31);
    Range::const_iterator rit;

    // create the topology node, and the cell and cell map nodes
    CCMIONewEntity(&error, rootID, kCCMIOTopology, "Topology", &topologyID);
    CHKCCMERR(error, "Trouble creating topology node.");

    CCMIONewEntity(&error, rootID, kCCMIOMap, "Cell map", &cellMapID);
    CHKCCMERR(error, "Failure creating Cell Map node.");

    CCMIONewEntity(&error, topologyID, kCCMIOCells, "Cells", &cells);
    CHKCCMERR(error, "Trouble creating Cell node under Topology node.");

    //================================================
    // loop over material sets, doing each one at a time
    //================================================
    Range all_elems;
    unsigned int i, num_elems = 0;
    int max_id = 1;
    std::vector<int> egids;
    int tot_elems = 0;
  
    for (unsigned int m = 0; m < matset_data.size(); m++)
      tot_elems += matset_data[m].elems.size();
    
    for (unsigned int m = 0; m < matset_data.size(); m++) {

      unsigned int this_num = matset_data[m].elems.size();

      //================================================
      // save all elements being output
      //================================================
      all_elems.merge(matset_data[m].elems);
  
      //================================================
      // Assign global ids for elements being written
      //================================================
      egids.resize(matset_data[m].elems.size());
      for (i = 0; i < this_num; i++) egids[i] = max_id++;
      result = mbImpl->tag_set_data(mGlobalIdTag, matset_data[m].elems, &egids[0]);
      CHKERR(result, "Failed to assign global ids for all elements being written.");

      //================================================
      // Write cell ids and material types for this matset; reuse egids for cell mat type
      //================================================
      CCMIOWriteMap(&error, cellMapID, CCMIOSIZEC(tot_elems),
		    CCMIOSIZEC(tot_elems), &egids[0],
		    CCMIOINDEXC(0 == m ? kCCMIOStart : num_elems), 
		    CCMIOINDEXC(matset_data.size() == m ? kCCMIOEnd : 
				num_elems + this_num));
      CHKCCMERR(error, "Trouble writing cell map.");

    if (-1 == matset_data[m].matsetId) 
      for (i = 0; i < this_num; i++) egids[i] = m;
    else
      for (i = 0; i < this_num; i++) egids[i] = matset_data[m].matsetId;

    CCMIOWriteCells(&error, cells, cellMapID, &egids[0],
                    CCMIOINDEXC(0 == m ? kCCMIOStart : num_elems), 
                    CCMIOINDEXC(matset_data.size() == m ? kCCMIOEnd : 
                              num_elems + this_num));
    CHKCCMERR(error, "Trouble writing Cell node.");

      //================================================
      // Write cell entity types
      //================================================
      const EntityHandle *conn;
      int num_conn;
      int has_mid_nodes[4];
      std::vector<EntityHandle> storage;
      for (i = 0, rit = matset_data[m].elems.begin(); i < this_num; i++, rit++) {
	result = mbImpl->get_connectivity(*rit, conn, num_conn, false, &storage);
	CHKERR(result, "Trouble getting connectivity for entity type check.");
	CN::HasMidNodes(mbImpl->type_from_handle(*rit), num_conn, has_mid_nodes);
	egids[i] = moab_to_ccmio_type(mbImpl->type_from_handle(*rit), has_mid_nodes);
      }
    
      CCMIOWriteOpt1i(&error, cells, "CellTopologyType", CCMIOSIZEC(tot_elems), &egids[0],
		      CCMIOINDEXC(0 == m ? kCCMIOStart : num_elems), 
		      CCMIOINDEXC(matset_data.size() == m ? kCCMIOEnd : 
				  num_elems + this_num));
      CHKCCMERR(error, "Failed to write cell topo types.");

      num_elems += this_num;
    }

    //================================================
    // get skin and neumann set faces
    //================================================
    Range neuset_facets, skin_facets;
    Skinner skinner(mbImpl);
    result = skinner.find_skin(all_elems, mDimension-1, skin_facets);
    CHKERR(result, "Failed to get skin facets.");

    // remove neumann set facets from skin facets, we have to output these
    // separately
    for (i = 0; i < neuset_data.size(); i++)
      neuset_facets.merge(neuset_data[i].elems);
    
    skin_facets -= neuset_facets;
    // make neuset_facets the union, and get ids for them
    neuset_facets.merge(skin_facets);
    result = mWriteIface->assign_ids(neuset_facets, mGlobalIdTag, 1);

    int fmaxid = neuset_facets.size();

    //================================================
    // write external faces
    //================================================
    for (i = 0; i < neuset_data.size(); i++) {
      Range::reverse_iterator rrit;
      unsigned char cmarks[2];
      Range ext_faces;
      std::vector<EntityHandle> mcells;
      // removing the faces connected to two regions
      for (rrit = neuset_data[i].elems.rbegin(); rrit != neuset_data[i].elems.rend(); ++rrit) {
	mcells.clear();
	result = mbImpl->get_adjacencies(&(*rrit), 1, mDimension, false, mcells);
	CHKERR(result, "Trouble getting bounding cells.");
      
	result = mbImpl->tag_get_data(mEntityMark, &mcells[0], mcells.size(), cmarks);
	CHKERR(result, "Trouble getting mark tags on cells bounding facets.");

	if( mcells.size() == 2 && (mWholeMesh || (cmarks[0] && cmarks[1]))) {
	}
	else{
	  // external face
	  ext_faces.insert(*rrit);
	}
      }
      if (ext_faces.size() != 0 && neuset_data[i].neusetId != 0)
	result = write_external_faces(rootID, topologyID, neuset_data[i].neusetId, 
				      ext_faces);
      CHKERR(result, "Trouble writing Neumann set facets.");
      ext_faces.clear ();
    }

    if (!skin_facets.empty()) {
      result = write_external_faces(rootID, topologyID, 0, skin_facets);
      CHKERR(result, "Trouble writing skin facets.");
    }

    //================================================
    // now inernal faces; loop over elements, do each face on the element
    //================================================
    // mark tag, for face marking on each non-polyhedral element

    if (num_elems > 1){ // no internal faces for just one element

      Tag fmark_tag;
      unsigned char mval = 0x0, omval;
      result = mbImpl->tag_get_handle("__fmark", 1, MB_TYPE_OPAQUE, 
				  fmark_tag, MB_TAG_DENSE|MB_TAG_CREAT, &mval);
      CHKERR(result, "Couldn't create mark tag.");

      std::vector<EntityHandle> tmp_face_cells, storage;
      std::vector<int> iface_connect, iface_cells;
      EntityHandle tmp_connect[CN::MAX_NODES_PER_ELEMENT]; // tmp connect vector
      const EntityHandle *connectc, *oconnectc; int num_connectc; // cell connectivity
      const EntityHandle *connectf; int num_connectf; // face connectivity

      for (i = 0, rit = all_elems.begin(); i < num_elems; i++, rit++) {
	EntityType etype = TYPE_FROM_HANDLE(*rit);

	//-----------------------
	// if not polyh, get mark
	//-----------------------
	if (MBPOLYHEDRON != etype && MBPOLYGON != etype) {
	  result = mbImpl->tag_get_data(fmark_tag, &(*rit), 1, &mval);
	  if (MB_SUCCESS != result) {
	    mWriteIface->report_error("Couldn't get mark data.");
	    return result;
	  }
	}

	//-----------------------
	// get cell connectivity, and whether it's a polyhedron
	//-----------------------
	result = mbImpl->get_connectivity(*rit, connectc, num_connectc, false, &storage);
	CHKERR(result, "Couldn't get entity connectivity.");

	// if polyh, write faces directly
	bool is_polyh = (MBPOLYHEDRON == etype);

	int num_facets = (is_polyh ? num_connectc : 
			  CN::NumSubEntities(etype, mDimension-1));

	//----------------------------------------------------------
	// loop over each facet of element, outputing it if not marked
	//----------------------------------------------------------
	for (int f = 0; f < num_facets; f++) {

	  //.............................................
	  // if this face marked, skip
	  //.............................................
	  if (!is_polyh && ((mval >> f) & 0x1)) continue;
    
	  //.................
	  // get face connect and adj cells
	  //.................
	  if (!is_polyh) {
	    // (from CN)
	    CN::SubEntityConn(connectc, etype, mDimension-1, f, tmp_connect, num_connectf);
	    connectf = tmp_connect;
	  }
	  else {
	    // directly
	    result = mbImpl->get_connectivity(connectc[f], connectf, num_connectf, false);
	    CHKERR(result, "Couldn't get polyhedron connectivity.");
	  }

	  //............................
	  // get adj cells from face connect (same for poly's and not, since both usually 
	  // go through vertices anyway)
	  //............................
	  tmp_face_cells.clear();
	  result = mbImpl->get_adjacencies(connectf, num_connectf, mDimension, false, tmp_face_cells);
	  CHKERR(result, "Error getting adj hexes.");

	  //...............................
	  // if this face only bounds one cell, skip, since we exported external faces
	  // before this loop
	  //...............................
	  if (tmp_face_cells.size() != 2) continue;
      
	  //.................
	  // switch cells so that *rit is always 1st (face connectivity is always written such
	  // that that one is with forward sense)
	  //.................
	  int side_num, sense, offset;
	  if (!is_polyh && tmp_face_cells[0] != *rit) {
	    EntityHandle tmph = tmp_face_cells[0]; 
	    tmp_face_cells[0] = tmp_face_cells[1]; 
	    tmp_face_cells[1] = tmph;
	  }
    
	  //.................
	  // save ids of cells
	  //.................
	  assert(tmp_face_cells[0] != tmp_face_cells[1]);
	  iface_cells.resize(iface_cells.size()+2);
	  result = mbImpl->tag_get_data(mGlobalIdTag, &tmp_face_cells[0], tmp_face_cells.size(),
					&iface_cells[iface_cells.size()-2]);
	  CHKERR(result, "Trouble getting global ids for bounded cells.");
	  iface_connect.push_back(num_connectf);

	  //.................
	  // save indices of face vertices
	  //.................
	  unsigned int tmp_size = iface_connect.size();
	  iface_connect.resize(tmp_size+num_connectf);
	  result = mbImpl->tag_get_data(mGlobalIdTag, connectf, num_connectf, 
					&iface_connect[tmp_size]);
	  CHKERR(result, "Trouble getting global id for internal face.");

	  //.................
	  // mark other cell with the right side #
	  //.................
	  if (!is_polyh) {
	    // mark other cell for this face, if there is another cell
        
	    result = mbImpl->get_connectivity(tmp_face_cells[1], oconnectc, num_connectc, 
					      false, &storage);
	    CHKERR(result, "Couldn't get other entity connectivity.");
      
	    // get side number in other cell
	    CN::SideNumber(TYPE_FROM_HANDLE(tmp_face_cells[1]), oconnectc, connectf, num_connectf,
			   mDimension-1, side_num, sense, offset);
	    // set mark for that face on the other cell
	    result = mbImpl->tag_get_data(fmark_tag, &tmp_face_cells[1], 1, &omval);
	    CHKERR(result, "Couldn't get mark data for other cell.");
	  }
        
	  omval |= (0x1 << (unsigned int)side_num);
	  result = mbImpl->tag_set_data(fmark_tag, &tmp_face_cells[1], 1, &omval);
	  CHKERR(result, "Couldn't set mark data for other cell.");

	} // loop over faces in elem
      } // loop over elems

      //================================================
      // write internal faces
      //================================================
    
      CCMIOID mapID;
      CCMIONewEntity(&error, rootID, kCCMIOMap, NULL, &mapID);
      CHKCCMERR(error, "Trouble creating Internal Face map node.");

      unsigned int num_ifaces = iface_cells.size()/2;

      // set gids for internal faces; reuse egids
      egids.resize(num_ifaces);
      for (i = 1; i <= num_ifaces; i++) egids[i-1] = fmaxid + i;
      CCMIOWriteMap(&error, mapID, CCMIOSIZEC(num_ifaces),
		    CCMIOSIZEC(fmaxid + num_ifaces),
		    &egids[0],
		    CCMIOINDEXC(kCCMIOStart), CCMIOINDEXC(kCCMIOEnd));
      CHKCCMERR(error, "Trouble writing Internal Face map node.");

      CCMIOID id;
      CCMIONewEntity(&error, topologyID, kCCMIOInternalFaces, "Internal faces", &id);
      CHKCCMERR(error, "Failed to create Internal face node under Topology node.");
      CCMIOWriteFaces(&error, id, kCCMIOInternalFaces, mapID,
		      CCMIOSIZEC(iface_connect.size()), &iface_connect[0],
		      CCMIOINDEXC(kCCMIOStart), CCMIOINDEXC(kCCMIOEnd));
      CHKCCMERR(error, "Failure writing Internal face connectivity.");
      CCMIOWriteFaceCells(&error, id, kCCMIOInternalFaces, mapID, &iface_cells[0],
			  CCMIOINDEXC(kCCMIOStart), CCMIOINDEXC(kCCMIOEnd));
      CHKCCMERR(error, "Failure writing Internal face cells.");
    }
    return MB_SUCCESS;
  }

  int WriteCCMIO::moab_to_ccmio_type(EntityType etype, int has_mid_nodes[]) 
  {
    int ctype = -1;
    if (has_mid_nodes[0] || has_mid_nodes[2] || has_mid_nodes[3]) return ctype;
  
    switch (etype) {
    case MBVERTEX:
      ctype = 1;
      break;
    case MBEDGE:
      if (!has_mid_nodes[1]) ctype = 2;
      else ctype = 28;
      break;
    case MBQUAD:
      if (has_mid_nodes[1]) ctype = 4;
      else ctype = 3;
      break;
    case MBTET:
      if (has_mid_nodes[1]) ctype = 23;
      else ctype = 13;
      break;
    case MBPRISM:
      if (has_mid_nodes[1]) ctype = 22;
      else ctype = 12;
      break;
    case MBPYRAMID:
      if (has_mid_nodes[1]) ctype = 24;
      else ctype = 14;
      break;
    case MBHEX:
      if (has_mid_nodes[1]) ctype = 21;
      else ctype = 11;
      break;
    case MBPOLYHEDRON:
      ctype = 255;
      break;
    default:
      break;
    }
  
    return ctype;
  }

  ErrorCode WriteCCMIO::write_external_faces(CCMIOID rootID, CCMIOID topologyID, 
					     int set_num, Range &facets) 
  {
    CCMIOError error = kCCMIONoErr;
    CCMIOID mapID, id;

    // get gids for these faces
    int *gids = NULL, minid, maxid;
    ErrorCode result = get_gids(facets, gids, minid, maxid);
    CHKERR(result, "Trouble getting global ids for facets.");
  
    // write the face id map
    CCMIONewEntity(&error, rootID, kCCMIOMap, NULL, &mapID);
    CHKCCMERR(error, "Problem creating face id map.");
  
    CCMIOWriteMap(&error, mapID, CCMIOSIZEC(facets.size()),
		  CCMIOSIZEC(maxid), gids,
		  CCMIOINDEXC(kCCMIOStart), CCMIOINDEXC(kCCMIOEnd));
    CHKCCMERR(error, "Problem writing face id map.");

    // get the connectivity of the faces; set size by how many verts in last facet
    const EntityHandle *connect;
    int num_connect;
    result = mbImpl->get_connectivity(*facets.rbegin(), connect, num_connect);
    CHKERR(result, "Failed to get connectivity of last facet.");
    std::vector<int> fconnect(facets.size() * (num_connect+1));

    result = mWriteIface->get_element_connect(facets.begin(), facets.end(),
					      num_connect, mGlobalIdTag, fconnect.size(),
					      &fconnect[0], true);
    CHKERR(result, "Failed to get facet connectivity.");
  
    // get and write a new external face entity
    CCMIONewIndexedEntity(&error, topologyID, kCCMIOBoundaryFaces, set_num,
			  "Boundary faces", &id);
    CHKCCMERR(error, "Problem creating boundary face entity.");

    CCMIOWriteFaces(&error, id, kCCMIOBoundaryFaces, mapID,
		    CCMIOSIZEC(fconnect.size()), &fconnect[0],
		    CCMIOINDEXC(kCCMIOStart), CCMIOINDEXC(kCCMIOEnd));
    CHKCCMERR(error, "Problem writing boundary faces.");

    // get info on bounding cells; reuse fconnect
    std::vector<EntityHandle> cells;
    unsigned char cmarks[2];
    int i, j = 0;
    Range dead_facets;
    Range::iterator rit;

    // about error checking in this loop: if any facets have no bounding cells,
    // this is an error, since global ids for facets are computed outside this loop
    for (rit = facets.begin(), i = 0; rit != facets.end(); rit++, i++) {
      cells.clear();
    
      // get cell then gid of cell
      result = mbImpl->get_adjacencies(&(*rit), 1, mDimension, false, cells);
      CHKERR(result, "Trouble getting bounding cells.");
      if (cells.empty()) {
	result = MB_FILE_WRITE_ERROR;
	CHKERR(result, "External facet with no output bounding cell.");
      }

      // check we don't bound more than one cell being output
      result = mbImpl->tag_get_data(mEntityMark, &cells[0], cells.size(), cmarks);
      CHKERR(result, "Trouble getting mark tags on cells bounding facets.");
      if (cells.size() == 2 && (mWholeMesh || (cmarks[0] && cmarks[1]))) {
	result = MB_FILE_WRITE_ERROR;
	CHKERR(result, "External facet with two output bounding cells.");
      }
      else if (1 == cells.size() && !mWholeMesh && !cmarks[0]) {
	result = MB_FILE_WRITE_ERROR;
	CHKERR(result, "External facet with no output bounding cells.");
      }
    
      // make sure 1st cell is the one being output
      if (2 == cells.size() && !(cmarks[0] | 0x0) && (cmarks[1] & 0x1))
	cells[0] = cells[1];

      // get gid for bounded cell
      result = mbImpl->tag_get_data(mGlobalIdTag, &cells[0], 1, &fconnect[j]);
      CHKERR(result, "Couldn't get global id tag for bounded cell.");
    
      j++;
    }
  
    // write the bounding cell data
    CCMIOWriteFaceCells(&error, id, kCCMIOBoundaryFaces, mapID, &fconnect[0],
			CCMIOINDEXC(kCCMIOStart), CCMIOINDEXC(kCCMIOEnd));
    CHKCCMERR(error, "Problem writing boundary cell data.");

    return MB_SUCCESS;
  }

  ErrorCode WriteCCMIO::get_neuset_elems(EntityHandle neuset, int current_sense,
					 Range &forward_elems, Range &reverse_elems) 
  {
    Range neuset_elems, neuset_meshsets;

    // get the sense tag; don't need to check return, might be an error if the tag
    // hasn't been created yet
    Tag sense_tag = 0;
    mbImpl->tag_get_handle("SENSE", 1, MB_TYPE_INTEGER, sense_tag);

    // get the entities in this set, non-recursive
    ErrorCode result = mbImpl->get_entities_by_handle(neuset, neuset_elems);
    if (MB_FAILURE == result) return result;
  
    // now remove the meshsets into the neuset_meshsets; first find the first meshset,
    Range::iterator range_iter = neuset_elems.begin();
    while (TYPE_FROM_HANDLE(*range_iter) != MBENTITYSET && range_iter != neuset_elems.end())
      range_iter++;
  
    // then, if there are some, copy them into neuset_meshsets and erase from neuset_elems
    if (range_iter != neuset_elems.end()) {
      std::copy(range_iter, neuset_elems.end(), range_inserter(neuset_meshsets));
      neuset_elems.erase(range_iter, neuset_elems.end());
    }
  

    // ok, for the elements, check the sense of this set and copy into the right range
    // (if the sense is 0, copy into both ranges)

    // need to step forward on list until we reach the right dimension
    Range::iterator dum_it = neuset_elems.end();
    dum_it--;
    int target_dim = CN::Dimension(TYPE_FROM_HANDLE(*dum_it));
    dum_it = neuset_elems.begin();
    while (target_dim != CN::Dimension(TYPE_FROM_HANDLE(*dum_it)) &&
	   dum_it != neuset_elems.end()) 
      dum_it++;

    if (current_sense == 1 || current_sense == 0)
      std::copy(dum_it, neuset_elems.end(), range_inserter(forward_elems));
    if (current_sense == -1 || current_sense == 0)
      std::copy(dum_it, neuset_elems.end(), range_inserter(reverse_elems));
  
    // now loop over the contained meshsets, getting the sense of those and calling this
    // function recursively
    for (range_iter = neuset_meshsets.begin(); range_iter != neuset_meshsets.end(); range_iter++) {

      // first get the sense; if it's not there, by convention it's forward
      int this_sense;
      if (0 == sense_tag ||
	  MB_FAILURE == mbImpl->tag_get_data(sense_tag, &(*range_iter), 1, &this_sense))
	this_sense = 1;
  
      // now get all the entities on this meshset, with the proper (possibly reversed) sense
      get_neuset_elems(*range_iter, this_sense*current_sense,
		       forward_elems, reverse_elems);
    }
  
    return result;
  }


} // namespace moab

  
