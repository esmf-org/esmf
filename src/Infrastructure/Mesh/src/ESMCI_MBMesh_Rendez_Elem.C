// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================

// Take out if MOAB isn't being used
#if defined ESMF_MOAB

#include <Mesh/include/Legacy/ESMCI_Exception.h>
#include <Mesh/include/ESMCI_MBMesh_Util.h>
#include <Mesh/include/ESMCI_MBMesh.h>
#include <Mesh/src/Zoltan/zoltan.h>
#include <Mesh/include/Legacy/ESMCI_ParEnv.h>
#include <Mesh/include/ESMCI_MBMesh_BBox.h>
#include <Mesh/include/ESMCI_MBMesh_Redist.h>

#include <limits>
#include <iostream>
#include <iterator>
#include <iomanip>
#include <cmath>
#include <vector>

#include <ESMCI_VM.h>
#include "ESMCI_LogErr.h"

#include "MBTagConventions.hpp"

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
 static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

using namespace ESMCI;

double geom_tol=1.0E-6;

struct ZData {
  // Could maybe store this more efficiently using a range. However,
  // since we are comparing with Mesh and that does it this way, and could
  // also do it more efficiently by using iterators, do it this way for now.
  std::vector<EntityHandle> dst_elems;
  std::vector<EntityHandle> src_elems;

  MBMesh *srcmesh;
  MBMesh *dstmesh;

  int sdim;
};

static int GetNumAssignedObj(void *user, int *err) {
  ZData &udata = *(static_cast<ZData *>(user));
  *err = 0;

  int num=0;
  num += udata.dst_elems.size();
  num += udata.src_elems.size();

  return num;
}

static void GetObjList(void *user, int numGlobalIds, int numLids, ZOLTAN_ID_PTR gids, ZOLTAN_ID_PTR lids,
          int wgt_dim, float *obj_wghts, int *err)
{
  ZData &udata = *(static_cast<ZData *>(user));
  std::vector<EntityHandle>::iterator ni,ne;
  UInt i = 0;

  // Put src into lists
  ni = udata.src_elems.begin();
  ne = udata.src_elems.end();
  for (; ni != ne; ++ni) {
    int elem_gid;
    MBMesh_get_gid(udata.srcmesh, *ni, &elem_gid);
    gids[2*i] = 0;  // 0 -> src elems
    gids[2*i + 1] = elem_gid;
    lids[i] = i;
    i++;
  }

  // Put dst into lists
  ni = udata.dst_elems.begin();
  ne = udata.dst_elems.end();
  for (; ni != ne; ++ni) {
    int elem_gid;
    MBMesh_get_gid(udata.dstmesh, *ni, &elem_gid);
    gids[2*i] = 1;  // 1 -> src elems
    gids[2*i + 1] = elem_gid;
    lids[i] = i;
    i++;
  }

  // Return success
  *err = 0;
}

static int GetNumGeom(void *user, int *err) {
  ZData &udata = *(static_cast<ZData*>(user));
  *err = 0;

  return udata.sdim;
}


static void GetObject(void *user, int numGlobalIds, int numLids, int numObjs,
  ZOLTAN_ID_PTR gids, ZOLTAN_ID_PTR lids, int numDim, double *pts, int *err)
{
  ZData &udata = *(static_cast<ZData*>(user));

  // Get size of source list
  UInt list1_size;
  list1_size = udata.src_elems.size();

  // Loop through
  for (UInt i = 0; i < (UInt) numObjs; i++) {
    UInt src_or_dst = gids[2*i]; // 0 = src, 1 = dst
    UInt idx = lids[i];

    std::vector<double> ndata(numDim);
    const double *c;

    // Get centroid depending on source or destination
    if (src_or_dst == 0) {
        EntityHandle src_eh = udata.src_elems[idx];
        MBMesh_get_elem_centroid(udata.srcmesh, src_eh, &ndata[0]);
        c = &ndata[0];
    } else if (src_or_dst == 1) {
      UInt sidx = idx - list1_size; // local indices start from first list, continue into second
      EntityHandle dst_eh= udata.dst_elems[sidx];
      MBMesh_get_elem_centroid(udata.dstmesh, dst_eh, &ndata[0]);
      c = &ndata[0];
    } else throw("GetObject, unknown mesh from global id");

    // Copy coords into list
    for (UInt d = 0; d < (UInt) numDim; d++) pts[i*numDim + d] = c[d];
  }

  // Return success
  *err = 0;
}

static void assign_elems_to_procs(MBMesh *mesh, std::vector<EntityHandle> *elems, double geom_tol, UInt sdim, Zoltan_Struct *zz, std::vector<EH_Comm_Pair> *elem_to_proc_list) {
  Trace __trace("rcb_isect(Zoltan_Struct *zz, MEField<> &coord, std::vector<MeshObj*> &objlist, std::vector<CommRel::CommNode> &res)");
#undef  ESMC_METHOD
#define ESMC_METHOD "assign_elems_to_procs()"

  // Get number of Pets
  int localrc;
  int petCount = VM::getCurrent(&localrc)->getPetCount();
  if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
    throw localrc;  // bail out with exception

  // Allocate list of procs where each elem should be assigned
  int numprocs;
  std::vector<int> procs(petCount);

  // Loop through objects assigning them to procs
  std::vector<EntityHandle>::iterator si = elems->begin(), se = elems->end();
  for (; si != se; ++si) {
    EntityHandle &elem = *si;
    int elem_id;
    MBMesh_get_gid(mesh, elem, &elem_id);

    // Get bounding box for elem
    MBMesh_BBox ebox(mesh, elem, geom_tol);

    // Assingn elems to procs using zoltan struct
    Zoltan_LB_Box_Assign(zz, ebox.getMin()[0]-geom_tol,
                             ebox.getMin()[1]-geom_tol,
                             (sdim > 2 ? ebox.getMin()[2] : 0) - geom_tol,
                             ebox.getMax()[0]+geom_tol,
                             ebox.getMax()[1]+geom_tol,
                             (sdim > 2 ? ebox.getMax()[2] : 0) + geom_tol,
                             &procs[0],
                             &numprocs);

    // Add to pattern
    for (int i = 0; i < numprocs; i++) {
      EH_Comm_Pair ecp(elem, elem_id, procs[i]);

      std::vector<EH_Comm_Pair>::iterator lb =
        std::lower_bound(elem_to_proc_list->begin(), elem_to_proc_list->end(), ecp);

      // Add if not already there
      if (lb == elem_to_proc_list->end() || *lb != ecp)
        elem_to_proc_list->insert(lb, ecp);
    } // for nproc
  } // for si
}

// Get a vector of elems in a mesh
void get_vector_of_elems(MBMesh *mesh, std::vector<EntityHandle> *elems) {
#undef  ESMC_METHOD
#define ESMC_METHOD "get_vector_of_elems()"
  int merr, localrc;

  // Get MOAB meshe
  Interface *mesh_moab=mesh->mesh;

  // Get a range containing all elements
  Range all_elem;
  merr=mesh_moab->get_entities_by_dimension(0,mesh->pdim,all_elem);
  if (merr != MB_SUCCESS) {
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
       moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
   }

  // Put into vector
  for(Range::iterator it=all_elem.begin(); it !=all_elem.end(); it++) {
    const EntityHandle eh=*it;

    elems->push_back(eh);
  }
}

// Get a vector of elems in a mesh that intersect with a bounding box
void get_vector_of_elems_that_overlap_box(MBMesh *mesh, MBMesh_BBox &bbox, std::vector<EntityHandle> *elems) {
#undef  ESMC_METHOD
#define ESMC_METHOD "get_vector_of_elems_that_overlap_box()"
  int merr, localrc;

  // Get MOAB meshe
  Interface *mesh_moab=mesh->mesh;

  // Get a range containing all elements
  Range all_elem;
  merr=mesh_moab->get_entities_by_dimension(0,mesh->pdim,all_elem);
  if (merr != MB_SUCCESS) {
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
       moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
   }

  // Put elems that overlap into box
  for(Range::iterator it=all_elem.begin(); it !=all_elem.end(); it++) {
    const EntityHandle eh=*it;

    // Get bbox of element
    MBMesh_BBox elem_bbox(mesh, eh, 0.25);

    if (MBMesh_BBoxIntersect(elem_bbox, bbox, geom_tol)) {
      elems->push_back(eh);
    }
  }
}



void calc_rendez_comm_pattern(MBMesh *srcmesh, MBMesh *dstmesh,
                              std::vector<EH_Comm_Pair> *src_elem_to_proc_list, std::vector<EH_Comm_Pair> *dst_elem_to_proc_list) {
#undef  ESMC_METHOD
#define ESMC_METHOD "calc_rendez_comm_pattern()"
  int rc, localrc;

  // Get mpi_comm for this VM
  MPI_Comm mpi_comm = VM::getCurrent(&localrc)->getMpi_c();
  if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
    throw localrc;  // bail out with exception


  // Init Zoltan
  float ver;
  rc = Zoltan_Initialize(0, NULL, &ver);

  // Create Zoltan struct
  struct Zoltan_Struct * zz = Zoltan_Create(mpi_comm);

  // Set Zoltan parameters
  Zoltan_Set_Param(zz, "DEBUG_LEVEL", "0");
  Zoltan_Set_Param(zz, "LB_METHOD", "RCB");
  Zoltan_Set_Param(zz, "NUM_GID_ENTRIES", "2"); // Two integers for an id
  Zoltan_Set_Param(zz, "NUM_LID_ENTRIES", "1");
  Zoltan_Set_Param(zz, "RETURN_LISTS", "ALL");
  //Zoltan_Set_Param(zz, "RCB_RECTILINEAR_BLOCKS", "1");
  Zoltan_Set_Param(zz, "AVERAGE_CUTS", "1");

  // RCB
  //Zoltan_Set_Param(zz, "RCB_LOCK_DIRECTIONS", "1");
  Zoltan_Set_Param(zz, "KEEP_CUTS", "1");
  Zoltan_Set_Param(zz, "RCB_OUTPUT_LEVEL", "0");


  // Set user data for zoltan call backs
  ZData zd;

  // Set sptial dimension
  zd.sdim=srcmesh->sdim;

  // Set meshes in struct
  zd.srcmesh=srcmesh;
  zd.dstmesh=dstmesh;

  //// Set list of destination elements
  get_vector_of_elems(dstmesh, &(zd.dst_elems));

  //// Get global bounding box of dst mesh
  MBMesh_BBox global_dst_bbox =MBMesh_BBoxParUnion(MBMesh_BBox(dstmesh));

  //// Set list of source elements (only those that overlap BBox of dstmesh)
  get_vector_of_elems_that_overlap_box(srcmesh, global_dst_bbox, &(zd.src_elems));

#if 0
  printf("%d# src_elems.size=%d\n",Par::Rank(),zd.src_elems.size());
  printf("%d# dst_elems.size=%d\n",Par::Rank(),zd.dst_elems.size());
  printf("%d# global_dst_bbox min=[%f,%f] max=[%f,%f]\n",Par::Rank(),
         global_dst_bbox.getMin()[0],global_dst_bbox.getMin()[1],
         global_dst_bbox.getMax()[0],global_dst_bbox.getMax()[1]);
#endif

  // Set the mesh description callbacks
  Zoltan_Set_Num_Obj_Fn(zz, GetNumAssignedObj, (void*) &zd);
  Zoltan_Set_Obj_List_Fn(zz, GetObjList, (void*) &zd);
  Zoltan_Set_Num_Geom_Fn(zz, GetNumGeom, (void*) &zd);
  Zoltan_Set_Geom_Multi_Fn(zz, GetObject, (void*) &zd);

  // Local vars needed by zoltan
  int changes;
  int numGidEntries;
  int numLidEntries;
  int numImport;
  ZOLTAN_ID_PTR importGlobalids;
  ZOLTAN_ID_PTR importLocalids;
  int *importProcs;
  int *importToPart;
  int numExport;
  ZOLTAN_ID_PTR exportGlobalids;
  ZOLTAN_ID_PTR exportLocalids;
  int *exportProcs;
  int *exportToPart;

  // Call zoltan
  rc = Zoltan_LB_Partition(zz, &changes, &numGidEntries, &numLidEntries,
    &numImport, &importGlobalids, &importLocalids, &importProcs, &importToPart,
    &numExport, &exportGlobalids, &exportLocalids, &exportProcs, &exportToPart);

  // Calc. where source elems are to go
  assign_elems_to_procs(srcmesh, &(zd.src_elems), geom_tol, srcmesh->sdim, zz, src_elem_to_proc_list);

  // Calc. where dest elems are to go
  assign_elems_to_procs(dstmesh, &(zd.dst_elems), geom_tol, dstmesh->sdim, zz, dst_elem_to_proc_list);

  // Free Zoltan struct
  Zoltan_Destroy(&zz);
}


// This creates src and dst rendezvous meshes where the overlap is based on elements
void create_rendez_mbmesh_elem(MBMesh *srcmesh, MBMesh *dstmesh, MBMesh **_srcmesh_rendez, MBMesh **_dstmesh_rendez) {
#undef  ESMC_METHOD
#define ESMC_METHOD "get_vector_of_elems_that_overlap_box()"

  // Get Parallel Information
   int localrc;
    int petCount = VM::getCurrent(&localrc)->getPetCount();
  if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
    throw localrc;  // bail out with exception

  // Compute communication pattern to build rendezvous meshes
  std::vector<EH_Comm_Pair> src_elem_to_proc_list;
  std::vector<EH_Comm_Pair> dst_elem_to_proc_list;
  calc_rendez_comm_pattern(srcmesh, dstmesh, &src_elem_to_proc_list, &dst_elem_to_proc_list);

#if 0
  // Debug print of src list
  for (int i=0; i<src_elem_to_proc_list.size(); i++) {
    EntityHandle eh=src_elem_to_proc_list[i].eh;
    int proc=src_elem_to_proc_list[i].proc;

    int gid=-55;
    MBMesh_get_gid(srcmesh, eh, &gid);

    printf("%d# src gid=%d to proc=%d \n",Par::Rank(),gid,proc);
  }

  printf("%d# ======== \n", Par::Rank());

  // Debug print of dst list
  for (int i=0; i<dst_elem_to_proc_list.size(); i++) {
    EntityHandle eh=dst_elem_to_proc_list[i].eh;
    int proc=dst_elem_to_proc_list[i].proc;

    int gid=-77;
    MBMesh_get_gid(dstmesh, eh, &gid);

    printf("%d# dst gid=%d to proc=%d \n",Par::Rank(),gid,proc);
  }
#endif

  // Create src rend mesh
  create_mbmesh_redist_elem(srcmesh, &src_elem_to_proc_list, _srcmesh_rendez);

  // Create dst rend mesh
  create_mbmesh_redist_elem(dstmesh, &dst_elem_to_proc_list, _dstmesh_rendez);
}






  // OLD MOAB STUFF SAVED HERE FOR CONVENIENCE
#if 0
  // MOAB error
   int merr;

  // Get MOAB meshes
  Interface *srcmesh_moab=srcmesh->mesh;

  // Need to copy src Mesh here, maybe?

  // migrate src Mesh
  // COULD DO THIS WHEN MESH IS CREATED, BUT WHAT IF DOING COUPLING
  // BETWEEN DIFFERENT VMs??
  ParallelComm *pcomm= new ParallelComm(srcmesh_moab, mpi_comm);


  pcomm->resolve_shared_ents(0, 2, 0);


 // Get a range containing all vertices
  Range src_orig_vert;
   merr=srcmesh_moab->get_entities_by_dimension(0,0,src_orig_vert);
  if (merr != MB_SUCCESS) {
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
       moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
  }

  // Print out data premigrate
  for(Range::iterator it=src_orig_vert.begin(); it !=src_orig_vert.end(); it++) {
    const EntityHandle *vertp=(&*it);


    // Get gid
    int gid;
    merr=srcmesh_moab->tag_get_data(srcmesh->gid_tag, vertp, 1, &gid);
    if (merr != MB_SUCCESS) {
       if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
           moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
    }

    int moab_gid;
    merr=srcmesh_moab->tag_get_data(srcmesh->moab_gid_tag, vertp, 1, &moab_gid);
    if (merr != MB_SUCCESS) {
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
           moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
    }
  }


  // Get a range containing all elements
  Range src_orig_elem;
   merr=srcmesh_moab->get_entities_by_dimension(0,srcmesh->pdim,src_orig_elem);
  if (merr != MB_SUCCESS) {
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
       moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
  }

  // Setup global id tag
  Tag src_to_proc_tag;
  int def_val=0;
    merr=srcmesh_moab->tag_get_handle("TO_PROC_ID", 1, MB_TYPE_INTEGER, src_to_proc_tag,MB_TAG_EXCL|MB_TAG_DENSE, &def_val);
  if (merr != MB_SUCCESS) {
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                     moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
   }

  // Print out data premigrate
  for(Range::iterator it=src_orig_elem.begin(); it !=src_orig_elem.end(); it++) {
    const EntityHandle *elemp=(&*it);

    // Set destination
    int dest;
    // THIS ONE WORKS!
    dest=petCount-localPet-1;

    // THIS ONE DOES NOT WORK
    // dest=1;
    merr=srcmesh_moab->tag_set_data(src_to_proc_tag, elemp, 1, &dest);
    if (merr != MB_SUCCESS) {
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
           moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
    }


    // Get gid
    int gid;
    merr=srcmesh_moab->tag_get_data(srcmesh->gid_tag, elemp, 1, &gid);
    if (merr != MB_SUCCESS) {
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
           moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
    }

    int moab_gid;
    merr=srcmesh_moab->tag_get_data(srcmesh->moab_gid_tag, elemp, 1, &moab_gid);
    if (merr != MB_SUCCESS) {
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
           moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
    }


    // Get to proc
    int to_proc;
    merr=srcmesh_moab->tag_get_data(src_to_proc_tag, elemp, 1, &to_proc);
    if (merr != MB_SUCCESS) {
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
            moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
    }
  }


   std::vector<EntityHandle> src_migrated_elem;
  merr=pcomm->migrate_entities(src_orig_elem, src_to_proc_tag, src_migrated_elem, true);
  if (merr != MB_SUCCESS) {
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
            moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
    }

#if 0
  // Print out migrated element ids
  for (int i=0; i<src_migrated_elem.size(); i++) {
    EntityHandle eh=src_migrated_elem[i];

    // Get gid
    int gid;
    merr=srcmesh_moab->tag_get_data(srcmesh->gid_tag, &eh, 1, &gid);
    if (merr != MB_SUCCESS) {
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
           moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
    }

    int moab_gid;
    merr=srcmesh_moab->tag_get_data(srcmesh->moab_gid_tag, &eh, 1, &moab_gid);
    if (merr != MB_SUCCESS) {
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
           moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
    }

    int dim=srcmesh_moab->dimension_from_handle(eh);

    // Print out
    printf("%d# src_migrated_elem gid=%d moab_gid=%d dim=%d\n",localPet,gid,moab_gid,dim);

  }
#endif


  // Get a range containing all elements
  Range src_new_elem;
  merr=srcmesh_moab->get_entities_by_dimension(0,srcmesh->pdim,src_new_elem);
  if (merr != MB_SUCCESS) {
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
       moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
   }


  // Print out data after migrate
  for(Range::iterator it=src_new_elem.begin(); it !=src_new_elem.end(); it++) {
    const EntityHandle *elemp=(&*it);

    // Get gid
    int gid;
    merr=srcmesh_moab->tag_get_data(srcmesh->gid_tag, elemp, 1, &gid);
    if (merr != MB_SUCCESS) {
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
           moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
    }

    int moab_gid;
    merr=srcmesh_moab->tag_get_data(srcmesh->moab_gid_tag, elemp, 1, &moab_gid);
     if (merr != MB_SUCCESS) {
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
           moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
    }

     int num_nodes;
     double coords[10*3];

     MBMesh_get_elem_coords(srcmesh, *elemp, 10, &num_nodes, coords);

    printf("%d# AFTER migrate elem gid=%d moab_gid=%d coords=",localPet,gid,moab_gid);
    for(int i=0; i<num_nodes; i++) {
      printf(" [%f %f]",coords[2*i],coords[2*i+1]);
    }
    printf("\n");
  }
#endif

#endif // ESMF_MOAB

