// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2014, University Corporation for Atmospheric Research, 
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
#include <Mesh/include/ESMCI_MBMesh_Redist.h>
#include <Mesh/include/ESMCI_MBMesh.h>
#include <Mesh/include/Legacy/ESMCI_ParEnv.h>
#include <Mesh/include/Legacy/ESMCI_SparseMsg.h>


#include <limits>
#include <iostream>
#include <iterator>
#include <iomanip>
#include <cmath>
#include <vector>
#include <set>
#include <map>

#include <ESMCI_VM.h>
#include "ESMCI_LogErr.h"

#include "MBTagConventions.hpp"

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
 static const char *const version = "$Id$";
//-----------------------------------------------------------------------------
          
namespace ESMCI {


  void create_mbmesh_copy_metadata(MBMesh *src_mesh, 
                                 MBMesh **_out_mesh);

  void create_mbmesh_redist_elem_move_verts(MBMesh *src_mesh, 
                                            std::vector<EH_Comm_Pair> *elem_to_proc_list, 
                                            std::map<int,EntityHandle> *out_gid_to_eh, 
                                            MBMesh *out_mesh);

  void create_mbmesh_redist_elem_move_elems(MBMesh *src_mesh, 
                                            std::vector<EH_Comm_Pair> *elem_to_proc_list, 
                                            std::map<int,EntityHandle> *out_gid_to_vert, 
                                            MBMesh *out_mesh);
  void create_pointlist_redist_move_points(PointList *src_pl,
                                          std::vector<PL_Comm_Pair> *point_to_proc_list,
                                          PointList **out_pl);

// This creates a copy of a mesh redisted according to the elem distribution described in elem_to_proc_list
// NOTE: this func. currently does not set vert owners or orig_ind, because they are not necessary for 
// rend. meshes. Evntually add this capability (with a flag to toggle it on and off).
void create_mbmesh_redist_elem(MBMesh *src_mesh, 
                               std::vector<EH_Comm_Pair> *elem_to_proc_list, 
                               MBMesh **_out_mesh) {
#undef  ESMC_METHOD
#define ESMC_METHOD "create_mbmesh_redist_elem()"

  // Get Parallel Information
      int localrc;
  int num_proc = VM::getCurrent(&localrc)->getPetCount();
  if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
    throw localrc;  // bail out with exception

  int localPet = VM::getCurrent(&localrc)->getLocalPet();
  if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
    throw localrc;  // bail out with exception

#if 0
  // Debug print of elem to proc list
  for (int i=0; i<elem_to_proc_list->size(); i++) {
    EntityHandle eh=(*elem_to_proc_list)[i].eh;
    int proc=(*elem_to_proc_list)[i].proc;

    int gid=-55;
    MBMesh_get_gid(src_mesh, eh, &gid);

    printf("%d# elem gid=%d to proc=%d \n",localPet,gid,proc);
  }

  printf("%d# ======\n",localPet);
#endif

  // First func that creates copy of Mesh with same metadata
  MBMesh *out_mesh;
  create_mbmesh_copy_metadata(src_mesh, &out_mesh);

  // Redist verts to new mesh
  std::map<int,EntityHandle> out_gid_to_vert;
  create_mbmesh_redist_elem_move_verts(src_mesh, elem_to_proc_list, &out_gid_to_vert, out_mesh);

  // Redist elems to new mesh
  create_mbmesh_redist_elem_move_elems(src_mesh, elem_to_proc_list, &out_gid_to_vert, out_mesh);

  // Output new mesh
  *_out_mesh=out_mesh;
}

void create_pointlist_redist_point(PointList *src_pl,
                                   std::vector<PL_Comm_Pair> *point_to_proc_list,
                                   PointList **_out_pl) {
#undef  ESMC_METHOD
#define ESMC_METHOD "create_mbmesh_redist_point()"

  // Get Parallel Information
      int localrc;
  int num_proc = VM::getCurrent(&localrc)->getPetCount();
  if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
    throw localrc;  // bail out with exception

  int localPet = VM::getCurrent(&localrc)->getLocalPet();
  if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
    throw localrc;  // bail out with exception

  // Redist points to new pointlist
  create_pointlist_redist_move_points(src_pl, point_to_proc_list, _out_pl);

}



//// TODO: Eventually move these to someplace global, so they can be used everywhere
//// that way as things change they only need to be updated in one place


// Create a new mbmesh that is a copy of src_mesh's metadata, 
// but not verts, elems, etc.
void create_mbmesh_copy_metadata(MBMesh *src_mesh, 
                                 MBMesh **_out_mesh) {

  int merr;
  
  // New Mesh    
  MBMesh *out_mesh = new MBMesh();
  
  // Create MOAB Mesh
  Interface *moab_mesh=new Core();
  
  // Set Moab Mesh
  out_mesh->mesh=moab_mesh;
  
  // Set dimensions
  out_mesh->pdim=src_mesh->pdim;
  out_mesh->sdim=src_mesh->sdim;
  
  
  // Add tags
  // TODO: eventually do this in one func shared with other creates, so only
    //       needs to be updated in one place
  // Default value
  int def_val = 0;
  
   // Setup global id tag
  def_val=0;
  merr=moab_mesh->tag_get_handle(GLOBAL_ID_TAG_NAME, 1, MB_TYPE_INTEGER, out_mesh->gid_tag, MB_TAG_DENSE, &def_val);
  if (merr != MB_SUCCESS) {
    int localrc;
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                     moab::ErrorCodeStr[merr], ESMC_CONTEXT, &localrc)) throw localrc;
  }     
  
  // Setup orig_pos tag
  def_val=-1;
  merr=moab_mesh->tag_get_handle("orig_pos", 1, MB_TYPE_INTEGER, out_mesh->orig_pos_tag, MB_TAG_EXCL|MB_TAG_DENSE, &def_val);
  if (merr != MB_SUCCESS) {
    int localrc;
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                     moab::ErrorCodeStr[merr], ESMC_CONTEXT, &localrc)) throw localrc;
  }     
  
  // Setup owner tag
  def_val=-1;
  merr=moab_mesh->tag_get_handle("owner", 1, MB_TYPE_INTEGER, out_mesh->owner_tag, MB_TAG_EXCL|MB_TAG_DENSE, &def_val);
  if (merr != MB_SUCCESS) {
    int localrc;
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                     moab::ErrorCodeStr[merr], ESMC_CONTEXT, &localrc)) throw localrc;
  }     

  // Setup masking tags
  if (src_mesh->has_elem_mask) {
    def_val=0;
    merr=moab_mesh->tag_get_handle("elem_mask", 1, MB_TYPE_INTEGER, out_mesh->elem_mask_tag, MB_TAG_EXCL|MB_TAG_DENSE, &def_val);
    if (merr != MB_SUCCESS) {
      int localrc;
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                       moab::ErrorCodeStr[merr], ESMC_CONTEXT, &localrc)) throw localrc;
    }     

    def_val=0;
    merr=moab_mesh->tag_get_handle("elem_mask_val", 1, MB_TYPE_INTEGER, out_mesh->elem_mask_val_tag, MB_TAG_EXCL|MB_TAG_DENSE, &def_val);
    if (merr != MB_SUCCESS) {
    int localrc;
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                     moab::ErrorCodeStr[merr], ESMC_CONTEXT, &localrc)) throw localrc;
    }
  }
  out_mesh->has_elem_mask=src_mesh->has_elem_mask;

  // Setup masking tags
  if (src_mesh->has_node_mask) {
    def_val=0;
    merr=moab_mesh->tag_get_handle("node_mask", 1, MB_TYPE_INTEGER, out_mesh->node_mask_tag, MB_TAG_EXCL|MB_TAG_DENSE, &def_val);
    if (merr != MB_SUCCESS) {
      int localrc;
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                       moab::ErrorCodeStr[merr], ESMC_CONTEXT, &localrc)) throw localrc;
    }     

    def_val=0;
    merr=moab_mesh->tag_get_handle("node_mask_val", 1, MB_TYPE_INTEGER, out_mesh->node_mask_val_tag, MB_TAG_EXCL|MB_TAG_DENSE, &def_val);
    if (merr != MB_SUCCESS) {
    int localrc;
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                     moab::ErrorCodeStr[merr], ESMC_CONTEXT, &localrc)) throw localrc;
    }
  }
  out_mesh->has_node_mask=src_mesh->has_node_mask;

  // Do output
  *_out_mesh=out_mesh;
}


  int calc_size_vert_comm(MBMesh *src_mesh) {

    // init size
    int size=0;
    
    // Gid
    size += sizeof(int);

    //// These will make no sense on a new proc, so don't send them
    //  // orig_pos
    //  size += sizeof(int);
     //
    //  // owner
    //  size += sizeof(int);

    // coords
    size += sizeof(double)*src_mesh->sdim;

    // ADD OTHER THINGS HERE AS ADDED TO VERT

    // output size
    return size;
  }


  void pack_vert_comm(MBMesh *src_mesh, EntityHandle vert, char *buff) {
#define MAX_VERT_COMM_SIZE 100

    // Offset
    int off=0;

    // Pack gid
    int gid;
    MBMesh_get_gid(src_mesh, vert, &gid);

    *((int *)(buff+off))=gid;
    off +=sizeof(int);
    

    ///// Don't send orig_pos and owner, since they will make no sense on
    ///// a new processor

    // Pack coords
    double c[3];
    int merr=src_mesh->mesh->get_coords(&vert,1,c);
    if (merr != MB_SUCCESS) {
      Throw() <<"MOAB ERROR: "<<moab::ErrorCodeStr[merr];
     }

    // Load coords
    int sdim=src_mesh->sdim;
    for (int i=0; i<sdim; i++) {
      *((double *)(buff+off))=c[i];
      off +=sizeof(double);
    }
  }

  void unpack_gid_vert_comm(MBMesh *out_mesh, char *buff, int *gid) {

    // Unpack gid
    *gid=*((int *)(buff));
  }

  void unpack_vert_comm(MBMesh *out_mesh, char *buff,  EntityHandle *_new_vert) {
    int merr,localrc;

    // Offset
    int off=0;

    // Unpack gid
    int gid;
    gid=*((int *)(buff+off));
    off +=sizeof(int);
    

    ///// Don't recv orig_pos and owner, since they will make no sense on
    ///// a new processor

    // Unpack coords
     double coords[3]={0.0,0.0,0.0};
    int sdim=out_mesh->sdim;
    for (int i=0; i<sdim; i++) {
      coords[i]=*((double *)(buff+off));
      off +=sizeof(double);
    }

    //    printf("RECV: gid=%d coords=[%f %f]\n",gid,c[0],c[1]);

    // Create new vertex
    EntityHandle new_vert;
    merr=out_mesh->mesh->create_vertex(coords, new_vert);
    if (merr != MB_SUCCESS) {
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                       moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
    }     

    // Set gid
    merr=out_mesh->mesh->tag_set_data(out_mesh->gid_tag, &new_vert, 1, &gid);
    if (merr != MB_SUCCESS) {
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
         moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
    }     

    // Output vertex
    *_new_vert=new_vert;
  }


  // Redist verts to new mesh
  //  - Loop figuring out which verts go to where, use set to unique to each proc, pack and send
  //  - on other side unpack and create map of gid to vert, use this when unpacking and creating elems
  void create_mbmesh_redist_elem_move_verts(MBMesh *src_mesh, 
                                            std::vector<EH_Comm_Pair> *elem_to_proc_list, 
                                            std::map<int,EntityHandle> *out_gid_to_vert, 
                                            MBMesh *out_mesh) {
 #undef  ESMC_METHOD
#define ESMC_METHOD "create_mbmesh_redist_elem_move_verts()"
    int merr;
    

    // Get Parallel Information
    int localrc;
    int num_proc = VM::getCurrent(&localrc)->getPetCount();
    if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
      throw localrc;  // bail out with exception
    
    int localPet = VM::getCurrent(&localrc)->getLocalPet();
    if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
      throw localrc;  // bail out with exception
    
    
    //// Get unique list of verts going to each proc ////
    // allocate set list
    std::set<int> *set_of_gids_per_proc=NULL;
    set_of_gids_per_proc=new std::set<int>[num_proc];
    
    //// Also create a gid to vert map
    std::map<int,EntityHandle> gid_to_vert;
    
    // Loop adding elem gids to sets
    for (int i=0; i<elem_to_proc_list->size(); i++) {
       EntityHandle eh=(*elem_to_proc_list)[i].eh;
      int proc=(*elem_to_proc_list)[i].proc;
      
      // Get verts
      int num_verts;
      const EntityHandle *verts;
        merr=src_mesh->mesh->get_connectivity(eh,verts,num_verts); // NEED TO PASS IN corners_only = true???
      if (merr != MB_SUCCESS) {
        Throw() <<"MOAB ERROR: "<<moab::ErrorCodeStr[merr];
      }
   
      // Loop verts getting gids and inserting into sets
      for (int v=0; v<num_verts; v++) {
        int gid;
        
        // Get gid of vert
        MBMesh_get_gid(src_mesh, verts[v], &gid);
        
        // Insert into set
        set_of_gids_per_proc[proc].insert(gid);

        // Insert into map
        gid_to_vert[gid]=verts[v];
      }   
    }
    
    // Count number of non-empty sets
    int num_nonempty=0;
    for (int p=0; p<num_proc; p++) {
      if (!set_of_gids_per_proc[p].empty()) num_nonempty++;
    }


    // Allocate pattern for sparse communication
    UInt *nonempty_pets=NULL;
    UInt *nonempty_sizes=NULL;
    if (num_nonempty > 0) {
      nonempty_pets=new  UInt[num_nonempty];
      nonempty_sizes=new UInt[num_nonempty];
    }

    // Fill communication pattern arrays
     int size_per_vert=calc_size_vert_comm(src_mesh); // constant per vert
    int j=0;
    for (int p=0; p<num_proc; p++) {
      if (!set_of_gids_per_proc[p].empty()) {
        nonempty_pets[j]=(UInt) p;
        nonempty_sizes[j]=(UInt)(set_of_gids_per_proc[p].size()*size_per_vert);
        j++;
       }
    }
    

    // Create communication structure
    SparseMsg comm;

    // Set patterns and sizes
    comm.setPattern(num_nonempty, nonempty_pets);
    comm.setSizes(nonempty_sizes);

    // Deallocate arrays for communication pattern
    if (num_nonempty > 0) {
      if (nonempty_pets != NULL) delete [] nonempty_pets;
      if (nonempty_sizes != NULL) delete [] nonempty_sizes;
    }

    // Reset buffers
    comm.resetBuffers();

    // Loop through sets, packing buffers
    for (int p=0; p<num_proc; p++) {
      if (!set_of_gids_per_proc[p].empty()) {

        // Get buffer
          SparseMsg:: buffer *b=comm.getSendBuffer(p);

        // Get iterators to set
        std::set<int>::iterator si=set_of_gids_per_proc[p].begin();
        std::set<int>::iterator se=set_of_gids_per_proc[p].end();

        // Loop through set
        for (; si != se; ++si) {
          int gid=*si;
          // printf("%d# vert gid=%d to proc=%d \n",localPet,gid,p);

          // Get entity handle from gid
          std::map<int,EntityHandle>::iterator gehi =  gid_to_vert.find(gid);
          if (gehi == gid_to_vert.end()) {
            Throw() << " Gid not found in map!";
          }

          // Get EntityHandle
          EntityHandle vert=gehi->second;

          // Pack vert to send
          char buff[MAX_VERT_COMM_SIZE];
          pack_vert_comm(src_mesh, vert, buff);

          // Put into send buffer
          b->push((UChar *)buff, (UInt)size_per_vert);
        }
      }
    }

    // Deallocate list of sets
    if (set_of_gids_per_proc != NULL) delete [] set_of_gids_per_proc;

    // Communicate verts
    comm.communicate();

     // Go through received buffers and create verts
    for (std::vector<UInt>::iterator p = comm.inProc_begin(); p != comm.inProc_end(); ++p) {
      UInt proc = *p;
      SparseMsg::buffer *b = comm.getRecvBuffer(proc);

      while (!b->empty()) {
        char buff[MAX_VERT_COMM_SIZE];

        // Get one verts info out of buffer
        b->pop((UChar *)buff, (UInt)size_per_vert);

        // Unpack gid
        int gid;
        unpack_gid_vert_comm(out_mesh, buff, &gid);

        // Check to see if the vert already exists, if not then unpack and add
        std::map<int,EntityHandle>::iterator ogehi =  out_gid_to_vert->find(gid);
        if (ogehi == out_gid_to_vert->end()) {
          // Unpack vert
          EntityHandle new_vert;
          unpack_vert_comm(out_mesh, buff, &new_vert);

          // Add to map
          (*out_gid_to_vert)[gid]=new_vert;          
        }
      }
    }

    //// Add list of verts to MBMesh struct ////
    
    // Allocate temp storage for verts
    int num_verts=out_gid_to_vert->size();
     EntityHandle *verts=new EntityHandle[num_verts];

    // Loop and put verts into struct
    int v=0;
    std::map<int,EntityHandle>::iterator mi=out_gid_to_vert->begin();
    std::map<int,EntityHandle>::iterator me=out_gid_to_vert->end();    
    for (; mi != me; ++mi) {
      verts[v]=mi->second;
      v++;
    }

    // Put into mesh
    out_mesh->num_verts=num_verts;
    out_mesh->verts=verts;
  }

  int calc_size_elem_comm(MBMesh *src_mesh, EntityHandle eh) {
    int merr;

    // init size
    int size=0;
    
    // Gid
    size += sizeof(int);

    ///// DON'T PACK THESE BECAUSE THEY WON'T MAKE SENSE ON THE NEW PROC
    // // orig_pos
    // size += sizeof(int);
    //
    // // owner
    // size += sizeof(int);

    // number of verts
    size += sizeof(int);

    // num verts
     int num_verts;
    const EntityHandle *verts;
    merr=src_mesh->mesh->get_connectivity(eh,verts,num_verts); // NEED TO PASS IN corners_only = true???
    if (merr != MB_SUCCESS) {
      Throw() <<"MOAB ERROR: "<<moab::ErrorCodeStr[merr];
    }

    // number of nodes
    size += num_verts*sizeof(int);

    // ADD OTHER THINGS HERE AS ADDED TO ELEM

    // Add masking
    if (src_mesh->has_elem_mask) {
      // Only pack mask field (not mask_val), since that's the only one needed for rend. 
      size += sizeof(int);
    }

    if (src_mesh->has_node_mask) {
      // Only pack mask field (not mask_val), since that's the only one needed for rend. 
      size += sizeof(int);
    }

    // output size
    return size;
  }

  void pack_elem_comm(MBMesh *src_mesh, EntityHandle elem, char *buff) {
#define MAX_ELEM_COMM_SIZE 100
    int merr;

    // Offset
    int off=0;
 
    // Pack gid
    int gid;
    MBMesh_get_gid(src_mesh, elem, &gid);

    *((int *)(buff+off))=gid;
    off +=sizeof(int);
    
    ///// Don't send orig_pos and owner, since they will make no sense on
    ///// a new processor

     // Get number of verts and vert list 
    int num_verts;
    const EntityHandle *verts;
    merr=src_mesh->mesh->get_connectivity(elem,verts,num_verts); // NEED TO PASS IN corners_only = true???
    if (merr != MB_SUCCESS) {
      Throw() <<"MOAB ERROR: "<<moab::ErrorCodeStr[merr];
    }

    // Pack number of verts
    *((int *)(buff+off))=num_verts;
    off +=sizeof(int);

    // Pack vert gids
    for (int v=0; v<num_verts; v++) {
       int vert_gid;
        
      // Get gid of vert
      MBMesh_get_gid(src_mesh, verts[v], &vert_gid);

      // Pack into buffer
      *((int *)(buff+off))=vert_gid;
      off +=sizeof(int);        

      if (src_mesh->has_node_mask) {
        // Get dst node mask 
        int masked, merr;
        merr=src_mesh->mesh->tag_get_data(src_mesh->node_mask_tag, &verts[v], 1, &masked);
        if (merr != MB_SUCCESS) {
          Throw() <<"MOAB ERROR: "<<moab::ErrorCodeStr[merr];
        }
        
        // Pack number of mask
        *((int *)(buff+off))=masked;
        off +=sizeof(int);
      }
    }

    // Pack mask data
    // (Only pack mask field (not mask_val), since that's the only one needed for rend)
    if (src_mesh->has_elem_mask) {
      // Get dst elem mask 
      int masked;
      int merr=src_mesh->mesh->tag_get_data(src_mesh->elem_mask_tag, &elem, 1, &masked);
      if (merr != MB_SUCCESS) {
        Throw() <<"MOAB ERROR: "<<moab::ErrorCodeStr[merr];
      }
      
      // Pack number of mask
      *((int *)(buff+off))=masked;
      off +=sizeof(int);
    }

  }


  int calc_size_from_buff_elem_comm(MBMesh *out_mesh, char *buff) {

    // Init size
    int size=0;
    
    // Offset
    int off=0;

    // Size of gid, plus offset past gid
    size += sizeof(int);
    off +=sizeof(int);
    
    // Get number of verts
    int num_verts= *((int *)(buff+off));

    // Size to hold number of verts
    size +=sizeof(int);

    // Size to hold an integer gid for each vert
    size += num_verts*sizeof(int);
    
    // Mask Size
    if (out_mesh->has_elem_mask) {
      size += sizeof(int);
    }

    // Mask Size
    if (out_mesh->has_node_mask) {
      size += sizeof(int);
    }

     // return size
    return size;
  }

  void unpack_gid_elem_comm(MBMesh *out_mesh, char *buff, int *_gid) {
    
    // Unpack gid
    *_gid=*((int *)(buff));
  }

  
  // Get the entity type from parametric dimension and number of corners
  static EntityType _get_entity_type(int pdim, int num_verts) {
    if (pdim==2) {
      if (num_verts==3) return MBTRI;
      else if (num_verts==4) return MBQUAD;
      else {
        int localrc;
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                         "- unrecognized ESMF element type",
                                         ESMC_CONTEXT, &localrc)) throw localrc;
      }
    } else if (pdim==3) {
      if (num_verts==4) return MBTET;
      else if (num_verts==8) return MBHEX;
      else {
        int localrc;
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                         "- unrecognized ESMF element type",
                                         ESMC_CONTEXT, &localrc)) throw localrc;
      }
    }
   }

  void unpack_elem_comm(MBMesh *out_mesh, char *buff, std::map<int,EntityHandle> *out_gid_to_vert, EntityHandle *_new_elem) {
    int merr,localrc;

    // Offset
    int off=0;

    // Unpack gid
    int gid;
    gid=*((int *)(buff+off));
    off +=sizeof(int);

    ///// Don't send orig_pos and owner, since they will make no sense on
    ///// a new processor

    // Unpack num_verts
    int num_verts;;
    num_verts=*((int *)(buff+off));
    off +=sizeof(int);

    // Define the maximum number of verts and error check
#define MAX_ELEM_VERTS 20
    if (num_verts >MAX_ELEM_VERTS) {
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
          "- element contains more nodes than are currently supported ",
                                       ESMC_CONTEXT, &localrc)) throw localrc;
    }

    // Connectivity array
    EntityHandle verts[MAX_ELEM_VERTS];
#undef MAX_ELEM_VERTS

    // Loop unpacking each vert gid and creating list of verts
    for(int v=0; v<num_verts; v++) {
      int vert_gid=*((int *)(buff+off));
      off +=sizeof(int);

      // Get vert handle from gid
      std::map<int,EntityHandle>::iterator ogtvi =  out_gid_to_vert->find(vert_gid);
      if (ogtvi == out_gid_to_vert->end()) {
        Throw() << "vertex gid not found in map!";
       }

      // Get vert EntityHandle
      verts[v]=ogtvi->second;
      
      // Unpack mask data
      if (out_mesh->has_node_mask) {
        // Unpack node mask tag
        int masked;
        masked=*((int *)(buff+off));
        off +=sizeof(int);
        
        // Set node mask tag
        merr=out_mesh->mesh->tag_set_data(out_mesh->node_mask_tag, &verts[v], 1, &masked);
        if (merr != MB_SUCCESS) {
          Throw() <<"MOAB ERROR: "<<moab::ErrorCodeStr[merr];
        }
      }

    }

    // Get entity type of element
    EntityType etype=_get_entity_type(out_mesh->pdim, num_verts);

    // Create new element
    EntityHandle new_elem;
    merr=out_mesh->mesh->create_element(etype,verts,num_verts,new_elem);
    if (merr != MB_SUCCESS) {
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
          moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
    }     

    // Set global id
    merr=out_mesh->mesh->tag_set_data(out_mesh->gid_tag, &new_elem, 1, &gid);
    if (merr != MB_SUCCESS) {
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
        moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
    }     

    // Unpack mask data
    // (Only unpack mask field (not mask_val), since that's the only one needed for rend)
     if (out_mesh->has_elem_mask) {
      // Unpack mask
      int masked=*((int *)(buff+off));
      off +=sizeof(int);

      // Set elem mask 
      merr=out_mesh->mesh->tag_set_data(out_mesh->elem_mask_tag, &new_elem, 1, &masked);
      if (merr != MB_SUCCESS) {
        Throw() <<"MOAB ERROR: "<<moab::ErrorCodeStr[merr];
      }
    }

    // Output elem
    *_new_elem=new_elem;
  }


  // Redist elems to new mesh
  void create_mbmesh_redist_elem_move_elems(MBMesh *src_mesh, 
                                            std::vector<EH_Comm_Pair> *elem_to_proc_list, 
                                            std::map<int,EntityHandle> *out_gid_to_vert, 
                                            MBMesh *out_mesh) {
#undef  ESMC_METHOD
#define ESMC_METHOD "create_mbmesh_redist_elem_move_elems()"
    int merr;

    // Get Parallel Information
    int localrc;
    int num_proc = VM::getCurrent(&localrc)->getPetCount();
    if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
      throw localrc;  // bail out with exception
     
    int localPet = VM::getCurrent(&localrc)->getLocalPet();
    if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
      throw localrc;  // bail out with exception
    

    // Allocate size array
    UInt *size_per_proc=NULL;
    size_per_proc=new UInt[num_proc];

    // Init to zero
    for (int i=0; i<num_proc; i++) {
      size_per_proc[i]=0;
    }

    // Calc. Size going to each proc
    for (int i=0; i<elem_to_proc_list->size(); i++) {
      EntityHandle eh=(*elem_to_proc_list)[i].eh;
      int proc=(*elem_to_proc_list)[i].proc;

      size_per_proc[proc] += calc_size_elem_comm(src_mesh, eh);
    }

  // Debug
#if 0
  for (int i=0; i<num_proc; i++) {
    printf("%d# size going to %d = %d\n",localPet,i,size_per_proc[i]);
  }
#endif
    
    // Count number of non-empty procs
    int num_nonempty=0;
    for (int p=0; p<num_proc; p++) {
      if (size_per_proc[p]>0) num_nonempty++;
    }


    // Allocate pattern for sparse communication
    UInt *nonempty_pets=NULL;
    UInt *nonempty_sizes=NULL;
    if (num_nonempty > 0) {
      nonempty_pets=new  UInt[num_nonempty];
      nonempty_sizes=new UInt[num_nonempty];
     }

    // Fill communication pattern arrays
    int j=0;
    for (int p=0; p<num_proc; p++) {
      if (size_per_proc[p]>0) {
        nonempty_pets[j]=(UInt) p;
        nonempty_sizes[j]=(UInt)(size_per_proc[p]);
        j++;
       }
    }
    
    // Deallocate size_per_proc
    if (size_per_proc != NULL) delete [] size_per_proc;

    // Create communication structure
    SparseMsg comm;

    // Set patterns and sizes
    comm.setPattern(num_nonempty, nonempty_pets);
     comm.setSizes(nonempty_sizes);

    // Deallocate arrays for communication pattern
    if (num_nonempty > 0) {
      if (nonempty_pets != NULL) delete [] nonempty_pets;
      if (nonempty_sizes != NULL) delete [] nonempty_sizes;
    }

    // Reset buffers
    comm.resetBuffers();

    // Loop through elem distribution list packing buffers
    for (int i=0; i<elem_to_proc_list->size(); i++) {
      EntityHandle elem=(*elem_to_proc_list)[i].eh;
      int proc=(*elem_to_proc_list)[i].proc;

      // Get buffer
      SparseMsg:: buffer *b=comm.getSendBuffer(proc);

      // Pack vert to send
      char buff[MAX_ELEM_COMM_SIZE];
      pack_elem_comm(src_mesh, elem, buff);

      // Calc size
      int elem_comm_size=calc_size_elem_comm(src_mesh, elem);

      // Put into send buffer
      b->push((UChar *)buff, (UInt)elem_comm_size);
    }

    // Communicate elems
    comm.communicate();

    // Track new elems, so we don't create copies
    std::map<int,EntityHandle> out_gid_to_elem; 

    // Go through received buffers and create verts
    for (std::vector<UInt>::iterator p = comm.inProc_begin(); p != comm.inProc_end(); ++p) {
      UInt proc = *p;
      SparseMsg::buffer *b = comm.getRecvBuffer(proc);

      while (!b->empty()) {
        char buff[MAX_ELEM_COMM_SIZE];
 
        // Look at buffer to figure out what size we need to pop
        int elem_size=calc_size_from_buff_elem_comm(out_mesh, (char *)(b->get_current()));

        // Get one elem's info out of buffer
        b->pop((UChar *)buff, (UInt)elem_size);

        // Unpack gid
        int gid;
        unpack_gid_elem_comm(out_mesh, buff, &gid);

        // Check to see if the elem already exists, if not then unpack and add
        std::map<int,EntityHandle>::iterator ogtei =  out_gid_to_elem.find(gid);
        if (ogtei == out_gid_to_elem.end()) {
          // Unpack elem
          EntityHandle new_elem;
          unpack_elem_comm(out_mesh, buff, out_gid_to_vert, &new_elem);

          // Add to map
          out_gid_to_elem[gid]=new_elem;          
        }
      }
    }
  }

  // Redist elems to new mesh
  void create_pointlist_redist_move_points(PointList *pl,
                                           std::vector<PL_Comm_Pair> *point_to_proc_list,
                                           PointList **out_pl) {
#undef  ESMC_METHOD
#define ESMC_METHOD "create_mbmesh_redist_elem_move_points()"
    int merr;

    // Get Parallel Information
    int localrc;
    int petCount = VM::getCurrent(&localrc)->getPetCount();
    if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
      throw localrc;  // bail out with exception

    int localPet = VM::getCurrent(&localrc)->getLocalPet();
    if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
      throw localrc;  // bail out with exception

    SparseMsg comm;

    // RLO: construct a per proc list of location in pl to be sent to procs
    //      also mark which points are moving
    std::vector<int> proc_counts;
    proc_counts.resize(petCount,0);
    std::vector< std::vector<int> > idx_list; // TODO: optimize sparse vector
    idx_list.resize(petCount);

    std::vector<PL_Comm_Pair>::iterator pb = point_to_proc_list->begin();
    std::vector<PL_Comm_Pair>::iterator pe = point_to_proc_list->end();
    for (pb; pb != pe; ++pb) {
        proc_counts[pb->proc]++;
        idx_list[pb->proc].push_back(pb->loc);
    }

    // RLO: construct lists used to set up the communication pattern
    int sdim = pl->get_coord_dim();
    int snd_size=(sdim+1)*sizeof(double); // buffer item size
    int num_snd_procs=0;
    std::vector<int> snd_pets;
    std::vector<int> snd_sizes;
    std::vector<int> snd_counts;
    std::vector< std::vector<int> > idx_list2;
    for (int i=0; i<petCount; i++) {
      if (proc_counts[i] > 0) {
        num_snd_procs++;
        snd_pets.push_back(i);
        snd_sizes.push_back(proc_counts[i]*snd_size);
        snd_counts.push_back(proc_counts[i]);
        idx_list2.push_back(idx_list[i]); // dense version of idx_list
      }
    }


    // Setup pattern and sizes
    if (num_snd_procs >0) {
      comm.setPattern(num_snd_procs, (const UInt *)&(snd_pets[0]));
      comm.setSizes((UInt *)&(snd_sizes[0]));
    } else {
      comm.setPattern(0, (const UInt *)NULL);
      comm.setSizes((UInt *)NULL);
    }

    // Reset buffers
    comm.resetBuffers();

    // Pack points into buffers
    for (int i=0; i< num_snd_procs; i++) {
      SparseMsg:: buffer *b=comm.getSendBuffer(snd_pets[i]);
      for (int j=0; j<snd_counts[i]; j++) {


        // Get index of node
        int loc=idx_list2[i][j];

        // RLO: coordinates and gid of point
        const double *pnt = pl->get_coord_ptr(loc);
        int this_id = pl->get_id(loc);

        // pack buf
        double buf[4]; // 4 is biggest this should be (i.e. 3D+id)
        buf[0]=pnt[0];
        buf[1]=pnt[1];
        if (sdim < 3)
          buf[2]=this_id;  //passing an int through a double...inefficient but ok?
        else {
          buf[2]=pnt[2];
          buf[3]=this_id;  //passing an int through a double...inefficient but ok?
        }

        // Push buf onto send struct
        b->push((const UChar *)buf, (UInt)snd_size);
      }
    }

    // Communicate point information
    comm.communicate();

    int num_pts = 0;
    for (std::vector<UInt>::iterator p = comm.inProc_begin(); p != comm.inProc_end(); ++p) {
      UInt proc = *p;
      SparseMsg::buffer *b = comm.getRecvBuffer(proc);

      // Unpack everything from this processor
      while (!b->empty()) {
        double buf[4]; // 4 is biggest this should be (i.e. 3D+dist)

        b->pop((UChar *)buf, (UInt)snd_size);
        num_pts++;
      }
    }
    comm.resetBuffers();

    // int pl_rend_size=pl->get_curr_num_pts() - num_snd_pts + num_rcv_pts;
    int pl_rend_size=num_pts;
#ifdef DEBUG
    printf("PET %d - create_pointlist_redist_move_points: pl_rend_size (%d) = pl->size (%d)\n", localPet, pl_rend_size, pl->get_curr_num_pts());
#endif
    // Create source rendezvous point list (create outside of if, so will work even if 0-sized)
    PointList *pl_rend;
    pl_rend = new ESMCI::PointList(pl_rend_size,sdim);

    if (pl_rend_size > 0) {

      // srcplist_rend = new ESMCI::PointList(plist_rend_size,sdim);

      // TODO: verify that Zoltan is not checking that point are sent to their native processors, if so we can remove this loop and the mymoving vector
      // int orig_srcpointlist_size = pl->get_curr_num_pts();
      // for (int i=0; i<orig_srcpointlist_size; i++) {
      //   if (mymoving[i] == 0) {
      //     int temp_id=pl->get_id(i);
      //     double *temp_pnt = (double *)pl->get_coord_ptr(i);
      //     pl_rend->add(temp_id,temp_pnt);
      //   }
      // }

      int ip=0;
      for (std::vector<UInt>::iterator p = comm.inProc_begin(); p != comm.inProc_end(); ++p) {
        UInt proc = *p;
        SparseMsg::buffer *b = comm.getRecvBuffer(proc);

        // Unpack everything from this processor
        while (!b->empty()) {
          double buf[4]; // 4 is biggest this should be (i.e. 3D+dist)

          b->pop((UChar *)buf, (UInt)snd_size);
          //      printf(" [%f %f %f], ",pnt[0],pnt[1],pnt[2]);


          // Unpack buf
          double pnt[3]={0.0,0.0,0.0};
          int loc_id;

          pnt[0]=buf[0];
          pnt[1]=buf[1];
          if (sdim < 3) {
            loc_id=(int)buf[2];
          } else {
            pnt[2]=buf[2];
            loc_id=(int)buf[3];
          }

          pl_rend->add(loc_id,pnt);
        }

        ip++;
      }
    }
    //RLO: pointer magic
    *out_pl = pl_rend;
  }

} // ESMCI namespace

#endif // ESMF_MOAB
