// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2022, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================

#include <Mesh/include/Regridding/ESMCI_MeshRegrid.h>
#include <Mesh/include/Legacy/ESMCI_MeshRead.h>
#include <Mesh/include/Regridding/ESMCI_Interp.h>
#include <Mesh/include/Regridding/ESMCI_CreepFill.h>
#include <Mesh/include/Regridding/ESMCI_Extrap.h>

#include "ESMCI_TraceMacros.h"  // for profiling

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

namespace ESMCI {


 int regrid(Mesh *srcmesh, PointList *srcpointlist, Mesh *dstmesh, PointList *dstpointlist,
            Mesh *midmesh, IWeights &wts,
            int *regridMethod, 
            int *regridPoleType, int *regridPoleNPnts,
            int *map_type,
            int *extrapMethod,
            int *extrapNumSrcPnts,
            ESMC_R8 *extrapDistExponent,
            int *extrapNumLevels,
            int *extrapNumInputLevels, 
            int *unmappedaction,
            bool set_dst_status, WMat &dst_status,
            bool checkFlag) {


   // See if it could have a pole
   bool maybe_pole=false;
   if ((*regridMethod == ESMC_REGRID_METHOD_BILINEAR ||
        *regridMethod == ESMC_REGRID_METHOD_PATCH) &&
       (srcmesh->parametric_dim()==2) &&
       (srcmesh->spatial_dim()==3)) maybe_pole=true;

    // Output Mesh without poles for Debugging
#ifdef ESMF_REGRID_DEBUG_WRITE_MESH_WO_POLE
    if (srcmesh != NULL) WriteMesh(*srcmesh, "src_rgd_mesh_wo_p");
    else if (srcpointlist != NULL) srcpointlist->WriteVTK("src_rgd_mesh_wo_p");
    // No pole is added to dst mesh
#endif

    // If a srcMesh exists add Pole constraints to it
    UInt pole_constraint_id;
    if (srcmesh != NULL) {
      pole_constraint_id = srcmesh->DefineContext("pole_constraints");
    }

    // Add Pole
    IWeights pole_constraints, stw;
     if (maybe_pole) {
      if (*regridPoleType == ESMC_REGRID_POLETYPE_ALL) {
        for (UInt i = 1; i <= 7; ++i)
          MeshAddPole(*srcmesh, i, pole_constraint_id, pole_constraints);
      } else if (*regridPoleType == ESMC_REGRID_POLETYPE_NPNT) {
        for (UInt i = 1; i <= 7; ++i)
          MeshAddPoleNPnts(*srcmesh, *regridPoleNPnts, i, pole_constraint_id, pole_constraints);
      } else if (*regridPoleType == ESMC_REGRID_POLETYPE_TEETH) {
        for (UInt i = 1; i <= 7; ++i)
          MeshAddPoleTeeth(*srcmesh, i, pole_constraint_id, pole_constraints);
      }
    }

     // Output Mesh for Debugging
#ifdef ESMF_REGRID_DEBUG_WRITE_MESH
    if (srcmesh != NULL) WriteMesh(*srcmesh, "src_rgd_mesh");
    else if (srcpointlist != NULL) srcpointlist->WriteVTK("src_rgd_mesh");

    if (dstmesh != NULL) WriteMesh(*dstmesh, "dst_rgd_mesh");
    else if (dstpointlist != NULL) dstpointlist->WriteVTK("dst_rgd_mesh");
#endif


    // Create a layer of ghost elements since the patch method needs
    // a larger stencil.
    if (*regridMethod == ESMC_REGRID_METHOD_PATCH) {
      // Get coordinate fields
      MEField<> &scoord = *srcmesh->GetCoordField();

      int num_snd=0;
      MEField<> *snd[2],*rcv[2];

      // Load coord field
      MEField<> *psc = &scoord;
      snd[num_snd]=psc;
      rcv[num_snd]=psc;
      num_snd++;

      // Load mask field
      MEField<> *psm = srcmesh->GetField("mask");
      if (psm != NULL) {
        snd[num_snd]=psm;
        rcv[num_snd]=psm;
        num_snd++;
      }

      srcmesh->CreateGhost();
      srcmesh->GhostComm().SendFields(num_snd, snd, rcv);
    }

    // Create a layer of ghost elements since the higher order conservative needs a
    // a larger stencil.
    if (*regridMethod == ESMC_REGRID_METHOD_CONSERVE_2ND) {
      // Get coordinate fields
      MEField<> &scoord = *srcmesh->GetCoordField();

      int num_snd=0;
      MEField<> *snd[4],*rcv[4];

      // Load coord field
      MEField<> *psc = &scoord;
      snd[num_snd]=psc;
      rcv[num_snd]=psc;
      num_snd++;

      // Load mask field
      MEField<> *psm = srcmesh->GetField("elem_mask");
      if (psm != NULL) {
        snd[num_snd]=psm;
        rcv[num_snd]=psm;
        num_snd++;
      }

      // Load area field
      MEField<> *psa = srcmesh->GetField("elem_area");
      if (psa != NULL) {
        snd[num_snd]=psa;
        rcv[num_snd]=psa;
        num_snd++;
      }

      // Load frac2 field
      MEField<> *psf = srcmesh->GetField("elem_frac2");
      if (psf != NULL) {
        snd[num_snd]=psf;
        rcv[num_snd]=psf;
        num_snd++;
      }

      srcmesh->CreateGhost();
      srcmesh->GhostComm().SendFields(num_snd, snd, rcv);

#if 0
      // DEBUG
      {
        MeshDB::const_iterator ei = srcmesh->elem_begin_all(), ee = srcmesh->elem_end_all();
        for (; ei != ee; ++ei) {
          const MeshObj &elem=*ei;

          if (!GetAttr(elem).GetContext().is_set(Attr::ACTIVE_ID)) {
            printf("AFTER GHOST id=%d not active\n",elem.get_id());
          }
        }
      }
#endif

    }



    // Convert to map type
    MAP_TYPE mtype;
    if (*map_type==0) mtype=MAP_TYPE_CART_APPROX;
    else if (*map_type==1) mtype=MAP_TYPE_GREAT_CIRCLE;
    else Throw() << "Unrecognized map type";
 
    // Put interp in a block so that it and the rendezvous meshes are
    // destroyed before we do other things like the extrapolation below
    {

      // Only pass the dstMesh into rendezvous grid creation, if the dstpointlist doesn't exist.
      // (sometimes we have both for extrapolation)
      Mesh *tmp_dstmesh=NULL;
      if (dstpointlist == NULL) tmp_dstmesh=dstmesh;

      ESMCI_REGRID_TRACE_ENTER("NativeMesh regrid interp 1");
      // Build the rendezvous grids
      Interp interp(srcmesh, srcpointlist, tmp_dstmesh, dstpointlist,
                    midmesh, false, *regridMethod,
                    set_dst_status, dst_status,
                    mtype, *unmappedaction, checkFlag);
      ESMCI_REGRID_TRACE_EXIT("NativeMesh regrid interp 1");

      ESMCI_REGRID_TRACE_ENTER("NativeMesh regrid interp 2");
      // Create the weight matrix
      interp(0, wts, set_dst_status, dst_status);
      ESMCI_REGRID_TRACE_EXIT("NativeMesh regrid interp 2");

      // Release the Zoltan struct if we used it for the mid mesh
      if(midmesh) interp.release_zz();

    } // block which contains inter object existance

     // Factor out poles if they exist
     if (maybe_pole) {
       if (*regridPoleType == ESMC_REGRID_POLETYPE_ALL) {
         wts.GatherToCol(pole_constraints);
         wts.AssimilateConstraints(pole_constraints);
       } else if (*regridPoleType == ESMC_REGRID_POLETYPE_NPNT) {
         wts.GatherToRowSrc(pole_constraints);
         wts.AssimilateConstraintsNPnts(pole_constraints);
       }
     }

     // Do extrapolation if the user has requested it
     if (*extrapMethod != ESMC_EXTRAPMETHOD_NONE) {
       extrap(srcmesh, srcpointlist, dstmesh, dstpointlist,
              wts, mtype, pole_constraint_id,
              *extrapMethod, *extrapNumSrcPnts, *extrapDistExponent,
              *extrapNumLevels, *extrapNumInputLevels,
              set_dst_status, dst_status);
     }

    return 1;
 }



  // This should only be used where srcmesh->is_split is true. This function translates
  // the src indices in iientires to their original (before splitting) versions.
  void translate_split_src_elems_in_wts(Mesh *srcmesh, int num_entries,
                                        int *iientries) {
    
    
    // Get a list of split ids that we own
    UInt num_gids=0;
    UInt *gids_split=NULL;
    UInt *gids_orig=NULL;
    
    // Get number of split points
    num_gids=srcmesh->split_to_orig_id.size();
    
    // Allocate space
    if (num_gids>0) {
      gids_split= new UInt[num_gids];
      gids_orig= new UInt[num_gids];
      
      // Loop and get split-orig id pairs
      std::map<UInt,UInt>::iterator mi=srcmesh->split_to_orig_id.begin();
      std::map<UInt,UInt>::iterator me=srcmesh->split_to_orig_id.end();
      
      int pos=0;
      for ( ; mi != me; mi++) {
        gids_split[pos]=mi->first;
        gids_orig[pos]=mi->second;
        pos++;
      }
      
      //    for (int i=0; i<num_gids; i++) {
      //  printf("%d# s=%d o=%d\n",Par::Rank(),gids_split[i],gids_orig[i]);
      //}
    }
    
    // Put into DDir
    DDir<> id_map_dir;
    id_map_dir.Create(num_gids,gids_split,gids_orig);
    
    // Clean up
    if (num_gids>0) {
      if (gids_split!= NULL) delete [] gids_split;
      if (gids_orig != NULL) delete [] gids_orig;
    }
    
    
    // Gather list of spit src ids
    //// TODO: Maybe use a std::set instead to reduce the amount of communication??
    std::vector<UInt> src_split_gids;
    std::vector<int> src_split_gids_idx;
    
    // Loop through weights modifying split dst elements
    for (int i=0; i<num_entries; i++) {
      
      // Get src id
      UInt src_id=iientries[2*i];
      
      // If a split id then add to list
      if (src_id > srcmesh->max_non_split_id) {
        src_split_gids.push_back(src_id);
        src_split_gids_idx.push_back(2*i);
      }
      
    }
    
    // Do remote lookup to translate
    UInt num_src_split_gids=src_split_gids.size();
    UInt *src_split_gids_proc=NULL;
    UInt *src_split_gids_orig=NULL;
    
    if (num_src_split_gids > 0) {
      src_split_gids_proc = new UInt[num_src_split_gids];
      src_split_gids_orig = new UInt[num_src_split_gids];
    }
    
    // Get mapping of split ids to original ids
    id_map_dir.RemoteGID(num_src_split_gids, &src_split_gids[0], src_split_gids_proc, src_split_gids_orig);
    
    // Loop setting new ids
    for (int i=0; i<num_src_split_gids; i++) {
      iientries[src_split_gids_idx[i]]=src_split_gids_orig[i];
    }
    
    // Clean up
    if (num_src_split_gids > 0) {
      if (src_split_gids_proc != NULL) delete [] src_split_gids_proc;
      if (src_split_gids_orig != NULL) delete [] src_split_gids_orig;
    }
  }
  
  
  // This should only be used where dstmesh->is_split is true. This function translates
  // the dst indices in iientires and the corresponding factors to what they should be
  // without splitting 
  void translate_split_dst_elems_in_wts(Mesh *dstmesh, int num_entries,
                                        int *iientries, double *factors) {
    
    // Loop through weights modifying split dst elements
    for (int i=0; i<num_entries; i++) {
      int dst_id=iientries[2*i+1];
      
      // See if the element is part of a larger polygon
      std::map<UInt,double>::iterator mi =  dstmesh->split_id_to_frac.find(dst_id);
      
      // It is part of a larger polygon, so process
      if (mi != dstmesh->split_id_to_frac.end()) {
        
        // Modify weight by fraction of orig polygon
        factors[i] *= mi->second;
        
        // See if the id needs to be translated, if so then translate
        std::map<UInt,UInt>::iterator soi =  dstmesh->split_to_orig_id.find(dst_id);
        if (soi != dstmesh->split_to_orig_id.end()) {
          iientries[2*i+1]=soi->second;
        }
      }
    }
    
  }
  

  // to generate the iwts again, and return to Fortran
  int get_iwts(Mesh &mesh, MEField<> *iwts) {

    // generate integration weights
    Integrate ig(mesh);

    // Clear weights
    ig.clearWeights(iwts);

    // Add weights to meshes before poles
    // so all the weights are on user data points
    if (mesh.coordsys != ESMC_COORDSYS_CART) {
      for (UInt i = 1; i <= 7; ++i) {
        ig.AddPoleWeights(mesh,i,iwts);
      }
    }

    // Add in other none-pole weights
    // (and do cross processor sum)
    ig.intWeights(iwts);

    return 1;
  }



  
}
