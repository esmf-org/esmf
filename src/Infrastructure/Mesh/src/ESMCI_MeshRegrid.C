// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================

#include <Mesh/include/ESMCI_MeshRegrid.h>
#include <Mesh/include/ESMCI_MeshRead.h>
#include <Mesh/include/ESMCI_Interp.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

namespace ESMCI {

#ifdef DEBUG
int print_debug_info(IWeights &wts) {
  // print out info of weight matrix
  int snegcount = 0;
  int stotalcount = 0;
  int negcount = 0;
  int rowsum = 0;
  int rowsumcount = 0;
  int totalcount = 0;
  int gt1count = 0;
  double max = 0;
  double maxneg = 0;
  double min = 0;
  double badcolid = 0;
  double badrowid = 0;


   IWeights::WeightMap::iterator wit = wts.begin_row(), wet = wts.end_row();
  //IWeights::WeightMap::iterator wit = stw.begin_row(), wet = stw.end_row();
  for (; wit != wet; ++wit) {
    const IWeights::Entry &_row = wit->first;
    const std::vector<IWeights::Entry> &_col = wit->second;

//    std::cout<<Par::Rank()<<"  "<<_row.id<<"    ";
    rowsum = 0;
    for (UInt c = 0; c < _col.size(); ++c) {
      double value = _col[c].value;
      if (value < 0) negcount++;
      if (value > max) max = value;
      if (value < min) min = value;
      if (value > 1) gt1count++;
      if (value < 0 && std::abs(value) > std::abs(maxneg)) {
        maxneg = value;
        badcolid = _col[c].id;
        badrowid = _row.id;
      }
      rowsum += value;

//      std::cout<<std::setprecision(3)<<_col[c].value<<"  ";
    }
    if (rowsum > 1.01 || rowsum < .99) rowsumcount++;
    totalcount++;
//    std::cout<<std::endl;
    for (UInt c = 0; c < _col.size(); ++c) {
      double value = _col[c].value;

    }
  }

  std::cout<<std::endl<<std::setw(30)<<"Negative weights count = "<<negcount
           <<std::endl<<std::setw(30)<<"Greater than 1 count = "<<gt1count
           <<std::endl<<std::setw(30)<<"Row sum not 1 count = "<<rowsumcount
           <<std::endl<<std::setw(30)<<"Total row count = "<<totalcount
           <<std::endl<<std::setw(30)<<"Max weight  = "<<max
           <<std::endl<<std::setw(30)<<"Min weight = "<<min
           <<std::endl<<std::setw(30)<<"Max neg weight = "<<maxneg
           <<std::endl<<std::setw(30)<<"Bad weight ["<<badrowid<<","<<badcolid<<"]"
           <<std::endl<<std::endl;

  return 1;
}

int form_neg_wts_field(IWeights &wts, Mesh &srcmesh, MEField<> *src_neg_wts,
                                  Mesh &dstmesh, MEField<> *dst_neg_wts)
{
  IWeights::WeightMap::const_iterator wi = wts.begin_row(), we = wts.end_row();
  for (; wi != we; ++wi) {
    const IWeights::Entry &_row = wi->first;
    const std::vector<IWeights::Entry> &_col = wi->second;
    // look for destination node id matching _row.id
    MeshDB::MeshObjIDMap::iterator ndi =
      dstmesh.map_find(MeshObj::NODE, _row.id);
    ThrowRequire(ndi != dstmesh.map_end(MeshObj::NODE));
    double *Ddata = dst_neg_wts->data(*ndi);
    double min = 1.0;
    double max = 0.0;
    for (UInt c = 0; c < _col.size(); ++c) {
      double value = _col[c].value;
      if (value < min) min = value;
      if (value > max) max = value;
    }
    *Ddata = 0;
    if (min < 0) *Ddata = min;
    if (max > 1.0) *Ddata = max;
    if (min < 0 && max > 1.0) *Ddata = std::max(std::abs(max),std::abs(min));
  }
  return 1;
}
#endif

// Meshes are already committed
  int online_regrid(Mesh *srcmesh, PointList *srcpointlist,
                    Mesh *dstmesh, PointList *dstpointlist,
                    IWeights &wts,
                    int *regridConserve, int *regridMethod,
                    int *regridPoleType, int *regridPoleNPnts,
                    int *regridScheme,
                    int *map_type,
                    int *extrapMethod,
                    int *extrapNumSrcPnts,
                    ESMC_R8 *extrapDistExponent,
                    int *unmappedaction,
                    bool set_dst_status, WMat &dst_status) {


    // Conservative regridding
    switch (*regridConserve) {
    case (ESMC_REGRID_CONSERVE_ON): {

      // Get the integration weights
      MEField<> *src_iwts = srcmesh->GetField("iwts");
      if (!src_iwts) Throw() << "Integration weights needed for conservative regridding."
                             <<std::endl;
      MEField<> *dst_iwts = dstmesh->GetField("iwts");
      if (!dst_iwts) Throw() << "Integration weights needed for conservative regridding."
                             <<std::endl;

      if (!csrv(*srcmesh, *dstmesh, wts, src_iwts, dst_iwts, regridMethod, regridScheme,
                regridPoleType, regridPoleNPnts, unmappedaction))
        Throw() << "Conservative regridding error" << std::endl;

    } break;
    // NON Conservative regridding
    case (ESMC_REGRID_CONSERVE_OFF): {

      if (!regrid(srcmesh, srcpointlist, dstmesh, dstpointlist, NULL,
                  wts, regridMethod, regridScheme,
                  regridPoleType, regridPoleNPnts, map_type,
                  extrapMethod,
                  extrapNumSrcPnts,
                  extrapDistExponent,
                  unmappedaction,
                  set_dst_status, dst_status)) {
        Throw() << "Regridding error" << std::endl;
      }

#ifdef ESMF_REGRID_DEBUG_OUTPUT_WTS_ALL
      WMat::WeightMap::iterator wi = wts.begin_row(), we = wts.end_row();
      for (; wi != we; ++wi) {
        const WMat::Entry &w = wi->first;

        std::vector<WMat::Entry> &wcol = wi->second;

        // Construct factor index list
        for (UInt j = 0; j < wcol.size(); ++j) {
          const WMat::Entry &wc = wcol[j];

          printf("d_id=%d  s_id=%d w=%20.17E \n",w.id,wc.id,wc.value);
        }
      }
#endif

    } break;

    default:
      Throw() << "Regridding method:" << *regridConserve << " is not implemented";
    }

  return 1;
}

// Mesh are not committed yet
#ifdef REGRIDTIMING
int offline_regrid(Mesh &srcmesh, Mesh &dstmesh, Mesh &dstmeshcpy,
             int *regridConserve, int *regridMethod,
             int *regridPoleType, int *regridPoleNPnts,
             char *srcGridFile, char *dstGridFile, char *wghtFile,
             regridTimer &rt) {
#else
int offline_regrid(Mesh &srcmesh, Mesh &dstmesh, Mesh &dstmeshcpy,
             int *regridConserve, int *regridMethod,
             int *regridPoleType, int *regridPoleNPnts,
             char *srcGridFile, char *dstGridFile, char *wghtFile) {
#endif

  // Conflict management
  int regridScheme = ESMC_REGRID_SCHEME_FULL3D;
  int unmappedaction = ESMCI_UNMAPPEDACTION_ERROR;

  IWeights wts;
  MEField<> *src_iwts, *dst_iwts, *dst_iwtscpy;

#ifdef DEBUG
  // Add fields to mesh to hold negative weights
  MEField<> *src_neg_wts, *dst_neg_wts;
  Context ctxt; ctxt.flip();
  src_neg_wts = srcmesh.RegisterField("negwts",
    MEFamilyStd::instance(), MeshObj::ELEMENT, ctxt, 1, true);
  dst_neg_wts = dstmesh.RegisterField("negwts",
    MEFamilyStd::instance(), MeshObj::ELEMENT, ctxt, 1, true);
#endif

    switch (*regridConserve) {

    // Conservative regridding
    case (ESMC_REGRID_CONSERVE_ON): {

      // Add fields to mesh
      Context ctxt; ctxt.flip();
      src_iwts = srcmesh.RegisterField("iwts",
        MEFamilyStd::instance(), MeshObj::ELEMENT, ctxt, 1, true);

      dst_iwts = dstmesh.RegisterField("iwts",
        MEFamilyStd::instance(), MeshObj::ELEMENT, ctxt, 1, true);

      // generate integration weights on the copy
      // TODO: remove this (and the dstcpy mesh passed in) when the
      //       write bug with pole assimilation is fixed.
      dst_iwtscpy = dstmeshcpy.RegisterField("iwts",
        MEFamilyStd::instance(), MeshObj::ELEMENT, ctxt, 1, true);
      dstmeshcpy.Commit();
      Integrate dig(dstmeshcpy);
      dig.clearWeights(dst_iwtscpy);
      if ((regridScheme == ESMC_REGRID_SCHEME_FULL3D) ||
          (regridScheme == ESMC_REGRID_SCHEME_DCON3DWPOLE) ||
          (regridScheme == ESMC_REGRID_SCHEME_FULLTOREG3D)) {
        for (UInt i = 1; i <= 7; ++i)
          dig.AddPoleWeights(dstmeshcpy,i,dst_iwtscpy);
      }
      dig.intWeights(dst_iwtscpy);

      // Commit the meshes
      srcmesh.Commit();
      dstmesh.Commit();

      if (!csrv(srcmesh, dstmesh, wts, src_iwts, dst_iwts, regridMethod, &regridScheme,
                regridPoleType, regridPoleNPnts, &unmappedaction))
        Throw() << "Conservative regridding error" << std::endl;
    } break;

    // NON Conservative regridding
    case (ESMC_REGRID_CONSERVE_OFF): {

      // Commit the meshes
      srcmesh.Commit();
      dstmesh.Commit();
      dstmeshcpy.Commit();

      bool tmp_set_dst_status=false;
      WMat tmp_dst_status;
      int tmpExtrapNumSrcPnts=1;
      int map_type=0;
      int tmpExtrapMethod=0;
      ESMC_R8 tmpDistExponent=2.0;
      if (!regrid(&srcmesh, NULL, &dstmesh, NULL, NULL, wts, regridMethod, &regridScheme,
                  regridPoleType, regridPoleNPnts, &map_type,
                  &tmpExtrapMethod,
                  &tmpExtrapNumSrcPnts,
                  &tmpDistExponent,
                  &unmappedaction,
                  tmp_set_dst_status, tmp_dst_status))

        Throw() << "Regridding error" << std::endl;

      // the mask
      MEField<> *mask = dstmesh.GetField("MASK_IO");
      ThrowRequire(mask);
      wts.Prune(dstmesh, mask);

    } break;

    default:
      Throw() << "Regridding method:" << *regridConserve << " is not implemented";
    }

    // regridTimer
    #ifdef REGRIDTIMING
    MPI_Barrier(MPI_COMM_WORLD);
    rt.regridComplete = MPI_Wtime();
    #endif

    // Redistribute weights in an IO friendly decomposition
    if (Par::Rank() == 0) std::cout << "Writing weights to " << wghtFile << std::endl;
    GatherForWrite(wts);

    // Write the weights
    WriteNCMatFilePar(srcGridFile, dstGridFile, wghtFile,
                      wts, srcmesh, dstmesh, dstmeshcpy,
                      regridConserve, regridMethod, NCMATPAR_ORDER_SEQ);

#ifdef DEBUG
    print_debug_info(wts);
    form_neg_wts_field(wts, srcmesh, src_neg_wts, dstmesh, dst_neg_wts);
    WriteMesh(srcmesh, "srcmesh");
    WriteMesh(dstmesh, "dstmesh");
#endif

  return 1;

}



// This method converts unmasked Mesh nodes to a PointList
 static void _create_pointlist_from_mesh_nodes(Mesh *mesh, UInt pole_constraint_id,PointList **_pointlist) {

   // Coord Pointer
   MEField<> *coord_ptr = mesh->GetCoordField();

   // Mask Pointer
   MEField<> *mask_ptr = mesh->GetField("mask");

   // Loop through counting local unmasked nodes
   int num_points=0;
   MeshDB::iterator ni = mesh->node_begin(), ne = mesh->node_end();
   for (; ni != ne; ++ni) {
     MeshObj &node=*ni;

     // If not local, then go on to next node
     if (!GetAttr(node).is_locally_owned()) continue;

     // If this node is a newly added pole, then skip, because
     // it's not a point in the original grid
     if (GetMeshObjContext(node).is_set(pole_constraint_id)) {
       continue;
     }

     // If masked, then go on to next
     if (mask_ptr) {
       double *msk=mask_ptr->data(node);
       if (*msk > 0.5) continue;
     }

     // Count point
     num_points++;
   }

   // Create PointList
   PointList *pointlist = new ESMCI::PointList(num_points, mesh->spatial_dim());

   // Loop through filling PointList
   ni = mesh->node_begin();
   for (; ni != ne; ++ni) {
     MeshObj &node=*ni;

     // If not local, then go on to next node
     if (!GetAttr(node).is_locally_owned()) continue;

     // If this node is a newly added pole, then skip, because
     // it's not a point in the original grid
     if (GetMeshObjContext(node).is_set(pole_constraint_id)) {
       continue;
     }

     // If masked, then go on to next
     if (mask_ptr) {
       double *msk=mask_ptr->data(node);
       if (*msk > 0.5) continue;
     }

     // Get pointer to coords of this node
     double *coords=coord_ptr->data(node);

     // Add point
     pointlist->add(node.get_id(), coords);
   }


  // Output
  *_pointlist=pointlist;

}

#if 0
 // This hasn't been tested yet, but I left it in in case I need it later.
static void _create_pointlist_of_mesh_elems_not_in_wmat(Mesh *mesh, WMat &wts, PointList **_missing_points) {

  // Get element coordinate Field
  MEField<> *cptr = mesh->GetField("elem_coordinates");

  // If there aren't element coordinates we can't make a pointlist
  if (!cptr) {
    Throw() << " Can't extrapolate to cell/elem centers there are no coordinates there.";
  }

  // Get mask Field
  MEField<> *mptr = mesh->GetField("elem_mask");

  // Get weight iterators
  WMat::WeightMap::iterator wi =wts.begin_row(),we = wts.end_row();

  // Get mesh node iterator that goes through in order of id
  Mesh::MeshObjIDMap::const_iterator ei=mesh->map_begin(MeshObj::ELEMENT), ee=mesh->map_end(MeshObj::ELEMENT);

  // Count all points that don't have weights
  int num_missing=0;
  for (; ei != ee; ++ei) {
    const MeshObj &elem=*ei;

    // Skip non local nodes
    if (!GetAttr(elem).is_locally_owned()) continue;

    // Skip masked elements
    if (mptr != NULL) {
      double *m=mptr->data(elem);
      if (*m > 0.5) continue;
    }

    // get node id
    int elem_id=elem.get_id();

    // get weight id
    int wt_id=wi->first.id;

    // Advance weights until not less than elem id
    while ((wi != we) && (wi->first.id <elem_id)) {
      wi++;
    }

    // If teh current weight is not equal to the node id, then we must have passed it,
    // so count it.
    if (wi->first.id != elem_id) {
      num_missing++;
    }
  }

  // Create Pointlist
  PointList *missing_points = new ESMCI::PointList(num_missing, mesh->spatial_dim());

  // Get weight iterators
  wi =wts.begin_row();

  // Get mesh elem iterator that goes through in order of id
  ei=mesh->map_begin(MeshObj::ELEMENT);

  // Count all points that don't have weights
  for (; ei != ee; ++ei) {
    const MeshObj &elem=*ei;

    // Skip non local nodes
    if (!GetAttr(elem).is_locally_owned()) continue;

    // Skip masked elements
    if (mptr != NULL) {
      double *m=mptr->data(elem);
      if (*m > 0.5) continue;
    }

    // get node id
    int elem_id=elem.get_id();

    // get weight id
    int wt_id=wi->first.id;

    // Advance weights until not less than elem id
    while ((wi != we) && (wi->first.id <elem_id)) {
      wi++;
    }

    // If the current weight is not equal to the elem id, then we must have passed it, so add it to the list
    if (wi->first.id != elem_id) {
      double *elem_coord=cptr->data(elem);
      missing_points->add(elem_id, elem_coord);
    }
  }

  // Output
  *_missing_points=missing_points;
}
#endif

// Get the list of ids in the mesh, but not in the wts
// (i.e. if mesh is the dest. mesh, the unmapped points)
static void _create_pointlist_of_points_not_in_wmat(PointList *pointlist, WMat &wts, PointList **_missing_points) {

  // Get weight iterators
  WMat::WeightMap::iterator wi =wts.begin_row(),we = wts.end_row();

  // Count number of missing points
  int num_missing=0;
  int curr_num_pts = pointlist->get_curr_num_pts();
  for (int i=0; i<curr_num_pts; i++) {

    // get node id
    int id = pointlist->get_id(i);

    // get weight id
    int wt_id=wi->first.id;

    // Advance weights until not less than node id
    while ((wi != we) && (wi->first.id < id)) {
      wi++;
    }

    // If the current weight is not equal to the node id, then we must have passed it, so add it to the list
    if (wi->first.id != id) {
      num_missing++;
    }
  }

  // Create Pointlist
  PointList *missing_points = new ESMCI::PointList(num_missing, pointlist->get_coord_dim());

  // Add missing points to PointList
  wi =wts.begin_row();
  for (int i=0; i<curr_num_pts; i++) {

    // get node id
    int id = pointlist->get_id(i);

    // get weight id
    int wt_id=wi->first.id;

    // Advance weights until not less than node id
    while ((wi != we) && (wi->first.id < id)) {
      wi++;
    }

    // If teh current weight is not equal to the node id, then we must have passed it, so add it to the list
    if (wi->first.id != id) {
      missing_points->add(id, pointlist->get_coord_ptr(i));
    }
  }

  // Output
  *_missing_points=missing_points;
}

 void _replace_mapped_with_mapped_extrap(WMat &status) {

   WMat::WeightMap::iterator wi = status.begin_row(), we = status.end_row();
   for (; wi != we; ++wi) {

     // Get row and column info
     const WMat::Entry &row = wi->first;
     std::vector<WMat::Entry> &col = wi->second;

     // Loop column
     for (int i = 0; i < col.size(); ++i) {
       WMat::Entry &entry = col[i];

       if (entry.id == ESMC_REGRID_STATUS_MAPPED) entry.id=ESMC_REGRID_STATUS_EXTRAP_MAPPED;
     }
   }
 }

 // Do extrapolation. The wts structure which comes out will have the new weights for the extrapolation merged in
 // TODO: move to another file
 void extrap(Mesh *srcmesh, PointList *srcpointlist, Mesh *dstmesh, PointList *dstpointlist,
             IWeights &wts,
             MAP_TYPE mtype,
             UInt pole_constraint_id, // Only valid when srcmesh exists
             int extrapMethod,
             int extrapNumSrcPnts,
             ESMC_R8 extrapDistExponent,
             bool set_dst_status, WMat &dst_status) {


   //  printf("BOB: in extrap() extrapMethod=%d\n",extrapMethod);

   // Construct pointlist for srcmesh
   // We need a pointlist to be able to use nearest neighbor
   // TODO: Change nearest neighor weight calc method, so it can
   //       optionally run on a mesh to avoid this step and make things more efficient
   PointList *srcpointlist_extrap=NULL;
   PointList *srcpointlist_from_mesh=NULL;
   if (srcmesh != NULL) {
     _create_pointlist_from_mesh_nodes(srcmesh, pole_constraint_id, &srcpointlist_from_mesh);
     srcpointlist_extrap=srcpointlist_from_mesh;
   } else if (srcpointlist != NULL) {
     srcpointlist_extrap=srcpointlist;
   } else {
     Throw() << "No destination geometry object.";
   }

   // DEBUG
   //srcpointlist_extrap->WriteVTK("src_pl");

   // Construct a new point list which just contains destination points not in the weight matrix
   PointList *missing_points=NULL;
   if (dstmesh != NULL) {
     Throw() << "Conservative methods not supported in extrapolation, because extrapolation would cause the methods to no longer be conservative.";
   } else if (dstpointlist != NULL) {
     _create_pointlist_of_points_not_in_wmat(dstpointlist, wts, &missing_points);
   } else {
     Throw() << "No destination geometry object.";
   }

   // DEBUG
   //missing_points->WriteVTK("missing_pl");

   // Translate extrap method to regrid method
   int regridMethod;
   if (extrapMethod == ESMC_EXTRAPMETHOD_NEAREST_STOD) {
     regridMethod=ESMC_REGRID_METHOD_NEAREST_SRC_TO_DST;
   } else if (extrapMethod == ESMC_EXTRAPMETHOD_NEAREST_IDAVG) {
     regridMethod=ESMC_REGRID_METHOD_NEAREST_IDAVG;
   } else {
     Throw() << "unrecognized extrapolation method";
   }

   // Set info for calling into interp
   IWeights extrap_wts;
   WMat extrap_dst_status;

   // Build the rendezvous grids
   Interp interp((Mesh *)NULL, srcpointlist_extrap,(Mesh *)NULL, missing_points,
                 (Mesh *)NULL, false, regridMethod,
                 set_dst_status, extrap_dst_status,
                 mtype, ESMCI_UNMAPPEDACTION_IGNORE, extrapNumSrcPnts,
                 extrapDistExponent);

   // Create the weight matrix
   interp(0, extrap_wts, set_dst_status, extrap_dst_status);

   // Merge extrap weights into regridding matrix
   wts.MergeDisjoint(extrap_wts);

   // If status was requested merge that too
   if (set_dst_status) {
     // Change from ...MAPPED to ...MAPPED_EXTRAP
     _replace_mapped_with_mapped_extrap(extrap_dst_status);

     // Replace old status with extrap ones
     dst_status.MergeReplace(extrap_dst_status);
   }

   // Cleanup
   if (srcpointlist_from_mesh != NULL) delete srcpointlist_from_mesh;
   if (missing_points != NULL) delete missing_points;
 }


 int regrid(Mesh *srcmesh, PointList *srcpointlist, Mesh *dstmesh, PointList *dstpointlist,
            Mesh *midmesh, IWeights &wts,
            int *regridMethod, int *regridScheme,
            int *regridPoleType, int *regridPoleNPnts,
            int *map_type,
            int *extrapMethod,
            int *extrapNumSrcPnts,
            ESMC_R8 *extrapDistExponent,
            int *unmappedaction,
            bool set_dst_status, WMat &dst_status) {


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

      // Build the rendezvous grids
      Interp interp(srcmesh, srcpointlist, dstmesh, dstpointlist,
                    midmesh, false, *regridMethod,
                    set_dst_status, dst_status,
                    mtype, *unmappedaction);


      // Create the weight matrix
      interp(0, wts, set_dst_status, dst_status);

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
              set_dst_status, dst_status);
     }

    return 1;
  }


  // csrv - Args are NON-COMMITTED meshes
  int csrv(Mesh &srcmesh, Mesh &dstmesh, IWeights &wts,
           MEField<> *src_iwts, MEField<> *dst_iwts,
           int *regridMethod, int *regridScheme,
           int *regridPoleType, int *regridPoleNPnts,
           int *unmappedaction) {


    // generate integration weights before pole, so
    // they are distributed across non-pole nodes
    // (the node is factored out in the end)
    Integrate sig(srcmesh), dig(dstmesh);

    // Clear weights
    sig.clearWeights(src_iwts);
    dig.clearWeights(dst_iwts);

    // Add weights to meshes before poles
    // so all the weights are on user data points
    if ((*regridScheme == ESMC_REGRID_SCHEME_FULL3D) ||
        (*regridScheme == ESMC_REGRID_SCHEME_DCON3DWPOLE) ||
        (*regridScheme == ESMC_REGRID_SCHEME_FULLTOREG3D)) {
      for (UInt i = 1; i <= 7; ++i) {
        sig.AddPoleWeights(srcmesh,i,src_iwts);
        dig.AddPoleWeights(dstmesh,i,dst_iwts);
      }
    }

    // Add in other none-pole weights
    // (and do cross processor sum)
    sig.intWeights(src_iwts);
    dig.intWeights(dst_iwts);

#if 0
  // print out info of the iwts
  Mesh::iterator sni=srcmesh.node_begin(), sne=srcmesh.node_end();
  Mesh::iterator dni=dstmesh.node_begin(), dne=dstmesh.node_end();

  double ssum=0.0;
  for (; sni != sne; ++sni) {
    double *Sdata = src_iwts->data(*sni);
    ssum += *Sdata;
  }

  double dsum=0.0;
 for (; dni != dne; ++dni) {
    double *Ddata = dst_iwts->data(*dni);

    dsum += *Ddata;
  }

    printf("SW Sum=%20.17f \n",ssum);
    printf("DW Sum=%20.17f \n",dsum);
#endif

    // Pole constraints
    IWeights pole_constraints, stw;
    UInt constraint_id = dstmesh.DefineContext("pole_constraints");
    if ((*regridScheme == ESMC_REGRID_SCHEME_FULL3D) ||
        (*regridScheme == ESMC_REGRID_SCHEME_DCON3DWPOLE) ||
        (*regridScheme == ESMC_REGRID_SCHEME_FULLTOREG3D)) {
      if (*regridPoleType == ESMC_REGRID_POLETYPE_ALL) {
        for (UInt i = 1; i <= 7; ++i)
          MeshAddPole(dstmesh, i, constraint_id, pole_constraints);
      } else if (*regridPoleType == ESMC_REGRID_POLETYPE_NPNT) {
        for (UInt i = 1; i <= 7; ++i)
          MeshAddPoleNPnts(dstmesh, *regridPoleNPnts, i, constraint_id, pole_constraints);
      } else if (*regridPoleType == ESMC_REGRID_POLETYPE_TEETH) {
        for (UInt i = 1; i <= 7; ++i)
          MeshAddPoleTeeth(dstmesh, i, constraint_id, pole_constraints);
      }
    }

    // Get coordinate fields
    MEField<> &scoord = *srcmesh.GetCoordField();
    MEField<> &dcoord = *dstmesh.GetCoordField();

    // Create a layer of ghost elements since the patch method needs
    // a larger stencil.
    if (*regridMethod == ESMC_REGRID_METHOD_PATCH) {
      int num_snd=0;
      MEField<> *snd[3],*rcv[3];

      // Load coord field
      MEField<> *pdc = &dcoord;
      snd[num_snd]=pdc;
      rcv[num_snd]=pdc;
      num_snd++;

      // Load mask field
      MEField<> *pdm = dstmesh.GetField("mask");
      if (pdm != NULL) {
        snd[num_snd]=pdm;
        rcv[num_snd]=pdm;
        num_snd++;
      }

      // Load iwts field
      MEField<> *pdw = dstmesh.GetField("iwts");
      if (pdw != NULL) {
        snd[num_snd]=pdw;
        rcv[num_snd]=pdw;
        num_snd++;
      }

      dstmesh.CreateGhost();
      dstmesh.GhostComm().SendFields(num_snd, snd, rcv);
    }

    // tmp variables
    bool tmp_set_dst_status=false;
    WMat tmp_dst_status;

    // Build the rendezvous grids
    Interp interp(&dstmesh, NULL, &srcmesh, NULL, NULL, false, *regridMethod,
                  tmp_set_dst_status, tmp_dst_status,
                  MAP_TYPE_CART_APPROX, *unmappedaction);

    // Generate the backwards interpolation matrix
    interp(0, stw, tmp_set_dst_status, tmp_dst_status);


     // Factor out poles if they exist
    if ((*regridScheme == ESMC_REGRID_SCHEME_FULL3D) ||
        (*regridScheme == ESMC_REGRID_SCHEME_DCON3DWPOLE) ||
        (*regridScheme == ESMC_REGRID_SCHEME_FULLTOREG3D)) {
      if (*regridPoleType == ESMC_REGRID_POLETYPE_ALL) {
        stw.GatherToCol(pole_constraints);
        stw.AssimilateConstraints(pole_constraints);
      } else if (*regridPoleType == ESMC_REGRID_POLETYPE_NPNT) {
        stw.GatherToRowSrc(pole_constraints);
        stw.AssimilateConstraintsNPnts(pole_constraints);
      }
    }

    // L2 projection conservative interpolation
    interp.interpL2csrvM(stw, &wts, src_iwts, dst_iwts);

  // print out info of the iwts
  Mesh::iterator sni=srcmesh.node_begin(), sne=srcmesh.node_end();
  Mesh::iterator dni=dstmesh.node_begin(), dne=dstmesh.node_end();

#ifdef DEBUG
  int snegcount = 0;
  int stotalcount = 0;
  for (; sni != sne; ++sni) {
    double *Sdata = src_iwts->data(*sni);
    stotalcount++;
    if (*Sdata < 0) ++snegcount;
  }

  int dnegcount = 0;
  int dtotalcount = 0;
  for (; dni != dne; ++dni) {
    double *Ddata = dst_iwts->data(*dni);
    dtotalcount++;
    if (*Ddata < 0) ++dnegcount;
  }
#endif

  return 1;
  }

  // to generate the iwts again, and return to Fortran
  int get_iwts(Mesh &mesh, MEField<> *iwts, int *regridScheme) {

    // generate integration weights
    Integrate ig(mesh);

    // Clear weights
    ig.clearWeights(iwts);

    // Add weights to meshes before poles
    // so all the weights are on user data points
    if ((*regridScheme == ESMC_REGRID_SCHEME_FULL3D) ||
        (*regridScheme == ESMC_REGRID_SCHEME_DCON3DWPOLE) ||
        (*regridScheme == ESMC_REGRID_SCHEME_FULLTOREG3D)) {
      for (UInt i = 1; i <= 7; ++i)
        ig.AddPoleWeights(mesh,i,iwts);
    }

    // Add in other none-pole weights
    // (and do cross processor sum)
    ig.intWeights(iwts);

    return 1;
  }

}
