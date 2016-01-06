// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================

#include <Mesh/include/ESMCI_MeshRegrid.h>
#include <Mesh/include/ESMCI_MeshRead.h>

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
		    int *map_type, int *unmappedaction) {


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
                  regridPoleType, regridPoleNPnts, map_type, unmappedaction)) {
        Throw() << "Regridding error" << std::endl;
      }

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

      int map_type=0;
      if (!regrid(&srcmesh, NULL, &dstmesh, NULL, NULL, wts, regridMethod, &regridScheme,
                  regridPoleType, regridPoleNPnts, &map_type,  &unmappedaction))
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

 int regrid(Mesh *srcmesh, PointList *srcpointlist, Mesh *dstmesh, PointList *dstpointlist, 
	    Mesh *midmesh, IWeights &wts,
	    int *regridMethod, int *regridScheme, 
	    int *regridPoleType, int *regridPoleNPnts, 
	    int *map_type, int *unmappedaction) {


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


    IWeights pole_constraints, stw;
    if (maybe_pole) {
      // Pole constraints
      UInt constraint_id = srcmesh->DefineContext("pole_constraints");
      if (*regridPoleType == ESMC_REGRID_POLETYPE_ALL) {
        for (UInt i = 1; i <= 7; ++i)
          MeshAddPole(*srcmesh, i, constraint_id, pole_constraints);
      } else if (*regridPoleType == ESMC_REGRID_POLETYPE_NPNT) {
        for (UInt i = 1; i <= 7; ++i)
          MeshAddPoleNPnts(*srcmesh, *regridPoleNPnts, i, constraint_id, pole_constraints);
      } else if (*regridPoleType == ESMC_REGRID_POLETYPE_TEETH) {
        for (UInt i = 1; i <= 7; ++i)
          MeshAddPoleTeeth(*srcmesh, i, constraint_id, pole_constraints);
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

    // Convert to map type
    MAP_TYPE mtype;
    if (*map_type==0) mtype=MAP_TYPE_CART_APPROX;
    else if (*map_type==1) mtype=MAP_TYPE_GREAT_CIRCLE;
    else Throw() << "Unrecognized map type";


    // Build the rendezvous grids
    Interp interp(srcmesh, srcpointlist, dstmesh, dstpointlist, 
		  midmesh, false, *regridMethod, mtype, *unmappedaction);
    
     // Create the weight matrix
     interp(0, wts);

     // Release the Zoltan struct if we used it for the mid mesh
     if(midmesh) interp.release_zz();

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


    // Build the rendezvous grids
    Interp interp(&dstmesh, NULL, &srcmesh, NULL, NULL, false, *regridMethod, MAP_TYPE_CART_APPROX, *unmappedaction);

    // Generate the backwards interpolation matrix
    interp(0, stw);


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
