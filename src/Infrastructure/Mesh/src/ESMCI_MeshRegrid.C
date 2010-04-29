// $Id: ESMCI_MeshRegrid.C,v 1.7 2010/04/29 19:25:18 rokuingh Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================

#include <Mesh/include/ESMCI_MeshRegrid.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMCI_MeshRegrid.C,v 1.7 2010/04/29 19:25:18 rokuingh Exp $";
//-----------------------------------------------------------------------------

namespace ESMCI {

// Meshes are already committed
int online_regrid(Mesh &srcmesh, Mesh &dstmesh, IWeights &wts,
                  int *regridConserve, int *regridMethod, 
                  int *regridScheme, int *unmappedaction) {

  // Conflict management
  int regridPoleType = ESMC_REGRID_POLETYPE_ALL;
  int regridPoleNPnts = 0;

    // Conservative regridding
    switch (*regridConserve) {
    case (ESMC_REGRID_CONSERVE_ON): {

      // Get the integration weights
      MEField<> *src_iwts = srcmesh.GetField("iwts");
      if (!src_iwts) Throw() << "Integration weights needed for conservative regridding."
                             <<std::endl;
      MEField<> *dst_iwts = dstmesh.GetField("iwts");
      if (!dst_iwts) Throw() << "Integration weights needed for conservative regridding."
                             <<std::endl;

      if (!csrv(srcmesh, dstmesh, wts, src_iwts, dst_iwts, regridMethod, regridScheme, 
                &regridPoleType, &regridPoleNPnts, unmappedaction))
        Throw() << "Conservative regridding error" << std::endl;

    } break;
    // NON Conservative regridding
    case (ESMC_REGRID_CONSERVE_OFF): {

      if (!regrid(srcmesh, dstmesh, wts, regridMethod, regridScheme, 
                &regridPoleType, &regridPoleNPnts, unmappedaction))
        Throw() << "Regridding error" << std::endl;

      // Remove non-locally owned weights (assuming destination mesh decomposition)
      wts.Prune(dstmesh, 0);

    } break;

    default:
      Throw() << "Regridding method:" << *regridConserve << " is not implemented";
    }

  return 1;
}

// Mesh are not committed yet
int offline_regrid(Mesh &srcmesh, Mesh &dstmesh,
             int *regridConserve, int *regridMethod, 
             int *regridPoleType, int *regridPoleNPnts,
             char *srcGridFile, char *dstGridFile, char *wghtFile) {

  // Conflict management
  int regridScheme = ESMC_REGRID_SCHEME_NATIVE;
  int unmappedaction = ESMC_UNMAPPEDACTION_ERROR;

  IWeights wts;
  MEField<> *src_iwts, *dst_iwts;

    switch (*regridConserve) {

    // Conservative regridding
    case (ESMC_REGRID_CONSERVE_ON): {

      // Add fields to mesh
      Context ctxt; ctxt.flip();
      src_iwts = srcmesh.RegisterField("iwts",
        MEFamilyStd::instance(), MeshObj::ELEMENT, ctxt, 1, true);

      dst_iwts = dstmesh.RegisterField("iwts",
        MEFamilyStd::instance(), MeshObj::ELEMENT, ctxt, 1, true);

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

      if (!regrid(srcmesh, dstmesh, wts, regridMethod, &regridScheme,
                  regridPoleType, regridPoleNPnts, &unmappedaction))
        Throw() << "Regridding error" << std::endl;

      // the mask
      MEField<> *mask = dstmesh.GetField("MASK_IO");
      ThrowRequire(mask);
      wts.Prune(dstmesh, mask);

    } break;

    default:
      Throw() << "Regridding method:" << *regridConserve << " is not implemented";
    }

    // Redistribute weights in an IO friendly decomposition
    if (Par::Rank() == 0) std::cout << "Writing weights to " << wghtFile << std::endl;
    GatherForWrite(wts);

    // Write the weights
    WriteNCMatFilePar(srcGridFile, dstGridFile, wghtFile,
                      wts, *src_iwts, *dst_iwts, srcmesh, dstmesh, NCMATPAR_ORDER_SEQ);

  return 1;

}

int regrid(Mesh &srcmesh, Mesh &dstmesh, IWeights &wts,
           int *regridMethod, int *regridScheme, 
           int *regridPoleType, int *regridPoleNPnts, 
           int *unmappedaction) {

    // Pole constraints
    IWeights pole_constraints, stw;
    UInt constraint_id = srcmesh.DefineContext("pole_constraints");

    if (*regridScheme == ESMC_REGRID_SCHEME_FULL3D) {
      if (*regridPoleType == ESMC_REGRID_POLETYPE_ALL) {
        for (UInt i = 1; i <= 7; ++i)
          MeshAddPole(srcmesh, i, constraint_id, pole_constraints);
      } else if (*regridPoleType == ESMC_REGRID_POLETYPE_NPNT) {
        for (UInt i = 1; i <= 7; ++i)
          MeshAddPoleNPnts(srcmesh, *regridPoleNPnts, i, constraint_id, pole_constraints);
      } else if (*regridPoleType == ESMC_REGRID_POLETYPE_TEETH) {
        for (UInt i = 1; i <= 7; ++i)
          MeshAddPoleTeeth(srcmesh, i, constraint_id, pole_constraints);
      }
    }

    // Get coordinate fields
    MEField<> &scoord = *srcmesh.GetCoordField();
    MEField<> &dcoord = *dstmesh.GetCoordField();

    // Create a layer of ghost elements since the patch method needs
    // a larger stencil.
    if (*regridMethod == ESMC_REGRID_METHOD_PATCH) {
      int num_snd=0;
      MEField<> *snd[2],*rcv[2];

      // Load coord field
      MEField<> *psc = &scoord;
      snd[num_snd]=psc;
      rcv[num_snd]=psc;
      num_snd++;

      // Load mask field
      MEField<> *psm = srcmesh.GetField("mask");
      if (psm != NULL) {
        snd[num_snd]=psm;
        rcv[num_snd]=psm;
        num_snd++;
      }

      srcmesh.CreateGhost();
      srcmesh.GhostComm().SendFields(num_snd, snd, rcv);
    }

    // make the field pairs for interpolation
    std::vector<Interp::FieldPair> fpairs;
    if (*regridMethod == ESMC_REGRID_METHOD_BILINEAR)
      fpairs.push_back(Interp::FieldPair(&scoord, &dcoord, Interp::INTERP_STD));
    else if (*regridMethod == ESMC_REGRID_METHOD_PATCH)
      fpairs.push_back(Interp::FieldPair(&scoord, &dcoord, Interp::INTERP_PATCH));

     // Build the rendezvous grids
     Interp interp(srcmesh, dstmesh, fpairs, *unmappedaction);

     // Create the weight matrix
     interp(0, wts);

     // Factor out poles if they exist
     if (*regridScheme == ESMC_REGRID_SCHEME_FULL3D) {
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

    // Pole constraints
    IWeights pole_constraints, stw;
    UInt constraint_id = dstmesh.DefineContext("pole_constraints");

    if (*regridScheme == ESMC_REGRID_SCHEME_FULL3D) {
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

    // generate integration weights
    Integrate sig(srcmesh), dig(dstmesh);
    sig.intWeights(src_iwts);
    dig.intWeights(dst_iwts);

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

    // make the field pairs for interpolation
    std::vector<Interp::FieldPair> fpairs;
    if (*regridMethod == ESMC_REGRID_METHOD_BILINEAR)
      fpairs.push_back(Interp::FieldPair(&dcoord, &scoord, Interp::INTERP_STD));
    else if (*regridMethod == ESMC_REGRID_METHOD_PATCH)
      fpairs.push_back(Interp::FieldPair(&dcoord, &scoord, Interp::INTERP_PATCH));

    // Build the rendezvous grids
    Interp interp(dstmesh, srcmesh, fpairs, *unmappedaction);

    // Generate the backwards interpolation matrix
    interp(0, stw);

     // Factor out poles if they exist
     if (*regridScheme == ESMC_REGRID_SCHEME_FULL3D) {
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
/*
  // print out info of the iwts
  Mesh::iterator sni=srcmesh.node_begin(), sne=srcmesh.node_end();
  Mesh::iterator dni=dstmesh.node_begin(), dne=dstmesh.node_end();

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

  // print out info of weight matrix
  int negcount = 0;
  int rowsum = 0;
  int rowsumcount = 0;
  int totalcount = 0;
  int gt1count = 0;
  double max = 0;
  double min = 0;
double badcolid = 0;
double badrowid = 0;

  IWeights::WeightMap::iterator wit = wts.begin_row(), wet = wts.end_row();
  //IWeights::WeightMap::iterator wit = stw.begin_row(), wet = stw.end_row();
  for (; wit != wet; ++wit) {
    const IWeights::Entry &_row = wit->first;
    const std::vector<IWeights::Entry> &_col = wit->second;

    std::cout<<Par::Rank()<<"  "<<_row.id<<"    ";
    rowsum = 0;
    for (UInt c = 0; c < _col.size(); ++c) {
      double value = _col[c].value;
      if (value < 0) negcount++;
      if (value > max) {
        max = value;
        badcolid = _col[c].id;
        badrowid = _row.id;
      }
      if (value < min) min = value;
      if (value > 1) gt1count++;
      rowsum += value;

      std::cout<<std::setprecision(3)<<_col[c].value<<"  ";
    }
    if (rowsum > 1.01 || rowsum < .99) rowsumcount++;
    totalcount++;
    std::cout<<std::endl;
    for (UInt c = 0; c < _col.size(); ++c) {
      double value = _col[c].value;
      
    }
  }
  std::cout<<std::endl<<"Negative weights count = "<<negcount
           <<std::endl<<"Greater than 1 count = "<<gt1count
           <<std::endl<<"Row sum not 1 count = "<<rowsumcount
           <<std::endl<<"Total row count = "<<totalcount<<std::endl
           <<std::endl<<"Max weight  = "<<max
           <<std::endl<<"Min weight = "<<min<<std::endl;

  std::cout<<std::setprecision(4)<<std::endl<<"Bad weight ["<<badrowid<<","<<badcolid<<"]"<<std::endl<<std::endl;

  std::cout<<std::endl<<"Source iwts total count = "<<stotalcount
                      <<"  and negcount = "<<snegcount<<std::endl;
  std::cout<<std::endl<<"Destination iwts total count = "<<dtotalcount
                      <<"  and negcount = "<<dnegcount<<std::endl<<std::endl;
*/

    return 1;
  }

}
