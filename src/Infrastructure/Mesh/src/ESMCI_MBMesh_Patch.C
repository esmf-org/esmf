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

#include <Mesh/include/Regridding/ESMCI_Interp.h>

#include <Mesh/include/ESMCI_MBMesh.h>
#include <Mesh/include/ESMCI_MBMesh_Patch.h>
#include <Mesh/include/ESMCI_MBMesh_BBox.h>
#include <Mesh/include/ESMCI_MBMesh_Search_EtoP.h>
#include <Mesh/include/ESMCI_MBMesh_Util.h>
#include <Mesh/include/ESMCI_MBMesh_Rendez_EtoP.h>

#include <Mesh/include/ESMCI_MBMesh_Bilinear.h> // for pcoords, add to class?

#include <Mesh/include/Legacy/ESMCI_ParEnv.h>

#include "ESMCI_PointList.h"

#include <iostream>
#include <iterator>
#include <iomanip>
#include <cmath>
#include <vector>
#include <map>

#include <ESMCI_VM.h>
#include <ESMCI_LogErr.h>

#include "ESMCI_TraceMacros.h"  // for profiling

using std::vector;
using std::iterator;

// #define DEBUG_POINTLIST
// #define DEBUG_WEIGHTS
//#define ESMF_REGRID_DEBUG_MAP_NODE 4323801

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------


#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif


using namespace ESMCI;

void calc_patch_mat(MBMesh *mbmp, PointList *dplp, 
                    MBMesh_Search_EToP_Result_List &sres, IWeights &iw) {
// void mat_patch_serial_transfer(MEField<> &src_coord_field, MEField<> &_sfield, SearchResult &sres,  Mesh *srcmesh, IWeights &iw, PointList *dstpointlist) {
#undef  ESMC_METHOD
#define ESMC_METHOD "calc_patch_mat()"
  try {
    int localrc, merr;

    // Initialize the parallel environment for mesh (if not already done)
    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_THROW(localrc);

    VM *vm = VM::getCurrent(&localrc);
    int petCount = vm->getPetCount();
    int localPet = vm->getLocalPet();
    ESMC_CHECK_THROW(localrc);


    Throw () << "Patch weight generation not yet implemented.";
#if 0
  const int pdeg = 2; // TODO: deduce this.  For instance, deg source + 1 is reasonable

  // Get mask field pointer
  MEField<> *src_mask_ptr = srcmesh->GetField("mask");

  MEField<>* field = &_sfield;
  MEField<> &sfield = _sfield;
   UInt nrhs = field->dim();


   // Create the sensitivity field
  MEField<SField> sF(sfield);
  MEField<SField> *sFp = &sF;

  // Create the  recovery field
  SearchResult::iterator sb = sres.begin(), se = sres.end();

  int dstpointlist_dim=dstpointlist->get_coord_dim();

  for (; sb != se; sb++) {

    Search_result &sres = **sb;

    // Trick:  Gather the data from the source field so we may call interpolate point
    MeshObj &elem = const_cast<MeshObj&>(*(*sb)->elem);

    // Create a sensitivity field residing on the degrees of
    // freedom used in this interpolation.
    std::set<MeshObj*> elems;

    MeshObjConn::NeighborElements(elem, elems);

     UInt nlocal_dof = sF.AssignElements(elems.begin(), elems.end());

    std::vector<fad_type> fads(nlocal_dof, 0);

     sF.ReInit(&fads[0]);

     // Set up the fad degrees of freedom.
    for (UInt i = 0; i < nlocal_dof; i++) {
      fads[i].diff(i, nlocal_dof);
    }

    ElemPatch<MEField<SField>, fad_type> epatch;

    epatch.CreateElemPatch(pdeg, ElemPatch<>::GAUSS_PATCH,
                           elem,
                           src_coord_field,
                           src_mask_ptr,
                           1,
                           &sFp,
                           700000
                            );

    // Gather parametric coords into an array.
    UInt pdim = GetMeshObjTopo(elem)->parametric_dim;
    UInt npts = sres.nodes.size(); // number of points to interpolate
    std::vector<double> pc(pdim*npts);

    for (UInt np = 0; np < npts; np++) {

      for (UInt pd = 0; pd < pdim; pd++) {

        pc[np*pdim+pd] = sres.nodes[np].pcoord[pd];

       }

    }

    std::vector<fad_type> result(nrhs*npts);
    epatch.Eval(npts, &pc[0], &result[0]);

     // Now copy data into fields and save sensitivies
    for (UInt n = 0; n < npts; n++) {

      // DEBUG printf(">>>> snode=%d# elem=%d \n",snode.get_id(),elem.get_id());

      //     UInt fdim = dfield->dim();

      //for (UInt d = 0; d < fdim; d++) {
      for (UInt d = 0; d < 1; d++) { // weights are redundant per entry

        // DON'T ACTUALLY DO REGRID BECAUSE WE DON'T USE IT
        // data[d] = result[n*nrhs+d].val();

        IWeights::Entry row(sres.nodes[n].dst_gid, d, 0.0, elem.get_id());

        std::vector<IWeights::Entry> col;
        col.reserve(nlocal_dof);

#ifdef CHECK_SENS
        Par::Out() << "sens=" << result[n*nrhs+d] << std::endl;
        double sval = 0;
#endif

        double *sens = &(result[n*nrhs+d].fastAccessDx(0));

        dof_add_col addc(col, dstpointlist_dim, sens);

        sF.dof_iterator(addc);

        iw.InsertRow(row, col);

#ifdef CHECK_SENS
        int dof_div_dim = nlocal_dof/dstpointlist_dim;
        for (UInt s = 0; s < dof_div_dim; s++) {
          sval += fads[s*nrhs+d].val()*sens[s*nrhs+d];
        } // for s

        double diff = sval - result[n*nrhs+d].val();
        Par::Out() << "**diff=" << diff << std::endl;
        if (std::abs(diff) > 1e-4) {

          for (UInt s = 0; s < nlocal_dof/dstpointlist_dim; s++) {
            Par::Out() << fads[s*nrhs+d].val() << " ";
          }
          Par::Out() << std::endl;
        }
#endif

      } // for d

     } // for np

  } // for searchresult

#endif // all old code ifdefed out
  } 
  CATCH_MBMESH_RETHROW
}

void calc_patch_regrid_wgts(MBMesh *mbmp, PointList *dplp, IWeights &wts, 
                       int *map_type, bool set_dst_status, WMat &dst_status) {
#undef  ESMC_METHOD
#define ESMC_METHOD "calc_patch_regrid_wgts()"
  try {
    int localrc, merr;

    // Initialize the parallel environment for mesh (if not already done)
    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_THROW(localrc);

    VM *vm = VM::getCurrent(&localrc);
    int petCount = vm->getPetCount();
    int localPet = vm->getLocalPet();
    ESMC_CHECK_THROW(localrc);


    Throw () << "Patch weight generation not yet implemented.";

    // Set meshes to use for regrid weight calculations
    MBMesh *mbmp_regrid=mbmp;
    PointList *dplp_regrid=dplp;

#ifdef DEBUG_POINTLIST
    {printf("%d# calc_patch_regrid_wgts (%d) [", Par::Rank(), dplp->get_curr_num_pts());
    for (int p = 0; p < dplp->get_curr_num_pts(); ++p) {
      const int *id = dplp->get_id_ptr(p);
      double coords[3];
      dplp->get_coord(p, &coords[0]);
       printf("%d [%f,%f,%f], ", dplp->get_id_ptr(p), coords[0], coords[1], coords[2]);
    }
    printf("]\n");}
#endif
 
    // If parallel then generate rendezvous meshes...and use them instead
    MBMesh *mbmp_rend = nullptr;
    PointList *dplp_rend = nullptr;
    if (petCount > 1) {

      // Create rendez meshes
      ESMCI_REGRID_TRACE_ENTER("MBMesh regrid patch rendezvous");
      create_rendez_mbmesh_etop(mbmp, dplp, &mbmp_rend, &dplp_rend, map_type);
      ESMCI_REGRID_TRACE_EXIT("MBMesh regrid patch rendezvous");

      // Use rendezvous meshes instead
      mbmp_regrid=mbmp_rend;
      dplp_regrid=dplp_rend;
    }

    // Do search
    ESMCI_REGRID_TRACE_ENTER("MBMesh regrid patch search");
    MBMesh_Search_EToP_Result_List result;
    MBMesh_Search_EToP(mbmp_regrid, 
                        dplp_regrid, ESMCI_UNMAPPEDACTION_IGNORE,
                        map_type, 1.0E-8, result, 
                        set_dst_status, dst_status, NULL, NULL);
    ESMCI_REGRID_TRACE_EXIT("MBMesh regrid patch search");

    // Calculate the patch weight matrix
    ESMCI_REGRID_TRACE_ENTER("MBMesh regrid patch calculate weights");
    calc_patch_mat(mbmp_regrid, dplp_regrid, result, wts);
    ESMCI_REGRID_TRACE_EXIT("MBMesh regrid patch calculate weights");

    // If parallel then migrate weights back to decompostion of original mesh
    ESMCI_REGRID_TRACE_ENTER("MBMesh regrid patch migrate weights");
    if (petCount > 1) {
      wts.Migrate(*dplp);
      if (set_dst_status) dst_status.Migrate(*dplp);
    }
    ESMCI_REGRID_TRACE_EXIT("MBMesh regrid patch migrate weights");

    // If parallel then get rid of rendezvous meshes.
    if (petCount > 1) {
      if (mbmp_rend) delete mbmp_rend;
      if (dplp_rend) delete dplp_rend;
    }

  } CATCH_MBMESH_RETHROW
}

#endif // ESMF_MOAB
