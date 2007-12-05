//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <ESMC_ZZest.h>
#include <ESMC_Mesh.h>
#include <ESMC_Kernel.h>
#include <ESMC_Exception.h>
#include <ESMC_MEField.h>
#include <ESMC_ParEnv.h>
#include <ESMC_PatchRecovery.h>
#include <ESMC_MEValues.h>


namespace ESMC {

ZZest::ZZest(Mesh &mesh) :
mesh(mesh)
{
}

ZZest::~ZZest() {
}

void ZZest::Estimate(MEField<> &field, MEField<> &err) {
  Trace __trace("ZZest::Estimate(MEField<> &field, MEField<> &err)");

  MEField<> *fptr = &field;
  MEField<> &coord = *mesh.GetCoordField();
  ThrowRequire(err.is_elemental());

  UInt sdim = mesh.spatial_dim();
  UInt pdim = mesh.parametric_dim();

  bool had_ghost = mesh.HasGhost(); // leave state as it was

  if (!had_ghost) {
    mesh.CreateGhost();
  }

  // Update the field to recover on the ghost
  mesh.GhostComm().SendFields(1, &fptr, &fptr);

  MEValues<> mev(field.GetMEFamily(), &coord);
  
  // for each kernel we keep a map that stores the node patches
  // so we don't have to recompute them.
  ElemPatch<>::NodePatchMap nmap;

  // Loop all kernels where the field lives.
  KernelList::iterator ki = mesh.set_begin(), ke = mesh.set_end();
  for (; ki != ke; ++ki) {

    Kernel &ker = *ki;
    const Context &ctxt = ker.GetContext();

    if (ker.type() != MeshObj::ELEMENT || !ctxt.is_set(Attr::ACTIVE_ID)) continue;

    const MeshObjTopo *etopo = ker.GetTopo();

    MasterElement<> &me = *field.GetMEFamily().getME(etopo->name, METraits<>());
    const intgRule *irule = field.GetMEFamily().GetIntg(etopo->name);

    mev.Setup(ker, MEV::update_sfg | MEV::update_jxw, irule);

    UInt nqpoints = mev.GetNQPoints();
    UInt nfunc = mev.GetNumFunctions();

    // Storage for function grads
    std::vector<double> fgrads(nqpoints*sdim);
    std::vector<double> pgrads(nqpoints*sdim);

    //mev.Setup(ker, MEV::update_sf | MEV::update_map, &pintg);

    Kernel::obj_iterator oi = ker.obj_begin(), oe = ker.obj_end();
 
    for (; oi != oe; ++oi) {

      MeshObj &elem = *oi;
Par::Out() << "zzest elem:" << elem.get_id() << std::endl;

      mev.ReInit(elem);

      // Form a patch
      ElemPatch<> patch(nmap);
//      ElemPatch<> patch;

      patch.CreateElemPatch(me.IntgOrder(),
                            ElemPatch<>::GAUSS_PATCH, 
                            elem,
                            coord,
                            1, &fptr,
                            70000
                           );

      double *ed = err.data(elem);

      mev.GetFunctionGrads(field, &fgrads[0]);
      patch.EvalGrad(nqpoints, irule->locations(), &pgrads[0]);

// test grad:
 //ed[0] = pgrads[3*sdim];
      ed[0] = 0;
      for (UInt q = 0; q < nqpoints; q++) {

        for (UInt d = 0; d < sdim; d++) {
          double contr = pgrads[q*sdim+d] - fgrads[q*sdim+d];
/*
if (std::abs(contr) > 1) Par::Out() << "** large contr ** ";
 Par::Out() << "contr=" << contr <<" ";
*/

           ed[0] += contr*contr*mev.GetJxW(q);
        } // sdim
//Par::Out() << std::endl;

      } // nqpoints 

//std::cout << "deriv=" << val[0] << ", " << val[1] << std::endl;

    } // oi

    // Delete the patches
    ElemPatch<>::FreePatchMap(nmap);

  } // ki


  // If there were no ghosts, remove them now that we are done.
  if (!had_ghost)
    mesh.RemoveGhost();

}


} // namespace
