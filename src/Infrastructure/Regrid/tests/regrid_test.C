//-----------------------------------------------------------------------------
// $Id: regrid_test.C,v 1.1 2008/02/11 17:16:48 dneckels Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// Testing of DistDir object
//
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//-----------------------------------------------------------------------------
#include "GridUtil/include/ESMC_GridToMesh.h"
#include "ESMC_LogErr.h"                  // for LogErr
#include "ESMF_LogMacros.inc"
#include <Mesh/include/ESMC_Mesh.h>
#include <Mesh/include/ESMC_MeshRead.h>
#include <Mesh/include/ESMC_Exception.h>
#include <Mesh/include/ESMC_Interp.h>

#include <iostream>

using namespace ESMC;

extern "C" void FTN(regrid_test)(ESMCI::VM **vmpp, ESMCI::Grid **gridsrcpp, int *srcstaggerLoc,
                             ESMCI::Grid **griddstpp, int *dststaggerLoc,
                             int*rc) {
  Trace __trace(" FTN(regrid_test)(ESMCI::VM **vmpp, ESMCI::Grid **gridsrcpp, int *srcstaggerLoc, ESMCI::Grid **griddstcpp, int *dststaggerLoc, int*rc");
  ESMCI::VM *vm = *vmpp;
  ESMCI::Grid &srcgrid = **gridsrcpp;
  ESMCI::Grid &dstgrid = **griddstpp;

  int localPet = vm->getLocalPet();
  int petCount = vm->getPetCount();

  Mesh srcmesh;
  Mesh dstmesh;

  try {

    ESMCI::GridToMesh(srcgrid, *srcstaggerLoc, srcmesh);
    ESMCI::GridToMesh(dstgrid, *dststaggerLoc, dstmesh);

    WriteMesh(srcmesh, "src_grid");
    WriteMesh(dstmesh, "dst_grid");

    // Nifty to all nifty.  Now we have the meshes, so go ahead and calc bilinear weights.
    // Since bilin needs a field, just do coords, I mean, what does it matter in this biz.
    // bilinear is bilinear.
    MEField<> &scoord = *srcmesh.GetCoordField();
    MEField<> &dcoord = *dstmesh.GetCoordField();
    std::vector<Interp::FieldPair> fpairs;
    fpairs.push_back(Interp::FieldPair(&scoord, &dcoord, Interp::INTERP_STD));

    Interp interp(srcmesh, dstmesh, fpairs);

    IWeights wts;
 
    interp(0, wts);

    wts.Print(Par::Out());

  }
  catch(std::exception &x) {
    std::cout << "Error!!! Exception, P:" << localPet << ", <" << x.what() << ">" << std::endl;
    *rc = ESMF_FAILURE;
    return;
  }
  catch(...) {
    std::cout << "Error, unknown exception" << std::endl;
    *rc = ESMF_FAILURE;
    return;
  }

  *rc = ESMF_SUCCESS;

}

