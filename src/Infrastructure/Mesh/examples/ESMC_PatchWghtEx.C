//==============================================================================
//
// Earth System Modeling Framework
// Copyright 2002-2008, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <ESMC_MeshExodus.h>
#include <ESMC_Mesh.h>
#include <ESMC_MeshSkin.h>
#include <ESMC_ShapeFunc.h>
#include <ESMC_Mapping.h>
#include <ESMC_Search.h>
#include <ESMC_MeshField.h>
#include <ESMC_MeshUtils.h>
#include <ESMC_MasterElement.h>
#include <ESMC_MeshRead.h>
#include <ESMC_MeshTypes.h>
#include <ESMC_ParEnv.h>
#include <ESMC_MeshGen.h>
#include <ESMC_HAdapt.h>
#include <ESMC_Rebalance.h>
#include <ESMC_Interp.h>
#include <ESMC_MeshPNC.h>
#include <ESMC_Extrapolation.h>
#include <ESMC_WriteWeightsPar.h>

#include <iterator>
#include <ostream>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <iostream>
#include <stdexcept>
#include <cmath>

/**
 * This program may be used to generate a patch-interpolation weight matrix from
 * two SCRIP style input files.  The output is a SCRIP format matrix file.
 */


using namespace ESMCI;

int main(int argc, char *argv[]) {

  Par::Init("PATCHLOG", true);

  Mesh srcmesh, dstmesh;

  try {

    if (argc != 4) {
      if (Par::Rank() == 0) std::cerr << "Usage:" << argv[0] << " ingrid outgrid weightfile" << std::endl;
      Throw() << "Bye" << std::endl;
    }

     if (Par::Rank() == 0) std::cout << "Loading " << argv[1] << std::endl;
     LoadNCDualMeshPar(srcmesh, argv[1]);
     if (Par::Rank() == 0) std::cout << "Loading " << argv[2] << std::endl;
     LoadNCDualMeshPar(dstmesh, argv[2]);

     // Commit the meshes
     srcmesh.Commit();
     dstmesh.Commit();


     // Use the coordinate fields for interpolation purposes
     MEField<> &scoord = *srcmesh.GetCoordField();
     MEField<> &dcoord = *dstmesh.GetCoordField();

     // Set up parallel ghosting (necessary for patch)
     MEField<> *psc = &scoord;
     srcmesh.CreateGhost();
     srcmesh.GhostComm().SendFields(1, &psc, &psc);

     // Pole constraints
     IWeights pole_constraints;
     UInt constraint_id = srcmesh.DefineContext("pole_constraints");
     MeshAddPole(srcmesh, 1, constraint_id, pole_constraints);
     MeshAddPole(srcmesh, 2, constraint_id, pole_constraints);

WriteMesh(srcmesh, "src");
WriteMesh(dstmesh, "dst");

     std::vector<Interp::FieldPair> fpairs;
     fpairs.push_back(Interp::FieldPair(&scoord, &dcoord, Interp::INTERP_PATCH));

     // Build the rendezvous grids
     if (Par::Rank() == 0) std::cout << "Building rendezvous grids..." << std::endl;
     Interp interp(srcmesh, dstmesh, fpairs);

     IWeights wts;

     // Create the weight matrix
     if (Par::Rank() == 0) std::cout << "Forming patch weights..." << std::endl;
     interp(0, wts);

     // Get the pole matrix on the right processors
     wts.GatherToCol(pole_constraints);

     // Take pole constraint out of matrix
     wts.AssimilateConstraints(pole_constraints);

     // Remove non-locally owned weights (assuming destination mesh decomposition)
     MEField<> *mask = dstmesh.GetField("mask");
     ThrowRequire(mask);
     wts.Prune(dstmesh, mask);

     // Redistribute weights in an IO friendly decomposition
     if (Par::Rank() == 0) std::cout << "Writing weights to " << argv[3] << std::endl;
     GatherForWrite(wts);

//wts.Print(Par::Out());

     // Write the weights
     WriteNCMatFilePar(argv[1], argv[2], argv[3], wts, NCMATPAR_ORDER_SEQ);

  } 
   catch (std::exception &x) {
    std::cerr << "std::Exception:" << x.what() << std::endl;
    Par::Out() << "std::Exception:" << x.what() << std::endl;
    std::cerr.flush();
    Par::Abort();
  }
   catch (const char *msg) {
    std::cerr << "Exception:" << msg << std::endl;
    Par::Out() << "Exception:" << msg << std::endl;
    std::cerr.flush();
    Par::Abort();
  }
  catch(...) {
    std::cerr << "Unknown exception:" << std::endl;
    Par::Out() << "Unknown exception:" << std::endl;
    std::cerr.flush();
    Par::Abort();
  }

  std::cout << "Run has completed" << std::endl;

  Par::End();

  return 0;
  
}
