//==============================================================================
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <ESMCI_MeshExodus.h>
#include <ESMCI_Mesh.h>
#include <ESMCI_MeshSkin.h>
#include <ESMCI_ShapeFunc.h>
#include <ESMCI_Mapping.h>
#include <ESMCI_Search.h>
#include <ESMCI_MeshField.h>
#include <ESMCI_MeshUtils.h>
#include <ESMCI_MasterElement.h>
#include <ESMCI_MeshRead.h>
#include <ESMCI_MeshTypes.h>
#include <ESMCI_ParEnv.h>
#include <ESMCI_MeshGen.h>
#include <ESMCI_HAdapt.h>
#include <ESMCI_Rebalance.h>
#include <ESMCI_Interp.h>
#include <ESMCI_MeshPNC.h>
#include <ESMCI_Extrapolation.h>
#include <ESMCI_WriteWeightsPar.h>

#include <iterator>
#include <ostream>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <iostream>
#include <stdexcept>
#include <cmath>
#include <cstring>

/**
 * This program may be used to generate a patch-interpolation weight matrix from
 * two SCRIP style input files.  The output is a SCRIP format matrix file.
 */


using namespace ESMCI;

int parsePoleString(char *poleStr, int *poletype, int *poleNPnts);

int main(int argc, char *argv[]) {

  Par::Init("PATCHLOG", false);

  Mesh srcmesh, dstmesh;
  char *srcGridFile,*dstGridFile,*wghtFile, *poleString;
  bool argsOk,addPole;
  int poleType,poleNPnts;
  #define POLETYPE_NONE  0
  #define POLETYPE_ALL   1
  #define POLETYPE_NPNT  2


  try {

    // Parse commandline

    // Set Defaults
    argsOk = false;
    poleType = POLETYPE_ALL;
    poleNPnts=0;

    if (argc == 4) {
      srcGridFile=argv[1];
      dstGridFile=argv[2];
      wghtFile=argv[3];
      argsOk=true;
    } else if (argc == 6) {
      if (strcmp(argv[1],"-pole")==0) {
        if (parsePoleString(argv[2], &poleType, &poleNPnts)) {
	  srcGridFile=argv[3];
	  dstGridFile=argv[4];
	  wghtFile=argv[5];
	  argsOk=true;	
	}
      }
    }    
    
    if (!argsOk) {
      if (Par::Rank() == 0) std::cerr << "Usage:" << argv[0] << " [-pole none, all, 1,2,...]  ingrid outgrid weightfile" << std::endl;
      Throw() << "Bye" << std::endl;
    }

    //    printf(">>>> poleType=%d poleNPnts=%d src=%s dst=%s wghts=%s \n",
    //	   poleType,poleNPnts,srcGridFile,dstGridFile,wghtFile);


    // Load files into Meshes
    if (Par::Rank() == 0) std::cout << "Loading " << srcGridFile << std::endl;
    LoadNCDualMeshPar(srcmesh, srcGridFile);
    if (Par::Rank() == 0) std::cout << "Loading " << dstGridFile << std::endl;
    LoadNCDualMeshPar(dstmesh, dstGridFile);
    
    // Commit the meshes
    srcmesh.Commit();
    dstmesh.Commit();
    
    // Pole constraints
    IWeights pole_constraints;

    // Add poles if requested
    UInt constraint_id = srcmesh.DefineContext("pole_constraints");
    if (poleType==POLETYPE_ALL) {
      MeshAddPole(srcmesh, 1, constraint_id, pole_constraints);
      MeshAddPole(srcmesh, 2, constraint_id, pole_constraints);
    } else if (poleType==POLETYPE_NPNT) {
      MeshAddPoleNPnts(srcmesh, poleNPnts, 1, constraint_id, pole_constraints);
      MeshAddPoleNPnts(srcmesh, poleNPnts, 2, constraint_id, pole_constraints);
    }

    // Use the coordinate fields for interpolation purposes
    MEField<> &scoord = *srcmesh.GetCoordField();
    MEField<> &dcoord = *dstmesh.GetCoordField();

     // Set up parallel ghosting (necessary for patch)
     MEField<> *psc = &scoord;
     srcmesh.CreateGhost();
     srcmesh.GhostComm().SendFields(1, &psc, &psc);

     // Only for Debug
     //WriteMesh(srcmesh, "src");
     //WriteMesh(dstmesh, "dst");

     std::vector<Interp::FieldPair> fpairs;
     fpairs.push_back(Interp::FieldPair(&scoord, &dcoord, Interp::INTERP_PATCH));

     // Build the rendezvous grids
     if (Par::Rank() == 0) std::cout << "Building rendezvous grids..." << std::endl;
     Interp interp(srcmesh, dstmesh, fpairs);

     IWeights wts;

     // Create the weight matrix
     if (Par::Rank() == 0) std::cout << "Forming patch weights..." << std::endl;
     interp(0, wts);

     // Factor out poles if they exist
     if (poleType==POLETYPE_ALL) {
       // Get the pole matrix on the right processors
       wts.GatherToCol(pole_constraints);
       
       // Take pole constraint out of matrix
       wts.AssimilateConstraints(pole_constraints);
     } else if (poleType==POLETYPE_NPNT) {
       // Get the pole matrix on the right processors
       wts.GatherToRowSrc(pole_constraints);

       // Take pole constraint out of matrix
       wts.AssimilateConstraintsNPnts(pole_constraints);
     }


     // Remove non-locally owned weights (assuming destination mesh decomposition)
     MEField<> *mask = dstmesh.GetField("MASK_IO");
     ThrowRequire(mask);
     wts.Prune(dstmesh, mask);

     // Redistribute weights in an IO friendly decomposition
     if (Par::Rank() == 0) std::cout << "Writing weights to " << wghtFile << std::endl;
     GatherForWrite(wts);

//wts.Print(Par::Out());

     // Write the weights
     WriteNCMatFilePar(srcGridFile, dstGridFile, wghtFile, wts, NCMATPAR_ORDER_SEQ);

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

int parsePoleString(char *poleStr, int *poleType, int *poleNPnts) {

      if (strcmp(poleStr,"none")==0) {
	*poleType=POLETYPE_NONE;
	*poleNPnts=0;
      } else if (strcmp(poleStr,"all")==0) {
	*poleType=POLETYPE_ALL;
	*poleNPnts=0;
      } else {
	*poleType=POLETYPE_NPNT;
	*poleNPnts=atoi(poleStr);
	
	// Check return from atoi
	if (*poleNPnts < 1) {
	  return 0; // Failure - Shouldn't be 0 or less
                    //           for no average use POLETYPE_NONE
	} 
      }

  return 1; // Success
}
