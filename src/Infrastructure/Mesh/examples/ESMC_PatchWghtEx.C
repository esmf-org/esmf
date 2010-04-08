// $Id: ESMC_PatchWghtEx.C,v 1.20 2010/04/08 16:47:57 theurich Exp $
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
#include <ESMCI_Integrate.h>
#include <ESMCI_Interp.h>
#include <ESMCI_MeshPNC.h>
#include <ESMCI_Extrapolation.h>
#include <ESMCI_WriteWeightsPar.h>

#include <mpi.h>

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

int parseCsrvString(char *csrvStr, int *csrvtype);
int parseMethodString(char *methodStr, int *methodtype);
int parsePoleString(char *poleStr, int *poletype, int *poleNPnts);

int main(int argc, char *argv[]) {
  
  MPI_Init(&argc, &argv);

  Par::Init("PATCHLOG", false);

  Mesh srcmesh, dstmesh;
  char *srcGridFile,*dstGridFile,*wghtFile, *poleString;
  bool argsOk,addPole;
  int csrvType, methodType, poleType, poleNPnts;

  #define POLETYPE_NONE        0
  #define POLETYPE_ALL         1
  #define POLETYPE_NPNT        2

  #define CSRVTYPE_OFF         0
  #define CSRVTYPE_ON          1

  #define METHODTYPE_BILINEAR  0
  #define METHODTYPE_PATCH     1

  try {

    // Parse commandline

    // Set Defaults
    argsOk = false;
    csrvType = CSRVTYPE_OFF;
    methodType = METHODTYPE_BILINEAR;
    poleType = POLETYPE_ALL;
    poleNPnts=0;

    if (argc == 4) {
      srcGridFile=argv[1];
      dstGridFile=argv[2];
      wghtFile=argv[3];
      argsOk=true;
    } else if (argc == 6) {
      if (strcmp(argv[1],"-conservative")==0) {
        if (parseCsrvString(argv[2], &csrvType))
        srcGridFile=argv[3];
        dstGridFile=argv[4];
        wghtFile=argv[5];
        argsOk=true;
      } else if (strcmp(argv[1],"-method")==0) {
        if (parseMethodString(argv[2], &methodType))
        srcGridFile=argv[3];
        dstGridFile=argv[4];
        wghtFile=argv[5];
        argsOk=true;
      } else if (strcmp(argv[1],"-pole")==0) {
        if (parsePoleString(argv[2], &poleType, &poleNPnts)) {
          srcGridFile=argv[3];
          dstGridFile=argv[4];
          wghtFile=argv[5];
          argsOk=true;
        }
      }
    } else if (argc == 10) {
      if (strcmp(argv[1],"-conservative")==0 &&
          strcmp(argv[3],"-method")==0 &&
          strcmp(argv[5],"-pole")==0) {
        if (parseCsrvString(argv[2], &csrvType) &&
            parseMethodString(argv[4], &methodType) &&
            parsePoleString(argv[6], &poleType, &poleNPnts)) {
          srcGridFile=argv[7];
          dstGridFile=argv[8];
          wghtFile=argv[9];
          argsOk=true;
        }
      }
    }

    if (!argsOk) {
      if (Par::Rank() == 0) std::cerr << "Usage:" << argv[0] << " [-conservative on, off] [-method bilinear, patch] [-pole none, all, 1,2,...]  ingrid outgrid weightfile" << std::endl;
      Throw() << "Bye" << std::endl;
    }

    //    printf(">>>> poleType=%d poleNPnts=%d src=%s dst=%s wghts=%s \n",
    //	   poleType,poleNPnts,srcGridFile,dstGridFile,wghtFile);

    // Load files into Meshes
    if (Par::Rank() == 0) std::cout << "Loading " << srcGridFile << std::endl;
    LoadNCDualMeshPar(srcmesh, srcGridFile);
    if (Par::Rank() == 0) std::cout << "Loading " << dstGridFile << std::endl;
    LoadNCDualMeshPar(dstmesh, dstGridFile);
    
    // Add fields to mesh
    Context ctxt; ctxt.flip();
    MEField<> *src_iwts = srcmesh.RegisterField("iwts",
      MEFamilyStd::instance(), MeshObj::ELEMENT, ctxt, 1, true);

    MEField<> *dst_iwts = dstmesh.RegisterField("iwts",
      MEFamilyStd::instance(), MeshObj::ELEMENT, ctxt, 1, true);

    // Commit the meshes
    srcmesh.Commit();
    dstmesh.Commit();
    
    // Pole constraints
    IWeights pole_constraints;
    IWeights pole_constraints2;
    IWeights wts;
    IWeights stw;

    // Add poles if requested
    UInt constraint_id = srcmesh.DefineContext("pole_constraints");
    if (poleType==POLETYPE_ALL) {
      MeshAddPole(srcmesh, 1, constraint_id, pole_constraints);
      MeshAddPole(srcmesh, 2, constraint_id, pole_constraints);
    } else if (poleType==POLETYPE_NPNT) {
      MeshAddPoleNPnts(srcmesh, poleNPnts, 1, constraint_id, pole_constraints);
      MeshAddPoleNPnts(srcmesh, poleNPnts, 2, constraint_id, pole_constraints);
    }

  if (csrvType==CSRVTYPE_ON) {
    // Add poles if requested
    UInt constraint_id2 = dstmesh.DefineContext("pole_constraints2");
    if (poleType==POLETYPE_ALL) {
      MeshAddPole(dstmesh, 1, constraint_id2, pole_constraints2);
      MeshAddPole(dstmesh, 2, constraint_id2, pole_constraints2);
    } else if (poleType==POLETYPE_NPNT) {
      MeshAddPoleNPnts(dstmesh, poleNPnts, 1, constraint_id2, pole_constraints2);
      MeshAddPoleNPnts(dstmesh, poleNPnts, 2, constraint_id2, pole_constraints2);
    }

    // Use the coordinate fields for interpolation purposes
    MEField<> &scoord = *srcmesh.GetCoordField();
    MEField<> &dcoord = *dstmesh.GetCoordField();

    std::vector<Interp::FieldPair> fpairs;
    if (methodType==METHODTYPE_BILINEAR) {
      // make the field pairs for interpolation  -  BILINEAR
      fpairs.push_back(Interp::FieldPair(&dcoord, &scoord, Interp::INTERP_STD));
    } else if (methodType==METHODTYPE_PATCH) {
      // make the field pairs for interpolation  -  PATCH
      fpairs.push_back(Interp::FieldPair(&dcoord, &scoord, Interp::INTERP_PATCH));

      // Ghost elements across parallel boundaries (needed by INTERP_PATCH)
      MEField<> *psc = &scoord;
      MEField<> *pdc = &dcoord;
      srcmesh.CreateGhost();
      srcmesh.GhostComm().SendFields(1, &psc, &psc);
      dstmesh.CreateGhost();
      dstmesh.GhostComm().SendFields(1, &pdc, &pdc);
    }

    // generate integration weights
    Integrate sig(srcmesh), dig(dstmesh);
    sig.intWeights(src_iwts);
    dig.intWeights(dst_iwts);

    // Build the rendezvous grids
    Interp interp(dstmesh, srcmesh, fpairs);

    // generate integration weights, BEFORE generating weights matrix
    interp(0, stw);

  // Factor out poles if they exist
  if (poleType==POLETYPE_ALL) {
    stw.GatherToCol(pole_constraints);
    stw.AssimilateConstraints(pole_constraints);
    stw.GatherToCol(pole_constraints2);
    stw.AssimilateConstraints(pole_constraints2);
  } else if (poleType==POLETYPE_NPNT) {
    stw.GatherToRowSrc(pole_constraints);
    stw.AssimilateConstraintsNPnts(pole_constraints);
    stw.GatherToRowSrc(pole_constraints2);
    stw.AssimilateConstraintsNPnts(pole_constraints2);
  }

    // L2 projection conservative interpolation
    interp.interpL2csrvM(stw, &wts, src_iwts, dst_iwts);

    // Remove non-locally owned weights (assuming destination mesh decomposition)
    MEField<> *mask = dstmesh.GetField("MASK_IO");
    ThrowRequire(mask);
    wts.Prune(dstmesh, mask);

    // Redistribute weights in an IO friendly decomposition
    if (Par::Rank() == 0) std::cout << "Writing weights to " << wghtFile << std::endl;
    GatherForWrite(wts);

    // Write the weights
    WriteNCMatFilePar(srcGridFile, dstGridFile, wghtFile, wts, NCMATPAR_ORDER_INTERLEAVE);

  } else if (csrvType==CSRVTYPE_OFF) {

    // Use the coordinate fields for interpolation purposes
    MEField<> &scoord = *srcmesh.GetCoordField();
    MEField<> &dcoord = *dstmesh.GetCoordField();

    // make the field pairs for interpolation
    std::vector<Interp::FieldPair> fpairs;
    if (methodType==METHODTYPE_BILINEAR)
      fpairs.push_back(Interp::FieldPair(&dcoord, &scoord, Interp::INTERP_STD));
    else if (methodType==METHODTYPE_PATCH) {
      fpairs.push_back(Interp::FieldPair(&dcoord, &scoord, Interp::INTERP_PATCH));

      // Ghost elements across parallel boundaries (needed by INTERP_PATCH)
      MEField<> *psc = &scoord;
      srcmesh.CreateGhost();
      srcmesh.GhostComm().SendFields(1, &psc, &psc);
    }

     // Build the rendezvous grids
     if (Par::Rank() == 0) std::cout << "Building rendezvous grids..." << std::endl;
     Interp interp(srcmesh, dstmesh, fpairs);

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

     // Write the weights
     WriteNCMatFilePar(srcGridFile, dstGridFile, wghtFile, wts, NCMATPAR_ORDER_SEQ);
  } // conservative

  } // try
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

int parseCsrvString(char *csrvStr, int *csrvType) {
      if (strcmp(csrvStr,"off")==0)
        *csrvType=CSRVTYPE_OFF;
      else if (strcmp(csrvStr,"on")==0)
        *csrvType=CSRVTYPE_ON;
      else return 0; // FAILURE

  return 1; // SUCCESS
}

int parseMethodString(char *methodStr, int *methodType) {
      if (strcmp(methodStr,"bilinear")==0)
        *methodType=METHODTYPE_BILINEAR;
      else if (strcmp(methodStr,"patch")==0)
        *methodType=METHODTYPE_PATCH;
      else return 0; // FAILURE

  return 1; // SUCCESS
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
