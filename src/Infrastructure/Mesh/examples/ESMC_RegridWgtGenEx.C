// $Id: ESMC_RegridWgtGenEx.C,v 1.1 2010/04/30 20:19:00 rokuingh Exp $
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
#include <ESMCI_MeshRegrid.h>
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
 * This program may be used to generate an interpolation weight matrix from
 * two SCRIP style input files.  The output is a SCRIP format matrix file.
 */


using namespace ESMCI;

int parseCsrvString(char *csrvStr, int *csrvtype);
int parseMethodString(char *methodStr, int *methodtype);
int parsePoleString(char *poleStr, int *poletype, int *poleNPnts);

int main(int argc, char *argv[]) {

  Par::Init("PATCHLOG", false);

  Mesh srcmesh, dstmesh;
  char *srcGridFile,*dstGridFile,*wghtFile, *poleString;
  bool argsOk,addPole;
  int csrvType, methodType, poleType, poleNPnts;

  try {

    // Parse commandline

    // Set Defaults
    argsOk = false;
    csrvType = ESMC_REGRID_CONSERVE_OFF;
    methodType = ESMC_REGRID_METHOD_BILINEAR;
    poleType = ESMC_REGRID_POLETYPE_ALL;
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

  //printf(">>>> csrvType=%d methodType=%d poleType=%d poleNPnts=%d src=%s dst=%s wghts=%s \n",
  //  	 csrvType,methodType,poleType,poleNPnts,srcGridFile,dstGridFile,wghtFile);

    // Load files into Meshes
    if (Par::Rank() == 0) std::cout << "Loading " << srcGridFile << std::endl;
    LoadNCDualMeshPar(srcmesh, srcGridFile);
    if (Par::Rank() == 0) std::cout << "Loading " << dstGridFile << std::endl;
    LoadNCDualMeshPar(dstmesh, dstGridFile);

    if(!offline_regrid(srcmesh, dstmesh, &csrvType, &methodType, &poleType, &poleNPnts,
                       srcGridFile, dstGridFile, wghtFile))
      Throw() << "Offline regridding error" << std::endl;

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

  if (Par::Rank() == 0) std::cout << "Run has completed" << std::endl;

  Par::End();

  return 0;
  
}

  int parseCsrvString(char *csrvStr, int *csrvType) {
      if (strcmp(csrvStr,"off")==0)
        *csrvType=ESMC_REGRID_CONSERVE_OFF;
      else if (strcmp(csrvStr,"on")==0)
        *csrvType=ESMC_REGRID_CONSERVE_ON;
      else return 0; // FAILURE

    return 1; // SUCCESS
  }

  int parseMethodString(char *methodStr, int *methodType) {
      if (strcmp(methodStr,"bilinear")==0)
        *methodType=ESMC_REGRID_METHOD_BILINEAR;
      else if (strcmp(methodStr,"patch")==0)
        *methodType=ESMC_REGRID_METHOD_PATCH;
      else return 0; // FAILURE

    return 1; // SUCCESS
  }

  int parsePoleString(char *poleStr, int *poleType, int *poleNPnts) {

      if (strcmp(poleStr,"none")==0) {
	*poleType=ESMC_REGRID_POLETYPE_NONE;
	*poleNPnts=0;
      } else if (strcmp(poleStr,"all")==0) {
	*poleType=ESMC_REGRID_POLETYPE_ALL;
	*poleNPnts=0;
      } else {
	*poleType=ESMC_REGRID_POLETYPE_NPNT;
	*poleNPnts=atoi(poleStr);
	
	// Check return from atoi
	if (*poleNPnts < 1) {
	  return 0; // Failure - Shouldn't be 0 or less
                    //           for no average use POLETYPE_NONE
	} 
      }

    return 1; // Success
  }
