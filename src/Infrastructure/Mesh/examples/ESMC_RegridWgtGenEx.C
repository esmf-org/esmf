// $Id: ESMC_RegridWgtGenEx.C,v 1.4 2010/06/28 20:07:10 rokuingh Exp $
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

#include <ESMCI_VM.h>
#include <ESMC_Init.h>

#ifdef REGRIDTIMING
#include <mpi.h>
#endif

/**
 * This program may be used to generate an interpolation weight matrix from
 * two SCRIP style input files.  The output is a SCRIP format matrix file.
 */


using namespace ESMCI;

int parseCsrvString(char *csrvStr, int *csrvtype);
int parseMethodString(char *methodStr, int *methodtype);
int parsePoleString(char *poleStr, int *poletype, int *poleNPnts);

int main(int argc, char *argv[]) {

  int localrc = 0;

  Par::Init("PATCHLOG", false);

  Mesh srcmesh, dstmesh, dstmeshcpy;
  char *srcGridFile,*dstGridFile,*wghtFile, *poleString;
  bool argsOk,addPole;
  int csrvType, methodType, poleType, poleNPnts;

  // declare the timer
  #ifdef REGRIDTIMING
  regridTimer rt;
  #endif

  try {

    // start the timer
    #ifdef REGRIDTIMING
    rt.start = MPI_Wtime();
    #endif

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
    LoadNCDualMeshPar(srcmesh, srcGridFile);
    LoadNCDualMeshPar(dstmesh, dstGridFile);
    LoadNCDualMeshPar(dstmeshcpy, dstGridFile);

    // regridTimer
    #ifdef REGRIDTIMING
    MPI_Barrier(MPI_COMM_WORLD);
    rt.gridsInput = MPI_Wtime();

    if(!offline_regrid(srcmesh, dstmesh, dstmeshcpy, &csrvType, &methodType, &poleType, 
                       &poleNPnts, srcGridFile, dstGridFile, wghtFile, rt))
      Throw() << "Offline regridding error" << std::endl;

    rt.weightsOutput = MPI_Wtime();
    double T0 = rt.start;
    double T1 = rt.gridsInput;
    double T2 = rt.regridComplete;
    double T3 = rt.weightsOutput;
    ofstream tF;
    char filename[100];
    sprintf(filename, "regrid_timing_%d.out", Par::Rank());
    tF.open(filename);
    tF << Par::Rank()<< "\t" << T0-T0 << "\t" << T1-T0 << "\t" << T2-T0 << "\t" << T3-T0 << "\t" << std::endl;
    tF.close();
    #else
    if(!offline_regrid(srcmesh, dstmesh, dstmeshcpy, &csrvType, &methodType, &poleType, 
                       &poleNPnts, srcGridFile, dstGridFile, wghtFile))
      Throw() << "Offline regridding error" << std::endl;
    #endif

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
