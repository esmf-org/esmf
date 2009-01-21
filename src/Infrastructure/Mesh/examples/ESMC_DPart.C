// $Id: ESMC_DPart.C,v 1.1.2.2 2009/01/21 21:25:22 cdeluca Exp $
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

/*
 * Program splits a serial mesh into a set of meshes for parallel exection 
 */

#include <unistd.h>
#include <stdlib.h>
//#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <iostream>

#include <stdexcept>
#include <ESMC_MeshExodus.h>
#include <ESMC_MeshDB.h>
#include <ESMC_MeshSkin.h>

#include <ESMC_ShapeFunc.h>
#include <ESMC_Mapping.h>
#include <ESMC_Search.h>
#include <ESMC_Transfer.h>

#include <ESMC_Field.h>

#include <ESMC_MeshUtils.h>
#include <ESMC_MasterElement.h>

#include <ESMC_MeshPartition.h>
#include <ESMC_MeshRead.h>

#include <iterator>
#include <ostream>

#include <cmath>

#include <ESMC_types.h>
#include <ESMC_ParEnv.h>

using namespace ESMCI::MESH;

int main(int argc, char *argv[]) {

  Par::Init(argc, argv, "DPARTLOG", true);

  Mesh srcmesh;
  UInt npart;

  if (argc != 3) {
    std::cout << "Usage:" << argv[0] << " srcfile nparts" << std::endl;
    std::exit(1);
  }

  npart = std::atoi(argv[2]);
  std::cout << "Splitting mesh " << argv[0] << " into " << npart << " poritions" << std::endl;

  try {
  ReadMesh(srcmesh, argv[1]);
  std::cout << "Mesh Loaded" << std::endl;

  Context ctxt; ctxt.flip();
  MEField<> *ep = srcmesh.RegisterField("_EPROC", MEFamilyDG0::instance(),
                            MeshObj::ELEMENT, ctxt, 1, true, _fieldType<int>::instance());
  MEField<> *np = srcmesh.RegisterField("_NPROC", MEFamilyStd::instance(),
                            MeshObj::ELEMENT, ctxt, 1, true, _fieldType<int>::instance());

  srcmesh.Commit();
//srcmesh.Print();

  MeshMetisPartition(srcmesh, npart, *ep, *np);
  
  } catch (std::runtime_error &ex) {
    std::cerr << "runtime exception:" << ex.what() << std::endl;
    Par::Abort();
  }
   catch (const char *msg) {
    std::cerr << "exception:" << msg << std::endl;
    Par::Abort();
  }

  Par::End();

  return 0;
  
}
