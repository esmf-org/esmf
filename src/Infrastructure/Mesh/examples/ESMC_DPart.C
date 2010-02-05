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

/*
 * Program splits a serial mesh into a set of meshes for parallel exection 
 */

#include <stdlib.h>
//#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <iostream>

#include <stdexcept>
#include <ESMCI_MeshExodus.h>
#include <ESMCI_MeshDB.h>
#include <ESMCI_MeshSkin.h>

#include <ESMCI_ShapeFunc.h>
#include <ESMCI_Mapping.h>
#include <ESMCI_Search.h>
#include <ESMC_Transfer.h>

#include <ESMCI_MeshUtils.h>
#include <ESMCI_MasterElement.h>

#include <ESMCI_MeshPartition.h>
#include <ESMCI_MeshRead.h>

#include <iterator>
#include <ostream>

#include <cmath>

#include <ESMC_types.h>
#include <ESMCI_ParEnv.h>

using namespace ESMCI::MESH;

int main(int argc, char *argv[]) {

  Par::Init("DPARTLOG");

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
