// $Id: ESMC_DCatEx.C,v 1.2.2.4 2009/01/21 21:25:22 cdeluca Exp $
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
//==============================================================================
// ESMF_disable_EXAMPLE        String used by test script to count examples.
//==============================================================================

/*
 * Program concatenates a set of parallel mesh files into a single file
*/
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
#include <ESMC_MeshPartition.h>
#include <ESMC_ParEnv.h>
#include <ESMC_MeshTypes.h>

#include <stdexcept>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <iostream>
#include <iterator>
#include <ostream>
#include <sstream>
#include <iomanip>
#include <cmath>


using namespace ESMCI::MESH;

void usage(const std::string &pname) {
    std::cout << "Usage:" << pname << " file nparts" << std::endl;
    std::exit(1);
}

/*---------------------------------------------------------------------*/
// Mesh concatenation program.  Takes a parallel set of mesh files
// and concatenates them into one file.  Used primarily for debugging
// a parallel mesh problem.  The concatenated files may be viewed using a
// tool like paraview (www.paraview.org).
/*---------------------------------------------------------------------*/
int main(int argc, char *argv[]) {


  Par::Init(argc, argv, "CATLOG", true);
  std::cout << "size=" << Par::Size() << std::endl;

  Mesh catmesh;

  if (argc != 3) {
    usage(argv[0]);
  }
  std::string fname(argv[1]);
  UInt npart = std::atoi(argv[2]);
  std::cout << "Collecting mesh " << argv[1] << " from " << npart << " poritions" << std::endl;
  catmesh.set_filename("cat_" + fname);

  std::vector<Mesh*> srcmesh(npart);


  try {
    // Read in the meshes
    for (UInt i = 0; i < npart; i++) {
      int vwidth = numDecimal(npart);
      std::stringstream os;
      os << fname << "." << npart << "." << std::setw(vwidth) << std::setfill('0') << i;
      std::string src_name = os.str();
      Mesh *mesh = new Mesh();
      ReadMesh(*mesh, src_name, false, ESMC_FILE_VTK);
      mesh->Commit();
      srcmesh[i] = mesh;
    }

    // Concatenate
    MeshConcat(catmesh, srcmesh);

    // Write the resulting concatenated file.
    WriteMesh(catmesh, catmesh.filename(), 1, 0.0, ESMC_FILE_VTK);

  
  } catch (std::runtime_error &ex) {
    std::cerr << "runtime exception:" << ex.what() << std::endl;
    Par::Abort();
  }
   catch (const char *msg) {
    std::cerr << "exception:" << msg << std::endl;
    Par::Abort();
  }
  catch(std::exception &ex) {
    std::cerr << "exception:" << ex.what() << std::endl;
    Par::Abort();
  }

  Par::End();

  return 0;
  
}



extern "C" {
void __pgi_trace() {
}
void __pgi_tracee() {
}
}
