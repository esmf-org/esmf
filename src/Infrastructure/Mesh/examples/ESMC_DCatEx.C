// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
//==============================================================================
// EXAMPLE        String used by test script to count examples.
//==============================================================================

/*
 * Program concatenates a set of parallel mesh files into a single file
*/
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
#include <ESMCI_MeshPartition.h>
#include <ESMCI_ParEnv.h>
#include <ESMCI_MeshTypes.h>

#include <mpi.h>

#include <stdexcept>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <iostream>
#include <iterator>
#include <ostream>
#include <sstream>
#include <iomanip>
#include <cmath>


using namespace ESMCI;

void usage(const std::string &pname) {
    std::cout << "Usage:" << pname << " file nparts" << std::endl;
    exit(1);
}

/*---------------------------------------------------------------------*/
// Mesh concatenation program.  Takes a parallel set of mesh files
// and concatenates them into one file.  Used primarily for debugging
// a parallel mesh problem.  The concatenated files may be viewed using a
// tool like paraview (www.paraview.org).
/*---------------------------------------------------------------------*/
int main(int argc, char *argv[]) {

  MPI_Init(&argc, &argv);

  Par::Init("CATLOG");
  std::cout << "size=" << Par::Size() << std::endl;

  Mesh catmesh;

  if (argc != 3) {
    usage(argv[0]);
  }
  std::string fname(argv[1]);
  UInt npart = atoi(argv[2]);
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
      std::cout << "Reading file: " << src_name << '\n';
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
