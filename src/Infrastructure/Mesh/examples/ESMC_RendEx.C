//==============================================================================
// $Id: ESMC_RendEx.C,v 1.5.2.2 2009/01/21 21:25:22 cdeluca Exp $
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

#include <iterator>
#include <ostream>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <iostream>
#include <stdexcept>
#include <cmath>


using namespace ESMCI::MESH;


void fill_src(const Mesh &mesh, MEField<> *s, double t) {
  int rank = Par::Rank();

  Mesh::const_iterator ni = mesh.node_begin(), ne = mesh.node_end();
  MEField<> *cfield = mesh.GetCoordField();
 
  for (; ni != ne; ++ni) {
    const MeshObj &node = *ni;
    
    double *c = cfield->data(node);
    double *data = s->data(node);

    data[0] = sin((7*c[0]+t*6*c[1])*M_PI)*cos(4*c[2]*M_PI);
  }
}

void set_dest_coords(const Mesh &mesh) {

  Mesh::const_iterator ni = mesh.node_begin(), ne = mesh.node_end();
  MEField<> *cfield = mesh.GetCoordField();

  // Rotate so meshes dont align and so the RCB decompositions don't match
  const double theta = 110*M_PI/180.0;
  const double cos_theta = cos(theta);
  const double sin_theta = sin(theta);
 
  for (; ni != ne; ++ni) {
    const MeshObj &node = *ni;
    
    double *c = cfield->data(node);

    // Rotate (around origin) the mesh so that the regrid will be more challenging
    const double c0 = c[0]-0.5;
    const double c1 = c[1]-0.5;
    c[0] = (cos_theta*c0 - sin_theta*c1) + 0.5;
    c[1] = (sin_theta*c0 + cos_theta*c1) + 0.5;

    // Shrink the mesh so that it still fits in the source
    c[0] = (c[0]-0.5)*0.66+0.5; c[1] = (c[1]-0.5)*0.66+0.5; c[2] = (c[2]-0.5)*0.66+0.5;


    // Make the mesh 'arc' in the center
    c[2] = 0.5*sin(c[0]*M_PI)*sin(c[1]*M_PI) + 0.1;

  }
}

int main(int argc, char *argv[]) {



  Par::Init(argc, argv, "RENDLOG");
  Par::Out() << "Rend, proc:" << Par::Rank() << ", starting..." << std::endl;
  

  Mesh srcmesh, dstmesh;

  try {

  // Generate the 3d unit square for the source
  {
    const MeshObjTopo *stopo = GetTopo("HEX");
    HyperCube(srcmesh, stopo);
  }

  // Generate a 2d manifold for the destination
  {
    const MeshObjTopo *dtopo = GetTopo("SHELL");
    HyperCube(dstmesh, dtopo);
  }

  // We will uniformly refine each mesh
  HAdapt src_hadapt(srcmesh);
  HAdapt dst_hadapt(dstmesh);

  // Register the source and destination fields on the entire mesh
  Context Omega; Omega.flip();
  MEField<> *s = srcmesh.RegisterField("source", MEFamilyStd::instance(), MeshObj::ELEMENT, Omega, 1, true);
  
  MEField<> *d = dstmesh.RegisterField("dest", MEFamilyStd::instance(), MeshObj::ELEMENT, Omega, 1, true, true);
  MEField<> *d1 = dstmesh.RegisterField("dest1", MEFamilyStd::instance(), MeshObj::ELEMENT, Omega, 1, true, true);

  // Commit the meshes
  srcmesh.Commit();
  dstmesh.Commit();


  // Now refine srcmesh
  for (UInt i = 0; i < 5; i++) {
    src_hadapt.RefineUniformly(false);
    if (Par::Rank() == 0)
        std::cout << "Refined source Mesh" << std::endl;
    // Now rebalance across processors.  Do this during refinement so refinement
    // will be load balanced.
    if ((i%2) == 0) Rebalance(srcmesh);
  }
  Rebalance(srcmesh);

  // Now refine dstmesh
  for (UInt i = 0; i < 7; i++) {
    dst_hadapt.RefineUniformly(false);
    if (Par::Rank() == 0)
        std::cout << "Refined destination Mesh" << std::endl;
    if ((i % 2) == 0) Rebalance(dstmesh);
  }
  Rebalance(dstmesh);

  // Adjust the destination coordinates so that the destination mesh is an arcing manifold
  set_dest_coords(dstmesh);

  double T = 0, tstep = 0.3, TEND = 5;

  // Ghost elements across parallel boundaries (needed by INTERP_PATCH)
  srcmesh.CreateGhost();
  
  Interp interp(srcmesh, dstmesh, fpairs);

  UInt nstep = 0;
  while (T < TEND) {

    fill_src(srcmesh, s, T);

    // Send field values to the ghosted cells
    srcmesh.GhostComm().SendFields(1, &s, &s);

    // Perform the interpolation
    interp();
  
    char buf[512];
    std::sprintf(buf, "results_dest_%03d", nstep++);
    WriteMesh(dstmesh, buf);
  
    if (Par::Rank() == 0) std::cout << "t=" << T << std::endl;
    T += tstep;
  }

  srcmesh.RemoveGhost();


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
