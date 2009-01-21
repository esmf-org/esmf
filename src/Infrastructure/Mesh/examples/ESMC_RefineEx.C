//==============================================================================
// $Id: ESMC_RefineEx.C,v 1.7.2.2 2009/01/21 21:25:22 cdeluca Exp $
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
#include <iostream>

#include <stdexcept>
#include <ESMC_Mesh.h>
#include <ESMC_MeshSkin.h>

#include <ESMC_ShapeFunc.h>
#include <ESMC_Mapping.h>
#include <ESMC_Search.h>

#include <ESMC_MeshUtils.h>
#include <ESMC_MasterElement.h>

#include <ESMC_MeshRead.h>
#include <ESMC_ParEnv.h>
#include <ESMC_MeshRefine.h>
#include <ESMC_RefineTopo.h>
#include <ESMC_MeshObjConn.h>
#include <ESMC_MeshGen.h>

#include <ESMC_HAdapt.h>

#include <ESMC_Rebalance.h>

#include <iterator>
#include <ostream>

#include <cmath>

#include <ESMC_MeshTypes.h>

#include <mpi.h>

#include <stdlib.h>



using namespace ESMCI::MESH;


MEField<> *hf;

/*---------------------------------------------------------------------*/
// The basic command sequence that should happen after marking elements.
/*---------------------------------------------------------------------*/
void wave_refine_unrefine(HAdapt &hadapt, bool rebal)
{
  Mesh &mesh = hadapt.GetMesh();

  // Resolve the 2-1 rule, unrefinement logic.
  hadapt.MarkerResolution();

  Par::Out() << "** Unrefining mesh ** " << std::endl;
  hadapt.UnrefineMesh();

  // Resolve the mesh changes in parallel.
  hadapt.RefinementResolution();

  Par::Out() << "** Refining mesh ** " << std::endl;
  hadapt.RefineMesh();

  // Resolve the mesh changes in parallel.
  hadapt.RefinementResolution();

  // Rebalance the mesh when flag is set.
  if (rebal) {
    Par::Out() << "** Rebalancing mesh ** " << std::endl;
    Rebalance(mesh);
  }

}

// Get the centroid of an element.  Calc the diameter at the same time
void get_avgerage_nodecoord(double &diameter, double ave[], Mesh &mesh, MeshObj &parent_obj) {
  MEField<> &c_ref = *mesh.GetCoordField();

  ave[0] = ave[1] = ave[2] = 0.0;

  // Loop over parent's nodes
  MeshObjRelationList::iterator n_i  = parent_obj.Relations.begin();

  int num_parent_nodes = 0;

  double max_diam = 0;
  double * coord0 = c_ref.data(*n_i->obj); // child 0
  for ( ; n_i != parent_obj.Relations.end() &&
          n_i->obj->get_type() == MeshObj::NODE ; ++n_i )
  {
    double * coord = c_ref.data(*n_i->obj);

    if ( n_i->type == MeshObj::USES ) {
      ++num_parent_nodes ;
      ave[0] += coord[0];
      ave[1] += coord[1];
      if (mesh.spatial_dim() == 3) ave[2] += coord[2];
      double diam = 0;
      diam += std::fabs(coord0[0] - coord[0]);
      diam += std::fabs(coord0[1] - coord[1]);
      if (mesh.spatial_dim() == 3)
        diam += std::fabs(coord0[2] - coord[2]);

      if (diam > max_diam) max_diam = diam;
    }
  }

  ave[0] /= num_parent_nodes;
  ave[1] /= num_parent_nodes;
  ave[2] /= num_parent_nodes;

  diameter = max_diam;

}

/*-------------------------------------------------------------------------------------*/
// Refinement test driver.  This test refines around a hypercircle along the diagonal
// of the unit cube (2d or 3d).
// Variables:
//   circleRadius: the radius of the (hyper) circle to refine against
//   num_iter: number of iterations to traverse diagonal.  More = smaller steps.
//   epsilon: refine everything within this distance of the hypercircle surface
//   h: size down to which we refine.
//   load_bal: load balance interval (number of steps between load balancing).
/*-------------------------------------------------------------------------------------*/
void adapt_wave(HAdapt &hadapt, Mesh &mesh) {

  int nout = 0;
  UInt sdim = mesh.spatial_dim();
  const int num_itr = 300;

  const double circleRadius = 0.25;
  const double epsilon = 0.02; // refine within this distance of circle
  const double h = mesh.spatial_dim() == 2 ? 0.02 : 0.07; // refine elements to this size

  const int load_bal = 3;

  double sqrtd = std::sqrt((double)sdim);

  // circle center; x,y,z
  double center[] = {-circleRadius/sqrtd, -circleRadius/sqrtd, -circleRadius/sqrtd};

  // distance per step?

  // How far to update each coordinate per step
  double ddist = (1+2*circleRadius/sqrtd)/num_itr;

  int nstep = 0;

  // Adaptivity loop
  while (nstep < num_itr) {

    if (Par::Rank() == 0) std::cout << "**** Running step " << nstep << std::endl;

    // Loop and mark elements.
    Mesh::iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
    for (; ei != ee; ++ei) {
      MeshObj &elem = (*ei);

      // First, is the element within epsilon of the circle?
      double ave[3];
      double diameter;

      // Get the element centroid, and the diamter of the element
      get_avgerage_nodecoord(diameter, ave, mesh, elem);

      // Set the diameter variable.
      double *hv = hf->data(elem);
      *hv = diameter;
      

      // subtract center from ave
      ave[0] -= center[0]; ave[1] -= center[1]; ave[2] -= center[2];

      // Calculate distance to the circle.
      double d2circle = 0;
        d2circle = ave[0]*ave[0] + ave[1]*ave[1];
      if (sdim == 3) d2circle += ave[2]*ave[2];

      d2circle = std::sqrt(d2circle);

      // That is distance to center of circle, now refine if within the
      // tolerance.  Unrefine with a slight hysterisis, to avoid chattering between
      // the refine/unrefine state.
      if (std::fabs(d2circle-circleRadius) < epsilon && diameter > h) { 
        hadapt.MarkElement(elem, HAdapt::ELEM_REFINE);
      } else
      {
        if (std::fabs(d2circle-circleRadius) > 2*epsilon)
          hadapt.MarkElement(elem, HAdapt::ELEM_UNREFINE);
      }

    } // for elem


    // Now that all elements are marked, perform the refinement.  Loadbalance every load_bal steps.
    wave_refine_unrefine(hadapt, ((nstep % load_bal) == 0));


    // Write out the mesh at this frequency
    int freq = mesh.spatial_dim() == 2 ? 1 : 4;
    if ((nstep % freq) == 0) {
      char buf[512];

      std::sprintf(buf, "refine_out_%04d", nout++);
      WriteMesh(mesh, buf, 1, 0.0, ESMC_FILE_VTK);

    }

    // move circle
    center[0] += ddist; center[1] += ddist; center[2] += ddist;

    nstep++;
    
  } // while

}

/*-------------------------------------------------------------------*/
// Example Main.
/*-------------------------------------------------------------------*/
int main(int argc, char *argv[]) {


  // Initialize mpi, logfiles
  Par::Init(argc, argv, "REFOUT");

  int rank = Par::Rank(), csize = Par::Size();
  Mesh srcmesh;

  // Main execution loop; catch errors.
  try {

  // To avoid reading a datafile, we generate a start mesh by a simple procedure:
  // Build a hypercube on proc 0; Refine uniformly and then rebalance across
  // processors.

  // Choose any of the following topologies
  const MeshObjTopo *topo = GetTopo("QUAD");
  //const MeshObjTopo *topo = GetTopo("TRI3");

  //const MeshObjTopo *topo = GetTopo("HEX");
  //const MeshObjTopo *topo = GetTopo("TETRA");

  HyperCube(srcmesh, topo);

  // Initialize an adaptivity controller;
  HAdapt hadapt(srcmesh);

  // Register an element size variable (usefull for display in 3D)
  Context ctxt; ctxt.flip();
  hf = srcmesh.RegisterField("h", MEFamilyDG0::instance(), MeshObj::ELEMENT, ctxt, 1, true);

  // Commit the mesh.
  srcmesh.Commit();
  Par::Out() << "Mesh committed!" << std::endl;


  // Uniformly refine.  Destroy parents so that the refined mesh
  // is the genesis mesh.
  for (UInt i = 0; i < 4; i++) {
    hadapt.RefineUniformly(false);
    Rebalance(srcmesh);
  }

  std::cout << "********* Mesh Print: *********" << std::endl;
  srcmesh.Print(Par::Out());


  // Run the adaptivity test.
  adapt_wave(hadapt, srcmesh);

  } 
   catch (std::exception &x) {
    std::cerr << "std::Exception:" << x.what() << std::endl;
    std::cerr << std::flush;
    Par::Abort();
  }
   catch (const char *msg) {
    std::cerr << "Exception:" << msg << std::endl;
    std::cerr << std::flush;
    Par::Abort();
  }
  catch(...) {
    std::cerr << "Unknown exception:" << std::endl;
    std::cerr << std::flush;
    Par::Abort();
  }

  std::cout << "Run has completed" << std::endl;

  Par::End();

  return 0;
  
}

// These will be harmless on non pgi platforms.  On pgi, however, the inclusion of
// the fortran libraries on the link line causes these symbols to be needed.
extern "C" {
void __pgi_trace() {
}
void __pgi_tracee() {
}
}
