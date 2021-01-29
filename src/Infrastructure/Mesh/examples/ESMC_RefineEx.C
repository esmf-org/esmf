// $Id$
//==============================================================================
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <ESMCI_Mesh.h>
#include <ESMCI_MeshSkin.h>
#include <ESMCI_ShapeFunc.h>
#include <ESMCI_Mapping.h>
//#include <ESMCI_Search.h>
#include <ESMCI_MeshUtils.h>
#include <ESMCI_MasterElement.h>
#include <ESMCI_MeshRead.h>
#include <ESMCI_ParEnv.h>
#include <ESMCI_MeshRefine.h>
#include <ESMCI_RefineTopo.h>
#include <ESMCI_MeshObjConn.h>
#include <ESMCI_MeshGen.h>
#include <ESMCI_HAdapt.h>
#include <ESMCI_Rebalance.h>
#include <ESMCI_MeshTypes.h>

#include <mpi.h>

#include <iterator>
#include <ostream>
#include <iostream>
#include <stdexcept>
#include <cmath>
#include <cstdio>
#include <mpi.h>

/*
 * Example:x_refine
 * This example shows how to use mesh adaptivity and load balancing.
 * To run,
 * mpirun -np 10 x_refine
 * 
 * We build a hypercube on proc 0, and refine while load balancing until
 * we have a mesh of the unit cube/sphere load balanced.  We then 'advect' an
 * adpativity pattern through the mesh, load balancing at each (or some frequency)
 * step.
 * 
 * Output:
 * The output is a time series of exodus files refine_out_[tstep].proc_num.proc.g.
 * To view these files, run the MeshCat utility, provided in this directory.
 * Then load the files in paraview.
 * 
 * ./MeshCat x_dcat refine_out 10
 * 
 */


using namespace ESMCI;


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

  Par::Out() << "** Refining mesh ** " << std::endl;
  hadapt.RefineMesh();

  // Rebalance the mesh when flag is set.
  if (rebal) {
    Par::Out() << "** Rebalancing mesh ** " << std::endl;
    Rebalance(mesh);
  }

//mesh.DataStoreInfo(std::cout);

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
      diam += std::abs(coord0[0] - coord[0]);
      diam += std::abs(coord0[1] - coord[1]);
      if (mesh.spatial_dim() == 3)
        diam += std::abs(coord0[2] - coord[2]);

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
void test_adapt_wave_exec(HAdapt &hadapt, Mesh &mesh) {

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
    Mesh::iterator b_obj = mesh.elem_begin(), e_obj = mesh.elem_end(), i_obj;
    for (i_obj = b_obj; i_obj != e_obj; ++i_obj) {
      MeshObj &elem = (*i_obj);

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
      if (std::abs(d2circle-circleRadius) < epsilon && diameter > h) { 
        hadapt.MarkElement(elem, HAdapt::ELEM_REFINE);
      } else
      {
        if (std::abs(d2circle-circleRadius) > 2*epsilon)
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
      WriteMesh(mesh, buf, 0, NULL, 0, NULL, 1, 0.0, ESMC_FILE_VTK);

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
  
  MPI_Init(&argc, &argv);

  // Initialize mpi, logfiles
  Par::Init("REFOUT");

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
    hf = srcmesh.RegisterField("h",                      // field name
                                MEFamilyDG0::instance(), // master element family (element field, here)
                                MeshObj::ELEMENT,        // support type (face or element)
                                ctxt,                    // Where to register field 
                                1,                       // Field dimension
                                true                     // Output status
                                );
  
  
   // srcmesh.UseSides(true);
    
    // Commit the mesh.
    srcmesh.Commit();
    Par::Out() << "Mesh committed!" << std::endl;
  
//srcmesh.Print(Par::Out());
    // Uniformly refine.  Destroy parents so that the refined mesh
    // is the genesis mesh.
    const int nref = srcmesh.spatial_dim() == 3 ? 1 : 5;
    for (UInt i = 0; i < nref; i++) {
      hadapt.RefineUniformly(false);
      if ((i % 3) == 0) Rebalance(srcmesh);
    }
  
    // Now rebalance across processors
    Rebalance(srcmesh);
  
  
    //    std::cout << "********* Mesh Print: *********" << std::endl;
    //srcmesh.Print(Par::Out());
  
  
    // Run the adaptivity test.
    test_adapt_wave_exec(hadapt, srcmesh);

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
