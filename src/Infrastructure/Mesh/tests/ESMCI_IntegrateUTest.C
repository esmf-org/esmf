// $Id$
//==============================================================================
//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#ifndef MPICH_IGNORE_CXX_SEEK
#define MPICH_IGNORE_CXX_SEEK
#endif
#include <mpi.h>

#include <ESMCI_Extrapolation.h>
#include <ESMCI_Interp.h>
#include <ESMCI_Integrate.h>
#include <ESMCI_Mesh.h>
#include <ESMCI_MeshGen.h>
#include <ESMCI_MeshPNC.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <cmath>

// ESMF header
#include "ESMC.h"

// ESMF Test header
#include "ESMCI_Test.h"

#if !defined (M_PI)
// for Windows...
#define M_PI 3.14159265358979323846
#endif

using namespace ESMCI;

double compute_mass(const Mesh &mesh, MEField<> *field, MEField<> *wts) {
  double ans = 0;
  Mesh::const_iterator ni = mesh.node_begin(), ne = mesh.node_end();

  for (; ni != ne; ++ni) {
    const MeshObj &node = *ni;

    double *w = wts->data(node);
    double *f = field->data(node);
//printf("weight = %f  field = %f\n",*w, *f);
    ans += *w**f;
  }
  return ans;
}

void set_Fval(const Mesh &mesh, MEField<> *f1, MEField<> *f2, MEField<> *f3) {
  int rank = Par::Rank();

  Mesh::const_iterator ni = mesh.node_begin(), ne = mesh.node_end();
  MEField<> *cfield = mesh.GetCoordField();

  for (; ni != ne; ++ni) {
    const MeshObj &node = *ni;

    double *c = cfield->data(node);
    double *F1 = f1->data(node);
    double *F2 = f2->data(node);
    double *F3 = f3->data(node);

//   ~~~~~~~~~~~~~~~~~Fval 111111111 ~~~~~~~~~~~~~
      *F1 = 1;

//   ~~~~~~~~~~~~~~~~~Fval 222222222 ~~~~~~~~~~~~~
    if (c[0] < 0.0) *F2 = 
2.0 + (1 - (pow(c[1],2)/(pow(c[0],2) + pow(c[1],2))))
*cos(2.0*std::acos(c[2]/(std::sqrt(std::pow(c[0],2) + std::pow(c[1],2))+std::pow(c[2],2))))
;
    if (c[0] >= 0.0) *F2 = 
2.0 + pow(cos(M_PI - asin(pow(c[1],2)/(pow(c[0],2) + pow(c[1],2)))),2)
*cos(2.0*std::acos(c[2]/(std::sqrt(std::pow(c[0],2) + std::pow(c[1],2))+std::pow(c[2],2))))
;

//   ~~~~~~~~~~~~~~~~~Fval 3333333333 ~~~~~~~~~~~~~
    if (c[0] < 0.0) *F3 = 
2.0 + cos(16*(std::acos(c[2]/(std::sqrt(std::pow(c[0],2) + std::pow(c[1],2))+std::pow(c[2],2)))))
* pow(sin(2*(M_PI -(std::asin(c[1]/(std::sqrt(std::pow(c[0],2) + std::pow(c[1],2))))))),16) 
;
    if (c[0] >= 0.0) *F3 = 
2.0 + cos(16*(std::acos(c[2]/(std::sqrt(std::pow(c[0],2) + std::pow(c[1],2))+std::pow(c[2],2)))))
* pow(sin(2*(std::asin(c[1]/(std::sqrt(std::pow(c[0],2) + std::pow(c[1],2)))))),16) 
;
  }
}

int main(int argc, char *argv[]) {

  MPI_Init(&argc, &argv);
  
  Par::Init("PATCHLOG", false);

  Mesh mesh;

    // Create a curved cell mesh
    SphShell(mesh, 2, 2, M_PI/2, M_PI/2+M_PI/128, 0.0, 0.0+2*M_PI/64);

    // Add fields to mesh
    Context ctxt; ctxt.flip();
    MEField<> *iwts = mesh.RegisterField("iwts", 
      MEFamilyStd::instance(), MeshObj::ELEMENT, ctxt, 1, true); 
 
    MEField<> *sF1 = mesh.RegisterField("S:f=1",
      MEFamilyStd::instance(), MeshObj::ELEMENT, ctxt, 1, true);

    MEField<> *sF2 = mesh.RegisterField("S:f=f=2+cos^2(T)cos(2P)",
      MEFamilyStd::instance(), MeshObj::ELEMENT, ctxt, 1, true);

    MEField<> *sF3 = mesh.RegisterField("S:2+sin^16(2T)cos(16P)",
      MEFamilyStd::instance(), MeshObj::ELEMENT, ctxt, 1, true);

    // Commit the meshes
    mesh.Commit();

    // Use the coordinate fields for interpolation purposes
    MEField<> &scoord = *mesh.GetCoordField();

  // generate integration weights
  Integrate sig(mesh);
  sig.intWeights(iwts);

  // analyze the integration weights
  double analytic[3], mass[3];
/*
  // for full sphere functions
  analytic[0] = 12.566370614359172;
  analytic[1] = 23.038346126325148;
  analytic[2] = 25.123063614630066;
*/
  // for 1 element
  analytic[0] = 0.0024093294761765335;
  analytic[1] = 0.0024180194494409237;
  analytic[2] = 0.004818658952353682;

  // set the field values
  set_Fval(mesh, sF1, sF2, sF3);

  // compute the mass
  mass[0] = compute_mass(mesh,sF1,iwts);
  mass[1] = compute_mass(mesh,sF2,iwts);
  mass[2] = compute_mass(mesh,sF3,iwts);

  double massg[3];
  MPI_Allreduce(mass, massg, 3, MPI_DOUBLE, MPI_SUM, MPI_COMM_WORLD);

  using std::cout;
  using std::left;
  using std::endl;
  using std::scientific;
  using std::fixed;
  using std::setprecision;
  using std::setw;
  using std::abs;

  if (Par::Rank() == 0) {

  cout<<endl<<endl
    <<"Field integrals"
    <<endl<<endl
    <<left<<setw(12)<<"analytic"
    <<left<<setw(12)<<"ESMF"
    <<left<<setw(15)<<"ESMF err."
    <<endl;

  for(UInt i=0; i<3; ++i) {
    cout<<setprecision(8)<<fixed<<left<<setw(12)<<analytic[i]
        <<setprecision(8)<<fixed<<setw(12)<<massg[i]
        <<setprecision(8)<<scientific<<setw(15)<<abs(analytic[i]-massg[i])/abs(analytic[i])
        <<endl;
  }
  }

  bool pass;
  int result = 0;
  char name[80];
  char failMsg[80];

  pass=std::abs(analytic[0]-massg[0])/std::abs(analytic[0])<1e-14;

  //----------------------------------------------------------------------------
  TestStart(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEXdisable_UTest
  // Create a mesh
  strcpy(name, "Integrate");
  strcpy(failMsg, "Error in integral > 1e-14");
  //Test((pass==true), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  TestEnd(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------f

  Par::End();

  return 0;
  
}
