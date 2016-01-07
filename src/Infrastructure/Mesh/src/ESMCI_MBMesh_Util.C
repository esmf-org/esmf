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

// Take out if MOAB isn't being used
#ifdef ESMF_MOAB

#include <Mesh/include/ESMCI_Exception.h>
#include <Mesh/include/ESMCI_MBMesh.h>
#include <Mesh/include/ESMCI_WMat.h>
#include <Mesh/include/ESMCI_MBMesh_BBox.h>
#include <Mesh/include/ESMCI_MBMesh_Search.h>


#include <iostream>
#include <iterator>
#include <iomanip>
#include <cmath>
#include <vector>

#include <ESMCI_VM.h>

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------


#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

          
using namespace ESMCI;



  // Not really a math routine, but useful as a starting point for math routines
void MBMesh_get_elem_coords(MBMesh *mbmp, EntityHandle elem, int max_num_nodes, int *num_nodes, double *coords) {

  // MOAB Error
  int merr;

  // Coordinate dimension
  int sdim=mbmp->sdim;

  // Get Verts in element
  int num_verts;
  const EntityHandle *verts;
  merr=mbmp->mesh->get_connectivity(elem,verts,num_verts);
  if (merr != MB_SUCCESS) {
    Throw() <<"MOAB ERROR: "<<moab::ErrorCodeStr[merr];
  }


  // make sure that we're not bigger than max size
  if (num_verts > max_num_nodes) {
    Throw() << "Element exceeds maximum poly size";
  }

  // Loop over verts
  int k=0;
  for(int i=0; i<num_verts; i++) {
    // Get vert coords
    double c[3];
    merr=mbmp->mesh->get_coords(verts+i,1,c);
    if (merr != MB_SUCCESS) {
      Throw() <<"MOAB ERROR: "<<moab::ErrorCodeStr[merr];
    }

    // Load coords
    for (int j=0; j<sdim; j++) {
      coords[k]=c[j];
      k++;
    }
  }
  
  // Get number of nodes
  *num_nodes=num_verts;
}

#endif // ESMF_MOAB
