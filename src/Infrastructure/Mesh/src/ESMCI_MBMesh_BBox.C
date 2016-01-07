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

#include <Mesh/include/ESMCI_MBMesh_BBox.h>
#include <Mesh/include/ESMCI_Exception.h>

#include <limits>
#include <vector>
#include <cmath>

#include <mpi.h>

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

namespace ESMCI {

MBMesh_BBox::MBMesh_BBox(int _dim, const double _min[], const double _max[]) :
 isempty(false),
 dim(_dim)
{
  for (int i = 0; i < dim; i++) {
    min[i] = _min[i];
    max[i] = _max[i];
    if (min[i] > max[i]) isempty = true;
  }
}

MBMesh_BBox::MBMesh_BBox(const MBMesh_BBox &rhs) {
  *this = rhs;
}

MBMesh_BBox &MBMesh_BBox::operator=(const MBMesh_BBox &rhs) {
  if (this == &rhs) return *this;
  std::copy(&rhs.max[0], &rhs.max[0]+3, &max[0]);
  std::copy(&rhs.min[0], &rhs.min[0]+3, &min[0]);
  dim = rhs.dim;
  isempty = rhs.isempty;

  return *this;
}


MBMesh_BBox::MBMesh_BBox(MBMesh *mbmp, EntityHandle elem, double normexp) :
  isempty(false)
{
 
 int merr;

  // Set dim as spatial dim
  dim=mbmp->sdim;

  // Init 
  for (int i =0; i < dim; i++) {
    min[i] = std::numeric_limits<double>::max();
    max[i] = -std::numeric_limits<double>::max();
  }

  // Is a shell? TODO expand shell in normal directions
  if (mbmp->sdim != mbmp->pdim) {
    Throw() << "Doesn't work right now!";

  } else {

    // Get Verts in element
    int num_verts;
    const EntityHandle *verts;
    merr=mbmp->mesh->get_connectivity(elem,verts,num_verts);
    if (merr != MB_SUCCESS) {
      Throw() <<"MOAB ERROR: "<<moab::ErrorCodeStr[merr];
    }

    // Loop over verts
    for(int i=0; i<num_verts; i++) {
      // Get vert coords
      double coords[3];
      merr=mbmp->mesh->get_coords(verts+i,1,coords);
      if (merr != MB_SUCCESS) {
        Throw() <<"MOAB ERROR: "<<moab::ErrorCodeStr[merr];
      }

      // Modify min-max      
      for (int j = 0; j < dim; j++) {
        if (coords[j] < min[j]) min[j] = coords[j];
        if (coords[j] > max[j]) max[j] = coords[j];
      }
    }
  } // nonshell
}


MBMesh_BBox::MBMesh_BBox(MBMesh *mbmp) :
 isempty(false)
{

  int merr;

  // Set dim as spatial dim
  dim=mbmp->sdim;

  // Init
  for (int i =0; i < dim; i++) {
    min[i] = std::numeric_limits<double>::max();
    max[i] = -std::numeric_limits<double>::max();
  }

  // Loop verts
  for(int i=0; i<mbmp->num_verts; i++) {
      // Get vert coords
      double coords[3];
      merr=mbmp->mesh->get_coords(mbmp->verts+i,1,coords);
      if (merr != MB_SUCCESS) {
        Throw() <<"MOAB ERROR: "<<moab::ErrorCodeStr[merr];
      }

      // Modify min-max      
      for (int j = 0; j < dim; j++) {
        if (coords[j] < min[j]) min[j] = coords[j];
        if (coords[j] > max[j]) max[j] = coords[j];
      }
  }

}

void MBMesh_BBox::checkEmpty() {
  isempty = false;
  for (int i = 0; i < dim; i++) {
    if (min[i] > max[i]) {
      isempty = true;
      return;
    }
  }
}

bool MBMesh_BBoxIntersect(const MBMesh_BBox &b1, const MBMesh_BBox &b2, double tol) {
  double newmin;
  double newmax;

  ThrowAssert(b1.dimension() == b2.dimension());
  for (int i = 0; i < b1.dimension(); i++) {
    newmin = std::max(b1.getMin()[i], b2.getMin()[i]);
    newmax = std::min(b1.getMax()[i], b2.getMax()[i]);
    if (newmin > (newmax+tol)) {
//std::cout << "fail, dim=" << i << " newmin=" << newmin << ", newmax=" << newmax << std::endl;
      return false;
    }
  }

  return true;
}

MBMesh_BBox MBMesh_BBoxIntersection(const MBMesh_BBox &b1, const MBMesh_BBox &b2) {
  MBMesh_BBox newbox(b1.dimension());

  ThrowAssert(b1.dimension() == b2.dimension());
  for (int i = 0; i < b1.dimension(); i++) {
    newbox.setMin(i, std::max(b1.getMin()[i], b2.getMin()[i]));
    newbox.setMax(i, std::min(b1.getMax()[i], b2.getMax()[i]));
  }

  newbox.checkEmpty();

  return newbox;
}

MBMesh_BBox MBMesh_BBoxParUnion(const MBMesh_BBox &b1) {
  double val, valres;
  MBMesh_BBox newbox(b1.dimension());


  for (int i = 0; i < b1.dimension(); i++) {
    // Find max 
    val = b1.getMax()[i];
    //    MPI_Allreduce(&val, &valres, 1, MPI_DOUBLE, MPI_MAX, Par::Comm());
    newbox.setMax(i, valres);
    val = b1.getMin()[i];
    //MPI_Allreduce(&val, &valres, 1, MPI_DOUBLE, MPI_MIN, Par::Comm());
    newbox.setMin(i, valres);
  }

  newbox.checkEmpty();

  return newbox;
}

bool MBMesh_BBoxPointIn(const MBMesh_BBox &b, double point[], double tol) {
  for (int i = 0; i < b.dimension(); i++) {
    if (point[i] < b.getMin()[i] - tol || point[i] > b.getMax()[i] + tol) return false;
  }
  return true;
}


std::ostream &operator<<(std::ostream &os, const MBMesh_BBox &cn) {
  os << "empty:" << cn.isEmpty() << std::endl;
  os << "min: (";
  for (int i = 0; i < cn.dimension(); i++) {
    os << cn.getMin()[i] << ", ";
  }
  os << std::endl << "max: (";
  for (int i = 0; i < cn.dimension(); i++) {
    os << cn.getMax()[i] << ", ";
  }
  os << std::endl;

  return os;
}

}

#endif // ESMF_MOAB
