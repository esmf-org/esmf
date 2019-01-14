// $Id$
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

// Take out if MOAB isn't being used
#if defined ESMF_MOAB

#include <Mesh/include/ESMCI_MBMesh_BBox.h>
#include <Mesh/include/ESMCI_MBMesh_Util.h>
#include <Mesh/include/ESMCI_MathUtil.h>
#include <Mesh/include/Legacy/ESMCI_Exception.h>

#include <limits>
#include <vector>
#include <cmath>

#include <mpi.h>

#include <ESMCI_VM.h>
#include "ESMCI_LogErr.h"

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

namespace ESMCI {

/// Eventually move the following to ESMCI_MBMesh_Util.C
void MU_calc_unit_normal(double *p1, double *p2, double *p3, double *out_normal) {

  // calc outward normal to triangle
  double vec12[3];
  vec12[0]=p2[0]-p1[0];
  vec12[1]=p2[1]-p1[1];
  vec12[2]=p2[2]-p1[2];

  double vec13[3];
  vec13[0]=p3[0]-p1[0];
  vec13[1]=p3[1]-p1[1];
  vec13[2]=p3[2]-p1[2];

  // normal to plane
  MU_CROSS_PRODUCT_VEC3D(out_normal,vec12,vec13);

  // normalize
  double len=MU_LEN_VEC3D(out_normal);
  if (len > 0.0) {
    out_normal[0] /= len;
    out_normal[1] /= len;
    out_normal[2] /= len;
  }
}

  void MBMesh_get_elem_unorm(MBMesh *mbmp, EntityHandle elem, double *unorm) {
      // Struct to hold coords
#define MAX_NUM_NODES 5
    double p[3*MAX_NUM_NODES];   
    int num_p;

    // Get coords
    MBMesh_get_elem_coords(mbmp, elem, MAX_NUM_NODES, &num_p, p);
 
    // Compute normal based on number of sides    
    if (num_p == 3) {
      MU_calc_unit_normal(p, p+3, p+6, unorm);
    } else if (num_p == 4) {
      double unorm1[3], unorm2[3];

      // Calc normals for two triangles in quad.
      MU_calc_unit_normal(p, p+3, p+6, unorm1);
      MU_calc_unit_normal(p, p+6, p+9, unorm2);

      // Sum vectors
      MU_ADD_VEC3D(unorm1,unorm1,unorm2);

      // I DON'T THINK THAT WE NEED TO DO THIS SINCE WE'RE DIVIDING BY THE LENGTH BELOW
      // Divide by 2
      // MU_DIV_BY_SCALAR_VEC3D(unorm,unorm1,2.0);     

      // Make sum a unit vector
      double len=MU_LEN_VEC3D(unorm1);
      if (len > 0.0) {
        MU_DIV_BY_SCALAR_VEC3D(unorm,unorm1,len);     
      }
    } else {
      Throw() << "Normal computation currently only supports polygons with 3 or 4 sides.";
    }

#undef MAX_NUM_NODES
  }


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


MBMesh_BBox::MBMesh_BBox(MBMesh *mbmp, EntityHandle elem, double normexp, bool is_sph) :
  isempty(false)
{

 int merr;

 // Don't handle 3D things right now
 // if (mbmp->pdim==3) Throw() << "Need to handle 3D spheres!";

  // Set dim as spatial dim
  dim=mbmp->sdim;

  // Init
  for (int i =0; i < dim; i++) {
    min[i] = std::numeric_limits<double>::max();
    max[i] = -std::numeric_limits<double>::max();
  }

  // Is a shell? TODO expand shell in normal directions
  if (mbmp->sdim != mbmp->pdim) {

    // Shell, expand in normal direction
    for (UInt i =0; i < dim; i++) {
      min[i] = std::numeric_limits<double>::max();
      max[i] = -std::numeric_limits<double>::max();
    }

    // Get normal
    double norm[3];
    MBMesh_get_elem_unorm(mbmp, elem, norm);

    // Get elem corner points
#define MAX_NUM_NODES 5
    double p[3*MAX_NUM_NODES];
    int num_p;

    // Get coords
    MBMesh_get_elem_coords(mbmp, elem, MAX_NUM_NODES, &num_p, p);

    // Get cell diameter
    double diam = 0;
    for (UInt n = 1; n < num_p; n++) {
      double dist = std::sqrt( (p[0]-p[3*n])*(p[0]-p[3*n]) +
                               (p[1]-p[3*n+1])*(p[1]-p[3*n+1]) +
                               (p[2]-p[3*n+2])*(p[2]-p[3*n+2]));

      if (dist > diam) diam = dist;
    }

    // Exapnd by twice the diameter, because that should take
    // care of including any volume included by the sphere buldging out inside
    // the cell
    double expand=2.0*diam;
    for (int n = 0; n < num_p; n++) {
      for (int j = 0; j < dim; j++) {
        double lm;
        if ((lm = (p[n*dim + j] + expand*norm[j])) < min[j]) min[j] = lm;
        if ((lm = (p[n*dim + j] - expand*norm[j])) < min[j]) min[j] = lm;

        if ((lm = (p[n*dim + j] + expand*norm[j])) > max[j]) max[j] = lm;
        if ((lm = (p[n*dim + j] - expand*norm[j])) > max[j]) max[j] = lm;
      }
    } // for n

#undef MAX_NUM_NODES

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
    
    // If this is on a 3D sphere then extend outward to include the bulge
    // Spatial dimension is assumed to be 3, because we don't allow sdim<pdim
    if ((mbmp->pdim==3) && is_sph) {
      // Compute diameter of min max box
      // (as an easy stand in for diameter of the cell)
      double diam=std::sqrt((max[0]-min[0])*(max[0]-min[0])+
                            (max[1]-min[1])*(max[1]-min[1])+
                            (max[2]-min[2])*(max[2]-min[2]));
    
      // Reduce the diameter by 1/2 because
      // that's the most it can be (in the case that the cell is the diameter of the whole sphere)
      diam *=0.5;
    
      // // Loop through extending the min max box if necessary
      // for (UInt n = 0; n < topo.num_nodes; n++) {
      //   const MeshObj &node = *(obj.Relations[n].obj);
      //   const double *coord = coords.data(node);

      // Loop over verts
      for(int i=0; i<num_verts; i++) {
        // Get vert coords
        double coord[3];
        merr=mbmp->mesh->get_coords(verts+i,1,coord);
        if (merr != MB_SUCCESS) {
          Throw() <<"MOAB ERROR: "<<moab::ErrorCodeStr[merr];
        }
    
        // Compute unit vector in direction of point 
        double len=std::sqrt(coord[0]*coord[0]+coord[1]*coord[1]+coord[2]*coord[2]);
        double uvec[3];
        uvec[0]=coord[0]/len;
        uvec[1]=coord[1]/len;
        uvec[2]=coord[2]/len;
    
        // Compute new point
        double new_pnt[3];
        new_pnt[0]=coord[0]+diam*uvec[0];
        new_pnt[1]=coord[1]+diam*uvec[1];
        new_pnt[2]=coord[2]+diam*uvec[2];
    
        for (UInt j = 0; j < 3; j++) {
          if (new_pnt[j] < min[j]) min[j] = new_pnt[j];
          if (new_pnt[j] > max[j]) max[j] = new_pnt[j];
        }
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

// mixed version of the duplicated BBoxIntersect versions for MBMesh_BBox & BBox
bool Mixed_BBoxIntersect(const MBMesh_BBox &b1, const BBox &b2, double tol) {
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
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_BBoxParUnion()"
  double val, valres;
  MBMesh_BBox newbox(b1.dimension());

  // Get Parallel Information from VM
  int localrc;
  MPI_Comm comm = VM::getCurrent(&localrc)->getMpi_c();
  if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
    throw localrc;  // bail out with exception

  // TODO: change this to just be 2 mpi calls instead of a loop
  for (int i = 0; i < b1.dimension(); i++) {
    // Find max
    val = b1.getMax()[i];
    MPI_Allreduce(&val, &valres, 1, MPI_DOUBLE, MPI_MAX, comm);
    newbox.setMax(i, valres);
    val = b1.getMin()[i];
    MPI_Allreduce(&val, &valres, 1, MPI_DOUBLE, MPI_MIN, comm);
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
