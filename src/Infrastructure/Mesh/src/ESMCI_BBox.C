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
#include <Mesh/include/ESMCI_BBox.h>
#include <Mesh/include/ESMCI_MeshTypes.h>
#include <Mesh/include/ESMCI_MeshObjTopo.h>
#include <Mesh/include/ESMCI_MeshUtils.h>
#include <Mesh/include/ESMCI_Mapping.h>
#include <Mesh/include/ESMCI_MeshllField.h>
#include <Mesh/include/ESMCI_ParEnv.h>

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

BBox::BBox(UInt _dim, const double _min[], const double _max[]) :
 isempty(false),
 dim(_dim)
{
  for (UInt i = 0; i < dim; i++) {
    min[i] = _min[i];
    max[i] = _max[i];
    if (min[i] > max[i]) isempty = true;
  }
}

BBox::BBox(const BBox &rhs) {
  *this = rhs;
}

BBox &BBox::operator=(const BBox &rhs) {
  if (this == &rhs) return *this;
  std::copy(&rhs.max[0], &rhs.max[0]+3, &max[0]);
  std::copy(&rhs.min[0], &rhs.min[0]+3, &min[0]);
  dim = rhs.dim;
  isempty = rhs.isempty;

  return *this;
}


  BBox::BBox(const MEField<> &coords, const MeshObj &obj, double normexp, bool is_sph) :
 isempty(false)
{
  if (obj.get_type() != MeshObj::ELEMENT) Throw() << "Not able to create BBOx for non element";
  const MeshObjTopo &topo = *GetMeshObjTopo(obj);
  const UInt npe = topo.num_nodes;

  dim = topo.spatial_dim;

  // Is a shell? TODO expand shell in normal directions
  if (topo.spatial_dim != topo.parametric_dim) {
    // Shell, expand by normexp in normal direction
    for (UInt i =0; i < dim; i++) {
      min[i] = std::numeric_limits<double>::max();
       max[i] = -std::numeric_limits<double>::max();
    }

    double nr[3];
    MasterElement<> *me = GetME(coords, obj)(METraits<>());
    std::vector<double> cd(3*me->num_functions());
    GatherElemData<>(*me, coords, obj, &cd[0]);

    double pc[] = {0,0};
    const Mapping<> *mp = GetMapping(obj)(MPTraits<>());
    mp->normal(1,&cd[0], &pc[0], &nr[0]);
   
    double ns = std::sqrt(nr[0]*nr[0]+nr[1]*nr[1]+nr[2]*nr[2]);

    // Only normalize if bigger than 0.0
    if (ns >1.0E-19) {
       nr[0] /= ns; nr[1] /= ns; nr[2] /= ns;
    } else {
      nr[0] =0.0; nr[1] =0.0; nr[2] =0.0;
    }

    /*
    if (obj.get_id() == 2426) {
      std::cout << "elem 2426 coords:";
       std::copy(&cd[0], &cd[0] + 3*me->num_functions(), std::ostream_iterator<double>(std::cout, " "));
      std::cout << std::endl;
    }*/
    // Get cell diameter
    double diam = 0;
    for (UInt n = 1; n < me->num_functions(); n++) {
      double dist = std::sqrt( (cd[0]-cd[3*n])*(cd[0]-cd[3*n]) +
               (cd[1]-cd[3*n+1])*(cd[1]-cd[3*n+1])
               + (cd[2]-cd[3*n+2])*(cd[2]-cd[3*n+2]));
      
      if (dist > diam) diam = dist;
    }
    
    // BOB   normexp *= diam;    
    normexp=2.0*diam;

    for (UInt n = 0; n < npe; n++) {
      for (UInt j = 0; j < dim; j++) {
        double lm;
        if ((lm = (cd[n*dim + j] + normexp*nr[j])) < min[j]) min[j] = lm;
        if ((lm =(cd[n*dim + j] - normexp*nr[j])) < min[j]) min[j] = lm;

        if ((lm=(cd[n*dim + j] + normexp*nr[j])) > max[j]) max[j] = lm;
         if ((lm=(cd[n*dim + j] - normexp*nr[j])) > max[j]) max[j] = lm;
      }
    } // for n
  } else {
    // Good old fashioned element
    for (UInt i =0; i < dim; i++) {
      min[i] = std::numeric_limits<double>::max();
      max[i] = -std::numeric_limits<double>::max();
    }

    // Loop the nodes
    for (UInt n = 0; n < topo.num_nodes; n++) {
      const MeshObj &node = *(obj.Relations[n].obj);
      const double *coord = coords.data(node);
      for (UInt j = 0; j < dim; j++) {
        if (coord[j] < min[j]) min[j] = coord[j];
        if (coord[j] > max[j]) max[j] = coord[j];
      }
    }

    // If this is on a 3D sphere then extend outward to include the bulge
    // Spatial dimension is assumed to be 3, because we don't allow sdim<pdim
    if ((topo.parametric_dim==3) && is_sph) {
      // Compute diameter of min max box
      // (as an easy stand in for diameter of the cell)
      double diam=std::sqrt((max[0]-min[0])*(max[0]-min[0])+
                            (max[1]-min[1])*(max[1]-min[1])+
                            (max[2]-min[2])*(max[2]-min[2]));

      // Reduce the diameter by 1/2 because
      // that's the most it can be (in the case that the cell is the diameter of the whole sphere)
      diam *=0.5;

      // Loop through extending the min max box if necessary
      for (UInt n = 0; n < topo.num_nodes; n++) {
        const MeshObj &node = *(obj.Relations[n].obj);
        const double *coord = coords.data(node);
        
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



BBox::BBox(const MEField<> &coords, const MeshDB &mesh) :
 isempty(false)
{

  dim = mesh.spatial_dim();

  for (UInt i =0; i < dim; i++) {
    min[i] = std::numeric_limits<double>::max();
    max[i] = -std::numeric_limits<double>::max();
  }

  // Loop nodes
  MeshDB::const_iterator ni = mesh.node_begin(), ne = mesh.node_end();
  for (; ni != ne; ni++) {
    const double *coord = coords.data(*ni);
    for (UInt i = 0; i < dim; i++) {
      if (coord[i] < min[i]) min[i] = coord[i];
      if (coord[i] > max[i]) max[i] = coord[i];
    }
  }
}

BBox::BBox(_field &coords, const MeshDB &mesh) {
  
  dim = mesh.spatial_dim();

  for (UInt i =0; i < dim; i++) {
    min[i] = std::numeric_limits<double>::max();
    max[i] = -std::numeric_limits<double>::max();
  }

  // Loop nodes
  MeshDB::const_iterator ni = mesh.node_begin(), ne = mesh.node_end();
  for (; ni != ne; ni++) {
    const double *coord = coords.data(*ni);
    for (UInt i = 0; i < dim; i++) {
      if (coord[i] < min[i]) min[i] = coord[i];
      if (coord[i] > max[i]) max[i] = coord[i];
    }
  }
  
}

#if 0
  // NOTE THAT THIS DOENS'T DO THE EXPANSION IN THE NORMAL DIRECTION
BBox::BBox(_field &coords, const MeshObj &obj) :
 isempty(false)
{
  if (obj.get_type() != MeshObj::ELEMENT) Throw() << "Not able to create BBOx for non element";
  const MeshObjTopo &topo = *GetMeshObjTopo(obj);
  const UInt npe = topo.num_nodes;

  if (topo.spatial_dim != topo.parametric_dim) 
    Throw() << "Won't work for shell elements !!!!";

  dim = topo.spatial_dim;

  for (UInt i =0; i < dim; i++) {
    min[i] = std::numeric_limits<double>::max();
    max[i] = -std::numeric_limits<double>::max();
  }
  
  // Loop the nodes
  for (UInt n = 0; n < topo.num_nodes; n++) {
    const MeshObj &node = *(obj.Relations[n].obj);
    const double *coord = coords.data(node);
    for (UInt j = 0; j < dim; j++) {
      if (coord[j] < min[j]) min[j] = coord[j];
      if (coord[j] > max[j]) max[j] = coord[j];
    }
  }
}

#endif

void BBox::checkEmpty() {
  isempty = false;
  for (UInt i = 0; i < dim; i++) {
    if (min[i] > max[i]) {
      isempty = true;
      return;
    }
  }
}

bool BBoxIntersect(const BBox &b1, const BBox &b2, double tol) {
  double newmin;
  double newmax;

  ThrowAssert(b1.dimension() == b2.dimension());
  for (UInt i = 0; i < b1.dimension(); i++) {
    newmin = std::max(b1.getMin()[i], b2.getMin()[i]);
    newmax = std::min(b1.getMax()[i], b2.getMax()[i]);
    if (newmin > (newmax+tol)) {
//std::cout << "fail, dim=" << i << " newmin=" << newmin << ", newmax=" << newmax << std::endl;
      return false;
    }
  }

  return true;
}

BBox BBoxIntersection(const BBox &b1, const BBox &b2) {
  BBox newbox(b1.dimension());

  ThrowAssert(b1.dimension() == b2.dimension());
  for (UInt i = 0; i < b1.dimension(); i++) {
    newbox.setMin(i, std::max(b1.getMin()[i], b2.getMin()[i]));
    newbox.setMax(i, std::min(b1.getMax()[i], b2.getMax()[i]));
  }

  newbox.checkEmpty();

  return newbox;
}

BBox BBoxParUnion(const BBox &b1) {
  double val, valres;
  BBox newbox(b1.dimension());


  for (UInt i = 0; i < b1.dimension(); i++) {
    // Find max 
    val = b1.getMax()[i];
    MPI_Allreduce(&val, &valres, 1, MPI_DOUBLE, MPI_MAX, Par::Comm());
    newbox.setMax(i, valres);
    val = b1.getMin()[i];
    MPI_Allreduce(&val, &valres, 1, MPI_DOUBLE, MPI_MIN, Par::Comm());
    newbox.setMin(i, valres);
  }

  newbox.checkEmpty();

  return newbox;
}

bool BBoxPointIn(const BBox &b, double point[], double tol) {
  for (UInt i = 0; i < b.dimension(); i++) {
    if (point[i] < b.getMin()[i] - tol || point[i] > b.getMax()[i] + tol) return false;
  }
  return true;
}


std::ostream &operator<<(std::ostream &os, const BBox &cn) {
  os << "empty:" << cn.isEmpty() << std::endl;
  os << "min: (";
  for (UInt i = 0; i < cn.dimension(); i++) {
    os << cn.getMin()[i] << ", ";
  }
  os << std::endl << "max: (";
  for (UInt i = 0; i < cn.dimension(); i++) {
    os << cn.getMax()[i] << ", ";
  }
  os << std::endl;

  return os;
}

} // namespace
