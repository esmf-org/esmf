// $Id: ESMC_BBox.C,v 1.2.2.2 2009/01/21 21:25:22 cdeluca Exp $
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
#include <ESMC_BBox.h>

#include <ESMC_MeshTypes.h>

#include <ESMC_MeshObjTopo.h>

#include <ESMC_MeshUtils.h>
#include <ESMC_Mapping.h>

#include <limits>
#include <vector>
#include <cmath>

#include <mpi.h>

namespace ESMCI {
namespace MESH {

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
  std::copy(&rhs.max[0], &rhs.max[3], &max[0]);
  std::copy(&rhs.min[0], &rhs.min[3], &min[0]);
  dim = rhs.dim;
  isempty = rhs.isempty;

  return *this;
}

BBox::BBox(const MEField<> &coords, const MeshObj &obj, double normexp) :
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
    GatherElemData(*me, coords, obj, &cd[0]);

    double pc[] = {0,0};
    const Mapping<> *mp = GetMapping(obj)(MPTraits<>());
    mp->normal(1,&cd[0], &pc[0], &nr[0]);
    
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
    MPI_Allreduce(&val, &valres, 1, MPI_DOUBLE, MPI_MAX, MPI_COMM_WORLD);
    newbox.setMax(i, valres);
    val = b1.getMin()[i];
    MPI_Allreduce(&val, &valres, 1, MPI_DOUBLE, MPI_MIN, MPI_COMM_WORLD);
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
} // namespace
