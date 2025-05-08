// $Id$
// Earth System Modeling Framework
// Copyright (c) 2002-2025, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_BBox_h
#define ESMCI_BBox_h

#include <Mesh/include/Legacy/ESMCI_MeshDB.h>
#include <Mesh/include/Legacy/ESMCI_MeshObj.h>
#include <Mesh/include/Legacy/ESMCI_MEField.h>  // for coords


#include <Mesh/include/Legacy/ESMCI_MeshTypes.h>

#include "ESMCI_PointList.h"

#include <iostream>

// Class to support basic bounding box type operations such
// as creation, intersection, etc...
namespace ESMCI {

class _field;

/**
 * A bounding box utility class.  Forms boxes around meshes and
 * mesh objects, intersects these, etc...
 */
class BBox {
public:
BBox() {}
BBox(UInt _dim) : isempty(true), dim(_dim) {}
BBox(UInt dim, const double min[], const double max[]); 
// Construct a box around an element.  If the object is a shell, the box
// will be expanded in the normal direction by normexp*diameter of object
 BBox(const MEField<> &coords, const MeshObj &obj, double normexp = 0.0, bool is_sph=false);

// Build a box around the whole mesh.  Not a cheap operation (loops nodes)
 BBox(const MEField<> &coords, const MeshDB &mesh, bool is_sph=false);

BBox(_field &coords, const MeshDB &mesh);

#if 0
  BBox(_field &coords, const MeshObj &obj);
#endif

BBox(const BBox &rhs);
BBox &operator=(const BBox &rhs);

const double *getMin() const { return &min[0];}
const double *getMax() const { return &max[0];}
void setMin(UInt i, double val) { min[i] = val;}
void setMax(UInt i, double val) { max[i] = val;}
bool isEmpty() const { return isempty;}
// Check to see if empty
void checkEmpty();
UInt dimension() const { return dim; }
private:
double min[3];
double max[3];
bool isempty;
UInt dim;
};

// Return the box intersection
BBox BBoxIntersection(const BBox &b1, const BBox &b2);

bool BBoxPointIn(const BBox &b1, double point[], double tol);

// Form a bounding box by taking the (outer) union of a parallel suite
BBox BBoxParUnion(const BBox &b1);

// Return true if the two boxes have nontrivial intersection
bool BBoxIntersect(const BBox &b1, const BBox &b2, double tol);

// Return true if 
bool BBoxSubset(const BBox &b1, const BBox &b2);

std::ostream &operator<<(std::ostream &os, const BBox &cn);

void build_pl_bbox(double *cmin, double *cmax, PointList *pl);

} // namespace

#endif
