// $Id$
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_MBMesh_BBox_h
#define ESMCI_MBMesh_BBox_h

// Take out if MOAB isn't being used
#if defined ESMF_MOAB

#include <Mesh/include/ESMCI_MBMesh.h>

#include "ESMCI_BBox.h"

#include <iostream>

// Class to support basic bounding box type operations such
// as creation, intersection, etc...
namespace ESMCI {

/**
 * A bounding box utility class.  Forms boxes around meshes and
 * mesh objects, intersects these, etc...
 */
class MBMesh_BBox {
public:
MBMesh_BBox() {}
MBMesh_BBox(int _dim) : isempty(true), dim(_dim) {}
MBMesh_BBox(int dim, const double min[], const double max[]); 

#if defined ESMF_MOAB
// Construct a box around an element.  If the object is a shell, the box
// will be expanded in the normal direction by normexp*diameter of object
 MBMesh_BBox(MBMesh *mbmp, EntityHandle elem, double normexp = 0.0);
#endif

// Build a box around the whole mesh.  Not a cheap operation (loops nodes)
MBMesh_BBox(MBMesh *mesh);

#if 0
 MBMesh_BBox(_field &coords, const MeshObj &obj);
#endif

MBMesh_BBox(const MBMesh_BBox &rhs);
MBMesh_BBox &operator=(const MBMesh_BBox &rhs);

const double *getMin() const { return &min[0];}
const double *getMax() const { return &max[0];}
void setMin(int i, double val) { min[i] = val;}
void setMax(int i, double val) { max[i] = val;}
bool isEmpty() const { return isempty;}
// Check to see if empty
void checkEmpty();
int dimension() const { return dim; }
private:
double min[3];
double max[3];
bool isempty;
int dim;
};

// Return the box intersection
MBMesh_BBox MBMesh_BBoxIntersection(const MBMesh_BBox &b1, const MBMesh_BBox &b2);

bool MBMesh_BBoxPointIn(const MBMesh_BBox &b1, double point[], double tol);

// Form a bounding box by taking the (outer) union of a parallel suite
MBMesh_BBox MBMesh_BBoxParUnion(const MBMesh_BBox &b1);

// Return true if the two boxes have nontrivial intersection
bool MBMesh_BBoxIntersect(const MBMesh_BBox &b1, const MBMesh_BBox &b2, double tol);
bool Mixed_BBoxIntersect(const MBMesh_BBox &b1, const BBox &b2, double tol);

// Return true if 
bool MBMesh_BBoxSubset(const MBMesh_BBox &b1, const MBMesh_BBox &b2);

std::ostream &operator<<(std::ostream &os, const MBMesh_BBox &cn);


} // namespace

#endif // ESMF_MOAB

#endif
