// $Id: ESMC_ShapeHierarchic.h,v 1.1.2.2 2009/01/21 21:25:22 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.


// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMC_Hierarchic_h
#define ESMC_Hierarchic_h

#include <ESMC_MeshTypes.h>
#include <ESMC_ShapeFunc.h>
#include <ESMC_Polynomial.h>
#include <map>

namespace ESMCI {
namespace MESH {

// ********* Base class for Hierarchic.  Implements Interpolation *****

class ShapeHier : public ShapeFunc {
protected:
  ShapeHier() {}
  ~ShapeHier() {}
  
   void do_Interpolate(UInt pdim, const double ipoints[],
                       const double fvals[], double mcoef[]) const;
public:
private:
  // Return shape values at node
  virtual void shape_node(UInt stride, // how to stride when storing results
                          UInt npts, // how many points to evaluate at
                          const double *pcoord, // parametric coords of point
                          double res[] 
                         ) const =0;
  // Return shape values at edge points
  virtual void shape_edge(UInt edge, // which edge
                          UInt stride, // how to stride when storing results
                          UInt npts, // how many points to evaluate at
                          const double *pcoord, // parametric coords of point
                          double res[] 
                         ) const =0;
  // Return shape values at face points
  virtual void shape_face(UInt face, // which face
                          UInt stride, // how to stride when storing results
                          UInt npts, // how many points to evaluate at
                          const double *pcoord, // parametric coords of point
                          double res[] 
                         ) const =0;
  // Return shape values at face points
  virtual void shape_elem(UInt stride, // how to stride when storing results
                          UInt npts, // how many points to evaluate at
                          const double *pcoord, // parametric coords of point
                          double res[] 
                         ) const =0;

  // Some info needed for interpolation
  virtual UInt NumNodes() const = 0;
  virtual UInt NumEdges() const = 0;
  virtual UInt NumEFunc(UInt e) const = 0; // Functions on edge
  virtual UInt NumFFunc(UInt e) const = 0; // Functions on face
  virtual UInt NumBubble() const = 0;
};


// ******* Quadrilateral ***********

class QuadHier : public ShapeHier {
QuadHier(UInt q);
~QuadHier() {}
static std::map<UInt,QuadHier*> qhMap;
public:
static QuadHier *instance(UInt q);
UInt NumFunctions() const { return nfunc;}
UInt ParametricDim() const { return 2;}
// Return array of the shape function values at the given pcoords(npts,pdim)
// in results(npts,ndofs)
void shape(UInt npts, const double pcoord[], double results[]) const;
void shape(UInt npts, const fad_type pcoord[], fad_type results[]) const;

// Return gradients at the given points pcoord(npts,pdim) in results(npts,ndofs,pdim) 
void shape_grads(UInt npts, const double pcoord[], double results[]) const;
void shape_grads(UInt npts, const fad_type pcoord[], fad_type results[]) const;

const std::string &name() const { return m_name;}

UInt IntgOrder() const { return q; }

// Return true if values only occur at nodes and interpolation is from node values.
bool is_nodal() const { return false; }

UInt NumInterp() const { return nfunc; };

// List of parametric points needed for interpolation, (NumInterp x pdim).
const double *InterpPoints() const { return &iPoints[0]; }

void Interpolate(const double fvals[], double mcoef[]) const;

// description for each dof (Obj type, Obj ordinal, index).
const int *DofDescriptionTable() const { return &dtable[0]; }

private:


// ********* Provide the following to ShapeHier base class for
// purposes of interpolation **************

// Return shape values at node
void shape_node(UInt stride, // how to stride when storing results
                          UInt npts, // how many points to evaluate at
                          const double *pcoord, // parametric coords of point
                          double res[] 
                         ) const;
// Return shape values at edge points
void shape_edge(UInt edge, // which edge
                          UInt stride, // how to stride when storing results
                          UInt npts, // how many points to evaluate at
                          const double *pcoord, // parametric coords of point
                          double res[] 
                         ) const;
// Return shape values at face points
void shape_face(UInt face, // which face
                          UInt stride, // how to stride when storing results
                          UInt npts, // how many points to evaluate at
                          const double *pcoord, // parametric coords of point
                          double res[] 
                         ) const;
// Return shape values at face points
void shape_elem(UInt stride, // how to stride when storing results
                          UInt npts, // how many points to evaluate at
                          const double *pcoord, // parametric coords of point
                          double res[] 
                         ) const;
UInt NumNodes() const { return 4; }
UInt NumEdges() const { return 4; }
UInt NumEFunc(UInt) const { return (q-1); }
UInt NumFFunc(UInt) const { return 0; }
UInt NumBubble() const { return (q-1)*(q-1); }

// ******** Data *********
UInt q;
std::vector<int> dtable; // dof description.
UInt nfunc;
std::string m_name;
// Need integrated legendre up to order q
std::vector<ILegendre<double> > ild;
std::vector<ILegendre<fad_type> > ilf;
std::vector<double> iPoints; // interpolation points.
};



// *************** Triangle *****************
class TriHier : public ShapeHier {
TriHier(UInt q);
~TriHier() {}
static std::map<UInt,TriHier*> qhMap;
public:
static TriHier *instance(UInt q);
UInt NumFunctions() const { return nfunc;}
UInt ParametricDim() const { return 2;}
// Return array of the shape function values at the given pcoords(npts,pdim)
// in results(npts,ndofs)
void shape(UInt npts, const double pcoord[], double results[]) const;
void shape(UInt npts, const fad_type pcoord[], fad_type results[]) const;

// Return gradients at the given points pcoord(npts,pdim) in results(npts,ndofs,pdim) 
void shape_grads(UInt npts, const double pcoord[], double results[]) const;
void shape_grads(UInt npts, const fad_type pcoord[], fad_type results[]) const;

const std::string &name() const { return m_name;}

UInt IntgOrder() const { return q; }

// Return true if values only occur at nodes and interpolation is from node values.
bool is_nodal() const { return false; }

UInt NumInterp() const { return nfunc; }

// List of parametric points needed for interpolation, (NumInterp x pdim).
const double *InterpPoints() const { return &iPoints[0]; }

void Interpolate(const double fvals[], double mcoef[]) const;

// description for each dof (Obj type, Obj ordinal, index).
const int *DofDescriptionTable() const { return &dtable[0]; }

private:

// ********* Provide the following to ShapeHier base class for
// purposes of interpolation **************

// Return shape values at node
void shape_node(UInt stride, // how to stride when storing results
                          UInt npts, // how many points to evaluate at
                          const double *pcoord, // parametric coords of point
                          double res[] 
                         ) const;
// Return shape values at edge points
void shape_edge(UInt edge, // which edge
                          UInt stride, // how to stride when storing results
                          UInt npts, // how many points to evaluate at
                          const double *pcoord, // parametric coords of point
                          double res[] 
                         ) const;
// Return shape values at face points
void shape_face(UInt face, // which face
                          UInt stride, // how to stride when storing results
                          UInt npts, // how many points to evaluate at
                          const double *pcoord, // parametric coords of point
                          double res[] 
                         ) const;
// Return shape values at face points
void shape_elem(UInt stride, // how to stride when storing results
                          UInt npts, // how many points to evaluate at
                          const double *pcoord, // parametric coords of point
                          double res[] 
                         ) const;
UInt NumNodes() const { return 3; }
UInt NumEdges() const { return 3; }
UInt NumEFunc(UInt) const { return (q-1); }
UInt NumFFunc(UInt) const { return 0; }
UInt NumBubble() const { return (q-1)*(q-2)/2; }


// ************** Data ************
UInt q;
std::vector<int> dtable; // dof description.
UInt nfunc;
std::string m_name;
// Need integrated legendre up to order q
std::vector<ILKernel<double> > ild;
std::vector<ILKernel<fad_type> > ilf;
std::vector<double> iPoints; // interpolation points.
};


// *************** Hex *****************
class HexHier : public ShapeHier {
HexHier(UInt q);
~HexHier() {}
static std::map<UInt,HexHier*> qhMap;
public:
static HexHier *instance(UInt q);
UInt NumFunctions() const { return nfunc;}
UInt ParametricDim() const { return 3;}
// Return array of the shape function values at the given pcoords(npts,pdim)
// in results(npts,ndofs)
void shape(UInt npts, const double pcoord[], double results[]) const;
void shape(UInt npts, const fad_type pcoord[], fad_type results[]) const;

// Return gradients at the given points pcoord(npts,pdim) in results(npts,ndofs,pdim) 
void shape_grads(UInt npts, const double pcoord[], double results[]) const;
void shape_grads(UInt npts, const fad_type pcoord[], fad_type results[]) const;

const std::string &name() const { return m_name;}

UInt IntgOrder() const { return q; }

// Return true if values only occur at nodes and interpolation is from node values.
bool is_nodal() const { return false; }

UInt NumInterp() const { return nfunc; }

// List of parametric points needed for interpolation, (NumInterp x pdim).
const double *InterpPoints() const { return &iPoints[0]; }

void Interpolate(const double fvals[], double mcoef[]) const;

// description for each dof (Obj type, Obj ordinal, index).
const int *DofDescriptionTable() const { return &dtable[0]; }

private:

// ********* Provide the following to ShapeHier base class for
// purposes of interpolation **************

// Return shape values at node
void shape_node(UInt stride, // how to stride when storing results
                          UInt npts, // how many points to evaluate at
                          const double *pcoord, // parametric coords of point
                          double res[] 
                         ) const;
// Return shape values at edge points
void shape_edge(UInt edge, // which edge
                          UInt stride, // how to stride when storing results
                          UInt npts, // how many points to evaluate at
                          const double *pcoord, // parametric coords of point
                          double res[] 
                         ) const;
// Return shape values at face points
void shape_face(UInt face, // which face
                          UInt stride, // how to stride when storing results
                          UInt npts, // how many points to evaluate at
                          const double *pcoord, // parametric coords of point
                          double res[] 
                         ) const;
// Return shape values at face points
void shape_elem(UInt stride, // how to stride when storing results
                          UInt npts, // how many points to evaluate at
                          const double *pcoord, // parametric coords of point
                          double res[] 
                         ) const;
UInt NumNodes() const { return 8; }
UInt NumEdges() const { return 12; }
UInt NumEFunc(UInt) const { return (q-1); }
UInt NumFFunc(UInt) const { return (q-1)*(q-1); }
UInt NumBubble() const { return (q-1)*(q-2)*(q-1); }


// ************** Data ************
UInt q;
std::vector<int> dtable; // dof description.
UInt nfunc;
std::string m_name;
// Need integrated legendre up to order q
std::vector<ILegendre<double> > ild;
std::vector<ILegendre<fad_type> > ilf;
std::vector<double> iPoints; // interpolation points.
};

} // namespace
} // namespace


#endif
