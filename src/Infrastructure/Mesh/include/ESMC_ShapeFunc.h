// $Id: ESMC_ShapeFunc.h,v 1.1.2.2 2009/01/21 21:25:22 cdeluca Exp $
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

#ifndef ESMC_ShapeFunc_h
#define ESMC_ShapeFunc_h

#include <ESMC_MeshTypes.h>

#include <string>

namespace ESMCI {
namespace MESH {

// A variety of shape functions defined on basic topological entities

// dof_description:
//       for each dof, a tuple = {type |node=0,edge=1,face=2,elem=3}|
//                                ordinal |e.g, node 5, edge 3, etc...|
//                                index |e.g, edge 4, second dof|


typedef enum {DOF_NODE=0, DOF_EDGE=1, DOF_FACE=2, DOF_ELEM=3} DofLoc;

// An abstract Base class that an implementation can (but doesn't have to) inherit from.

class ShapeFunc {
public:
  ShapeFunc() {}
  virtual ~ShapeFunc() {}
  virtual UInt NumFunctions() const = 0;
  virtual UInt ParametricDim() const = 0;
  virtual UInt IntgOrder() const = 0;
  // Return array of the shape function values at the given pcoords(npts,pdim)
  // in results(npts,ndofs)
  virtual void shape(UInt npts, const double pcoord[], double results[]) const = 0;
  virtual void shape(UInt npts, const fad_type pcoord[], fad_type results[]) const = 0;

  // Return gradients at the given points pcoord(npts,pdim) in results(npts,ndofs,pdim) 
  virtual void shape_grads(UInt npts, const double pcoord[], double results[]) const = 0;
  virtual void shape_grads(UInt npts, const fad_type pcoord[], fad_type results[]) const = 0;

  virtual const std::string &name() const = 0;

  // Return true if values only occur at nodes and interpolation is from node values.
  virtual bool is_nodal() const = 0;

  virtual UInt NumInterp() const = 0;

  // List of parametric points needed for interpolation, (NumInterp x pdim).
  virtual const double *InterpPoints() const = 0;

  // Form coefficients, given function values at ipoints;
  virtual void Interpolate(const double fvals[], double mcoef[]) const = 0;

  // description for each dof (Obj type, Obj ordinal, index).
  virtual const int *DofDescriptionTable() const = 0;
};

// PDIM
template<int PDIM>
class dg0_shape_func {
  public:
  dg0_shape_func(){}
  unsigned int num_dofs() { return ndofs;}
  const static unsigned int ndofs = 1;
  const static unsigned int pdim = PDIM;
  const static UInt iorder = 1;

  // Return array of the shape function values at the given pcoords(npts,pdim)
  // in results(npts,ndofs)
  template<typename ScalarT>
  static void shape(unsigned int npts, const ScalarT pcoord[], ScalarT results[]);

  // Return gradients at the given points pcoord(npts,pdim) in results(npts,ndofs,pdim) 
  template<typename ScalarT>
  static void shape_grads(unsigned int npts, const ScalarT pcoord[], ScalarT results[]);

  // Is the parametric point in the cell??
  static bool is_in(const double pcoord[], double *dist = NULL);
 
  static const std::string name;

  static bool is_nodal() { return false;}

  static const UInt NumInterp = 1;

  const static int dof_description[ndofs][4];
};

class bar_shape_func {
  public:
  bar_shape_func(){}
  unsigned int num_dofs() { return ndofs;}
  const static unsigned int ndofs = 2;
  const static unsigned int pdim = 1;
  const static UInt iorder = 2;

  // Return array of the shape function values at the given pcoords(npts,pdim)
  // in results(npts,ndofs)
  template<typename ScalarT>
  static void shape(unsigned int npts, const ScalarT pcoord[], ScalarT results[]);

  // Return gradients at the given points pcoord(npts,pdim) in results(npts,ndofs,pdim) 
  template<typename ScalarT>
  static void shape_grads(unsigned int npts, const ScalarT pcoord[], ScalarT results[]);

  // Is the parametric point in the cell??
  static bool is_in(const double pcoord[], double *dist = NULL);
 
  static const std::string name;

  static const UInt NumInterp = ndofs;

  const static int dof_description[ndofs][4];

  static bool is_nodal() { return true;}

};

class bar3_shape_func {
  public:
  bar3_shape_func(){}
  unsigned int num_dofs() { return ndofs;}
  const static unsigned int ndofs = 3;
  const static unsigned int pdim = 1;
  const static UInt iorder = 3;

  // Return array of the shape function values at the given pcoords(npts,pdim)
  // in results(npts,ndofs)
  template<typename ScalarT>
  static void shape(unsigned int npts, const ScalarT pcoord[], ScalarT results[]);

  // Return gradients at the given points pcoord(npts,pdim) in results(npts,ndofs,pdim) 
  template<typename ScalarT>
  static void shape_grads(unsigned int npts, const ScalarT pcoord[], ScalarT results[]);

  // Is the parametric point in the cell??
  static bool is_in(const double pcoord[], double *dist = NULL);
 
  static const std::string name;

  static const UInt NumInterp = ndofs;

  const static int dof_description[ndofs][4];

  static bool is_nodal() { return true;}

};

class tri_shape_func {
  public:
  tri_shape_func() {}
  unsigned int num_dofs() { return ndofs;}
  const static unsigned int ndofs = 3;
  const static unsigned int pdim = 2;
  const static UInt iorder = 2;

  // Return array of the shape function values at the given pcoords(npts,pdim)
  // in results(npts,ndofs)
  template<typename ScalarT>
  static void shape(unsigned int npts, const ScalarT pcoord[], ScalarT results[]);

  // Return gradients at the given points pcoord(npts,pdim) in results(npts,ndofs,pdim) 
  template<typename ScalarT>
  static void shape_grads(unsigned int npts, const ScalarT pcoord[], ScalarT results[]);

  // Is the parametric point in the cell??
  static bool is_in(const double pcoord[], double *dist=NULL);

  static const std::string name;
  const static int dof_description[ndofs][4];

  static const UInt NumInterp = ndofs;

  static bool is_nodal() { return true;}
};

class quad_shape_func {
  public:
  quad_shape_func() {}
  unsigned int num_dofs() { return ndofs;}
  const static unsigned int ndofs = 4;
  const static unsigned int pdim = 2;
  const static UInt iorder = 2;
  const static double one4th;

  // Return array of the shape function values at the given pcoords(npts,pdim)
  // in results(npts,ndofs)
  template<typename ScalarT>
  static void shape(unsigned int npts, const ScalarT pcoord[], ScalarT results[]);

  // Return gradients at the given points pcoord(npts,pdim) in results(npts,ndofs,pdim) 
  template<typename ScalarT>
  static void shape_grads(unsigned int npts, const ScalarT pcoord[], ScalarT results[]);

  // Is the parametric point in the cell??
  static bool is_in(const double pcoord[], double *dist=NULL);

  static const std::string name;
  const static int dof_description[ndofs][4];

  static const UInt NumInterp = ndofs;

  static bool is_nodal() { return true;}
};

class quad9_shape_func {
  public:
  quad9_shape_func() {}
  unsigned int num_dofs() { return ndofs;}
  const static unsigned int ndofs = 9;
  const static unsigned int pdim = 2;
  const static UInt iorder = 3;

  // Return array of the shape function values at the given pcoords(npts,pdim)
  // in results(npts,ndofs)
  template<typename ScalarT>
  static void shape(unsigned int npts, const ScalarT pcoord[], ScalarT results[]);

  // Return gradients at the given points pcoord(npts,pdim) in results(npts,ndofs,pdim) 
  template<typename ScalarT>
  static void shape_grads(unsigned int npts, const ScalarT pcoord[], ScalarT results[]);

  // Is the parametric point in the cell??
  static bool is_in(const double pcoord[], double *dist=NULL);

  static const std::string name;
  const static int dof_description[ndofs][4];

  static const UInt NumInterp = ndofs;

  static bool is_nodal() { return true;}
};

// A variety of shape functions defined on basic topological entities
class hex_shape_func {
  public:
  hex_shape_func() {}
  unsigned int num_dofs() { return ndofs;}
  const static unsigned int ndofs = 8;
  const static unsigned int pdim = 3;
  const static double one8th;
  const static UInt iorder = 2;

  // Return array of the shape function values at the given pcoords(npts,pdim)
  // in results(npts,ndofs)
  template<typename ScalarT>
  static void shape(unsigned int npts, const ScalarT pcoord[], ScalarT results[]);

  // Return gradients at the given points pcoord(npts,pdim) in results(npts,ndofs,pdim) 
  template<typename ScalarT>
  static void shape_grads(unsigned int npts, const ScalarT pcoord[], ScalarT results[]);

  // Is the parametric point in the cell??
  static bool is_in(const double pcoord[], double *dist = NULL);

  static const std::string name;
  const static int dof_description[ndofs][4];

  static const UInt NumInterp = ndofs;

  static bool is_nodal() { return true;}
};

// A variety of shape functions defined on basic topological entities
class tet_shape_func {
  public:
  tet_shape_func() {}
  unsigned int num_dofs() { return ndofs;}
  const static unsigned int ndofs = 4;
  const static unsigned int pdim = 3;
  const static UInt iorder = 2;
  const static double one8th;

  // Return array of the shape function values at the given pcoords(npts,pdim)
  // in results(npts,ndofs)
  template<typename ScalarT>
  static void shape(unsigned int npts, const ScalarT pcoord[], ScalarT results[]);

  // Return gradients at the given points pcoord(npts,pdim) in results(npts,ndofs,pdim) 
  template<typename ScalarT>
  static void shape_grads(unsigned int npts, const ScalarT pcoord[], ScalarT results[]);

  // Is the parametric point in the cell??
  static bool is_in(const double pcoord[], double *dist = NULL);

  static const std::string name;
  const static int dof_description[ndofs][4];

  static const UInt NumInterp = ndofs;

  static bool is_nodal() { return true;}
};

class quad_zeroderiv_shape_func {
  public:
  quad_zeroderiv_shape_func() {}
  unsigned int num_dofs() { return ndofs;}
  const static unsigned int ndofs = 4;
  const static unsigned int pdim = 2;
  const static UInt iorder = 3;
  const static double one16th;

  // Return array of the shape function values at the given pcoords(npts,pdim)
  // in results(npts,ndofs)
  template<typename ScalarT>
  static void shape(unsigned int npts, const ScalarT pcoord[], ScalarT results[]);

  // Return gradients at the given points pcoord(npts,pdim) in results(npts,ndofs,pdim) 
  template<typename ScalarT>
  static void shape_grads(unsigned int npts, const ScalarT pcoord[], ScalarT results[]);

  // Is the parametric point in the cell??
  static bool is_in(const double pcoord[], double *dist = NULL);

  static const UInt NumInterp = ndofs;

  static const std::string name;

  static bool is_nodal() { return false;}
};

} // namespace
} // namespace

#endif
