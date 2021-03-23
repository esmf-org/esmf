// $Id$
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_ShapeFunc_h
#define ESMCI_ShapeFunc_h

#include <Mesh/include/Legacy/ESMCI_MeshTypes.h>

#include <string>

/**
 * @defgroup shapefunc
 * 
 * A basic shape function interface, which can be used by the virtual
 * master element interface to construct a full ME.
 * 
 * The degrees of freedom are to be enumerated by a descriptive map:
 * dof_description:
 *       for each dof, a tuple = {type |node=0,edge=1,face=2,elem=3}|
 *                                ordinal |e.g, node 5, edge 3, etc...|
 *                                index |e.g, edge 4, second dof|
 * @ingroup mesystem
 */

namespace ESMCI {


typedef enum {DOF_NODE=0, DOF_EDGE=1, DOF_FACE=2, DOF_ELEM=3} DofLoc;

/**
 * The shapefunction interface.  Basic shape function and dual notions.
 * @ingroup shapefunc
 */
class ShapeFunc {
public:

  enum { ME_NODAL = 0, // nodal element
         ME_ELEMENTAL, // elemental dofs
         ME_SIGN_ORIENTED, // hierarchical sign matter, no order
         ME_ORIENTED,      // Lagrange; dofs ordered, no sign
         ME_DG             // No gather is necessary all data on element.
  };

  ShapeFunc();
  virtual ~ShapeFunc();

  virtual UInt NumFunctions() const = 0;
  virtual UInt ParametricDim() const = 0;
  virtual UInt IntgOrder() const = 0;
  /**
   *  Return array of the shape function values at the given pcoords(npts,pdim)
   * in results(npts,ndofs)
   */
  virtual void shape(UInt npts, const double pcoord[], double results[]) const = 0;
  virtual void shape(UInt npts, const fad_type pcoord[], fad_type results[]) const = 0;

  /**
   *  Return gradients at the given points pcoord(npts,pdim) in results(npts,ndofs,pdim)
   */ 
  virtual void shape_grads(UInt npts, const double pcoord[], double results[]) const = 0;
  virtual void shape_grads(UInt npts, const fad_type pcoord[], fad_type results[]) const = 0;

  virtual ShapeFunc *side_shape(UInt side_num) const=0;

  virtual const std::string &name() const = 0;

  /**
   *  Return true if values only occur at nodes and interpolation is from node values.
   */
  virtual bool is_nodal() const = 0;
  
  /**
   * face/edge orientation type.
   */
  virtual UInt orientation() const = 0;

  /**
   * Number of interpolation points.
   */
  virtual UInt NumInterp() const = 0;

  /**
   *  List of parametric points needed for interpolation, (NumInterp x pdim).
   */
  virtual const double *InterpPoints() const = 0;

  /**
   *  Form coefficients, given function values at ipoints;
   */
  virtual void Interpolate(const double fvals[], double mcoef[]) const = 0;
  virtual void Interpolate(const fad_type fvals[], fad_type mcoef[]) const = 0;

  /**
   *  description for each dof (Obj type, Obj ordinal, index).
   */
  virtual const int *DofDescriptionTable() const = 0;
};

/**
 * @defgroup tshapefunc
 * These are template shape functions that can be used by the generic programming
 * master element interface.
 * 
 * @ingroup shapefunc 
 */

/**
 * DG shape func.
 * @ingroup tshapefunc 
 */
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
 
  static const char* name;

  static bool is_nodal() { return false;}

  static const UInt NumInterp = 1;

  const static int dof_description[1][4];

  const static double ipoints[1];

};

/**
 * 1d bar shape func.
 * @ingroup tshapefunc
 */
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
 
  static const char* name;

  static const UInt NumInterp = ndofs;

  const static int dof_description[ndofs][4];

  static bool is_nodal() { return true;}

  const static double ipoints[ndofs*pdim];
};

/**
 * 1d quadratic bar
 * @ingroup tshapefunc 
 */
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
 
  static const char* name;

  static const UInt NumInterp = ndofs;

  const static int dof_description[ndofs][4];

  static bool is_nodal() { return true;}

  const static double ipoints[ndofs*pdim];

};

/**
  3 node triangle.
 * @ingroup tshapefunc
 */
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

  static const char* name;
  const static int dof_description[ndofs][4];

  static const UInt NumInterp = ndofs;

  static bool is_nodal() { return true;}

  const static double ipoints[ndofs*pdim];
};

/**
 * 4 node quadrilateral
 * @ingroup tshapefunc
 */
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

  static const char* name;
  const static int dof_description[ndofs][4];

  static const UInt NumInterp = ndofs;

  static bool is_nodal() { return true;}

  const static double ipoints[ndofs*pdim];
};

/**
 * 9 node quadrilateral
 * @ingroup tshapefunc 
 */
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

  static const char *name;
  const static int dof_description[ndofs][4];

  static const UInt NumInterp = ndofs;

  static bool is_nodal() { return true;}

  const static double ipoints[ndofs*pdim];
};

/**
 * 8 node hex
 * @ingroup tshapefunc 
 */
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

  static const char *name;
  const static int dof_description[ndofs][4];

  static const UInt NumInterp = ndofs;

  static bool is_nodal() { return true;}

  const static double ipoints[ndofs*pdim];
};

/**
 * 4 node tet.
 * @ingroup tshapefunc 
 */
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

  static const char *name;
  const static int dof_description[ndofs][4];

  static const UInt NumInterp = ndofs;

  static bool is_nodal() { return true;}

  const static double ipoints[ndofs*pdim];
};

/**
 * A quadratic based partition of unity.
 * @ingroup tshapefunc 
 */
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

  static const char *name;

  static bool is_nodal() { return false;}

  const static double ipoints[ndofs*pdim];
};

} // namespace

#endif
