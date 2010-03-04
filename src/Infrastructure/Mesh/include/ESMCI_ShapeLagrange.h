//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_SHAPELAGRANGE_H_
#define ESMCI_SHAPELAGRANGE_H_

#include <Mesh/include/ESMCI_ShapeFunc.h>
#include <Mesh/include/ESMCI_Exception.h>


#include <vector>

namespace ESMCI {
  
/**
 * @defgroup shapelag
 * 
 * High order lagrange elements.
 * 
 * @ingroup shapefunc
 */

/** 
 * Handles basic interpolation and other common tasks of the lagrange family.
 * @ingroup shapelag
 */
class ShapeLagrangeBase : public ShapeFunc {
public:
  virtual UInt NumFunctions() const = 0;
  
  UInt IntgOrder() const { return q+1; }
  
  const std::string &name() const { return ename; }
  
  bool is_nodal() const { return false; }

  ShapeFunc *side_shape(UInt side_num) const {
    Throw() << "No side for lagrange";
  }
  
  UInt orientation() const { return ShapeFunc::ME_ORIENTED; }
  
  UInt NumInterp() const { return NumFunctions(); }
  
  const double *InterpPoints() const { return &ipoints[0]; }

  void Interpolate(const double fvals[], double mcoef[]) const; 
  void Interpolate(const fad_type fvals[], fad_type mcoef[]) const; 
  
  const int *DofDescriptionTable() const { return &dofs[0];}

protected:

  ShapeLagrangeBase() {}
  virtual ~ShapeLagrangeBase() {}
 
  // ***** Data *****
  UInt q;
  std::vector<double> lobatto_points;
  std::string ename;
  std::vector<int> dofs;
  std::vector<double> ipoints;
};

/**
 * Continous quadrilateral lagrange. 
 * Degrees of freedom are laid out first on nodes, then edges, 
 * For instance, the third order:
 *
 *       o----*----*----o
 *       0    2    3    1
 *
 *  The number is oriented in the same way for higher order elements.
 *  The dofs are stacked on the edges in increasing order.  Hence,
 *  if an edge has reverse polarity, they must be unpacked in 
 *  reverse order.
 * 
 * @ingroup shapelag
 */
class ShapeLagrangeBar : public ShapeLagrangeBase {
public:
  ShapeLagrangeBar(UInt q);
  ~ShapeLagrangeBar();
  
  static ShapeLagrangeBar *instance(UInt q);

  UInt NumFunctions() const;
  
  UInt ParametricDim() const { return 1; }
  
  void shape(UInt npts, const double pcoord[], double results[]) const;
  void shape(UInt npts, const fad_type pcoord[], fad_type results[]) const;
  
  void shape_grads(UInt npts, const double pcoord[], double results[]) const;
  void shape_grads(UInt npts, const fad_type pcoord[], fad_type results[]) const;
  
private:
  
  template <typename Real>
  void shape_eval(UInt npts, const Real pcoord[], Real results[]) const;
  
  void build_itable(UInt nfunc, UInt q, std::vector<double> &ip);

};


/**
 * Continous quadrilateral lagrange. 
 * Degrees of freedom are laid out first on nodes, then edges, 
 * and finally on the element.  For instance, the third order:
 *
 *       3    9    8     2
 *       o----*----*----o
 *       |              |
 *       |   14    15   |
 *    10 *    *    *    * 7
 *       |              |
 *       |              |
 *    11 *    *    *    * 6
 *       |   12    13   |
 *       |              |
 *       o----*----*----o
 *       0    4    5    1
 *
 *  The number is oriented in the same way for higher order elements.
 *  The dofs are stacked on the edges in increasing order.  Hence,
 *  if an edge has reverse polarity, they must be unpacked in 
 *  reverse order.
 * 
 * @ingroup shapelag
 */
class ShapeLagrangeQuad : public ShapeLagrangeBase {
public:
  ShapeLagrangeQuad(UInt q);
  ~ShapeLagrangeQuad();
  
  static ShapeLagrangeQuad *instance(UInt q);

  UInt NumFunctions() const;
  
  UInt ParametricDim() const { return 2; }
  
  void shape(UInt npts, const double pcoord[], double results[]) const;
  void shape(UInt npts, const fad_type pcoord[], fad_type results[]) const;
  
  void shape_grads(UInt npts, const double pcoord[], double results[]) const;
  void shape_grads(UInt npts, const fad_type pcoord[], fad_type results[]) const;
  
private:
  
  template <typename Real>
  void shape_eval(UInt npts, const Real pcoord[], Real results[]) const;
  
  void build_itable(UInt nfunc, UInt q, std::vector<double> &ip);

};


/**
 * DG quadrilateral lagrange.
 * All degrees of freedom are on the element.
 * The numbering is simpler; it is just the tensor product numbering:
 *
 *      12   13   14    15
 *       o----*----*----o
 *       |              |
 *       |    9    10   |
 *     8 *    *    *    * 11
 *       |              |
 *       |              |
 *     4 *    *    *    * 7
 *       |    5    6    |
 *       |              |
 *       o----*----*----o
 *       0    1    2    3
 * @ingroup shapelag
 */
class ShapeLagrangeQuadDG : public ShapeLagrangeBase {
public:
  ShapeLagrangeQuadDG(UInt q);
  ~ShapeLagrangeQuadDG();
  
  static ShapeLagrangeQuadDG *instance(UInt q);
  
  UInt NumFunctions() const;
  UInt ParametricDim() const { return 2; }
  
  void shape(UInt npts, const double pcoord[], double results[]) const;
  void shape(UInt npts, const fad_type pcoord[], fad_type results[]) const;
  
  void shape_grads(UInt npts, const double pcoord[], double results[]) const;
  void shape_grads(UInt npts, const fad_type pcoord[], fad_type results[]) const;
  
private:
  
  template <typename Real>
  void shape_eval(UInt npts, const Real pcoord[], Real results[]) const;
  
  void build_itable(UInt nfunc, UInt q, std::vector<double> &ip);
  
};

} // namespace

#endif /*ESMC_SHAPELAGRANGE_H_*/
