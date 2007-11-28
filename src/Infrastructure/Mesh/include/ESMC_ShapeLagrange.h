// $Id: ESMC_ShapeLagrange.h,v 1.1 2007/11/28 16:23:22 dneckels Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMC_SHAPELAGRANGE_H_
#define ESMC_SHAPELAGRANGE_H_

#include <mesh/ESMC_ShapeFunc.h>
#include <mesh/ESMC_MasterElement.h>


#include <vector>

namespace ESMC {
  
/**
 * @defgroup shapelag
 * 
 * High order lagrange elements.
 * 
 * @ingroup shapefunc
 */

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
class ShapeLagrangeQuad : public ShapeFunc {
public:
  ShapeLagrangeQuad(UInt q);
  ~ShapeLagrangeQuad();
  
  static ShapeLagrangeQuad *instance(UInt q);
  
  UInt NumFunctions() const;
  
  UInt ParametricDim() const { return 2; }
  
  UInt IntgOrder() const { return q+1; }
  
  void shape(UInt npts, const double pcoord[], double results[]) const;
  void shape(UInt npts, const fad_type pcoord[], fad_type results[]) const;
  
  void shape_grads(UInt npts, const double pcoord[], double results[]) const;
  void shape_grads(UInt npts, const fad_type pcoord[], fad_type results[]) const;
  
  const std::string &name() const { return ename; }
  
  bool is_nodal() const { return false; }
  
  UInt orientation() const { return MasterElementBase::ME_ORIENTED; }
  
  UInt NumInterp() const { return NumFunctions(); }
  
  const double *InterpPoints() const { return &ipoints[0]; }
  
  void Interpolate(const double fvals[], double mcoef[]) const;
  
  const int *DofDescriptionTable() const { return &dofs[0];}
  
private:
  
  template <typename Real>
  void shape_eval(UInt npts, const Real pcoord[], Real results[]) const;
  
  void build_itable(UInt nfunc, UInt q, std::vector<double> &ip);
  
  UInt q;
  std::vector<double> lobatto_points;
  std::string ename;
  std::vector<int> dofs;
  std::vector<double> ipoints;
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
class ShapeLagrangeQuadDG : public ShapeFunc {
public:
  ShapeLagrangeQuadDG(UInt q);
  ~ShapeLagrangeQuadDG();
  
  static ShapeLagrangeQuadDG *instance(UInt q);
  
  UInt NumFunctions() const;
  
  UInt ParametricDim() const { return 2; }
  
  UInt IntgOrder() const { return q; }
  
  void shape(UInt npts, const double pcoord[], double results[]) const;
  void shape(UInt npts, const fad_type pcoord[], fad_type results[]) const;
  
  void shape_grads(UInt npts, const double pcoord[], double results[]) const;
  void shape_grads(UInt npts, const fad_type pcoord[], fad_type results[]) const;
  
  const std::string &name() const { return ename; }
  
  bool is_nodal() const { return false; }
  
  UInt orientation() const { return MasterElementBase::ME_DG; }
  
  UInt NumInterp() const { return NumFunctions(); }
  
  const double *InterpPoints() const { return &ipoints[0]; }
  
  void Interpolate(const double fvals[], double mcoef[]) const;
  
  const int *DofDescriptionTable() const { return &dofs[0];}
  
private:
  
  template <typename Real>
  void shape_eval(UInt npts, const Real pcoord[], Real results[]) const;
  
  void build_itable(UInt nfunc, UInt q, std::vector<double> &ip);
  
  UInt q;
  std::vector<double> lobatto_points;
  std::string ename;
  std::vector<int> dofs;
  std::vector<double> ipoints;
};

} // namespace

#endif /*ESMC_SHAPELAGRANGE_H_*/
