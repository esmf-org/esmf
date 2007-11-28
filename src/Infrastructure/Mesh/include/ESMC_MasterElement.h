// $Id: ESMC_MasterElement.h,v 1.3 2007/11/28 16:23:21 dneckels Exp $
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
#ifndef ESMC_MasterElement_h
#define ESMC_MasterElement_h

#include <mesh/ESMC_ShapeFunc.h>
#include <mesh/ESMC_Mapping.h>
#include <mesh/ESMC_MeshTypes.h>
#include <mesh/ESMC_Quadrature.h>

#include <mesh/ESMC_MeshTypes.h>

#include <string>

/**
 * @defgroup mesystem
 * 
 * The master element subsystem provides an abstract interface
 * for accessing shape functions/gradients and for evaluating
 * finite element functions.  The subsystem is templated on the
 * numerical type, both for mapping data and for field data.  This
 * allows the system to be used for the Sacado automatic differentiation
 * package, so that sensitivities wrt. field coefficients (e.g. Newton
 * iteration) or coordinate data (think Newton for moving mesh problem)
 * are propogated through ME evaluations.
 *
 * The basic rule for this subsystem is that it includes anything that
 * must combine (fields, mapping, intg, shape func)
 * 
 */

namespace ESMC {

// A type to determine the various types in a master element
template<typename FIELD_TYPE=double, typename MDATA_TYPE=double>
struct METraits {
typedef FIELD_TYPE field_type;
typedef MDATA_TYPE mdata_type;
};


/**
 * Convert master element to mapping traits.
 * @ingroup mesystem
 */
template<typename METRAITS>
struct ME2MPTraits {
typedef MPTraits<typename METRAITS::mdata_type,double> value;
};

template <typename METRAITS>
class MasterElement;

/** 
 * Manufacture an ME with for a given
 * Topology and integration order.  Returns the standard lagrange
 * for the topology.
 * 
 * @ingroup mesystem
 */
template <typename METRAITS=METraits<>,UInt q=2>
struct Topo2ME {
MasterElement<METRAITS> *operator()(const std::string &tname);
};

/**
 * 
 * This base class abstracts all of the pieces that do not depend
 * on the templated traits.
 * 
 * @ingroup mesystem 
 */
class MasterElementBase {
protected:
  MasterElementBase(const std::string &_name);
  virtual ~MasterElementBase() {}
public:

  /** Return the number of finite element shape functions */
  virtual UInt num_functions() const = 0;

  /** A suggested integration order for using this ME */
  virtual UInt IntgOrder() const = 0; // suggested order of integration

  /**
   *  Return a tuple (DOF_OBJTYPE, ordinal, index, polarity)
   * nval corresponds to the context and gather field "fieldname_dof#nvalset"
   */
  virtual const int *GetDofDescription(UInt dof) const = 0;

  /**
   *  We partition fields by how much data to store per topological object.  This partitions
   * the topo objs into sets by how many indices on the object.  For a given dof, this
   * functions returns how many total dofs live on the topological object.
   */
  int GetDofValSet(UInt dof) const;

  /**
   *  Declare the interpolation points.  To interpolate to coefficient values for this
   * ME, one must provide values of the function to interpolate at the given points.
   * Interpolation points are provided in physical coordinates.
   * Note, if the me is_nodal, interpolation is accomplished merely by providing
   * values at the nodes.
   */
  virtual UInt NumInterpPoints() const = 0;
  
  /**
   * Return the array of locations where values are needed to form 
   * coefficients for this ME.  Points are in physical space.
   * @param result (ninterp*sdim)
   */ 
  virtual void InterpPoints(const MappingBase *mapping,
                const double mdata[], // mapping data
                double result[]) const = 0;

  /**
   * Given the values of a function at the interpolation points, manufacture
   * the array of ME coefficients.
   * @param fdim dimension of field
   * @param vals function values at ipoints, (fdim, ndof)
   * @param res manufactured coefficients 
   */
  virtual void Interpolate(UInt fdim, const double vals[], double res[]) const = 0;


  /**
   * These functions allow one to convert a master element between the
   * different trait types, but preserving the underlying element type.
   */
  
  virtual MasterElement<METraits<> > *operator()(METraits<>) const = 0;
  virtual MasterElement<METraits<double,fad_type> > *operator()(METraits<double,fad_type>) const = 0;
  virtual MasterElement<METraits<fad_type,double> > *operator()(METraits<fad_type,double>) const = 0;
  virtual MasterElement<METraits<fad_type,fad_type> > *operator()(METraits<fad_type,fad_type>) const = 0;

  enum { ME_NODAL = 0, // nodal element
         ME_ELEMENTAL, // elemental dofs
         ME_SIGN_ORIENTED, // hierarchical sign matter, no order
         ME_ORIENTED,      // Lagrange; dofs ordered, no sign
         ME_DG             // No gather is necessary all data on element.
  };
  
  /**
   * Return information about how the ME stores its coefficients, and how
   * edge/face orientation is to interact with the shape functions (i.e.
   * should certain functions by negated, or data permuted??)
   */
  virtual UInt orientation() const = 0;
  
  /**
   * True if dofs are all on nodes, else false.  Allows for
   * very efficient processing if true.
   */
  virtual bool is_nodal() const = 0;

  const std::string name;
  protected:
  std::vector<int> dofvalset; // dof layout

};

/**
 * The actual master element class.  Provides functions to get shape values,
 * shape grads, function values, etc... at some set of parametric coordinates.
 * Class is templated, as mentioned above, to provide various sensitivites
 * under auto-differentiation types.
 * 
 * @ingroup mesystem
 */
template<class METRAITS = METraits<> >
class MasterElement : public MasterElementBase {
public:
  MasterElement(const std::string &_name) : MasterElementBase(_name)
  {}

  virtual ~MasterElement() {}

  typedef METRAITS traits_type;
  typedef typename METRAITS::field_type field_type;
  typedef typename METRAITS::mdata_type mdata_type;

  /**
   * Query for all the shape function values (npts) at the parametric points
   * @param npts number of quadrature points
   * @param pcoord parametric coordinates (npts, pdim)
   * @param result shape function values (npts, nfunc)
   */
  virtual void shape_function(UInt npts, const double pcoord[],
          double result[]) const = 0;

  /**
   * Return the parametric shape gradients.  These may be evaluated once for
   * a set of common elements/quadrature rule.
   */
  virtual void param_shape_grads(UInt npts, const double pcoord[], double result[]) const = 0;
  
  /** 
   * Get physical gradients.
   * @param npts number of quadrature points
   * @param mapping for mapping grads  
   * @param mdata mapping data
   * @param pcoord parametric coords of quad points
   * @param param_shape_grad(npts, nfunc, pdim)
   * @param result shape grads(npts, nfunc, pdim)
   */
  virtual void shape_grads(UInt npts, const Mapping<typename ME2MPTraits<METRAITS>::value > *mapping, 
             const mdata_type mdata[], const double pcoord[],
             const double param_shape_grad[], mdata_type result[]) const = 0;

  /**
   *  Interpolate function to (parametric) points.
   */
  virtual void function_values(UInt npts, UInt fdim, const double pcoord[],
             const field_type fdata[], field_type results[]) const = 0;

  /** Same as above, but use precomputed shape vals */
  virtual void function_values(UInt npts, UInt fdim, const double pcoord[],
             const field_type fdata[], field_type results[],
             double shape_vals[]) const = 0;


  /** Same as above, but use precomputed shape vals */
  virtual void function_grads(UInt npts, UInt fdim, const Mapping<typename ME2MPTraits<METRAITS>::value > *mapping,
           const mdata_type mdata[], const double pcoord[],
           const field_type fdata[],
           typename richest_type<mdata_type,field_type>::value result[],
           mdata_type shape_grads[]) const = 0;

  /**
   * Compute curl of a vector field.
   * @param mdata  mapping data
   * @param pcoord (coordinates to evaluate at)
   * @param fdata field data (ndofs, sdim)
   * @param result (npts, sdim)
   */

  /** Same as above, but use precomputed shape vals */
  virtual void function_curl(UInt npts, const Mapping<typename ME2MPTraits<METRAITS>::value > *mapping, const mdata_type mdata[],
             const double pcoord[], const field_type fdata[],
             typename richest_type<mdata_type,field_type>::value result[],
             mdata_type shape_grads[]) const = 0;

  /**
   * Provide the integration weights*mapping jacobian.
   */
  virtual void JxW(const Mapping<typename ME2MPTraits<METRAITS>::value > *mapping, const mdata_type mdata[],
            const intgRule *intg,
            mdata_type result[]) const = 0;

  /**
   *  Return a tuple (DOF_OBJTYPE, ordinal, index, nval_set)
   * nval corresponds to the context and gather field "fieldname_dof#nvalset"
   */
  virtual const int *GetDofDescription(UInt dof) const = 0;

  // Return the master element for a side (may be different by side; for instance prisms)
  virtual MasterElement<METRAITS> *side_element(UInt side) const {
    Throw() << "side element not implemented";
  }


  /**
   * Switch out various traits.
   */
  virtual MasterElement<METraits<> > *operator()(METraits<>) const = 0;
  virtual MasterElement<METraits<double,fad_type> > *operator()(METraits<double,fad_type>) const = 0;
  virtual MasterElement<METraits<fad_type,double> > *operator()(METraits<fad_type,double>) const = 0;
  virtual MasterElement<METraits<fad_type,fad_type> > *operator()(METraits<fad_type,fad_type>) const = 0;

};


/**
 * An implementation based on generic programming.  Piece together the Mapping,
 * shapefunctions and quadrature rule based on template pieces.
 * @ingroup mesystem
 */
template<class SHAPE_FUNC, class METRAITS = METraits<> >
class MasterElementImpl : public MasterElement<METRAITS> {
private:
  MasterElementImpl();
  ~MasterElementImpl();
public:
  typedef typename METRAITS::field_type field_type;
  typedef typename METRAITS::mdata_type mdata_type;

  UInt num_functions() const { return ndofs; }

  virtual UInt IntgOrder() const { return SHAPE_FUNC::iorder; }

  static MasterElementImpl *classInstance;
  static MasterElementImpl *instance();

  static const UInt pdim = SHAPE_FUNC::pdim;
  static const UInt ndofs = SHAPE_FUNC::ndofs;

  typedef SHAPE_FUNC shape_func_type;

  // Query for all the shape function values (npts) at the parametric points
  // pcoord(npts, pdim) and return in result(npts, nfunc)
  void shape_function(UInt npts, const double pcoord[], double result[]) const {
    SHAPE_FUNC::shape(npts, pcoord, result);
  }
  
  void param_shape_grads(UInt npts, const double pcoord[], double result[]) const {
    SHAPE_FUNC::shape_grads(npts, pcoord, result);
  }

  // Get physical gradients.  mdata = mapping data
  // result(npts, nfunc, sdim)
  void shape_grads(UInt npts, const Mapping<typename ME2MPTraits<METRAITS>::value > *mapping, const mdata_type mdata[],
          const double pcoord[], const double param_sgrad[], mdata_type result[]) const;


  void function_grads(UInt npts, UInt fdim, const Mapping<typename ME2MPTraits<METRAITS>::value > *mapping,
              const mdata_type mdata[], const double pcoord[],
              const field_type fdata[], 
              typename richest_type<mdata_type,field_type>::value result[],
              mdata_type shape_grads[]) const;


  void function_curl(UInt npts, const Mapping<typename ME2MPTraits<METRAITS>::value > *mapping, 
             const mdata_type mdata[], const double pcoord[],
             const field_type fdata[], 
             typename richest_type<mdata_type,field_type>::value result[],
             mdata_type shape_grads[]) const;

  // Interpolate a field
  // pcoord(npts,pdim)
  // fdata(ndofs,fdim)
  // results(npts,fdim)
  void function_values(
               UInt npts, UInt fdim, const double pcoord[],
                const field_type fdata[], field_type results[]) const;

  void function_values(
               UInt npts, UInt fdim, const double pcoord[],
                const field_type fdata[], field_type results[],
                double shape_vals[]) const;

  // Get tangent at intg points (line elements)
  //void Itangent(const double mdata[], double result[]);

  // Return the master element for a side (may be different by side; for instance prisms)
  MasterElement<METRAITS> *side_element(UInt side) const;

  void JxW(const Mapping<typename ME2MPTraits<METRAITS>::value > *mapping, const mdata_type mdata[],
           const intgRule *irule, mdata_type result[]) const;

  const int *GetDofDescription(UInt dof) const;

  UInt NumInterpPoints() const { return SHAPE_FUNC::NumInterp; }
  // result(ninterp*sdim)
  void InterpPoints(const MappingBase *mapping,
                const double mdata[], // mapping data
                double result[]) const;
  void Interpolate(UInt fdim, const double vals[], double res[]) const;

  MasterElement<METraits<> > *operator()(METraits<>) const;
  MasterElement<METraits<double,fad_type> > *operator()(METraits<double,fad_type>) const;
  MasterElement<METraits<fad_type,double> > *operator()(METraits<fad_type,double>) const;
  MasterElement<METraits<fad_type,fad_type> > *operator()(METraits<fad_type,fad_type>) const;

  bool is_nodal() const { return SHAPE_FUNC::is_nodal(); }

  // For now, assume these me's are nodal or elemental
  UInt orientation() const { return SHAPE_FUNC::is_nodal() ? MasterElementBase::ME_NODAL : MasterElementBase::ME_ELEMENTAL; }
  
  private:

};

void compute_imprint(const MasterElementBase &me, std::vector<int> &res);

} //namespace

#endif
