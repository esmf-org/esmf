// $Id$
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_MasterElement_h
#define ESMCI_MasterElement_h

#include <Mesh/include/Regridding/ESMCI_ShapeFunc.h>
#include <Mesh/include/Regridding/ESMCI_Mapping.h>
#include <Mesh/include/Legacy/ESMCI_MeshTypes.h>
#include <Mesh/include/Legacy/ESMCI_Quadrature.h>

#include <Mesh/include/Legacy/ESMCI_MeshTypes.h>

#include <map>
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

namespace ESMCI {

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
   * Parametric interpolation points.
   */
  virtual const double *InterpPoints() const = 0;


  /**
   * These functions allow one to convert a master element between the
   * different trait types, but preserving the underlying element type.
   */
  
  virtual MasterElement<METraits<> > *operator()(METraits<>) const = 0;
  virtual MasterElement<METraits<double,fad_type> > *operator()(METraits<double,fad_type>) const = 0;
  virtual MasterElement<METraits<fad_type,double> > *operator()(METraits<fad_type,double>) const = 0;
  virtual MasterElement<METraits<fad_type,fad_type> > *operator()(METraits<fad_type,fad_type>) const = 0;

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
   * Given the values of a function at the interpolation points, manufacture
   * the array of ME coefficients.
   * @param fdim dimension of field
   * @param vals function values at ipoints, (fdim, ndof)
   * @param res manufactured coefficients 
   * 
   * We provide a fad version of these function so that we may produce
   * the interpolationg matrix.  This is useful, for instance, in the
   * case of the prolongation and constraint matrix.
   */
  virtual void Interpolate(UInt fdim, const field_type vals[], field_type res[]) const = 0;


  /**
   * Switch out various traits.
   */
  virtual MasterElement<METraits<> > *operator()(METraits<>) const = 0;
  virtual MasterElement<METraits<double,fad_type> > *operator()(METraits<double,fad_type>) const = 0;
  virtual MasterElement<METraits<fad_type,double> > *operator()(METraits<fad_type,double>) const = 0;
  virtual MasterElement<METraits<fad_type,fad_type> > *operator()(METraits<fad_type,fad_type>) const = 0;

};

/**
 * We implement the master element interface by using inheritance.
 * This system uses a virtual ShapeFunction class, which, once
 * implemented, can be extended to a full master element by the
 * routines herein.
 */

/**
 * Master elements implemented by using a virtual shape function
 * object.
 * @ingroup mesystem
 */
template<class METRAITS = METraits<> >
class MasterElementV : public MasterElement<METRAITS> {
private:
  MasterElementV(const ShapeFunc *shape);
  ~MasterElementV();
  
  /**
   * Pointer to the shape function object used by this ME.
   */
  const ShapeFunc *m_shape;
public:
  
  /**
   * Singleton constructor.  The objects are stashed by
   * the name of the shape function, so calling this again
   * with the same name will return a pre-cached object.
   */ 
  static MasterElementV *instance(const ShapeFunc *shape);
  
  typedef typename METRAITS::field_type field_type;
  typedef typename METRAITS::mdata_type mdata_type;
  
  /**
   * For documentation of the following functions, see the MasterElement
   * base class (this is called comment reuse).
   */

  UInt num_functions() const { return m_shape->NumFunctions(); }

  UInt IntgOrder() const { return m_shape->IntgOrder(); }

  void shape_function(UInt npts, const double pcoord[], double result[]) const {
    m_shape->shape(npts, pcoord, result);
  }

  void param_shape_grads(UInt npts, const double pcoord[], double result[]) const {
    m_shape->shape_grads(npts, pcoord, result);
  }
  
  void shape_grads(UInt npts, const Mapping<typename ME2MPTraits<METRAITS>::value > *mapping, const mdata_type mdata[],
          const double pcoord[], const double param_shape_grads[], mdata_type result[]) const;

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
             

  void function_values(
               UInt npts, UInt fdim, const double pcoord[],
                const field_type fdata[], field_type results[]) const;

  void function_values(
               UInt npts, UInt fdim, const double pcoord[],
               const field_type fdata[], field_type results[],
               double shape_vals[]) const;
 
  MasterElement<METRAITS> *side_element(UInt side) const;

  void JxW(const Mapping<typename ME2MPTraits<METRAITS>::value > *mapping, const mdata_type mdata[],
           const intgRule *irule, mdata_type result[]) const;

  const int *GetDofDescription(UInt dof) const;

  UInt NumInterpPoints() const { return m_shape->NumInterp(); }

  void InterpPoints(const MappingBase *mapping,
                const double mdata[], // mapping data
                double result[]) const;

  const double *InterpPoints() const;

  void Interpolate(UInt fdim, const field_type vals[], field_type res[]) const;

  MasterElement<METraits<> > *operator()(METraits<>) const;
  MasterElement<METraits<double,fad_type> > *operator()(METraits<double,fad_type>) const;
  MasterElement<METraits<fad_type,double> > *operator()(METraits<fad_type,double>) const;
  MasterElement<METraits<fad_type,fad_type> > *operator()(METraits<fad_type,fad_type>) const;

  bool is_nodal() const { return m_shape->is_nodal(); }
  
  UInt orientation() const { return m_shape->orientation(); }

  private:

};



/** 
 * Manufacture an ME with for a given
 * Topology and integration order.  Returns the standard lagrange
 * for the topology.
 * 
 * @ingroup mesystem
 */
template <typename METRAITS=METraits<> >
struct Topo2ME {
MasterElement<METRAITS> *operator()(const std::string &tname);
};

void compute_imprint(const MasterElementBase &me, std::vector<int> &res);

} //namespace

#endif
