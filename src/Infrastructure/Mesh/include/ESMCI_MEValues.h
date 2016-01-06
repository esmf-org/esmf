// $Id$
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_MEValues_h
#define ESMCI_MEValues_h

#include <Mesh/include/ESMCI_MasterElement.h>
#include <Mesh/include/ESMCI_MeshObj.h>
#include <Mesh/include/ESMCI_MeshUtils.h>
#include <Mesh/include/ESMCI_MEField.h>
#include <Mesh/include/ESMCI_Mapping.h>
#include <Mesh/include/ESMCI_Kernel.h>

#include <Mesh/include/ESMCI_MeshTypes.h>


namespace ESMCI {

namespace MEV {
typedef enum {update_sf=0x01,    // shape functions
              update_sfg=0x02,   // shape function grads
              update_jxw=0x04,   // jacobian*iweights
              update_map=0x08   // mapping data
              } update_flag_enum;
};

/**
 * This object manages the interactions between mesh fields and
 * master elements.  It has a secondary purpose of providing
 * an abstraction for this interface such that all types of
 * field/me pairs may be used under a single interface, such
 * as for regridding.
 * 
 * Typically in a finite element simulation, one loops through the
 * elements and computes shape functions, functions, etc on the element.
 * This requires gathering the field coefficients into a contiguous
 * buffer, since the master elements are written to accept
 * buffers of coefficients.
 * 
 * This class manages this gather.  It tries to be efficient, in that
 * a gather may occur once on an element, and then multiple values
 * can be achived, such as gradients, function values, etc...
 * 
 * The entire class is templated on the fad variable type, so that
 * we may use this interface on a MEField<SField>, thus attaining
 * sensitivities of the operations provided herein to field
 * coefficients.
 * 
 * The object can work on a number of fields with the same MEFamily.  For 
 * this reason, field coefficients are not gather at Setup, but in the
 * call to GetFunction*.
 * 
 * 11-16-2007:This class still needs conceptual work, but the basis 
 * is usefull and sound....
 */
template<typename METRAITS=METraits<>, typename FIELD=MEField<> >
class MEValues {
public:
  
/**
 *  A mevalues operates for a single field, so that field must
 * be specified upon construction.  If gradients, interpolation
 * points, etc... are to be requested, we must pass a coordinate
 * field.
 */
MEValues(const MEFamily &_fmef, const MEField<> *cf = NULL);

virtual ~MEValues();

/**
 *  Assign the me, irule, etc.... By default, we use the 
 * me's intg rule, but you may override this by specifying one 
 * Call this for each kernel, not more, as it sets the meptr for both the
 * field and for the coordinate field.
 * The flag uflag specifies what type of data we will gather 
 * when we call ReInit per element.  It should be a compilation
 * of the (update_*) bits above.
 */
virtual void Setup(const Kernel &ker, UInt uflag, const intgRule *ir,
       const Mapping<typename ME2MPTraits<METRAITS>::value> *map = NULL);

/**
 * Provide a list of integration rules.
 */
virtual void Setup(const Kernel &ker, UInt uflag, std::vector<const intgRule*> ir_list,
       const Mapping<typename ME2MPTraits<METRAITS>::value> *map = NULL);

/**
 * For some loops, we cannot guarantee that we will be inside a
 * kernel loop, but we still wish to use the capability herein
 * to interpolate from an element.  Hence, this function provides
 * the same as above, but on a per-element basis.
 * In theory, this should be less efficient, so if one can nest
 * their computations in a kernel loop, they should do so and
 * use the Setup above.
 */
virtual void Setup(const MeshObj &obj, UInt uflag, const intgRule *ir,
       const Mapping<typename ME2MPTraits<METRAITS>::value> *map = NULL);

/**
 * Load the arrays with the requested values for the given element.
 * Note function values (below) requires update_sf to be given.
 * field(values,grads,curls) only require the element to be assigned(
 * they operate on the current element).
 */
virtual void ReInit(const MeshObj &elem, UInt cur_rule = 0);

typedef typename METRAITS::field_type field_type;

/**
 * Number of functions in the finite element associated with the
 * field.
 */
UInt GetNumFunctions() const { return bmg->meptr->num_functions(); }

/**
 * Number of quadrature points.
 */
UInt GetNumQPoints() const { return bmg->irule->npoints(); }

/**
 * Number of functions associated with the coordinate master element.
 */
UInt GetNumCoordFunctions() const { return bmg->cmeptr->num_functions(); }

/**
 * Use the coordinate field and me to interpolate and evaluate
 * the coordinates of the integration points.
 */
void GetCoordinateValues(double values[]);

/**
 * @par Interpolation routines. 
 * For a non-nodal finite element, we must ask the element where
 * it needs values to interpolate, and then request the function
 * manufacture values from these coefficients and place them
 * on the mesh (in the field).
 */

/**
 * How many function values are needed for interpolation?
 */ 
UInt GetNumInterpPoints() const { return bmg->meptr->NumInterpPoints(); }

/**
 * Returns an array of the interpolation points, in physical space
 * (uses the mapping to forward the parametric points to physcial
 * space).
 */
void GetInterpolationPoints(double vals[]);

/**
 * Given the function values (fvals), perform the interpolation
 * and set the function vals on the mesh.
 */
void Interpolate(const FIELD &field, const double fvals[]);

/** Get Values of a field at intg points.  
 * values(nqpoints,field.dim).
 * This function will use the shape values; needs update_sf set.
 * This can provide a savings, since the shape values are only 
 * computed at the start of a kernel, not every element.
 */
void GetFunctionValues(const FIELD &field, field_type values[]);

template <typename FTYPE>
void GetFunctionValues(const FIELD &field, const FTYPE mcoef[], FTYPE values[]) {
  ThrowRequire(update_flag & MEV::update_sf);

  bmg->meptr->operator()(METraits<FTYPE,double>())->function_values(
        bmg->irule->npoints(), field.dim(), bmg->irule->locations(),
        &mcoef[0], &values[0], &sf[bmg->cur_sf_offset]);

}

/**
 * Gather the coefficients for the master element.
 */
void GetElemData(const FIELD &field, field_type res[]);

/**
 * Same as above, but function gradients.  
 * grads(nqpoints, field.dim, sdim).  Will use shape_grads;
 * update_sfg, see above.
 */
void GetFunctionGrads(const FIELD &field, field_type grads[]);

template <typename FTYPE>
void GetFunctionGrads(const FIELD &field, const FTYPE mcoef[], FTYPE grads[]) {

  ThrowRequire(update_flag & MEV::update_sfg);

  bmg->meptr->operator()(METraits<FTYPE,double>())->function_grads(
                        bmg->irule->npoints(), field.dim(),
                        mapping, &mdata[0],
                        bmg->irule->locations(), &mcoef[0],
                        &grads[0], &sfg[bmg->cur_sfg_offset]);

}

/**
 * Curls.  Same notes as above.
 */
void GetFunctionCurls(const FIELD &field, field_type curls[]);

/**
 * Unit normals at the quadrature points.
 */
virtual void GetUnitNormals(double unit_normals[]);

/**
 * Normals at the quadrature points.
 */
virtual void GetNormals(double normals[]);

/**
 * Return the values of the shape functions at the quadrature points.
 * Must have called Setup with update_sf
 */
double GetShapeValue(UInt q, UInt i) {
  return sf[bmg->cur_sf_offset + q*bmg->meptr->num_functions()+i];
}

/**
 * Shape gradients at the quadrature points.  Must have
 * used update_sfg in Setup.
 */
double *GetShapeGrads(UInt q, UInt i) {
  return &sfg[bmg->cur_sfg_offset+(q*bmg->meptr->num_functions()+i)*sdim];
}

/**
 * Integration weights at the quadrature points.  Should have used
 * update_jxw in Setup.
 */
double GetJxW(UInt q) const {
  return jxw[bmg->cur_jxw_offset+q];
}

/**
 * Returns the number of quadrature points for the default
 * quadrature rule.
 */
UInt GetNQPoints() { return bmg->irule->npoints();}

protected:

// virtual so Face values can compute face jxw
virtual void compute_jxw(const MeshObj &,const double mdata[]);

const Kernel *cur_ker;
const MEFamily &fmef;
const MEFamily *cmef;
const MeshObjTopo *topo;
UInt update_flag;

/**
 * State for the object (precomputed values).  We store all of
 * the data that will be used during an assembly in a single contigous
 * buffer to improve cache performance.
 */
double *sf;
double *sfg;
double *param_sfg;
double *jxw;
double *mdata;
const MeshObj *cur_elem;

UInt sdim;
const MEField<> *cf;
std::vector<const intgRule*> irule_list;
const Mapping<typename ME2MPTraits<METRAITS>::value> *mapping;

/**
 * A struct to hold some data that we will use in the assembly.  This
 * struct allows us to place that data in a buffer along with all other
 * data, for cache performance
 */
struct buf_manage {
  // Master element pointer for field
  const MasterElement<METRAITS> *meptr;
  // Master element pointer for coordinate field
  const MasterElement<METraits<> > *cmeptr;
  const intgRule *irule;
  UInt num_irules;
  UInt cur_rule;
  UInt cur_sf_offset;
  UInt cur_sfg_offset;
  UInt cur_psfg_offset;
  UInt cur_jxw_offset;
  UInt total_nqpoints;
};

buf_manage *bmg;

/** Precomputed values all in one big data buffer for cache efficiency */
UChar *buf;
};

/**
 * Extend the MEValues interface to element sides.  Allows for the calculation
 * of gradients and values on element sides.
 * This class takes a lower dimensional quadrature rule for Setup, and maps it
 * to a side quadrature.
 */
template<typename METRAITS=METraits<>, typename FIELD=MEField<> >
class MESideValues : public MEValues<METRAITS,FIELD> {
public:

MESideValues(const MEFamily &_fmef, const MEField<> *_cf=NULL);

~MESideValues();

/**
 * Overide setup behavior for the face case.
 * @param map the element map.  Class will use the side map for integration weights. 
 * @param intgRule Pass the lower-dimensional quadrature rule.
 */
void Setup(const Kernel &ker, UInt uflag, const intgRule *ir,
       const Mapping<typename ME2MPTraits<METRAITS>::value> *map = NULL);

void Setup(const MeshObj &obj, UInt uflag, const intgRule *ir,
       const Mapping<typename ME2MPTraits<METRAITS>::value> *map = NULL);

/**
 * Special case for Side.
 */
void ReInit(const MeshObj &elem, UInt side_num);

/**
 * Don't allow this version, since we must know which side.
 */
void ReInit(const MeshObj &elem) {
  Throw() << "Must call ReInit(elem, side_num) for side values";
}

private:
void compute_jxw(const MeshObj &,const double mdata[]);
const MasterElement<METraits<> > *side_cmeptr;
const Mapping<typename ME2MPTraits<METRAITS>::value> *side_mapping;
UInt cur_side;
};

} // namespace

#endif
