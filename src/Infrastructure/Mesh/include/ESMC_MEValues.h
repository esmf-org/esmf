// $Id: ESMC_MEValues.h,v 1.1.2.2 2009/01/21 21:25:22 cdeluca Exp $
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

#ifndef ESMC_MEValues_h
#define ESMC_MEValues_h

#include <ESMC_MasterElement.h>
#include <ESMC_MeshObj.h>
#include <ESMC_MEField.h>
#include <ESMC_Mapping.h>
#include <ESMC_Kernel.h>

#include <ESMC_MeshTypes.h>


namespace ESMCI {
namespace MESH {

namespace MEV {
typedef enum {update_sf=0x01,    // shape functions
              update_sfg=0x02,   // shape function grads
              update_jxw=0x04   // jacobian*iweights
              } update_flag_enum;
};

template<typename METRAITS=METraits<>, typename CMETRAITS=METraits<>, typename FIELD=MEField<>, typename CFIELD=MEField<> >
class MEValues {
public:
// Field master element family and coordinate me family
MEValues(const MEFamily &_fmef, const CFIELD &cf);

// Assign the me, irule, etc....
//  By default, we use the me's intg rule, but you may
// override this by specifying one
// Call this for each kernel, not more, as it sets the meptr for both the
// field and for the coordinate field
void Setup(const Kernel &ker, UInt uflag, const intgRule *ir,
       const Mapping<typename ME2MPTraits<METRAITS>::value> *map = NULL);

// Same as above, but for tight loops on objects.
void Setup(const MeshObj &obj, UInt uflag, const intgRule *ir,
       const Mapping<typename ME2MPTraits<METRAITS>::value> *map = NULL);

// Load the arrays with the requested values for the given element.
// Note function values (below) requires update_sf to be given.
// field(values,grads,curls) only require the element to be assigned(
// they operate on the current element).
void ReInit(const MeshObj &elem);

typedef typename METRAITS::mdata_type mdata_type;
typedef typename METRAITS::field_type field_type;
typedef typename richest_type<typename METRAITS::mdata_type,typename METRAITS::field_type>::value rich_type;

// Only call after calling setup
UInt GetNumFunctions() const { return meptr->num_functions(); }
UInt GetNumCoordFunctions() const { return cmeptr->num_functions(); }

// Interpolate coordinate values to integration points
void GetCoordinateValues(mdata_type values[]);

UInt GetNumInterpPoints() const { return meptr->NumInterpPoints(); }

// Return the interpolation points for the element
void GetInterpolationPoints(double vals[]);

// Given the function values (fvals), perform the interpolation
// and set the function vals on the mesh.
void Interpolate(const FIELD &field, const double fvals[]);

// Get Values of a field at intg points.  Should have reinit'ed with
// shape_func for values, and shape_grad for field_grads.  Your responsibility.
// values(nqpoints,field.dim)
void GetFunctionValues(const FIELD &field, field_type values[]);
// grads(nqpoints,sdim)
void GetFunctionGrads(const FIELD &field, field_type grads[]);
// curls(nqpoints,sdim)
void GetFunctionCurls(const FIELD &field, field_type curls[]);
// Unit normals
void GetUnitNormals(mdata_type unit_normals[]);

void GetNormals(mdata_type normals[]);

double GetShapeValue(UInt q, UInt i) {
  return sf[q*meptr->num_functions()+i];
}
mdata_type *GetShapeGrads(UInt q, UInt i) {
  return &sfg[(q*meptr->num_functions()+i)*sdim];
}

mdata_type GetJxW(UInt q) const {
  return jxw[q];
}

UInt GetNQPoints() { return nqpoints;}

private:
const Kernel *cur_ker;
const MEFamily &fmef;
const MEFamily &cmef;
const MeshObjTopo *topo;
// Master element pointer for field
const MasterElement<METRAITS> *meptr;
// Master element pointer for coordinate field
const MasterElement<CMETRAITS> *cmeptr;
UInt update_flag;
std::vector<double> sf;
std::vector<mdata_type> sfg;
std::vector<mdata_type> jxw;
UInt nqpoints;
const MeshObj *cur_elem;
UInt sdim;
const CFIELD *cf;
const intgRule *irule;
const Mapping<typename ME2MPTraits<METRAITS>::value> *mapping;
};

} // namespace
} // namespace

#endif
