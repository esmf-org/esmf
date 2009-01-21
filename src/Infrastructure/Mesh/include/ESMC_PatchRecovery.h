// $Id: ESMC_PatchRecovery.h,v 1.1.2.2 2009/01/21 21:25:22 cdeluca Exp $
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

#ifndef ESMC_PatchRecovery_h
#define ESMC_PatchRecovery_h


#include <ESMC_MeshDB.h>
#include <ESMC_MeshObj.h>
#include <ESMC_MEField.h>
#include <ESMC_MasterElement.h>
#include <ESMC_Exception.h>
#include <ESMC_MCoord.h>

namespace ESMCI {
namespace MESH {

template <typename NFIELD=MEField<>, typename Real=double>
class PatchRecov {
public:
PatchRecov();
PatchRecov(const PatchRecov &rhs);
PatchRecov &operator=(const PatchRecov &rhs);  

// Create a patch using gauss points.  If you send _mc, YOU must delete it later.
void CreatePatch(UInt pdeg,
           const MeshDB &mesh,
           const MeshObj &node,  // The node to gather around
           UInt numfields,
           NFIELD **rfield,
           UInt threshold,       // How far from num dofs to invalidate.  If the
                                 // patch becomes invalid, we reduce it to a first order patch.
           const MEField<> &coord, // node coords (if pdim<sdim, then the object can
           const MCoord *_mc = NULL
           );

// field = linearized field index
Real EvalPatch(UInt nfield, const double coords[]) const;

// This  may be done, for instance, on a boundary
void MarkPatchBad() { patch_ok = false; } 
bool PatchOk() { return patch_ok;}
private:
 void eval_poly(UInt nsamples, UInt sample, UInt ldb, UInt cur_rhs,
          UInt dim, std::vector<double> &mat, double coord[],
                          std::vector<Real> &rhs, const Real fvals[], UInt fdim);
UInt get_ncoeff(UInt dim, UInt deg);
UInt pdeg;
UInt idim;
UInt ncoeff;
bool patch_ok;
std::vector<Real> coeff;
const MCoord *mc;
};

// An object that wraps a patchrecov for each node of an element and
// allows for evaluations within.
// Uses linear lagrange averaging of the nodal patches.
template <typename NFIELD=MEField<>, typename Real=double>
class ElemPatch {
public:
ElemPatch();
~ElemPatch();
// Create a patch using gauss points.
typedef enum {GAUSS_PATCH, NODAL_PATCH} patch_type;

void CreateElemPatch(UInt pdeg,
           UInt ptype,  // Use nodal or gaussian??
           const MeshObj &elem,  // the elem in question
           const MEField<> &cfield,
           UInt numfields,
           NFIELD **rfield,
           UInt threshold       // How far from num dofs to invalidate.  If the
           );

// Evaluate the function at the given parametric coordinates
// results(npts*allfieldsize) // value at the points
// i.e. vals for every field at point1, the point2, etc...
void Eval( UInt npts,
          const double pcoord[], // parametric coords
          Real results[]) const;

private:
  std::vector<PatchRecov<NFIELD,Real> > patches;
  std::vector<MCoord*> mcs;
  const MeshObj *pelem;
  const MeshDB *pmesh;
  const MEField<> *pcfield;
  UInt flen; // total length of fields (including dimensions)
};

} // namespace
} // namespace

#endif
