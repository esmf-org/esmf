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
#ifndef ESMCI_PatchRecovery_h
#define ESMCI_PatchRecovery_h


#include <Mesh/include/ESMCI_MeshDB.h>
#include <Mesh/include/ESMCI_MeshObj.h>
#include <Mesh/include/ESMCI_MEField.h>
#include <Mesh/include/ESMCI_MasterElement.h>
#include <Mesh/include/ESMCI_Exception.h>
#include <Mesh/include/ESMCI_MCoord.h>

#include <map>

namespace ESMCI {

template <typename NFIELD=MEField<>, typename Real=double>
class PatchRecov {
public:
PatchRecov();
PatchRecov(const PatchRecov &rhs);
PatchRecov &operator=(const PatchRecov &rhs);  

PatchRecov operator*(double val); 


void operator+=(const PatchRecov &rhs);

/*
 * Create a patch that just gives back the value of the field
 * at the node.
 */
void CreateConstantPatch(const MeshObj &node,
                         UInt numfields,
                         NFIELD **rfield);

// Create a patch using gauss points.  If you send _mc, YOU must delete it later.
void CreatePatch(UInt pdeg,
           const MeshDB &mesh,
           const MeshObj &node,  // The node to gather around
           const MeshObj *elem_hint, // element forming patch around
           UInt numfields,
           NFIELD **rfield,
           UInt threshold,       // How far from num dofs to invalidate.  If the
                                 // patch becomes invalid, we reduce it to a first order patch.
           const MEField<> &coord, // node coords (if pdim<sdim, then the object can
		 const MCoord *_mc = NULL,
           MEField<> *src_mask_ptr=NULL
           );

// field = linearized field index
Real EvalPatch(UInt nfield, const double coords[]) const;

void EvalPatchGrad(UInt nfield, const double coords[], Real result[]) const;

Real EvalPatchAndGrad(UInt nfield, const double coords[], Real result[]) const;

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
MCoord mc;
bool use_mc;
};

// An object that wraps a patchrecov for each node of an element and
// allows for evaluations within.
// Uses linear lagrange averaging of the nodal patches.
template <typename NFIELD=MEField<>, typename Real=double>
class ElemPatch {
public:
ElemPatch();
typedef std::map<int, PatchRecov<NFIELD,Real>*> NodePatchMap;

ElemPatch(NodePatchMap &nmap);
~ElemPatch();
// Create a patch using gauss points.
typedef enum {GAUSS_PATCH, NODAL_PATCH} patch_type;

void CreateElemPatch(UInt pdeg,
           UInt ptype,  // Use nodal or gaussian??
           const MeshObj &elem,  // the elem in question
           const MEField<> &cfield,
	   MEField<> *src_mask_ptr,
           UInt numfields,
           NFIELD **rfield,
           UInt threshold,       // How far from num dofs to invalidate.  If the
           bool boundary_ok = false // if true, forms the patch with boundary nodes that have >= 2 elems
           );

/**
 * Evaluate the function at the given parametric coordinates
 * results(npts*allfieldsize) // value at the points
 * i.e. vals for every field at point1, the point2, etc...
 */
void Eval( UInt npts,
          const double pcoord[], // parametric coords
          Real results[]) const;

/**
 * Evaluate the patch gradients.  
 * results(npts, sdim)
 */
void EvalGrad( UInt npts,
          const double pcoord[], // parametric coords
          Real results[]) const;


/**
 * Utility to delete users nodepatchmap.
 */
static void FreePatchMap(NodePatchMap &nmap);

private:
  ElemPatch &operator=(const ElemPatch &rhs);
  ElemPatch(const ElemPatch &rhs);
  std::vector<PatchRecov<NFIELD,Real>*> patches;
  std::vector<MCoord> mcs;
  const MeshObj *pelem;
  const MeshDB *pmesh;
  const MEField<> *pcfield;
  UInt flen; // total length of fields (including dimensions)
  NodePatchMap *nmap;
  UInt pdeg;
};

} // namespace

#endif
