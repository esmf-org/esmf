// $Id: ESMC_HAdapt.h,v 1.2.2.2 2009/01/21 21:25:22 cdeluca Exp $
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

#ifndef ESMC_HAdapt_h
#define ESMC_HAdapt_h

#include <vector>


namespace ESMCI {
namespace MESH {

class Mesh;
class _field;

template<class> class MEField;

class MeshObj;

/** 
 * This class manages HAdaptivity for the mesh class.  It handles
 * enforcement of the 2-1 rule, ease for marking elements to refine/
 * unrefine, etc...
 *
 * Refinement proceeds as follows:
 *   -) Mark the elements
 *   -) Call MarkerResolution
 *   -) Call Unrefine Mesh
 *   -) Call Refine Mesh
 *   -) Call RefinementResolution
*/
class HAdapt {
public:

enum {ELEM_REQUEST_UNREFINE = -2, ELEM_UNREFINE = -1, ELEM_REFINE = 1};

/**
 * Refine the mesh uniformly.  If keep_parents is false, make the refined
 * mesh the active mesh and delete parents.
 */
void RefineUniformly(bool keep_parents=true);

/**
 * Create an HAdapt, attach to a mesh
*/
HAdapt(Mesh &mesh);

/** 
 * Zero the element refinement marker field
*/
void ZeroMarker() const;


/** 
 * Mark an element to refine or unrefine
*/
void MarkElement(const MeshObj &elem, int val) const;

/**
 * Resolve the refine/unrefine markers.  Elements start with
 * -1 = unrefine, 0 = nothing, 1 = refine.  At the end of the
 * function, some new elements will be marked with 1 (to satisfy
 * the 2-1 rule), and only a handful of the elements will still
 * have -1 (those that CAN refine).
*/
void MarkerResolution();

/**
 *  Refine the mesh according to markers (which should be consistent).
*/
void UnrefineMesh();

/**
 * Unrefine the mesh elements (markers should be consistent).
*/
void RefineMesh();

/**
 * Update the constraints, attributes, etc as needed.
*/
void RefinementResolution() const;

Mesh &GetMesh() { return mesh; }

const Mesh &GetMesh() const { return mesh; }

private:
HAdapt(const HAdapt &);
HAdapt &operator=(const HAdapt &);

Mesh &mesh;

void resolve_refinement(std::vector<MeshObj*>&);
void resolve_unrefinement(std::vector<MeshObj*>&);

/** Field for resolving 2-1 (helps across processors) */
MEField<_field> *node_marker; 

/** Field used to mark elements for refine/unrefine */
MEField<_field> *elem_marker; 

/** Elements to refine are kept on a list since adaptivity
 *  changes the mesh linkage
*/
std::vector<MeshObj*> refine_list;
std::vector<MeshObj*> unrefine_list;

};

} // namespace
} // namespace

#endif
