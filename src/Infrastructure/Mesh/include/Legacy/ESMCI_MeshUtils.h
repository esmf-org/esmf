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
#ifndef ESMCI_MeshUtils_h
#define ESMCI_MeshUtils_h

#include <Mesh/include/Legacy/ESMCI_MEField.h>
#include <Mesh/include/Legacy/ESMCI_MCoord.h>
#include <Mesh/include/ESMCI_Mesh.h>

namespace ESMCI {

MasterElementBase &GetME(const MEFieldBase &field, const MeshObj &obj);
MasterElementBase &GetME(const MEFieldBase &field, const Kernel &ker);
const MappingBase &GetMapping(const MeshObj &obj);
const intgRule *GetIntg(const MeshObj &obj);

// Return the coordinates of the nodes
// results(npe,sdim)
//void getNodeCoords(const Field<NodalField> &coords, const MeshObj &elem, double results[]);

// results(npe,fdim)
//template<typename FIELD, typename Real>
//void getNodeData(const FIELD &nfield, const MeshObj &elem, Real result[]);


// Gather data into the buffer res
template<typename METRAITS=METraits<>, typename FTYPE=MEField<>, typename RESTYPE=typename METRAITS::field_type>
struct GatherElemData {
GatherElemData(const MasterElement<METRAITS> &me, const FTYPE &f,
        const MeshObj &obj, RESTYPE res[]);
};

/**
 * Gather the field data for a side element.  At current, this
 * function will gather the data in accordance with the master element's view
 * of the side orientation, not the orientation of any actual existing object
 * that lives on that side.
 *
 * TODO: is this enough, or should the gather be wrt a side object orientation??
 *
 * @param me the side master element
 * @param obj the side object
 * @param elem the element that hosts the side
 * @param ordinal which side should we gather?
 * 
 */
template<typename METRAITS=METraits<>, typename FTYPE=MEField<>, typename RESTYPE=typename METRAITS::field_type>
struct GatherSideData {
GatherSideData(const MasterElement<METRAITS> &me, const FTYPE &f,
        const MeshObj &elem, UInt side_ordinal, RESTYPE res[]);
};

// Set the field values, given an array of me coefficients, mecoef.
template<typename METRAITS, typename FTYPE>
void ScatterElemData(const MasterElement<METRAITS> &me, const FTYPE &f,
        const MeshObj &obj, const double mcoef[]);


// Centroid of element.  result(sdim)
template<typename FIELD>
void elemCentroid(const FIELD &nfield, const MeshObj &elem, double result[]);

// Build coordinate field for mesh from coord attributes on nodes

// DEPREC
void getMeshCoords(const MeshDB &mesh, std::vector<double> &x, std::vector<double> &y, std::vector<double> &z);

void GetMeshCoords(const Mesh &mesh, std::vector<double> &x, std::vector<double> &y, std::vector<double> &z);

// Number of digits base 10, i.e. 1 = 1, 11 = 2, 103= 3
UInt numDecimal(UInt n);

// Get a local manifold coord system object from a shell
// Same, but around a node.  Uses an average of normals.
MCoord getMCoordNode(const MEField<> &nfield, const MeshObj &node);

// Same, but put at centroid of element
MCoord getMCoordElem(const MEField<> &nfield, const MeshObj &elem);

// Set field values at node equal to average of nodes around
void setToAverage(const MeshObj &node, const MEField<> &field);

// Change from Shape Function to mesh obj type
UInt dof2mtype(UInt dof);

// Gather the ids for given attribute
void getMeshGIDS(const Mesh &mesh, const Attr &a, std::vector<UInt> &gids);
void getMeshGIDS(const Mesh &mesh, const Attr &a, std::vector<int> &gids);


/*
 * Calculate a 1 dimensional decomposition of num_items among num_proc's.
 */
void decomp1d(long num_items, long num_proc, long rank, long &my_num, long &my_start);

} //namespace

#endif
