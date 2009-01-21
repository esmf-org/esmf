// $Id: ESMC_MeshUtils.h,v 1.1.2.2 2009/01/21 21:25:22 cdeluca Exp $
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

#ifndef ESMC_MeshUtils_h
#define ESMC_MeshUtils_h

#include <ESMC_MEField.h>
#include <ESMC_MCoord.h>
#include <ESMC_Mesh.h>

namespace ESMCI {
namespace MESH {

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
template<typename METRAITS, typename FTYPE>
void GatherElemData(const MasterElement<METRAITS> &me, const FTYPE &f,
        const MeshObj &obj, typename METRAITS::field_type res[]);

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
MCoord *getMCoordNode(const MEField<> &nfield, const MeshObj &node);

// Set field values at node equal to average of nodes around
void setToAverage(const MeshObj &node, const MEField<> &field);

// Change from Shape Function to mesh obj type
UInt dof2mtype(UInt dof);

} //namespace
} //namespace

#endif
