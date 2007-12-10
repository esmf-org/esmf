//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <Mesh/include/ESMC_MEFamily.h>
#include <Mesh/include/ESMC_Exception.h>

#include <Mesh/include/ESMC_ShapeHierarchic.h>
#include <Mesh/include/ESMC_ShapeLagrange.h>
#include <Mesh/include/ESMC_SFuncAdaptor.h>


namespace ESMC {

/*
MasterElement<METraits<> > *MEFamily::getME(const std::string &toponame) const {
  return getME(toponame, METraits<>());
};
*/

const intgRule *MEFamily::GetIntg(const std::string &toponame) const {
  return Topo2Intg()(this->getME(toponame, METraits<>())->IntgOrder(), toponame);
}


// STD
MEFamilyStd *MEFamilyStd::classInstance = NULL;

MEFamilyStd::MEFamilyStd() :
MEFamily(),
fname("Standard Lagrange")
{
}

const MEFamilyStd &MEFamilyStd::instance() {
  if (classInstance == NULL) {
    classInstance = new MEFamilyStd();
  }

  return *classInstance;
}

MasterElement<METraits<> > *MEFamilyStd::getME(const std::string &toponame, METraits<>) const {
  return Topo2ME<METraits<> >()(toponame);
}


// LOW
MEFamilyLow *MEFamilyLow::classInstance = NULL;

MEFamilyLow::MEFamilyLow() :
MEFamily(),
fname("Linear Lagrange")
{
}

const MEFamilyLow &MEFamilyLow::instance() {
  if (classInstance == NULL) {
    classInstance = new MEFamilyLow();
  }

  return *classInstance;
}

MasterElement<METraits<> > *MEFamilyLow::getME(const std::string &toponame, METraits<>) const {
  return Topo2ME<METraits<> >()(toponame+"_L");
}

// DG0
MEFamilyDG0 *MEFamilyDG0::classInstance = NULL;

MEFamilyDG0::MEFamilyDG0() :
MEFamily(),
fname("DG0")
{
}

const MEFamilyDG0 &MEFamilyDG0::instance() {
  if (classInstance == NULL) {
    classInstance = new MEFamilyDG0();
  }

  return *classInstance;
}

MasterElement<METraits<> > *MEFamilyDG0::getME(const std::string &toponame, METraits<>) const {
  // First get a suitable mapping
  const MeshObjTopo *topo = GetTopo(toponame);
  if (!topo) Throw() << "DG0 get Me, couldn't get topo";
  if (topo->parametric_dim == 2) {
    return MasterElementV<METraits<> >::instance(SFuncAdaptor<dg0_shape_func<2> >::instance());
  } else if (topo->parametric_dim == 3) {
    return MasterElementV<METraits<> >::instance(SFuncAdaptor<dg0_shape_func<3> >::instance());
  } else Throw() << "DG0 getME, unexpected pdim";
}

// Hier
std::map<UInt, MEFamilyHier*> MEFamilyHier::classInstances;

MEFamilyHier::MEFamilyHier(UInt _order) :
MEFamily(),
fname("Hier"),
order(_order)
{
}

const MEFamilyHier &MEFamilyHier::instance(UInt order) {
  std::map<UInt, MEFamilyHier*>::iterator fi = classInstances.find(order);
  MEFamilyHier *mef;
  if (fi == classInstances.end()) {
    mef = new MEFamilyHier(order);
    classInstances[order] = mef;
  } else mef = fi->second;

  return *mef;
}

MasterElement<METraits<> > *MEFamilyHier::getME(const std::string &name, METraits<>) const {
  if (name == "SHELL" || name == "SHELL4" || name == "QUAD" || name == "QUAD4" || name == "QUAD_3D") {
    return MasterElementV<METraits<> >::instance(QuadHier::instance(order));
  } else if (name == "SHELL3" || name == "TRI" || name == "TRI3" || name == "TRISHELL") {
    return MasterElementV<METraits<> >::instance(TriHier::instance(order));
  } else Throw() << "Hierar not implemented for topo:" << name;
}

// Lagrange

std::map<UInt, MEFLagrange*> MEFLagrange::classInstances;

MEFLagrange::MEFLagrange(UInt _order) :
MEFamily(),
fname("Lagrange"),
order(_order)
{
}

const MEFLagrange &MEFLagrange::instance(UInt order) {
  std::map<UInt, MEFLagrange*>::iterator fi = classInstances.find(order);
  MEFLagrange *mef;
  if (fi == classInstances.end()) {
    mef = new MEFLagrange(order);
    classInstances[order] = mef;
  } else mef = fi->second;

  return *mef;
}

MasterElement<METraits<> > *MEFLagrange::getME(const std::string &name, METraits<>) const {
  if (name == "SHELL" || name == "SHELL4" || name == "QUAD" || name == "QUAD4" || name == "QUAD_3D") {
    return MasterElementV<METraits<> >::instance(ShapeLagrangeQuad::instance(order));
  } else Throw() << "Hierar not implemented for topo:" << name;
}

// Lagrange DG
std::map<UInt, MEFLagrangeDG*> MEFLagrangeDG::classInstances;

MEFLagrangeDG::MEFLagrangeDG(UInt _order) :
MEFamily(),
fname("LagrangeDG"),
order(_order)
{
}

const MEFLagrangeDG &MEFLagrangeDG::instance(UInt order) {
  std::map<UInt, MEFLagrangeDG*>::iterator fi = classInstances.find(order);
  MEFLagrangeDG *mef;
  if (fi == classInstances.end()) {
    mef = new MEFLagrangeDG(order);
    classInstances[order] = mef;
  } else mef = fi->second;

  return *mef;
}

MasterElement<METraits<> > *MEFLagrangeDG::getME(const std::string &name, METraits<>) const {
  if (name == "SHELL" || name == "SHELL4" || name == "QUAD" || name == "QUAD4" || name == "QUAD_3D") {
    return MasterElementV<METraits<> >::instance(ShapeLagrangeQuadDG::instance(order));
  } else Throw() << "Hierar not implemented for topo:" << name;
}

} // namespace 

