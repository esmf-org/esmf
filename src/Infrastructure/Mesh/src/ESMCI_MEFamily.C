//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <Mesh/include/ESMCI_MEFamily.h>
#include <Mesh/include/ESMCI_Exception.h>

#include <Mesh/include/ESMCI_ShapeLagrange.h>
#include <Mesh/include/ESMCI_SFuncAdaptor.h>


namespace ESMCI {

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

// Lagrange


std::map<UInt, MEFLagrange*> &get_MEFLagrange_classInstances() {
  static std::map<UInt, MEFLagrange*> classInstances;

  return classInstances;
}


MEFLagrange::MEFLagrange(UInt _order) :
MEFamily(),
fname("Lagrange"),
order(_order)
{
}

const MEFLagrange &MEFLagrange::instance(UInt order) {
  std::map<UInt, MEFLagrange*> &classInstances =
           get_MEFLagrange_classInstances();
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
std::map<UInt, MEFLagrangeDG*> &get_MEFLagrangeDG_classInstances() {
  static std::map<UInt, MEFLagrangeDG*> classInstances;

  return classInstances;
}

MEFLagrangeDG::MEFLagrangeDG(UInt _order) :
MEFamily(),
fname("LagrangeDG"),
order(_order)
{
}

const MEFLagrangeDG &MEFLagrangeDG::instance(UInt order) {
  std::map<UInt, MEFLagrangeDG*> &classInstances =
             get_MEFLagrangeDG_classInstances();
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

