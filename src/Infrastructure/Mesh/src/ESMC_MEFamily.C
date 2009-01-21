// $Id: ESMC_MEFamily.C,v 1.1.2.2 2009/01/21 21:25:23 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <ESMC_MEFamily.h>
#include <ESMC_Exception.h>

#include <ESMC_ShapeHierarchic.h>


namespace ESMCI {
namespace MESH {


MasterElement<METraits<> > *MEFamily::getME(const std::string &toponame) const {
  return getME(toponame, METraits<>());
};

MasterElement<METraits<double,fad_type> > *MEFamily::getME(const std::string &toponame, METraits<double,fad_type> ) const {
  return getME(toponame, METraits<>())->operator()(METraits<double,fad_type>());
}
MasterElement<METraits<fad_type,double> > *MEFamily::getME(const std::string &toponame, METraits<fad_type,double> ) const {
  return getME(toponame, METraits<>())->operator()(METraits<fad_type,double>());
}

const intgRule *MEFamily::GetIntg(const std::string &toponame) const {
  return Topo2Intg()(this->getME(toponame)->IntgOrder(), toponame);
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
    return MasterElementImpl<dg0_shape_func<2>, METraits<> >::instance();
  } else if (topo->parametric_dim == 3) {
    return MasterElementImpl<dg0_shape_func<3>, METraits<> >::instance();
  } else Throw() << "DG0 getME, unexpected pdim";
}

// Hier
std::map<UInt, MEFamilyHier*> MEFamilyHier::classInstances;

MEFamilyHier::MEFamilyHier(UInt _order) :
MEFamily(),
fname("Hier"),
order(_order),
meMap()
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

} // namespace 
} // namespace 
