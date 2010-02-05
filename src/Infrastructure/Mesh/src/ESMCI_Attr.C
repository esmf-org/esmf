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
#include <Mesh/include/ESMCI_Attr.h>
#include <Mesh/include/ESMCI_MeshObj.h>

namespace ESMCI {

std::ostream &operator<<(std::ostream &os, const Attr &attr) {
  os << MeshObjTypeString(attr.type);
  os << ", global setkey:" << attr.globalKey;
  os << ", context=" << attr.context << "= {";
 
  // verbose print the local contexts
  for (UInt i = 0; i < Attr::numReservedContexts; i++) {
    if (attr.GetContext().is_set(i))
      os << Attr::reservedContextNames[i] << ", ";
  }
  os << "} ";
  
  return os;
}

bool Attr::subset(const Attr &rhs) const {
  
  if ((type & rhs.type) == 0) return false;
  
  return context.subset(rhs.context);
}

bool Attr::any(const Attr &rhs) const {
  if (!(type & rhs.type)) return false;
  return context.any(rhs.context);
}

char *Attr::reservedContextNames[] = 
{
  "_active",
  "_locally_owned",
  "_shared",
  "_exposed_boundary",
  "_pendingCreate",
  "_pendingDelete",
  "_genesis",
  "_refined",
  "_constrained"
};





} // namespace
