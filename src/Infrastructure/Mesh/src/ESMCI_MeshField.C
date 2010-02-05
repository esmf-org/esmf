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
#include <Mesh/include/ESMCI_MeshField.h>

#include <iostream>
#include <algorithm>
#include <iterator>

#include <Mesh/include/sacado/Sacado.hpp>


namespace ESMCI {



// ************ SField ************

SField::SField(const _field &fd) :
field(fd),
fname(fd.name()),
fdim(fd.dim()),
dmap(),
fdata(),
nobj(0)
{
}

SField::~SField() {
}

void SField::Reset() {
  dmap.clear();
  objs.clear();
}

fad_type *SField::data(const MeshObj &obj) const {
  DMapType::const_iterator mi =
    //dmap.find(obj.get_id());
    dmap.find(&obj);

  if (mi == dmap.end())
    Throw() << "SField.data, idx=" << obj.get_id() << " not in SField!";

  // use const cast to emulate when data is only in an array.
  UInt lidx = mi->second; // *fdim rolled into lidx already.
  return const_cast<fad_type*>(&fdata[lidx]);
}

} // namespace
