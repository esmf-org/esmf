// $Id: ESMC_MeshField.C,v 1.3.2.2 2009/01/21 21:25:23 cdeluca Exp $
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
#include <ESMC_MeshField.h>

#include <iostream>
#include <algorithm>
#include <iterator>

#include <sacado/Sacado.hpp>


namespace ESMCI {
namespace MESH {



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
  DMapType().swap(dmap); // remove data
}

fad_type *SField::data(const MeshObj &obj) const {
  DMapType::const_iterator mi =
    dmap.find(obj.get_data_index());

  if (mi == dmap.end())
    Throw() << "SField.data, idx=" << obj.get_data_index() << " not in SField!";

  // use const cast to emulate when data is only in an array.
  UInt lidx = mi->second; // *fdim rolled into lidx already.
  return const_cast<fad_type*>(&fdata[lidx]);
}

} // namespace
} // namespace
