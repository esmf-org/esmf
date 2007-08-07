// $Id: ESMC_MeshField.C,v 1.2 2007/08/07 20:08:09 dneckels Exp $
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
#include <ESMC_MeshField.h>

#include <iostream>
#include <algorithm>
#include <iterator>

#include <Sacado.hpp>


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
