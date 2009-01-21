// $Id: ESMC_IOField.C,v 1.2.2.2 2009/01/21 21:25:23 cdeluca Exp $
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
#include <ESMC_IOField.h>

namespace ESMCI {
namespace MESH {

// ********** Old style nodal fields ************
template<class DataAssoc,typename DTYPE>
IOField<DataAssoc,DTYPE>::IOField(const MeshDB &mesh, const std::string &name, UInt dim, bool ostat) :
  ptrmesh(&mesh),
  data_storage(),
  fdim(dim),
  fname(name),
  output(ostat)
{

  // Alloc storage
  UInt fsize = num_mesh_obj();
//std::cout << "num = " << fsize << std::endl;
  data_storage.resize(fdim*fsize, 0);
}

template<class DataAssoc,typename DTYPE>
IOField<DataAssoc,DTYPE>::~IOField() {
}

// Some specializations for getting the number of mesh obj by type
template<class DataAssoc,typename DTYPE>
UInt IOField<DataAssoc,DTYPE>::num_mesh_obj() {
  throw("Unspecified type for field");
}

template<>
UInt IOField<NodalField,double>::num_mesh_obj() {
  return ptrmesh->num_nodes();
}
template<>
UInt IOField<NodalField,int>::num_mesh_obj() {
  return ptrmesh->num_nodes();
}

template<>
UInt IOField<ElementField,double>::num_mesh_obj() {
  return ptrmesh->num_elems();
}

template<>
UInt IOField<ElementField,int>::num_mesh_obj() {
  return ptrmesh->num_elems();
}

// Access method
template<class DataAssoc,typename DTYPE>
DTYPE * IOField<DataAssoc,DTYPE>::data(const MeshObj &obj) const {
  if (obj.get_type() != DataAssoc::dtype) 
    throw("MeshObj type and field type not compatible!!");

  // remove the const behavior so this acts like a data array;
  return const_cast<DTYPE*>(&data_storage[obj.get_data_index()*fdim]);
}

template class IOField<NodalField,double>;
template class IOField<ElementField,double>;
template class IOField<NodalField,int>;
template class IOField<ElementField,int>;

} //namespace 
} //namespace 
