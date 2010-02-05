//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_IOField_h
#define ESMCI_IOField_h

#include <Mesh/include/ESMCI_MeshTypes.h>
#include <Mesh/include/ESMCI_MeshDB.h>

namespace ESMCI {

struct NodalField { static const MeshObj::MeshObjType dtype = MeshObj::NODE;};
struct ElementField { static const MeshObj::MeshObjType dtype = MeshObj::ELEMENT;};

/**
 * A simple field class that is used to bootstrap the real field class from
 * the IO file.  The issue is this:  When we read the mesh, we also have the opportunity
 * to read initialization fields, but we do not yet have all the information necessary
 * to create the dynamic fields.  Hence we must temporarily store the init fields in
 * some manner, so use this array based method.  This method is similar to the way
 * most grad-student level finite element codes represent fields: simply an array of
 * size num nodes, or num elements, etc...
 * We abandon this representation later, since it is not suitable for the dynamic mesh.
 * It would require resizing and copying the entire array every time a mesh object is
 * create/deleted/modified; an unacceptable performance hit.
*/
template<class DataAssoc, typename DTYPE=double>
class IOField {
public:
  typedef DTYPE real_type;
  // Create a field.  IOField must be on a given mesh.
  // dim == vector dim of field
  // IOField will delete whatever _mef is sent in!
  IOField(const MeshDB & mesh, const std::string &name, UInt dim=1, bool ostat=false);

  DataAssoc dataAssoc;

  ~IOField();

  // Retrieve the data point for a given mesh object
  DTYPE *data(const MeshObj &obj) const;
  UInt dim() const { return fdim;}

  void set_output_status(bool stat) {output = stat;}
  bool output_status() const { return output; }

  const std::string &name() const { return fname;}
  DTYPE *raw_data() const { return const_cast<DTYPE*>(&data_storage[0]);}

private:
  UInt num_mesh_obj();
  const MeshDB *ptrmesh;
  std::vector<DTYPE> data_storage;
        UInt fdim;
  const std::string fname;
  bool output;
};


} // namespace

#endif
