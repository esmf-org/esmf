// $Id: ESMC_IOField.h,v 1.1.2.2 2009/01/21 21:25:22 cdeluca Exp $
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

#ifndef ESMC_IOField_h
#define ESMC_IOField_h

#include <ESMC_MeshTypes.h>
#include <ESMC_MeshDB.h>

namespace ESMCI {
namespace MESH {

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
} // namespace

#endif
