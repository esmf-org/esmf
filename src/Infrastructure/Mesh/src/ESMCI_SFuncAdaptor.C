// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <Mesh/include/ESMCI_ShapeFunc.h>
#include <Mesh/include/ESMCI_SFuncAdaptor.h>
#include <Mesh/include/sacado/Sacado.hpp>

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

namespace ESMCI {

template<typename SFUNC>
SFuncAdaptor<SFUNC> *SFuncAdaptor<SFUNC>::classInstance = 0;

template<typename SFUNC>
SFuncAdaptor<SFUNC> *SFuncAdaptor<SFUNC>::instance() {
  if (classInstance == 0) {

    classInstance = new SFuncAdaptor();
  
  }

  return classInstance;

}

template <typename SFUNC>
struct to_side_adaptor {
  static ShapeFunc *instance() { Throw() << "No side for " << SFUNC::name; }
};

template<> struct to_side_adaptor<quad_shape_func>{
  static ShapeFunc *instance() { return SFuncAdaptor<bar_shape_func>::instance(); }
};
template<> struct to_side_adaptor<quad9_shape_func>{
  static ShapeFunc *instance() { return SFuncAdaptor<bar3_shape_func>::instance(); }
};
template<> struct to_side_adaptor<tri_shape_func>{
  static ShapeFunc *instance() { return SFuncAdaptor<bar_shape_func>::instance(); }
};
template<> struct to_side_adaptor<hex_shape_func>{
  static ShapeFunc *instance() { return SFuncAdaptor<quad_shape_func>::instance(); }
};
template<> struct to_side_adaptor<tet_shape_func>{
  static ShapeFunc *instance() { return SFuncAdaptor<tri_shape_func>::instance(); }
};

template <typename SFUNC>
ShapeFunc *SFuncAdaptor<SFUNC>::side_shape(UInt) const {

  return to_side_adaptor<SFUNC>::instance();

}

template class SFuncAdaptor<quad_shape_func>;
template class SFuncAdaptor<quad9_shape_func>;
template class SFuncAdaptor<tri_shape_func>;
template class SFuncAdaptor<hex_shape_func>;
template class SFuncAdaptor<tet_shape_func>;
template class SFuncAdaptor<bar_shape_func>;

template class SFuncAdaptor<dg0_shape_func<1> >;
template class SFuncAdaptor<dg0_shape_func<2> >;
template class SFuncAdaptor<dg0_shape_func<3> >;


} //namespace
