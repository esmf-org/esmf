// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2022, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================

//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------

#ifndef ESMCI_Mesh_Vector_Regrid_H
#define ESMCI_Mesh_Vector_Regrid_H

#include <vector>

#include "ESMCI_Macros.h"
#include "ESMCI_F90Interface.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_VM.h"
#include "VM/include/ESMC_VM.h"
#include "ESMCI_Array.h"

#include <limits>
#include <string>
#include <ostream>
#include <iterator>
#include <iostream>
#include <vector>

#include "Mesh/include/ESMCI_Mesh.h"
#include "ESMCI_CoordSys.h"
#include <Mesh/include/Legacy/ESMCI_SparseMsg.h>
#include "Mesh/include/Legacy/ESMCI_ParEnv.h"
#include <Mesh/include/Legacy/ESMCI_Exception.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
// static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

using namespace ESMCI;

// Get information about vector dimensions for vector regrid capability
void get_vec_dims_for_vectorRegrid(ESMCI::Array &array, int &num_vec_dims, int *vec_dims_undist_seqind);


// This interface transforms a standard weight matrix into one that works on vectors.
// (For more info. see the associated .C file)
void create_vector_sparse_mat_from_reg_sparse_mat(int num_entries, int *iientries, double *factors,
                                                          int num_vec_dims, int *src_vec_dims_undist_seqind, int *dst_vec_dims_undist_seqind,
                                                          Mesh *src_mesh, PointList *src_pl,
                                                          Mesh *dst_mesh, PointList *dst_pl,
                                                          int &num_entries_vec, int *&iientries_vec, double *&factors_vec);





#endif // ESMCI_Mesh_Vector_Regrid_H
