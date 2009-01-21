// $Id: ESMC_Interp.h,v 1.2.2.2 2009/01/21 21:25:22 cdeluca Exp $
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
#ifndef ESMC_Interp_h
#define ESMC_Interp_h

#include <ESMC_MEField.h>
#include <ESMC_Search.h>
#include <ESMC_GeomRendezvous.h>

#include <vector>

namespace ESMCI {
namespace MESH {

/*
 * Provides interpolation for serial and parallel meshes.  For 
 * serial the interpolatin is a simple search/application.  For parallel
 * we use the Rendezvous algorithm of Stewart et al.  The interface
 * is the same for both cases.
 */

class Mesh;
class Geom;
   
/*
 * Hold the necessary state to perform an interpolation (either parallel
 * or serial).  The interpolated fields must have a certain uniform nature.
 * In particular, they must all be defined on the same part of the meshes,
 * and the destination fields must all be either nodal or interpolatory,
 * but not mixed.
 * Note: if INTERP_PATCH is used, it is up to the caller to make sure they
 * have ghosting enabled on the mesh.  It will also be their responsibility
 * to make sure the data is halo'ed to the ghosts before the Interp() operation.
 */ 
class Interp {
public:

enum {INTERP_STD = 0, INTERP_PATCH};

struct FieldPair {
  FieldPair(MEField<> *_sF, MEField<> *_dF, UChar _idata=INTERP_STD)
  : first(_sF), second(_dF), idata(_idata) {}
  MEField<> *first, *second;
  UChar idata; // interpolation specification.
};

/* 
 * Build the interpolation object.  The MEFields must be compatible in the
 * sense that they are all element based, or node based, etc...
 */
Interp(Mesh &src, Mesh &dest, const std::vector<FieldPair> &Fields);

~Interp();

// Actually process the interpolation
void operator()();

private:

void transfer_serial();

void transfer_parallel();

SearchResult sres;
GeomRend grend;
std::vector<FieldPair> fpairs;
bool is_parallel;
std::vector<MEField<>*> srcF;
std::vector<MEField<>*> dstF;
std::vector<_field*> dstf; // interp fields
std::vector<int> iflag;
bool has_std;  // true if a standard interpolation exists
bool has_patch; // true if a patch interp exists
Mesh &srcmesh;
Mesh &dstmesh;
};
  
} // namespace
} // namespace

#endif /*ESMC_INTERP_H_*/
