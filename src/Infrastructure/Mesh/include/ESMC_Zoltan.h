// $Id: ESMC_Zoltan.h,v 1.2 2007/08/07 20:45:56 dneckels Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
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

#ifndef ESMC_Zoltan_h
#define ESMC_Zoltan_h

#include <ESMC_MeshField.h>
#include <ESMC_CommReg.h>
#include <ESMC_Search.h>

namespace ESMCI {
namespace MESH {

class Mesh;
class CommReg;

typedef enum {ZOLT_BILIN=0, ZOLT_PATCH=1} Zolt_Interp;
// Rendezvous the two meshes.  Forms a load balanced geometric partition for both meshes.
// Copies the meshes there (building srcR and dstR)
void ZoltanRendezvous(Mesh &srcmesh, Mesh &destMesh, Mesh &srcR, Mesh &dstR,
  UInt num_fields, MEField<> **sfields, MEField<> **dfields,
  const UInt zinterp[]);

} // namespace
} // namespace

#endif
