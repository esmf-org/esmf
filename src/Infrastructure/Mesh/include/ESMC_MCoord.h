// $Id: ESMC_MCoord.h,v 1.1.2.2 2009/01/21 21:25:22 cdeluca Exp $
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

#ifndef ESMC_MCoord_h
#define ESMC_MCoord_h

#include <ESMC_MeshTypes.h>

namespace ESMCI {
namespace MESH {

// Define local manifold coordinates from a normal vector.  Hardcoded for 2d in 3d.
// Make an abstract class and inherit to generalize.

class MCoord {
public:
// coordinates where normal was taken, normal.
MCoord(const double c[], const double n[]);
// Transform the in coords to local system (in out);
void Transform(const double in[], double out[]) const;
UInt ManifoldDim() const { return 2;}
UInt SpaceDim() const { return 3;}
private:
double ct[3];
double u[2*3];
};

} // namespace
} // namespace

#endif
