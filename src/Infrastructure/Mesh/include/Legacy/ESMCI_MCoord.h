// $Id$
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_MCoord_h
#define ESMCI_MCoord_h

#include <Mesh/include/Legacy/ESMCI_MeshTypes.h>

namespace ESMCI {

// Define local manifold coordinates from a normal vector.  Hardcoded for 2d in 3d.
// Make an abstract class and inherit to generalize.

class MCoord {
public:
// coordinates where normal was taken, normal.
MCoord(const double c[], const double n[]);

MCoord();

MCoord &operator =(const MCoord &rhs);
MCoord(const MCoord &rhs);

// Transform the in coords to local system (in out);
void Transform(const double in[], double out[]) const;
UInt ManifoldDim() const { return 2;}
UInt SpaceDim() const { return 3;}
private:
double ct[3];
double u[2*3];
};

} // namespace

#endif
