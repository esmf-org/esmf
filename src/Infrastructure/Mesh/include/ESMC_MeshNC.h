// $Id: ESMC_MeshNC.h,v 1.1.2.2 2009/01/21 21:25:22 cdeluca Exp $
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

#ifndef ESMC_MeshNC_h
#define ESMC_MeshNC_h

#include <ESMC_MEField.h>
#include <string>

namespace ESMCI {
namespace MESH {

class Mesh;
class MeshDB;

// If the function returns true, the node is put in the pole nodeset for special
// procesing.
typedef bool (pole_func)(double*);
typedef bool (latlon_func)(double lat, double lon);

class MeshDB;
// land = false doesn't instantiate the land cells.
void LoadNCMesh(MeshDB &mesh, const std::string name, bool land=true, pole_func pf = NULL, latlon_func lf = NULL);

// Return true if lowerleft corner lat lon means include cell
void LoadNCTMesh(Mesh &mesh, const std::string name, latlon_func lf = NULL);
// Read in a vector field.  Returns true unless timestep > file timesteps, then false.
// timestep starts at 1
bool LoadNCTData(MeshDB &mesh,
                 const std::string &filename,  // file to read from
                 const std::vector<std::string> &vnames, // the names for each component
                 const MEField<> &field, // Read into this field
                 int timestep);

} // namespace
} // namespace

#endif
