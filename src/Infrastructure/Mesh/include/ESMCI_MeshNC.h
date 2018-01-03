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
#ifndef ESMCI_MeshNC_h
#define ESMCI_MeshNC_h

#include <Mesh/include/ESMCI_MEField.h>
#include <Mesh/include/ESMCI_Mesh.h>
#include <string>

namespace ESMCI {

class Mesh;
class MeshDB;

// If the function returns true, the node is put in the pole nodeset for special
// procesing.
typedef bool (pole_func)(double*);
typedef bool (latlon_func)(double lat, double lon);

class MeshDB;
// land = false doesn't instantiate the land cells.
void LoadNCMesh(Mesh &mesh, const std::string name);

/* Load the dual mesh of a given netcdf file.  Note: this loads the file on
 * processor zero only.  It should, however, be called from all processors.
 * Will generate a bilinear mesh unless use_quad = true, then a quadratic.
 */
void LoadNCDualMesh(Mesh &mesh, const std::string fname, bool use_quad=false);

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

#endif
