// $Id: ESMC_MeshRead.C,v 1.3.2.2 2009/01/21 21:25:23 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <ESMC_MeshRead.h>
#include <ESMC_CommRel.h>
#include <ESMC_MeshField.h>
#include <ESMC_MeshUtils.h>
#include <ESMC_Attr.h>
#include <ESMC_MeshExodus.h>
#include <ESMC_MeshSkin.h>
#include <ESMC_ParEnv.h>
#include <ESMC_MeshVTK.h>

#include <mpi.h>

#include <iostream>
#include <iterator>
#include <iomanip>
#include <sstream>

namespace ESMCI {
namespace MESH {

void WriteMesh(const Mesh &mesh, const std::string &fbase, int nstep, double tstep, int file_type) 
{
  Trace __trace("WriteMesh(const Mesh &mesh, const std::string &fbase, int nstep, double tstep)");
  // Manufacture this processors filename
  int rank = Par::Rank();
  int psize = Par::Size();
  
  std::string newname;

  std::string extension = file_type == ESMC_FILE_EXODUS ? ".g" : ".vtk";

  // If csize = 1, read fbase.g
  if (psize > 1) {
    std::ostringstream newname_str;
    UInt ndec = numDecimal(psize);
    newname_str << fbase << "." << psize << ".";
    newname_str << std::setw(ndec) << std::setfill('0') << rank;
    newname = newname_str.str() + extension;
  } else newname = fbase + extension;

  if (file_type == ESMC_FILE_EXODUS) {
    WriteExMesh(mesh, newname, nstep, tstep);
  } else if (file_type == ESMC_FILE_VTK) {
    WriteVTKMesh(mesh, newname);
  } else Throw() << "Unknown filetype:" << file_type;
}

void WriteMeshTimeStep(const Mesh &mesh, const std::string &fbase, int nstep, double tstep) 
{
  Trace __trace("WriteMeshTimeStep(const Mesh &mesh, const std::string &fbase, int nstep, double tstep)");
  // Manufacture this processors filename
  int rank = Par::Rank();
  int psize = Par::Size();
  
  std::string newname;

  // If csize = 1, read fbase.g
  if (psize > 1) {
    std::ostringstream newname_str;
    UInt ndec = numDecimal(psize);
    newname_str << fbase << "." << psize << ".";
    newname_str << std::setw(ndec) << std::setfill('0') << rank;
    newname = newname_str.str() + ".g";
  } else newname = fbase+".g";

  WriteExMeshTimeStep(nstep, tstep, mesh, newname);
}

void ReadMesh(Mesh &mesh, const std::string &fbase, bool skin, int file_type) 
{
  Trace __trace("ReadMesh(Mesh &mesh, const std::string &fbase)");
  // Manufacture this processors filename
  int rank = Par::Rank();
  int psize = Par::Size();
  
  std::string newname;
  std::string extension = (file_type == ESMC_FILE_EXODUS ? ".g" : ".vtk");

  // If csize = 1, read fbase.g
  if (psize > 1) {
    std::ostringstream newname_str;
    UInt ndec = numDecimal(psize);
    newname_str << fbase << "." << psize << ".";
    newname_str << std::setw(ndec) << std::setfill('0') << rank;
    newname = newname_str.str() + extension;
  } else newname = fbase + extension;


  // Load the local serial mesh
  if (file_type == ESMC_FILE_EXODUS) {
    LoadExMesh(mesh, newname);
  } else if (file_type == ESMC_FILE_VTK) {
    ReadVTKMesh(mesh, newname);
  } else Throw() << "Unknown file type:" << file_type;

  mesh.remove_unused_nodes();

  mesh.ResolvePendingCreate(); // sides need global ids.

  mesh.set_filename(fbase);

  mesh.build_sym_comm_rel(MeshObj::NODE);

  if (skin) Skin(mesh);
  
/*
  // Sort list
  std::sort(snodes.begin(), snodes.end(), std::less<CommRel::CommNode>());
//  std::cout << "proc:" << rank << ", number shared=" << snodes.size() << ", out of " << mesh.num_nodes() << std::endl;


  // Add to comm
  ncomm->add_domain(snodes);

  

  ncomm->complete_range();
*/


}

} // namespace
} // namespace
