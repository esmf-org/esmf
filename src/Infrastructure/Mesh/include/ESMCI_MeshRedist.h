// $Id: ESMCI_Search.h,v 1.13 2012/11/13 22:22:41 oehmke Exp $
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_MeshRedist_h
#define ESMCI_MeshRedist_h

#include <list>
#include <Mesh/include/ESMCI_MeshTypes.h>
#include <Mesh/include/ESMCI_MeshObj.h>



namespace ESMCI {

class Mesh;

  void MeshRedist(Mesh *src_mesh, int num_node_gids, int *node_gids, 
                                  int num_elem_gids, int *elem_gids, 
                  Mesh **_output_mesh);

  void MeshRedistElem(Mesh *src_mesh, int num_elem_gids, int *elem_gids, 
                      Mesh **_output_mesh);

  void MeshRedistNode(Mesh *src_mesh, int num_node_gids, int *node_gids, 
                      Mesh **_output_mesh);

} //namespace

#endif
