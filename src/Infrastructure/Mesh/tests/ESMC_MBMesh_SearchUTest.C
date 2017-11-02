// $Id$
//==============================================================================
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
#ifndef MPICH_IGNORE_CXX_SEEK
#define MPICH_IGNORE_CXX_SEEK
#endif
#include <mpi.h>

// ESMF header
#include "ESMC.h"

// ESMF Test header
#include "ESMC_Test.h"

#if defined ESMF_MOAB

#include "ESMC_MBMeshTestUtilMBMesh.C"
#include "ESMC_MBMeshTestUtilPL.C"

// other headers
#include "ESMCI_MBMesh.h"
#include "ESMCI_MBMesh_Glue.h"
#include "ESMCI_MBMesh_Search_EToP.h"
#include "ESMCI_MBMesh_Util.h"

#include "MBTagConventions.hpp"
#include "moab/Core.hpp"
#include "moab/ElemEvaluator.hpp"
#endif

#include <iostream>
#include <iterator>
#include <vector>
#include <cstring>


#if !defined (M_PI)
// for Windows...
#define M_PI 3.14159265358979323846
#endif

using namespace std;

#if defined ESMF_MOAB

bool compare(const double *c1, double *c2) {
  bool pass = false;

  if (c1[0] == c2[0]) pass = true;
  if (c1[1] == c2[1]) pass = true;
  
  return pass;
}

bool pcoords(MBMesh *mesh) {
  int rc = ESMF_RC_NOT_IMPL;
  char name[80];
  char failMsg[80];
  int result = 0;

  rc = ESMF_RC_NOT_IMPL;
  //Get MOAB Mesh
  Interface *moab_mesh=mesh->mesh;

  // Get regions, by dimension, so we stay generic to entity type
  Range elems;
  int rval = moab_mesh->get_entities_by_dimension(0, 2, elems);

  for (Range::iterator it = elems.begin(); it != elems.end(); ++it) {
    EntityHandle elem = *it;
    // ElemEvaluator(Interface *impl, EntityHandle ent = 0, Tag tag = 0, int tagged_ent_dim = -1);
    ElemEvaluator ee = ElemEvaluator(moab_mesh, elem);
    //ee.set_eval_set(elem);
    // set it to look for coordinates not tags
    ee.set_tag(0, 0);

    // setup
    double point[3], pcoords[3];
    int is_inside = 0;
    point[0] = 0.3; point[1] = 0.7; point[2] = 0;
    pcoords[0] = 0.5; pcoords[1] = 0.5; pcoords[2] = 0;

    // try using reverse_eval to get the parametric coordinates of a point inside and element
    // NOTE: Moab currently uses domain [-1,1] for parameterizations,
    //       we will have to add 1 and divide by two to get to [0,1]
    // ErrorCode reverse_eval(const double *posn, double iter_tol, double inside_tol, double *params, int *is_inside = NULL) const;
    ee.reverse_eval(point, 1e-8, 1e-6, pcoords, &is_inside);

    if (is_inside) {
      translate(pcoords);
      if ((pcoords[0] == point[0]) && (pcoords[1] == point[1]))
        rc = ESMF_SUCCESS;
      else rc = ESMF_FAILURE;
    }

/*
    // try using inside to return whether a point is inside an element
    // NOTE: this function takes parameterized point coordinates
    // int moab::ElemEvaluator::inside(const double *params, const double tol) const
    //int is_inside2 = ee.inside(point, 1e-6);

    // get coordinates and id of an element
    double coords[2];
    moab_mesh->get_coords(&(*it), 1, coords);
    printf("elem %d: = [%f, %f, %f]\n", moab_mesh->id_from_handle(elem),
                                        coords[0], coords[1], coords[2]);

    printf("pcoords = [%f, %f, %f]\n", pcoords[0], pcoords[1], pcoords[2]);
    printf("is_inside = %d\n", is_inside);
    printf("\n");
*/
  }
  strcpy(name, "Return parametric coordinates of a point in a cell");
  strcpy(failMsg, "Could not return parametric coordinates of a point in a cell");
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);

  if (rc == ESMF_SUCCESS) return true;
  else return false;

}


bool search_gen(MBMesh *mesh, PointList *pl, vector<double*> &cv) {
  int rc = ESMF_RC_NOT_IMPL;
  char name[80];
  char failMsg[80];
  int result = 0;

  // search between mesh and pointlist
  MBMesh_Search_EToP_Result_List sr;
  MBMesh_Search_EToP(mesh, ESMCI_UNMAPPEDACTION_IGNORE,
                     pl, ESMCI_UNMAPPEDACTION_IGNORE,
                     10E-8, sr);
  rc = ESMF_SUCCESS;

  strcpy(name, "Search between a Mesh and a PointList");
  strcpy(failMsg, "Mesh to PointList search failed");
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);

  // print out search results - elem id with destination id
  rc = ESMF_SUCCESS;

  int sdim = pl->get_coord_dim();
  int ind0 = 0;
  int ind1 = 0;
  for (MBMesh_Search_EToP_Result_List::iterator it = sr.begin();
                                                it != sr.end(); ++it) {
    /*
    // get moab mesh element coords
    const EntityHandle src_elem=(*it)->src_elem;
    int num_nodes = 4;
    double elem_coords[num_nodes*sdim];
    MBMesh_get_elem_coords(mesh, src_elem, num_nodes, &num_nodes, elem_coords);

    std::cout << "Source Element" << ind0 << ":" << std::endl;
    for (int i = 0; i < num_nodes; ++i) {
      std::cout << "  [" << elem_coords[i*sdim+0] << ", "
                         << elem_coords[i*sdim+1];
      if (sdim == 3)
        std::cout << ", " << elem_coords[i*sdim+2] << "]";
      else
        std::cout << "]";
      std::cout << std::endl;
    }
    */
    // get point coords
    for (std::vector<etop_sr>::iterator it2=((*it)->dst_nodes).begin();
                                 it2 != ((*it)->dst_nodes).end(); ++it2) {

      if (ind1 >= cv.size()) {rc = ESMC_RC_VAL_OUTOFRANGE; break;}

      double *c; c = cv.at(ind1);
      if (!compare(it2->node->coords,c)) {
        rc = ESMC_RC_VAL_WRONG;
      }
        //print
        std::cout << "    " << "Destination Point " << it2->dst_gid << ": coords = ["
                << it2->node->coords[0] << ", " << it2->node->coords[1];
        if (sdim == 3)
          std::cout << ", " << it2->node->coords[2] << "]";
        else
          std::cout << "]";

        std::cout << " /= [" << c[0] << ", " << c[1] << "]" << std::endl;
      //}
      ++ind1;
    }
    ++ind0;
  }
  if (rc == ESMF_SUCCESS) return true;
  else return false;
}
#endif

int main(int argc, char *argv[]) {

  char name[80];
  char failMsg[80];
  int result = 0;
  int rc;
  int localPet, petCount;
  ESMC_VM vm;

  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  rc=ESMC_LogSet(true);

#if defined ESMF_MOAB
  //----------------------------------------------------------------------------
  //ESMC_MoabSet(true);
#endif

  // Get parallel information
  vm=ESMC_VMGetGlobal(&rc);
  if (rc != ESMF_SUCCESS) return 0;

  rc=ESMC_VMGet(vm, &localPet, &petCount, (int *)NULL, (MPI_Comm *)NULL,
                (int *)NULL, (int *)NULL);

  if (rc != ESMF_SUCCESS) return 0;

  // common vector for pointlist verification
  vector<double*> cv;

#if defined ESMF_MOAB
  // build a mesh
  MBMesh *mesh_quad;
  mesh_quad = create_mesh_quad(rc);

  // build a pointlist
  PointList *pl_quad;
  pl_quad = create_pointlist_for_quad(&cv, rc);

  strcpy(name, "Simple mesh search");
  strcpy(failMsg, "Search results did not validate");
  ESMC_Test((search_gen(mesh_quad, pl_quad, cv)), name, failMsg, &result, __FILE__, __LINE__, 0);
#else
  strcpy(name, "Simple mesh search");
  strcpy(failMsg, "Search results did not validate");
  ESMC_Test(ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif

#if defined ESMF_MOAB
  // build a mesh
  MBMesh *mesh_tri;
  mesh_tri = create_mesh_tri(rc);

  // build a pointlist
  PointList *pl_tri;
  pl_tri = create_pointlist_for_tri(&cv, rc);

  strcpy(name, "Triangles mesh search");
  strcpy(failMsg, "Search results did not validate");
  ESMC_Test((search_gen(mesh_tri, pl_tri, cv)), name, failMsg, &result, __FILE__, __LINE__, 0);
#else
  strcpy(name, "Triangles mesh search");
  strcpy(failMsg, "Search results did not validate");
  ESMC_Test(ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif

#if defined ESMF_MOAB
  // clean up
  delete pl_quad;
  delete mesh_quad;
  delete pl_tri;
  delete mesh_tri;
#endif

  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);

  return 0;
}


