/**
 * MOAB, a Mesh-Oriented datABase, is a software component for creating,
 * storing and accessing finite element mesh data.
 * 
 * Copyright 2004 Sandia Corporation.  Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S. Government
 * retains certain rights in this software.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 */


#include "moab/HalfFacetRep.hpp"
#include "Internals.hpp"
#include <iostream>
#include <assert.h>
#include <vector>
#include <map>
#include "MBTagConventions.hpp"
#include "moab/ScdInterface.hpp"
#ifdef MOAB_HAVE_MPI
#include "moab/ParallelComm.hpp"
#endif

namespace moab {

  inline EntityHandle CREATE_HALFFACET(const unsigned lid, const EntityID id)
  {
    assert(id <= MB_END_ID && lid < 6);
    return (((HFacet)lid) << MB_ID_WIDTH)|id;
  }
  inline EntityID FID_FROM_HALFFACET(HFacet handle)
  {
    return (handle & MB_ID_MASK);
  }
  inline int LID_FROM_HALFFACET(HFacet handle)
  {
    return static_cast<int> (handle >> MB_ID_WIDTH);
  }


  HalfFacetRep::HalfFacetRep(Core *impl,   ParallelComm *comm, moab::EntityHandle rset)
    : mb(impl), pcomm(comm), _rset(rset)
  {
    assert(NULL != impl);
    mInitAHFmaps = false;
    chk_mixed = false;
    is_mixed = false;
  }

  HalfFacetRep::~HalfFacetRep() {}


  MESHTYPE HalfFacetRep::get_mesh_type(int nverts, int nedges, int nfaces, int ncells)
  {
    MESHTYPE mesh_type = CURVE;

    if (nverts && nedges && (!nfaces) && (!ncells))
      mesh_type = CURVE;
    else if (nverts && !nedges && nfaces && !ncells)
      mesh_type = SURFACE;
    else if (nverts && nedges && nfaces && !ncells)
      mesh_type = SURFACE_MIXED;
    else if (nverts && !nedges && !nfaces && ncells)
      mesh_type = VOLUME;
    else if (nverts && nedges && !nfaces && ncells)
      mesh_type = VOLUME_MIXED_1;
    else if (nverts && !nedges && nfaces && ncells)
      mesh_type = VOLUME_MIXED_2;
    else if (nverts && nedges && nfaces && ncells)
      mesh_type = VOLUME_MIXED;

    return mesh_type;
  }

  const HalfFacetRep::adj_matrix HalfFacetRep::adjMatrix[7] =
  {
      // Stores the adjacency matrix for each mesh type.
      //CURVE
      {{{0,1,0,0},{1,1,0,0},{0,0,0,0},{0,0,0,0}}},

      //SURFACE
      {{{0,0,1,0},{0,0,0,0},{1,0,1,0},{0,0,0,0}}},

      //SURFACE_MIXED
      {{{0,1,1,0},{1,1,1,0},{1,1,1,0},{0,0,0,0}}},

      //VOLUME
      {{{0,0,0,1},{0,0,0,0},{0,0,0,0},{1,0,0,1}}},

      //VOLUME_MIXED_1
      {{{0,1,0,1},{1,1,0,1},{0,0,0,0},{1,1,0,1}}},

      //VOLUME_MIXED_2
      {{{0,0,1,1},{0,0,0,0},{1,0,1,1},{1,0,1,1}}},

      //VOLUME_MIXED
      {{{0,1,1,1},{1,1,1,1},{1,1,1,1},{1,1,1,1}}}
  };

  int HalfFacetRep::get_index_for_meshtype(MESHTYPE mesh_type)
  {
      int index = 0;
      switch (mesh_type) {
        case CURVE:
          index = 0;
          break;
        case SURFACE:
          index = 1;
          break;
        case SURFACE_MIXED:
          index = 2;
          break;
        case VOLUME:
          index = 3;
          break;
        case VOLUME_MIXED_1:
          index = 4;
          break;
        case VOLUME_MIXED_2:
          index = 5;
          break;
        case VOLUME_MIXED:
          index = 6;
          break;
        }
      return index;
  }

  bool HalfFacetRep::check_mixed_entity_type()
  {
      if (!chk_mixed)
      {
          chk_mixed = true;

          ErrorCode error;
          Range felems, celems;

          error = mb->get_entities_by_dimension( this->_rset, 2, felems ); MB_CHK_ERR(error);

          if (felems.size()){
              Range tris, quad, poly;
              tris = felems.subset_by_type(MBTRI);
              quad = felems.subset_by_type(MBQUAD);
              poly = felems.subset_by_type(MBPOLYGON);
              if ((tris.size()&&quad.size())||(tris.size()&&poly.size())||(quad.size()&&poly.size()))
                  is_mixed = true;
              if (poly.size())
                  is_mixed = true;

              if (is_mixed) return is_mixed;
          }

          error = mb->get_entities_by_dimension( this->_rset, 3, celems);   MB_CHK_ERR(error);
          if (celems.size()){
              Range tet, pyr, prism, hex, polyhed;
              tet = celems.subset_by_type(MBTET);
              pyr = celems.subset_by_type(MBPYRAMID);
              prism = celems.subset_by_type(MBPRISM);
              hex = celems.subset_by_type(MBHEX);
              polyhed = celems.subset_by_type(MBPOLYHEDRON);
              if ((tet.size() && pyr.size())||(tet.size() && prism.size())||
                  (tet.size() && hex.size())||(tet.size()&&polyhed.size())||
                  (pyr.size() && prism.size())||(pyr.size() && hex.size()) ||
                  (pyr.size()&&polyhed.size())|| (prism.size() && hex.size())||
                  (prism.size()&&polyhed.size())||(hex.size()&&polyhed.size()))
                  is_mixed = true;

              if (polyhed.size())
                  is_mixed = true;
          }

          ScdInterface *scdi = NULL;
          error = mb->query_interface(scdi);MB_CHK_ERR(error);
          if (scdi){
              Range boxes;
              error = scdi->find_boxes(boxes);MB_CHK_ERR(error);

              if (!boxes.empty())
                is_mixed = true;
          }
      }
      return is_mixed;
  }

  /*******************************************************
   * initialize                                          *
   ******************************************************/

  ErrorCode HalfFacetRep::initialize()
  {
    ErrorCode error;

    if (!mInitAHFmaps){
        mInitAHFmaps = true;
#ifdef MOAB_HAVE_MPI
        if (pcomm){
            moab::Range _averts, _aedgs, _afacs, _acels;
            error = mb->get_entities_by_dimension(this->_rset, 0, _averts, true);MB_CHK_ERR(error);
            error = mb->get_entities_by_dimension(this->_rset, 1, _aedgs, true);MB_CHK_ERR(error);
            error = mb->get_entities_by_dimension(this->_rset, 2, _afacs, true);MB_CHK_ERR(error);
            error = mb->get_entities_by_dimension(this->_rset, 3, _acels, true);MB_CHK_ERR(error);

            // filter based on parallel status
            error = pcomm->filter_pstatus(_averts, PSTATUS_GHOST, PSTATUS_NOT, -1, &_verts);MB_CHK_ERR(error);
            error = pcomm->filter_pstatus(_aedgs, PSTATUS_GHOST, PSTATUS_NOT, -1, &_edges);MB_CHK_ERR(error);
            error = pcomm->filter_pstatus(_afacs, PSTATUS_GHOST, PSTATUS_NOT, -1, &_faces);MB_CHK_ERR(error);
            error = pcomm->filter_pstatus(_acels, PSTATUS_GHOST, PSTATUS_NOT, -1, &_cells);MB_CHK_ERR(error);
          }
        else {
            error = mb->get_entities_by_dimension( this->_rset, 0, _verts, true);MB_CHK_ERR(error);
            error = mb->get_entities_by_dimension( this->_rset, 1, _edges, true);MB_CHK_ERR(error);
            error = mb->get_entities_by_dimension( this->_rset, 2, _faces, true);MB_CHK_ERR(error);
            error = mb->get_entities_by_dimension( this->_rset, 3, _cells, true);MB_CHK_ERR(error);
          }
#else
        error = mb->get_entities_by_dimension( this->_rset, 0, _verts, true);MB_CHK_ERR(error);
        error = mb->get_entities_by_dimension( this->_rset, 1, _edges, true);MB_CHK_ERR(error);
        error = mb->get_entities_by_dimension( this->_rset, 2, _faces, true);MB_CHK_ERR(error);
        error = mb->get_entities_by_dimension( this->_rset, 3, _cells, true);MB_CHK_ERR(error);

#endif

        int nverts = _verts.size();
        int nedges = _edges.size();
        int nfaces = _faces.size();
        int ncells = _cells.size();


        MESHTYPE mesh_type = get_mesh_type(nverts, nedges, nfaces, ncells);
        thismeshtype = mesh_type;

        //Initialize mesh type specific maps
        if (thismeshtype == CURVE){
            error = init_curve(); MB_CHK_ERR(error);
          }
        else if (thismeshtype == SURFACE){
            error = init_surface();MB_CHK_ERR(error);
          }
        else if (thismeshtype == SURFACE_MIXED){
            error = init_curve();MB_CHK_ERR(error);
            error = init_surface();MB_CHK_ERR(error);
          }
        else if (thismeshtype == VOLUME){
            error = init_volume();MB_CHK_ERR(error);
          }
        else if (thismeshtype == VOLUME_MIXED_1){
            error = init_curve();MB_CHK_ERR(error);
            error = init_volume();MB_CHK_ERR(error);
          }
        else if (thismeshtype == VOLUME_MIXED_2){
            error = init_surface();MB_CHK_ERR(error);
            error = init_volume();MB_CHK_ERR(error);
          }
        else if (thismeshtype == VOLUME_MIXED){
            error = init_curve();MB_CHK_ERR(error);
            error = init_surface();MB_CHK_ERR(error);
            error = init_volume();MB_CHK_ERR(error);
          }
      }
    return MB_SUCCESS;
  }

  ErrorCode HalfFacetRep::deinitialize()
  {
    return MB_SUCCESS;
  }

  ErrorCode HalfFacetRep::init_curve()
  {
    ErrorCode error;

    int nv = ID_FROM_HANDLE(*(_verts.end()-1));
    int ne = ID_FROM_HANDLE(*(_edges.end()-1));

    v2hv.resize(nv,0);
    sibhvs.resize(ne*2,0);

    error = determine_sibling_halfverts(_verts, _edges);MB_CHK_ERR(error);
    error = determine_incident_halfverts( _edges);MB_CHK_ERR(error);

    return MB_SUCCESS;
  }

  ErrorCode HalfFacetRep::init_surface()
  {
    ErrorCode error;
    EntityType ftype = mb->type_from_handle(*_faces.begin());
    int nepf = lConnMap2D[ftype-2].num_verts_in_face;

    int nv = ID_FROM_HANDLE(*(_verts.end()-1));
    int nf = ID_FROM_HANDLE(*(_faces.end()-1));

    v2he.resize(nv, 0);
    sibhes.resize(nf*nepf,0);

    // Construct ahf maps
    error = determine_sibling_halfedges(_faces);MB_CHK_ERR(error);
    error = determine_incident_halfedges(_faces);MB_CHK_ERR(error);

    //Initialize queues for storing face and local id's during local search
    for (int i = 0; i< MAXSIZE; i++)
      {
        queue_fid[i] = 0;
        queue_lid[i] = 0;
        trackfaces[i] = 0;
      }

    return MB_SUCCESS;
  }

  ErrorCode HalfFacetRep::init_volume()
  {
    ErrorCode error;

    //Initialize std::map between cell-types and their index in lConnMap3D
    cell_index[MBTET] = 0;
    cell_index[MBPYRAMID] = 1;
    cell_index[MBPRISM] = 2;
    cell_index[MBHEX] = 3;

    int index = get_index_in_lmap(*_cells.begin());
    int nfpc = lConnMap3D[index].num_faces_in_cell;
    int nv = ID_FROM_HANDLE(*(_verts.end()-1));
    int nc = ID_FROM_HANDLE(*(_cells.end()-1));;

    v2hf.resize(nv, 0);
    sibhfs.resize(nc*nfpc, 0);

    //Construct the maps
    error = determine_sibling_halffaces(_cells);MB_CHK_ERR(error);
    error = determine_incident_halffaces(_cells);MB_CHK_ERR(error);

    //Initialize queues for storing face and local id's during local search
    for (int i = 0; i< MAXSIZE; i++)
      {
        Stkcells[i] = 0;
        cellq[i] = 0;
        trackcells[i] = 0;
      }

    return MB_SUCCESS;
  }

   //////////////////////////////////////////////////
   ErrorCode HalfFacetRep::print_tags(int dim)
   {
     if (dim==1){
         EntityHandle start_edge = *_edges.begin();
         std::cout<<"start_edge = "<<start_edge<<std::endl;
         std::cout<<"<SIBHVS_EID,SIBHVS_LVID>"<<std::endl;

         for (Range::iterator i = _edges.begin(); i != _edges.end(); ++i){
             EntityHandle eid[2];  int lvid[2];
             int eidx = ID_FROM_HANDLE(*i)-1;
             HFacet hf1 = sibhvs[2*eidx];
             HFacet hf2 = sibhvs[2*eidx+1];
             eid[0] = fid_from_halfacet(hf1,MBEDGE); eid[1] = fid_from_halfacet(hf2,MBEDGE);
             lvid[0] = lid_from_halffacet(hf1); lvid[1] = lid_from_halffacet(hf2);
             std::cout<<"Entity = "<<*i<<" :: <"<<eid[0]<<","<<lvid[0]<<">"<<"      "<<"<"<<eid[1]<<","<<lvid[1]<<">"<<std::endl;
           }

         std::cout<<"<V2HV_EID, V2HV_LVID>"<<std::endl;

         for (Range::iterator i = _verts.begin(); i != _verts.end(); ++i){
             int vidx = ID_FROM_HANDLE(*i)-1;
             HFacet hf = v2hv[vidx];
             EntityHandle eid = fid_from_halfacet(hf, MBEDGE);
             int lvid = lid_from_halffacet(hf);
             std::cout<<"Vertex = "<<*i<<" :: <"<<eid<<","<<lvid<<">"<<std::endl;
           }
       }
     else if (dim==2){
         EntityType ftype = mb->type_from_handle(*_faces.begin());
         int nepf = lConnMap2D[ftype-2].num_verts_in_face;
         EntityHandle start_face = *_faces.begin();
         std::cout<<"start_face = "<<start_face<<std::endl;
         std::cout<<"<SIBHES_FID,SIBHES_LEID>"<<std::endl;

         for (Range::iterator i = _faces.begin(); i != _faces.end(); ++i){
             int fidx = ID_FROM_HANDLE(*i)-1;
             std::cout<<"Entity = "<<*i;
             for (int j=0; j<nepf; j++){
                 HFacet hf = sibhes[nepf*fidx+j];
                 EntityHandle sib = fid_from_halfacet(hf, ftype);
                 int lid = lid_from_halffacet(hf);
                 std::cout<<" :: <"<<sib<<","<<lid<<">"<<"       ";
               }
             std::cout<<std::endl;
           }

         std::cout<<"<V2HE_FID, V2HE_LEID>"<<std::endl;

         for (Range::iterator i = _verts.begin(); i != _verts.end(); ++i){
             int vidx = ID_FROM_HANDLE(*i)-1;
             HFacet hf = v2he[vidx];
             EntityHandle fid = fid_from_halfacet(hf, ftype);
             int lid = lid_from_halffacet(hf);
             std::cout<<"Vertex = "<<*i<<" :: <"<<fid<<","<<lid<<">"<<std::endl;
           }
       }
     else if (dim==3){
         EntityType ctype = mb->type_from_handle(*_cells.begin());

         int index = get_index_in_lmap(*_cells.begin());
         int nfpc = lConnMap3D[index].num_faces_in_cell;
         EntityHandle start_cell = *_cells.begin();
         std::cout<<"start_cell = "<<start_cell<<std::endl;
         std::cout<<"<SIBHES_CID,SIBHES_LFID>"<<std::endl;

         for (Range::iterator i = _cells.begin(); i != _cells.end(); ++i){
             int cidx = ID_FROM_HANDLE(*i)-1;
             std::cout<<"Entity = "<<*i;
             for (int j=0; j<nfpc; j++){
                 HFacet hf = sibhfs[nfpc*cidx+j];
                 EntityHandle sib = fid_from_halfacet(hf, ctype);
                 int lid = lid_from_halffacet(hf);
                 std::cout<<" :: <"<<sib<<","<<lid<<">"<<"       ";
               }
             std::cout<<std::endl;
           }

         std::cout<<"<V2HF_CID, V2HF_LFID>"<<std::endl;
         EntityHandle cid;
         int lid;

         for (Range::iterator i = _verts.begin(); i != _verts.end(); ++i){
             int vidx = ID_FROM_HANDLE(*i)-1;
             HFacet hf = v2hf[vidx];

             if (hf ==0 && (v2hfs.find(*i) != v2hfs.end()))
               {
                 std::pair <std::multimap<EntityHandle, HFacet>::iterator, std::multimap<EntityHandle, HFacet>::iterator> it_hfs;
                 it_hfs = v2hfs.equal_range(*i);

                 for (std::multimap<EntityHandle, HFacet>::iterator it = it_hfs.first; it != it_hfs.second; ++it)
                   {
                     cid = fid_from_halfacet(it->second, ctype);
                     lid = lid_from_halffacet(hf);

                     std::cout<<"Vertex = "<<*i<<" :: <"<<cid<<","<<lid<<">"<<std::endl;
                   }
               }
             else {
                 cid = fid_from_halfacet(hf, ctype);
                 lid = lid_from_halffacet(hf);
                 std::cout<<"Vertex = "<<*i<<" :: <"<<cid<<","<<lid<<">"<<std::endl;
               }
           }

       }
     return MB_SUCCESS;
   }

   /**********************************************************
   *      User interface for adjacency functions            *
   ********************************************************/

  ErrorCode HalfFacetRep::get_adjacencies(const EntityHandle source_entity,
                                          const unsigned int target_dimension,
                                          std::vector<EntityHandle> &target_entities)
  {

      ErrorCode error;

      unsigned int source_dimension = mb->dimension_from_handle(source_entity);
      assert((source_dimension <= target_dimension) || (source_dimension > target_dimension));

      if (mInitAHFmaps == false)
      {
          error = initialize(); MB_CHK_ERR(error);
      }

      int mindex = get_index_for_meshtype(thismeshtype);
      int adj_possible = adjMatrix[mindex].val[source_dimension][target_dimension];

      if (adj_possible)
      {
          if (source_dimension < target_dimension)
          {
              error = get_up_adjacencies(source_entity, target_dimension, target_entities); MB_CHK_ERR(error);
          }
          else if (source_dimension == target_dimension)
          {
              error = get_neighbor_adjacencies(source_entity, target_entities);  MB_CHK_ERR(error);
          }
          else
          {           
              error = get_down_adjacencies(source_entity, target_dimension, target_entities); MB_CHK_ERR(error);
          }
      }
      else
          return MB_SUCCESS;

      return MB_SUCCESS;
  }


   ErrorCode HalfFacetRep::get_up_adjacencies(EntityHandle ent,
                                              int out_dim,
                                              std::vector<EntityHandle> &adjents,
                                              std::vector<int> * lids)
   {
    ErrorCode error;
    int in_dim = mb->dimension_from_handle(ent);
    assert((in_dim >=0 && in_dim <= 2) && (out_dim > in_dim));

    if (in_dim == 0)
      {
        if (out_dim == 1)
        {
            error = get_up_adjacencies_1d(ent, adjents, lids);  MB_CHK_ERR(error);
        }
        else if (out_dim == 2)
        {
            error = get_up_adjacencies_vert_2d(ent, adjents);  MB_CHK_ERR(error);
        }
        else if (out_dim == 3)
        {
            error = get_up_adjacencies_vert_3d(ent, adjents); MB_CHK_ERR(error);
        }
      }

    else if ((in_dim == 1) && (out_dim == 2))
      {
        error = get_up_adjacencies_2d(ent, adjents, lids); MB_CHK_ERR(error);
      }
    else if ((in_dim == 1) && (out_dim == 3))
      {
        error = get_up_adjacencies_edg_3d(ent, adjents, lids); MB_CHK_ERR(error);
      }
    else if ((in_dim == 2) && (out_dim ==3))
      {
        error = get_up_adjacencies_face_3d(ent, adjents, lids); MB_CHK_ERR(error);
      }
    return MB_SUCCESS;
   }

   ErrorCode HalfFacetRep::get_neighbor_adjacencies(EntityHandle ent,
                                                    std::vector<EntityHandle> &adjents)
   {
     ErrorCode error;
     int in_dim = mb->dimension_from_handle(ent);
     assert(in_dim >=1 && in_dim <= 3);

     if (in_dim == 1)
       {
         error = get_neighbor_adjacencies_1d(ent, adjents); MB_CHK_ERR(error);
       }

     else if (in_dim == 2)
       {
         error = get_neighbor_adjacencies_2d(ent, adjents); MB_CHK_ERR(error);
       }
     else if (in_dim == 3)
       {
         error = get_neighbor_adjacencies_3d(ent, adjents); MB_CHK_ERR(error);
       }
     return MB_SUCCESS;
   }

   ErrorCode HalfFacetRep::get_down_adjacencies(EntityHandle ent, int out_dim, std::vector<EntityHandle> &adjents)
   {
       ErrorCode error;
       int in_dim = mb->dimension_from_handle(ent);
       assert((in_dim >=2 && in_dim <= 3) && (out_dim < in_dim));

       if ((in_dim == 2)&&(out_dim == 1))
       {
           error = get_down_adjacencies_2d(ent, adjents);  MB_CHK_ERR(error);
       }
       else if ((in_dim == 3)&&(out_dim == 1))
       {
           error = get_down_adjacencies_edg_3d(ent, adjents); MB_CHK_ERR(error);
       }
       else if ((in_dim == 3)&&(out_dim == 2))
       {
           error = get_down_adjacencies_face_3d(ent, adjents); MB_CHK_ERR(error);
       }
       return MB_SUCCESS;
   }

   ErrorCode HalfFacetRep::count_subentities(Range &edges, Range &faces, Range &cells, int *nedges, int *nfaces)
   {
     ErrorCode error;
     if (edges.size() && !faces.size() && !cells.size())
       {
         nedges[0] = edges.size();
         nfaces[0] = 0;
       }
     else if (faces.size() && !cells.size())
       {
         nedges[0] = find_total_edges_2d(faces);
         nfaces[0] = 0;
       }
     else if (cells.size())
       {
         error = find_total_edges_faces_3d(cells, nedges, nfaces); MB_CHK_ERR(error);
       }
     return MB_SUCCESS;
   }

  /******************************************************** 
  * 1D: sibhvs, v2hv, incident and neighborhood queries   *
  *********************************************************/
  ErrorCode HalfFacetRep::determine_sibling_halfverts(Range &verts, Range &edges)
  {
    ErrorCode error;

    //Step 1: Create an index list storing the starting position for each vertex
    int nv = verts.size();
    int *is_index = new int[nv+1];
    for (int i =0; i<nv+1; i++)
      is_index[i] = 0;

    for (Range::iterator eid = edges.begin(); eid != edges.end(); ++eid)
      {
        const EntityHandle* conn;
        int num_conn = 0;
        error = mb->get_connectivity(*eid, conn, num_conn, true);MB_CHK_ERR(error);

        int index = verts.index(conn[0]);
        is_index[index+1] += 1;
        index = verts.index(conn[1]);
        is_index[index+1] += 1;
      }
    is_index[0] = 0;

    for (int i=0; i<nv; i++)
      is_index[i+1] = is_index[i] + is_index[i+1];

    //Step 2: Define two arrays v2hv_eid, v2hv_lvid storing every half-facet on a vertex
    EntityHandle *v2hv_map_eid = new EntityHandle[2*edges.size()];
    int *v2hv_map_lvid = new int[2*edges.size()];

    for (Range::iterator eid = edges.begin(); eid != edges.end(); ++eid)
      {
        const EntityHandle* conn;
        int num_conn = 0;
        error = mb->get_connectivity(*eid, conn, num_conn, true);MB_CHK_ERR(error);

        for (int j = 0; j< 2; j++)
          {
            int v = verts.index(conn[j]);
            v2hv_map_eid[is_index[v]] = *eid;
            v2hv_map_lvid[is_index[v]] = j;
            is_index[v] += 1;
          }
      }

    for (int i=nv-2; i>=0; i--)
      is_index[i+1] = is_index[i];
    is_index[0] = 0;

    //Step 3: Fill up sibling half-verts map
    for (Range::iterator vid = verts.begin(); vid != verts.end(); ++vid)
      {
        int v = verts.index(*vid);
        int last = is_index[v+1] - 1;
        if (last > is_index[v])
          {
            EntityHandle prev_eid = v2hv_map_eid[last];
            int prev_lvid = v2hv_map_lvid[last];

            for (int i=is_index[v]; i<=last; i++)
              {
                EntityHandle cur_eid = v2hv_map_eid[i];
                int cur_lvid = v2hv_map_lvid[i];

                int pidx = ID_FROM_HANDLE(prev_eid)-1;
                sibhvs[2*pidx+prev_lvid] = create_halffacet(cur_eid, cur_lvid);

                prev_eid = cur_eid;
                prev_lvid = cur_lvid;

              }
          }
      }

    delete [] is_index;
    delete [] v2hv_map_eid;
    delete [] v2hv_map_lvid;

    return MB_SUCCESS;
  }

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  ErrorCode HalfFacetRep::determine_incident_halfverts( Range &edges){
    ErrorCode error;

    for (Range::iterator e_it = edges.begin(); e_it != edges.end(); ++e_it){
        EntityHandle cur_eid = *e_it;
        const EntityHandle* conn;
        int num_conn = 0;
        error = mb->get_connectivity(*e_it, conn, num_conn, true);MB_CHK_ERR(error);

        for(int i=0; i<2; ++i){
            EntityHandle v = conn[i];
            int vidx = ID_FROM_HANDLE(v)-1;

            HFacet hf = v2hv[vidx];
            EntityHandle eid = fid_from_halfacet(hf, MBEDGE);
            if (eid==0){
                v2hv[vidx] = create_halffacet(cur_eid,i);
              }
          }
      }

    return MB_SUCCESS;
  }
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  ErrorCode  HalfFacetRep::get_up_adjacencies_1d( EntityHandle vid,
                                                  std::vector< EntityHandle > &adjents,
                                                  std::vector<int> * lvids)
  {
    adjents.clear();
    adjents.reserve(20);

    if (lvids != NULL)
      lvids->reserve(20);

    int vidx = ID_FROM_HANDLE(vid)-1;
    HFacet hf = v2hv[vidx];

    EntityHandle  start_eid = fid_from_halfacet(hf, MBEDGE);
    int start_lid = lid_from_halffacet(hf);

    EntityHandle eid = start_eid;
    int lid = start_lid;

    if (eid != 0){
      adjents.push_back(eid);
      if (lvids != NULL)
        lvids->push_back(lid);

      while (eid !=0) {
	  int eidx = ID_FROM_HANDLE(eid)-1;
	  HFacet shf = sibhvs[2*eidx+lid];
	  eid = fid_from_halfacet(shf, MBEDGE);
	  lid = lid_from_halffacet(shf);

	  if ((!eid)||(eid == start_eid))
	    break;

	  adjents.push_back(eid);
	  if (lvids != NULL)
	    lvids->push_back(lid);
      }
    }

    return MB_SUCCESS;
  }
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  ErrorCode  HalfFacetRep::get_neighbor_adjacencies_1d( EntityHandle eid,
                                                        std::vector<EntityHandle> &adjents)
  {
    adjents.clear();
    adjents.reserve(20);

    EntityHandle sibhv_eid;
    int sibhv_lid;
    int eidx = ID_FROM_HANDLE(eid)-1;

    for (int lid = 0;lid < 2; ++lid){
        HFacet shf = sibhvs[2*eidx+lid];
        sibhv_eid = fid_from_halfacet(shf, MBEDGE);
        sibhv_lid = lid_from_halffacet(shf);

      if (sibhv_eid != 0){
        adjents.push_back(sibhv_eid);

        eidx = ID_FROM_HANDLE(sibhv_eid)-1;
        HFacet nhf = sibhvs[2*eidx+sibhv_lid];
        EntityHandle hv_eid = fid_from_halfacet(nhf,MBEDGE);
        int hv_lid = lid_from_halffacet(nhf);
	
	while (hv_eid != 0){	    
	  if (hv_eid != eid)
	    adjents.push_back(hv_eid);

	  eidx = ID_FROM_HANDLE(hv_eid)-1;
	  HFacet hf = sibhvs[2*eidx+hv_lid];
	  EntityHandle edge = fid_from_halfacet(hf, MBEDGE);
	  if (edge == sibhv_eid)
	    break;

	  hv_eid = edge;
	  hv_lid = lid_from_halffacet(hf);
	}
      }
    } 

    return MB_SUCCESS;   
  }
  
  /*******************************************************
  * 2D: sibhes, v2he, incident and neighborhood queries  *
  ********************************************************/
  const HalfFacetRep::LocalMaps2D HalfFacetRep::lConnMap2D[2] = {
    //Triangle
    {3, {1,2,0}, {2,0,1}},
    //Quad
    {4,{1,2,3,0},{3,0,1,2}}
  };
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
   ErrorCode HalfFacetRep::determine_sibling_halfedges( Range &faces)
  {
    ErrorCode error;
    EntityHandle start_face = *faces.begin();
    EntityType ftype = mb->type_from_handle(start_face);
    int nfaces = faces.size();
    int nepf = lConnMap2D[ftype-2].num_verts_in_face;

    //Step 1: Create an index list storing the starting position for each vertex
    int nv = _verts.size();
    int *is_index = new int[nv+1];
    for (int i =0; i<nv+1; i++)
      is_index[i] = 0;

    int index;

    for (Range::iterator fid = faces.begin(); fid != faces.end(); ++fid)
       {
        const EntityHandle* conn;
        error = mb->get_connectivity(*fid, conn, nepf, true);MB_CHK_ERR(error);

         for (int i = 0; i<nepf; i++)
           {
             index = _verts.index(conn[i]);
             is_index[index+1] += 1;
           }
       }
     is_index[0] = 0;

     for (int i=0; i<nv; i++)
       is_index[i+1] = is_index[i] + is_index[i+1];

     //Step 2: Define two arrays v2hv_eid, v2hv_lvid storing every half-facet on a vertex
     EntityHandle * v2nv = new EntityHandle[nepf*nfaces];
     EntityHandle * v2he_map_fid = new EntityHandle[nepf*nfaces];
     int * v2he_map_leid = new int[nepf*nfaces];

     for (Range::iterator fid = faces.begin(); fid != faces.end(); ++fid)
       {
         const EntityHandle* conn;
         error = mb->get_connectivity(*fid, conn, nepf, true);MB_CHK_ERR(error);

         for (int j = 0; j< nepf; j++)
           {
             int v = _verts.index(conn[j]);
             int nidx = lConnMap2D[ftype-2].next[j];
             v2nv[is_index[v]] = conn[nidx];
             v2he_map_fid[is_index[v]] = *fid;
             v2he_map_leid[is_index[v]] = j;
             is_index[v] += 1;
           }
       }

     for (int i=nv-2; i>=0; i--)
       is_index[i+1] = is_index[i];
     is_index[0] = 0;


     //Step 3: Fill up sibling half-verts map
     for (Range::iterator fid = faces.begin(); fid != faces.end(); ++fid)
       {
         const EntityHandle* conn;
         error = mb->get_connectivity(*fid, conn, nepf, true);MB_CHK_ERR(error);

         int fidx = ID_FROM_HANDLE(*fid)-1;
         for (int k =0; k<nepf; k++)
           {
             HFacet hf = sibhes[nepf*fidx+k];
             EntityHandle sibfid = fid_from_halfacet(hf, ftype);

             if (sibfid != 0)
               continue;

             int nidx = lConnMap2D[ftype-2].next[k];
             int v = _verts.index(conn[k]);
             int vn = _verts.index(conn[nidx]);

             EntityHandle first_fid = *fid;
             int first_leid = k;

             EntityHandle prev_fid = *fid;
             int prev_leid = k;

             for (index = is_index[vn]; index <= is_index[vn+1]-1; index++)
               {
                 if (v2nv[index] == conn[k])
                   {
                     EntityHandle cur_fid = v2he_map_fid[index];
                     int cur_leid = v2he_map_leid[index];

                     int pidx = ID_FROM_HANDLE(prev_fid)-1;
                     sibhes[nepf*pidx+prev_leid] = create_halffacet(cur_fid, cur_leid);

                     prev_fid = cur_fid;
                     prev_leid = cur_leid;
                   }
               }

             for (index = is_index[v]; index <= is_index[v+1]-1; index++)
               {
                 if ((v2nv[index] == conn[nidx])&&(v2he_map_fid[index] != *fid))
                   {

                     EntityHandle cur_fid = v2he_map_fid[index];
                     int cur_leid = v2he_map_leid[index];

                     int pidx = ID_FROM_HANDLE(prev_fid)-1;
                     sibhes[nepf*pidx+prev_leid] = create_halffacet(cur_fid, cur_leid);

                     prev_fid = cur_fid;
                     prev_leid = cur_leid;

                   }
               }

             if (prev_fid != first_fid){

                 int pidx = ID_FROM_HANDLE(prev_fid)-1;
                 sibhes[nepf*pidx+prev_leid] = create_halffacet(first_fid, first_leid);
             }
           }
       }

     delete [] is_index;
     delete [] v2nv;
     delete [] v2he_map_fid;
     delete [] v2he_map_leid;

     return MB_SUCCESS;

  }
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  ErrorCode HalfFacetRep::determine_incident_halfedges( Range &faces)
  {
    ErrorCode error;    
    EntityType ftype = mb->type_from_handle(*faces.begin());
    int nepf = lConnMap2D[ftype-2].num_verts_in_face;

    std::vector<char> markEdges(nepf*faces.size(), 0);

    for (Range::iterator it = faces.begin(); it != faces.end(); ++it){      
        EntityHandle fid = *it;
        const EntityHandle* conn;
        error = mb->get_connectivity(fid, conn, nepf, true);MB_CHK_ERR(error);

        for(int i=0; i<nepf; ++i){
            EntityHandle v = conn[i];
            int vidx = ID_FROM_HANDLE(v)-1;
            HFacet hf = v2he[vidx];

            if (hf == 0 && (v2hes.empty() || (v2hes.find(v) == v2hes.end())))
              {
                //This is the first time a half-facet is assigned to a vertex.
                HFacet nwhf=0;
                error = mark_halfedges(v, fid, i, faces, markEdges,nwhf);MB_CHK_ERR(error);

                if (nwhf ==0)
                  nwhf = create_halffacet(fid, i);

                v2he[vidx] = nwhf;
              }
            else if (hf != 0 && !markEdges[nepf*faces.index(fid)+i])
              {
                //This is the first time a non-manifold vertex is encountered. Copy the existing he in v2he[v] to the multimap.
                v2hes.insert(std::pair<EntityHandle,HFacet>(v,hf));
                HFacet nwhf = 0;
                error = mark_halfedges(v, fid, i, faces, markEdges, nwhf);MB_CHK_ERR(error);

                if (nwhf == 0)
                  nwhf = create_halffacet(fid, i);

                v2hes.insert(std::pair<EntityHandle,HFacet>(v, nwhf));
                v2he[vidx] = 0;
              }
            else if (hf == 0 && (!v2hes.empty()) && (v2hes.find(v) != v2hes.end()) && !markEdges[nepf*faces.index(fid)+i])
              {
                //This is check if reached if the vertex is non-manifold and has encountered a half-facet to a new component.
                HFacet nwhf = 0;
                error = mark_halfedges(v, fid, i, faces, markEdges, nwhf);MB_CHK_ERR(error);

                if (nwhf == 0)
                  nwhf = create_halffacet(fid, i);

                v2hes.insert(std::pair<EntityHandle,HFacet>(v, nwhf));
              }
          }
      }

   // error = print_tags(2);

    return MB_SUCCESS;
  }

  ///////////////////////////////////////////////////////////////////
  ErrorCode HalfFacetRep::mark_halfedges(EntityHandle vid, EntityHandle he_fid, int he_lid, Range &faces, std::vector<char> &markHEdgs, HFacet &bnd_hf)
  {
    ErrorCode error;
    EntityType ftype = mb->type_from_handle(he_fid);
    int nepf = lConnMap2D[ftype-2].num_verts_in_face;

    int qsize = 0, count = -1;
    int num_qvals = 0;

    error = gather_halfedges(vid, he_fid, he_lid, &qsize, &count);MB_CHK_ERR(error);

    while (num_qvals < qsize)
      {
        EntityHandle curfid = queue_fid[num_qvals];
        int curlid = queue_lid[num_qvals];
        num_qvals += 1;

        int fidx = ID_FROM_HANDLE(curfid)-1;

        const EntityHandle* conn;
        error = mb->get_connectivity(curfid, conn, nepf, true);MB_CHK_ERR(error);

        if (!markHEdgs[nepf*faces.index(curfid)+curlid] && (conn[curlid]==vid)){
            markHEdgs[nepf*faces.index(curfid)+curlid] = 1;
            HFacet hf = sibhes[nepf*fidx+curlid];
            EntityHandle sibfid = fid_from_halfacet(hf, ftype);
            if (sibfid == 0)
              bnd_hf = create_halffacet(curfid, curlid);
          }

        EntityHandle he2_fid = 0; int he2_lid = 0;
        error = another_halfedge(vid, curfid, curlid, &he2_fid, &he2_lid);  MB_CHK_ERR(error);

        if (!markHEdgs[nepf*faces.index(curfid)+he2_lid] && (conn[he2_lid]==vid)){
            markHEdgs[nepf*faces.index(curfid)+he2_lid] = 1;
            HFacet hf = sibhes[nepf*fidx+he2_lid];
            EntityHandle sibfid = fid_from_halfacet(hf, ftype);
            if (sibfid == 0)
              bnd_hf = create_halffacet(he2_fid, he2_lid);
          }

        bool val = find_match_in_array(he2_fid, trackfaces, count);

        if (val)
          continue;

        count += 1;
        trackfaces[count] = he2_fid;

        error = get_up_adjacencies_2d(he2_fid, he2_lid, &qsize, &count);MB_CHK_ERR(error);
      }

    //Change the visited faces to false, also empty the queue
    for (int i = 0; i<=qsize; i++)
      {
        queue_fid[i] = 0;
        queue_lid[i] = 0;
      }

    for (int i = 0; i<=count; i++)
      trackfaces[i] = 0;
    return MB_SUCCESS;
  }

  ///////////////////////////////////////////////////////////////////
  ErrorCode HalfFacetRep::get_up_adjacencies_vert_2d(EntityHandle vid, std::vector<EntityHandle> &adjents)
  {
    ErrorCode error;
    EntityType ftype = mb->type_from_handle(*_faces.begin());

    int vidx = ID_FROM_HANDLE(vid)-1;
    HFacet hf = v2he[vidx];

    std::vector<EntityHandle> start_fids;
    std::vector<int> start_lids;

    if (hf == 0 && (v2hes.find(vid) != v2hes.end()))
      {
        std::pair <std::multimap<EntityHandle, HFacet>::iterator, std::multimap<EntityHandle, HFacet>::iterator> it_hes;
        it_hes = v2hes.equal_range(vid);

        for (std::multimap<EntityHandle, HFacet>::iterator it = it_hes.first; it != it_hes.second; ++it)
          {
            start_fids.push_back(fid_from_halfacet(it->second, ftype));
            start_lids.push_back(lid_from_halffacet(it->second));
          }
      }
    else if (hf != 0)
      {
        start_fids.push_back( fid_from_halfacet(hf, ftype));
        start_lids.push_back(lid_from_halffacet(hf));
      }

    if (start_fids.empty())
      return MB_SUCCESS;

    int qsize = 0, count = -1;
    int num_qvals = 0;

    adjents.reserve((int)start_fids.size());

    for (int i=0; i<(int)start_fids.size(); i++)
      {
        adjents.push_back(start_fids[i]);
        error = gather_halfedges(vid, start_fids[i], start_lids[i], &qsize, &count);MB_CHK_ERR(error);
      }

    while (num_qvals < qsize)
      {
        EntityHandle curfid = queue_fid[num_qvals];
        int curlid = queue_lid[num_qvals];
        num_qvals += 1;

        EntityHandle he2_fid = 0; int he2_lid = 0;
        error = another_halfedge(vid, curfid, curlid, &he2_fid, &he2_lid);MB_CHK_ERR(error);

        bool val = find_match_in_array(he2_fid, trackfaces, count);

        if (val)
          continue;

        count += 1;
        trackfaces[count] = he2_fid;

        error = get_up_adjacencies_2d(he2_fid, he2_lid, &qsize, &count);MB_CHK_ERR(error);

        adjents.push_back(he2_fid);
      }

    //Change the visited faces to false, also empty the queue
    for (int i = 0; i<=qsize; i++)
      {
        queue_fid[i] = 0;
        queue_lid[i] = 0;
      }

    for (int i = 0; i<=count; i++)
      trackfaces[i] = 0;

    return MB_SUCCESS;
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  ErrorCode  HalfFacetRep::get_up_adjacencies_2d( EntityHandle eid,
                                                  std::vector<EntityHandle> &adjents,
                                                  std::vector<int>  * leids)
{

  // Given an explicit edge eid, find the incident faces.
    ErrorCode error;
    EntityHandle he_fid=0; int he_lid=0;

    // Step 1: Given an explicit edge, find a corresponding half-edge from the surface mesh.
    bool found = find_matching_halfedge(eid, &he_fid, &he_lid);

    // Step 2: If there is a corresponding half-edge, collect all sibling half-edges and store the incident faces.
    if (found)
      { 
        error = get_up_adjacencies_2d(he_fid, he_lid, true, adjents, leids);MB_CHK_ERR(error);
      }

    return MB_SUCCESS;
  }

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  ErrorCode HalfFacetRep::get_up_adjacencies_2d(EntityHandle fid,
                                                 int leid,
                                                 bool add_inent,
                                                 std::vector<EntityHandle> &adj_ents,
                                                 std::vector<int> *adj_leids, std::vector<int> *adj_orients)
  {
    // Given an implicit half-edge <fid, leid>, find the incident half-edges.
    ErrorCode error;

    EntityType ftype = mb->type_from_handle(fid);
    int nepf = lConnMap2D[ftype-2].num_verts_in_face;

    if (!fid)
      return MB_FAILURE;
    adj_ents.reserve(20);

    bool local_id =false;
    bool orient = false;
    if (adj_leids != NULL)
      {
        local_id = true;
        adj_leids->reserve(20);
      }
    if (adj_orients != NULL)
      {
        orient = true;
        adj_orients->reserve(20);
      }

    if (add_inent)
      {
        adj_ents.push_back(fid);
        if (local_id)
          adj_leids->push_back(leid);
      }

    EntityHandle fedge[2] = {0,0};

    if (orient)
      {
        //get connectivity and match their directions
        const EntityHandle* fid_conn;
        error = mb->get_connectivity(fid, fid_conn, nepf, true);MB_CHK_ERR(error);

        int nidx = lConnMap2D[ftype-2].next[leid];
        fedge[0] = fid_conn[leid];
        fedge[1] = fid_conn[nidx];
      }

    int fidx = ID_FROM_HANDLE(fid)-1;
    HFacet hf = sibhes[nepf*fidx+leid];
    EntityHandle curfid = fid_from_halfacet(hf, ftype);
    int curlid = lid_from_halffacet(hf);
    
    while ((curfid != fid)&&(curfid != 0)){//Should not go into the loop when no sibling exists
        adj_ents.push_back(curfid);

        if (local_id)
          adj_leids->push_back(curlid);

        if (orient)
          {
            //get connectivity and match their directions
            const EntityHandle* conn;
            error = mb->get_connectivity(curfid, conn, nepf, true);MB_CHK_ERR(error);

            int nidx = lConnMap2D[ftype-2].next[curlid];

            if ((fedge[0] == conn[curlid])&&(fedge[1] == conn[nidx]))
              adj_orients->push_back(1);
            else if ((fedge[1] == conn[curlid])&&(fedge[0] == conn[nidx]))
              adj_orients->push_back(0);
          }

        int cidx = ID_FROM_HANDLE(curfid)-1;
        hf = sibhes[nepf*cidx+curlid];
        curfid = fid_from_halfacet(hf, ftype);
        curlid = lid_from_halffacet(hf);
    }

    return MB_SUCCESS;
  }

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
   ErrorCode HalfFacetRep::get_up_adjacencies_2d(EntityHandle fid,
                                                 int lid,
                                                 int *qsize,
                                                 int *count
                                                )
  {
    EntityType ftype = mb->type_from_handle(fid);
    int nepf = lConnMap2D[ftype-2].num_verts_in_face;

    int fidx = ID_FROM_HANDLE(fid)-1;
    HFacet hf = sibhes[nepf*fidx+lid];
    EntityHandle curfid = fid_from_halfacet(hf, ftype);
    int curlid = lid_from_halffacet(hf);

    if (curfid == 0){
        int index = 0;
        bool found_ent = find_match_in_array(fid, queue_fid, qsize[0]-1, true, &index);

        if ((!found_ent)||((found_ent) && (queue_lid[index] != lid)))
          {
            queue_fid[qsize[0]] = fid;
            queue_lid[qsize[0]] = lid;
            qsize[0] += 1;
          }
      }

    while ((curfid != fid)&&(curfid != 0)) {
        bool val = find_match_in_array(curfid, trackfaces, count[0]);

        if (!val){
            queue_fid[qsize[0]] = curfid;
            queue_lid[qsize[0]] = curlid;
            qsize[0] += 1;
          }

        int cidx = ID_FROM_HANDLE(curfid)-1;
        hf = sibhes[nepf*cidx+curlid];
        curfid = fid_from_halfacet(hf, ftype);
        curlid = lid_from_halffacet(hf);
      }

    return MB_SUCCESS;
   }

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  bool HalfFacetRep::find_matching_halfedge( EntityHandle eid,
                                             EntityHandle *hefid,
                                             int *helid){
    ErrorCode error;
    EntityType ftype = mb->type_from_handle(*_faces.begin());

    const EntityHandle* conn;
    int num_conn = 0;
    error = mb->get_connectivity(eid, conn, num_conn, true);MB_CHK_ERR(error);

    EntityHandle vid = conn[0];
    int vidx = ID_FROM_HANDLE(conn[0])-1;
    HFacet hf = v2he[vidx];

    if (hf == 0)
      {
        vidx = ID_FROM_HANDLE(conn[1])-1;
        hf = v2he[vidx];

        if (hf == 0)//The edge is either a dangling edge or attached to two non-manifold vertices
          return MB_SUCCESS;

        vid = conn[1];
      }

    EntityHandle fid = fid_from_halfacet(hf, ftype);
    int lid = lid_from_halffacet(hf);

    bool found = false;
    int qsize = 0, count = -1;

    error = gather_halfedges(vid, fid, lid, &qsize, &count);MB_CHK_ERR(error);

    found =  collect_and_compare(vid, conn, &qsize, &count, hefid, helid);MB_CHK_ERR(error);

    //Change the visited faces to false
    for (int i = 0; i<qsize; i++)
      {
        queue_fid[i] = 0;
        queue_lid[i] = 0;
      }

    for (int i = 0; i<= count; i++)
      trackfaces[i] = 0;

    return found;
  }
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
   ErrorCode HalfFacetRep::gather_halfedges( EntityHandle vid,
                                             EntityHandle he_fid,
                                             int he_lid,
                                             int *qsize,
                                             int *count
                                            )
  {  
    ErrorCode error;
    EntityHandle he2_fid = 0; int he2_lid = 0;

    error = another_halfedge(vid, he_fid, he_lid, &he2_fid, &he2_lid);MB_CHK_ERR(error);

    queue_fid[*qsize] = he_fid;
    queue_lid[*qsize] = he_lid;
    *qsize += 1;

    queue_fid[*qsize] = he2_fid;
    queue_lid[*qsize] = he2_lid;
    *qsize += 1;

    *count += 1;
    trackfaces[*count] = he_fid;

    error = get_up_adjacencies_2d(he_fid, he_lid, qsize, count);MB_CHK_ERR(error);
    error = get_up_adjacencies_2d(he2_fid, he2_lid, qsize, count);MB_CHK_ERR(error);

    return MB_SUCCESS;
  }

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  ErrorCode HalfFacetRep::another_halfedge( EntityHandle vid,
                                            EntityHandle he_fid,
                                            int he_lid,
                                            EntityHandle *he2_fid,
                                            int *he2_lid)
  {    
    ErrorCode error;
    EntityType ftype = mb->type_from_handle(he_fid);
    int nepf = lConnMap2D[ftype-2].num_verts_in_face;

    const EntityHandle* conn;
    error = mb->get_connectivity(he_fid, conn, nepf, true);MB_CHK_ERR(error);

    *he2_fid = he_fid;
    if (conn[he_lid] == vid)
      *he2_lid = lConnMap2D[ftype-2].prev[he_lid];
    else
      *he2_lid = lConnMap2D[ftype-2].next[he_lid];

    return MB_SUCCESS;
  }
  
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  bool HalfFacetRep::collect_and_compare(const EntityHandle vid, const EntityHandle *edg_vert,
                                         int *qsize,
                                         int *count,
                                         EntityHandle *he_fid,
                                         int *he_lid)
  {
    ErrorCode error;
    EntityType ftype = mb->type_from_handle(*_faces.begin());
    int nepf = lConnMap2D[ftype-2].num_verts_in_face;

    bool found = false;
    int num_qvals = 0, counter = 0;

    while (num_qvals < *qsize && counter <MAXSIZE )
      {
        EntityHandle curfid = queue_fid[num_qvals];
        int curlid = queue_lid[num_qvals];
        num_qvals += 1;       

        const EntityHandle* conn;
        error = mb->get_connectivity(curfid, conn, nepf, true);MB_CHK_ERR(error);

        int id = lConnMap2D[ftype-2].next[curlid];
        if (((conn[curlid]==edg_vert[0])&&(conn[id]==edg_vert[1]))||((conn[curlid]==edg_vert[1])&&(conn[id]==edg_vert[0]))){
            *he_fid = curfid;
            *he_lid = curlid;
            found = true;
            break;
        }

        bool val = find_match_in_array(curfid, trackfaces, count[0]);

        if (val)
            continue;

        count[0] += 1;
        trackfaces[*count] = curfid;

        EntityHandle he2_fid; int he2_lid;
        error = another_halfedge(vid, curfid, curlid, &he2_fid, &he2_lid);MB_CHK_ERR(error);
        error = get_up_adjacencies_2d(he2_fid, he2_lid, qsize, count);MB_CHK_ERR(error);

        counter += 1;
    }
    return found;
  }
  
  ///////////////////////////////////////////////////////////////////////////////////////////////////
  ErrorCode  HalfFacetRep::get_neighbor_adjacencies_2d( EntityHandle fid,
                                                        std::vector<EntityHandle> &adjents)
  {
    ErrorCode error; 

    if (fid != 0){
      EntityType ftype = mb->type_from_handle(fid);
      int nepf = lConnMap2D[ftype-2].num_verts_in_face;

      for (int lid = 0; lid < nepf; ++lid){
        error = get_up_adjacencies_2d(fid, lid, false, adjents);MB_CHK_ERR(error);
      }
    }
    
    return MB_SUCCESS;
  }


  /////////////////////////////////////////////////////////////////////////////////////////////////
  ErrorCode HalfFacetRep::get_down_adjacencies_2d(EntityHandle fid, std::vector<EntityHandle> &adjents)
  {
      //Returns explicit edges, if any, of the face
      ErrorCode error;
      adjents.reserve(10);
      EntityType ftype = mb->type_from_handle(fid);
      int nepf = lConnMap2D[ftype-2].num_verts_in_face;

      const EntityHandle* conn;
      error = mb->get_connectivity(fid, conn, nepf, true);MB_CHK_ERR(error);

      std::vector<EntityHandle> temp;

      //Loop over 2 vertices
      for (int i=0; i<2; i++)
        {
          //Choose the two adjacent vertices for a triangle, and two opposite vertices for a quad
          int l;
          if (ftype == MBTRI)
            l=i;
          else
            l = 2*i;

          //Get the current, next and prev vertices
          int nidx = lConnMap2D[ftype-2].next[l];
          int pidx = lConnMap2D[ftype-2].prev[l];
          EntityHandle v = conn[l];
          EntityHandle vnext = conn[nidx];
          EntityHandle vprev = conn[pidx];

          //Get incident edges on v
          error = get_up_adjacencies_1d(v, temp);MB_CHK_ERR(error);

          //Loop over the incident edges and check if its end vertices match those in the face
          for (int k=0; k<(int)temp.size(); k++)
            {
              const EntityHandle* econn;
              int num_conn = 0;
              error = mb->get_connectivity(temp[k],econn, num_conn, true);MB_CHK_ERR(error);

              if ((econn[0] == v && econn[1] == vnext)||(econn[0] == v && econn[1] == vprev)||(econn[0] == vnext && econn[1] == v)||(econn[0] == vprev && econn[1] == v))
                {
                  bool found = false;
                  for (int j=0; j<(int)adjents.size(); j++)
                    {
                      if (adjents[j] == temp[k])
                        {
                          found = true;
                          break;
                        }
                    }
                  if (!found)
                    adjents.push_back(temp[k]);
                }

            }
        }

      return MB_SUCCESS;
  }

  ////////////////////////////////////////////////////////////////////////////////////////////////
  int HalfFacetRep::find_total_edges_2d(Range &faces)
  {
    ErrorCode error;
    EntityType ftype = mb->type_from_handle(*faces.begin());
    int nepf = lConnMap2D[ftype-2].num_verts_in_face;
    int nfaces = faces.size();

    int total_edges = nepf*nfaces;
    
    std::vector<int> trackF(total_edges,0);
    std::vector<EntityHandle> adj_fids;
    std::vector<int> adj_lids;

    for (Range::iterator f = faces.begin(); f != faces.end(); ++f){
      for (int l = 0; l < nepf; l++){

	adj_fids.clear();
	adj_lids.clear();

	int id = nepf*(faces.index(*f))+l;
	if (!trackF[id])
	  {
	    error = get_up_adjacencies_2d(*f, l, false, adj_fids, &adj_lids);MB_CHK_ERR(error);

	    total_edges -= adj_fids.size();

	    for (int i = 0; i < (int)adj_fids.size(); i++)
	      trackF[nepf*(faces.index(adj_fids[i]))+adj_lids[i]] = 1;
	  };
      };
   };

    return total_edges;
  }

  ErrorCode HalfFacetRep::get_face_edges(EntityHandle fid, std::vector<EntityHandle> &edges)
  {
    ErrorCode error;
    edges.clear();

    EntityType ftype = mb->type_from_handle(fid);
    int nepf = lConnMap2D[ftype-2].num_verts_in_face;

    std::vector<EntityHandle> conn;
    error = mb->get_connectivity(&fid, 1, conn);MB_CHK_ERR(error);

    for (int i=0; i<nepf; i++)
      {
        EntityHandle v0 = conn[i];
        EntityHandle v1 = conn[lConnMap2D[ftype-2].next[i]];

        std::vector<EntityHandle> e0,e1, ecom;
        error = get_up_adjacencies_1d(v0, e0);MB_CHK_ERR(error);
        error = get_up_adjacencies_1d(v1, e1);MB_CHK_ERR(error);

        std::sort(e0.begin(), e0.end());
        std::sort(e1.begin(), e1.end());
        std::set_intersection(e0.begin(), e0.end(), e1.begin(), e1.end(),std::back_inserter(ecom));assert(ecom.size() == 1 || ecom.size() == 0);
        if (ecom.size() == 0)
          edges.push_back(0);
        else
          edges.push_back(ecom[0]);
      }

    return MB_SUCCESS;
  }


  /*******************************************************
  * 3D: sibhfs, v2hf, incident and neighborhood queries  *
  ********************************************************/

  int HalfFacetRep::get_index_in_lmap(EntityHandle cid)
  {
    EntityType type = mb->type_from_handle(cid);
    int index = cell_index.find(type)->second;
    return index;   
  }

   const HalfFacetRep::LocalMaps3D HalfFacetRep::lConnMap3D[4] =
    {
      // Tet
      {4, 6, 4, {3,3,3,3}, {{0,1,3},{1,2,3},{2,0,3},{0,2,1}},   {3,3,3,3},   {{0,2,3},{0,1,3},{1,2,3},{0,1,2}},   {{0,1},{1,2},{2,0},{0,3},{1,3},{2,3}},   {{3,0},{3,1},{3,2},{0,2},{0,1},{1,2}},   {{0,4,3},{1,5,4},{2,3,5},{2,1,0}},     {{-1,0,2,3},{0,-1,1,4},{2,1,-1,5},{3,4,5,-1}}, {3,0,1,2}, {0,1}, {{3,1,2,3},{2,2,3},{1,3}}},

      // Pyramid: Note: In MOAB pyramid follows the CGNS convention. Look up src/MBCNArrays.hpp
      {5, 8, 5, {4,3,3,3,3}, {{0,3,2,1},{0,1,4},{1,2,4},{2,3,4},{3,0,4}},  {3,3,3,3,4},   {{0,1,4},{0,1,2},{0,2,3},{0,3,4},{1,2,3,4}},   {{0,1},{1,2},{2,3},{3,0},{0,4},{1,4},{2,4},{3,4}},   {{0,1},{0,2},{0,3},{0,4},{1,4},{1,2},{2,3},{3,4}},    {{3,2,1,0},{0,5,4},{1,6,5},{2,7,6},{3,4,7}},    {{-1,0,-1,3,4},{0,-1,1,-1,5},{-1,1,-1,2,6},{3,-1,2,-1,7},{4,5,6,7,-1}}, {3,4,2,0}, {0,4}, {{4,0,1,2,3},{2,1,3},{2,1,3}}},

      // Prism
      {6, 9, 5, {4,4,4,3,3}, {{0,1,4,3},{1,2,5,4},{0,3,5,2},{0,2,1},{3,4,5}},  {3,3,3,3,3,3}, {{0,2,3},{0,1,3},{1,2,3},{0,2,4},{0,1,4},{1,4,2}},    {{0,1},{1,2},{2,0},{0,3},{1,4},{2,5},{3,4},{4,5},{5,3}},    {{0,3},{1,3},{2,3},{0,2},{0,1},{1,2},{0,4},{1,4},{2,4}},     {{0,4,6,3},{1,5,7,4},{2,3,8,5},{2,1,0},{6,7,8}},    {{-1,0,2,3,-1,-1},{0,-1,1,-1,4,-1},{2,1,-1,-1,-1,5},{3,-1,-1,-1,6,8},{-1,4,-1,6,-1,7},{-1,-1,5,8,7,-1}}, {4,0,5,4,1}, {0,5}, {{3,1,2,3},{3,2,4,3},{2,3,1},{1,2}}},

      // Hex
      {8, 12, 6, {4,4,4,4,4,4}, {{0,1,5,4},{1,2,6,5},{2,3,7,6},{3,0,4,7},{0,3,2,1},{4,5,6,7}},   {3,3,3,3,3,3,3,3},   {{0,3,4},{0,1,4},{1,2,4},{2,3,4},{0,3,5},{0,1,5},{1,2,5},{2,3,5}},    {{0,1},{1,2},{2,3},{3,0},{0,4},{1,5},{2,6},{3,7},{4,5},{5,6},{6,7},{7,4}},     {{0,4},{1,4},{2,4},{3,4},{0,3},{0,1},{1,2},{2,3},{0,5},{1,5},{2,5},{3,5}},     {{0,5,8,4},{1,6,9,5},{2,7,10,6},{3,4,11,7},{3,2,1,0},{8,9,10,11}},     {{-1,0,-1,3,4,-1,-1,-1},{0,-1,1,-1,-1,5,-1,-1},{-1,1,-1,2,-1,-1,6,-1},{3,-1,2,-1,-1,-1,-1,7},{4,-1,-1,-1,-1,8,-1,11},{-1,5,-1,-1,8,-1,9,-1},{-1,-1,6,-1,-1,9,-1,10},{-1,-1,-1,7,11,-1,10,-1}}, {4,0,2,5,7}, {0,6}, {{3,1,3,4},{3,1,3,6},{3,1,4,6},{3,3,6,4}}}
    };

  
  //////////////////////////////////////////////////////////////////////////////////
   ErrorCode HalfFacetRep::determine_sibling_halffaces( Range &cells)
  {
    ErrorCode error;
    EntityHandle start_cell = *cells.begin();
    EntityType ctype = mb->type_from_handle(start_cell);
    int index = get_index_in_lmap(start_cell);
    int nvpc = lConnMap3D[index].num_verts_in_cell;
    int nfpc = lConnMap3D[index].num_faces_in_cell;

    //Step 1: Create an index list storing the starting position for each vertex
    int nv = _verts.size();
    int *is_index = new int[nv+1];
    for (int i =0; i<nv+1; i++)
      is_index[i] = 0;

    int vindex;

    for (Range::iterator cid = cells.begin(); cid != cells.end(); ++cid)
       {
        const EntityHandle* conn;
        error = mb->get_connectivity(*cid, conn, nvpc, true);MB_CHK_ERR(error);

        for (int i = 0; i<nfpc; ++i)
          {
            int nvF = lConnMap3D[index].hf2v_num[i];
            EntityHandle v = 0;
            for (int k = 0; k<nvF; k++)
              {
                int id = lConnMap3D[index].hf2v[i][k];
                if (v <= conn[id])
                  v = conn[id];
              }
            vindex = _verts.index(v);
            is_index[vindex+1] += 1;
          }
      }
     is_index[0] = 0;

     for (int i=0; i<nv; i++)
       is_index[i+1] = is_index[i] + is_index[i+1];

     //Step 2: Define four arrays v2hv_eid, v2hv_lvid storing every half-facet on a vertex
     EntityHandle * v2oe_v1 = new EntityHandle[is_index[nv]];
     EntityHandle * v2oe_v2 = new EntityHandle[is_index[nv]];
     EntityHandle * v2hf_map_cid = new EntityHandle[is_index[nv]];
     int * v2hf_map_lfid = new int[is_index[nv]];

     for (Range::iterator cid = cells.begin(); cid != cells.end(); ++cid)
       {
         const EntityHandle* conn;
         error = mb->get_connectivity(*cid, conn, nvpc, true);MB_CHK_ERR(error);

         for (int i = 0; i< nfpc; i++)
           {
             int nvF = lConnMap3D[index].hf2v_num[i];
             std::vector<EntityHandle> vs(nvF);
             EntityHandle vmax = 0;
             int lv = -1;
             for (int k = 0; k<nvF; k++)
               {
                 int id = lConnMap3D[index].hf2v[i][k];
                 vs[k] = conn[id];
                 if (vmax <= conn[id])
                   {
                     vmax = conn[id];
                     lv = k;
                   }
               }

             int nidx = lConnMap2D[nvF-3].next[lv];
             int pidx = lConnMap2D[nvF-3].prev[lv];

             int v = _verts.index(vmax);
             v2oe_v1[is_index[v]] = vs[nidx];
             v2oe_v2[is_index[v]] = vs[pidx];
             v2hf_map_cid[is_index[v]] = *cid;
             v2hf_map_lfid[is_index[v]] = i;
             is_index[v] += 1;

           }
       }

     for (int i=nv-2; i>=0; i--)
       is_index[i+1] = is_index[i];
     is_index[0] = 0;

     //Step 3: Fill up sibling half-verts map
     for (Range::iterator cid = cells.begin(); cid != cells.end(); ++cid)
       {
         const EntityHandle* conn;
         error = mb->get_connectivity(*cid, conn, nvpc, true);MB_CHK_ERR(error);

         int cidx = ID_FROM_HANDLE(*cid)-1;
         for (int i =0; i<nfpc; i++)
           {
             HFacet hf = sibhfs[nfpc*cidx+i];
             EntityHandle sibcid = fid_from_halfacet(hf, ctype);

             if (sibcid != 0)
               continue;


             int nvF = lConnMap3D[index].hf2v_num[i];
             std::vector<EntityHandle> vs(nvF);
             EntityHandle vmax = 0;
             int lv = -1;
             for (int k = 0; k<nvF; k++)
               {
                 int id = lConnMap3D[index].hf2v[i][k];
                 vs[k] = conn[id];
                 if (vmax <= conn[id])
                   {
                     vmax = conn[id];
                     lv = k;
                   }
               }

             int nidx = lConnMap2D[nvF-3].next[lv];
             int pidx = lConnMap2D[nvF-3].prev[lv];

             int v = _verts.index(vmax);
             EntityHandle v1 = vs[pidx];
             EntityHandle v2 = vs[nidx];

             for (int ind = is_index[v]; ind <= is_index[v+1]-1; ind++)
               {
                 if ((v2oe_v1[ind] == v1)&&(v2oe_v2[ind] == v2))
                   {
                     // Map to opposite hf
                     EntityHandle cur_cid = v2hf_map_cid[ind];
                     int cur_lfid = v2hf_map_lfid[ind];

                     sibhfs[nfpc*cidx+i] = create_halffacet(cur_cid, cur_lfid);

                     int scidx = ID_FROM_HANDLE(cur_cid)-1;
                     sibhfs[nfpc*scidx+cur_lfid] = create_halffacet(*cid, i);
                   }
               }
           }
       }

     delete [] is_index;
     delete [] v2oe_v1;
     delete [] v2oe_v2;
     delete [] v2hf_map_cid;
     delete [] v2hf_map_lfid;

     return MB_SUCCESS;

  }


  ErrorCode HalfFacetRep::determine_incident_halffaces( Range &cells)
  {
    ErrorCode error;
    int index = get_index_in_lmap(*cells.begin());
    int nvpc = lConnMap3D[index].num_verts_in_cell;

    std::multimap<EntityHandle, EntityHandle> comps;
    HFacet nwhf;

    for (Range::iterator cid = cells.begin(); cid != cells.end(); ++cid){
      EntityHandle cell = *cid;
      const EntityHandle* conn;
      error = mb->get_connectivity(*cid, conn, nvpc, true);MB_CHK_ERR(error);

      for(int i=0; i<nvpc; ++i){
          EntityHandle v = conn[i];
          int vidx = ID_FROM_HANDLE(v)-1;
          HFacet hf  = v2hf[vidx];

          bool found = find_cell_in_component(v, cell, comps);

          if (hf==0 && !found && (v2hfs.empty() || (v2hfs.find(v) == v2hfs.end())))
            {
              nwhf = 0;
              error = add_cells_of_single_component(v, cell, lConnMap3D[index].v2hf[i][0], comps, nwhf);MB_CHK_ERR(error);

              v2hf[vidx] = nwhf;
            }
          else if (hf != 0 && !found)
            {
              nwhf = 0;
              error = add_cells_of_single_component(v, cell, lConnMap3D[index].v2hf[i][0], comps, nwhf);MB_CHK_ERR(error);

              v2hfs.insert(std::pair<EntityHandle, HFacet>(v, hf));
              v2hfs.insert(std::pair<EntityHandle, HFacet>(v, nwhf));
              v2hf[vidx] = 0;
            }
          else if (hf == 0 &&  !found && (!v2hfs.empty()) && (v2hfs.find(v) != v2hfs.end()))
            {
              nwhf = 0;
              error = add_cells_of_single_component(v, cell, lConnMap3D[index].v2hf[i][0], comps, nwhf);MB_CHK_ERR(error);
               v2hfs.insert(std::pair<EntityHandle, HFacet>(v, nwhf));
            }
	}
      }

   // error = print_tags(3);
    
    return MB_SUCCESS;
  }

  ErrorCode  HalfFacetRep::determine_border_vertices( Range &cells, Tag isborder)
  {
    ErrorCode error;
    EntityHandle start_cell = *cells.begin();
    EntityType ctype = mb->type_from_handle(*cells.begin());
    int index = get_index_in_lmap(start_cell);
    int nvpc = lConnMap3D[index].num_verts_in_cell;
    int nfpc = lConnMap3D[index].num_faces_in_cell;

    int val= 1;

    for(Range::iterator t= cells.begin(); t !=cells.end(); ++t){

        const EntityHandle* conn;
        error = mb->get_connectivity(*t, conn, nvpc, true);MB_CHK_ERR(error);

        int cidx = ID_FROM_HANDLE(*t)-1;
        for (int i = 0; i < nfpc; ++i){
            HFacet hf = sibhfs[nfpc*cidx+i];
            EntityHandle sib_cid = fid_from_halfacet(hf, ctype);

            if (sib_cid ==0){
                int nvF = lConnMap3D[index].hf2v_num[i];

                for (int j = 0; j < nvF; ++j){
                    int ind = lConnMap3D[index].hf2v[i][j];
                    error = mb->tag_set_data(isborder, &conn[ind], 1, &val); MB_CHK_ERR(error);
                  }
              }
          }
      }

    return MB_SUCCESS;
  }

  /////////////////////////////////////////////////////////////////////////
  ErrorCode HalfFacetRep::add_cells_of_single_component(EntityHandle vid, EntityHandle curcid, int curlid, std::multimap<EntityHandle, EntityHandle> &comps, HFacet &hf)
  {
    ErrorCode error;
    EntityType ctype = mb->type_from_handle(curcid);
    int index = get_index_in_lmap(curcid);
    int nvpc = lConnMap3D[index].num_verts_in_cell;
    int nfpc = lConnMap3D[index].num_faces_in_cell;

    int Stksize = 0, count = -1;
    Stkcells[0] = curcid;

    hf = create_halffacet(curcid, curlid);

    EntityHandle cur_cid;
    while (Stksize >= 0 ){
        cur_cid = Stkcells[Stksize];
        Stksize -= 1 ;

        bool found = find_match_in_array(cur_cid, trackcells, count);
        if (!found){
            count += 1;
            trackcells[count] = cur_cid;

            // Add the current cell
            comps.insert(std::pair<EntityHandle,EntityHandle>(vid, cur_cid));
          }

        // Connectivity of the cell
        const EntityHandle* conn;
        error = mb->get_connectivity(cur_cid, conn, nvpc, true);MB_CHK_ERR(error);

        // Local id of vid in the cell and the half-faces incident on it
        int lv = -1;
        for (int i = 0; i< nvpc; ++i){
            if (conn[i] == vid)
              {
                lv = i;
                break;
              }
          };

        int nhf_thisv = lConnMap3D[index].v2hf_num[lv];
        int cidx = ID_FROM_HANDLE(cur_cid)-1;

        // Add new cells into the stack
        EntityHandle ngb; HFacet hf_ngb;
        for (int i = 0; i < nhf_thisv; ++i){
            int ind = lConnMap3D[index].v2hf[lv][i];
            hf_ngb = sibhfs[nfpc*cidx+ind];
            ngb = fid_from_halfacet(hf_ngb, ctype);

            if (ngb) {
                bool found_ent = find_match_in_array(ngb, trackcells, count);

                if (!found_ent){
                    Stksize += 1;
                    Stkcells[Stksize] = ngb;
                  }
              }
            else
              hf = create_halffacet(cur_cid, ind);
          }
      }


    //Change the visited faces to false
    for (int i = 0; i<Stksize; i++)
      Stkcells[i] = 0;

    for (int i = 0; i <= count; i++)
      trackcells[i] = 0;

    return MB_SUCCESS;
  }


  bool HalfFacetRep::find_cell_in_component(EntityHandle vid, EntityHandle cell, std::multimap<EntityHandle, EntityHandle> &comps)
  {
    bool found = false;

    if (comps.empty())
      return found;

   std::pair <std::multimap<EntityHandle, EntityHandle>::iterator, std::multimap<EntityHandle, EntityHandle>::iterator> rit;

   rit = comps.equal_range(vid);

   for (std::multimap<EntityHandle, EntityHandle>::iterator it = rit.first; it != rit.second; ++it)
    {
       if (it->second == cell)
         {
           found = true;
           break;
         }
     }

    return found;
  }

  ////////////////////////////////////////////////////////////////////////
  ErrorCode HalfFacetRep::get_up_adjacencies_vert_3d(EntityHandle vid, std::vector<EntityHandle> &adjents)
  {
    ErrorCode error;
    adjents.reserve(20);
    EntityType ctype = mb->type_from_handle(*_cells.begin());

    // Obtain a half-face/s incident on v
    int vidx = ID_FROM_HANDLE(vid)-1;
    HFacet hf = v2hf[vidx];

    std::vector<EntityHandle> start_cells;
    if (hf == 0 && (v2hfs.find(vid) != v2hfs.end())) //Vertex is non-manifold
      {
        std::pair <std::multimap<EntityHandle, HFacet>::iterator, std::multimap<EntityHandle, HFacet>::iterator> it_hes;
        it_hes = v2hfs.equal_range(vid);

        for (std::multimap<EntityHandle, HFacet>::iterator it = it_hes.first; it != it_hes.second; ++it)
          {
            start_cells.push_back(fid_from_halfacet(it->second, ctype));
          }
      }
    else if (hf != 0)
      start_cells.push_back( fid_from_halfacet(hf, ctype));

    if (start_cells.empty())
      return MB_SUCCESS;

    int index = get_index_in_lmap(start_cells[0]);
    int nvpc = lConnMap3D[index].num_verts_in_cell;
    int nfpc = lConnMap3D[index].num_faces_in_cell;

    for (int i=0; i<(int)start_cells.size(); i++)
      cellq[i] = start_cells[i];

    int qsize = start_cells.size();
    EntityHandle cur_cid;
    int num_qvals = 0;

    while (num_qvals < qsize){
        cur_cid = cellq[num_qvals];
        num_qvals += 1 ;

        //Add the current cell to output adj vector
        adjents.push_back(cur_cid);

        // Connectivity of the cell
        const EntityHandle* conn;
        error = mb->get_connectivity(cur_cid, conn, nvpc, true);MB_CHK_ERR(error);

        // Local id of vid in the cell and the half-faces incident on it
        int lv = -1;
        for (int i = 0; i< nvpc; ++i){
            if (conn[i] == vid)
              {
                lv = i;
                break;
              }
          };

        //Number of local half-faces incident on the current vertex
        int nhf_thisv = lConnMap3D[index].v2hf_num[lv];
        int cidx = ID_FROM_HANDLE(cur_cid)-1;

        // Add new cells into the stack
        EntityHandle ngb;
        for (int i = 0; i < nhf_thisv; ++i){
            int ind = lConnMap3D[index].v2hf[lv][i];
            hf = sibhfs[nfpc*cidx+ind];
            ngb = fid_from_halfacet(hf, ctype);

            if (ngb) {
                bool found_ent = find_match_in_array(ngb, cellq, qsize-1);

                if (!found_ent){
                    cellq[qsize] = ngb;
                    qsize += 1;
                  }
              }
          }
      }


    //Change the visited faces to false
    for (int i = 0; i<qsize; i++)
      cellq[i] = 0;

    return MB_SUCCESS;

  }

ErrorCode HalfFacetRep::get_up_adjacencies_edg_3d( EntityHandle eid,
                                                    std::vector<EntityHandle> &adjents,
                                                    std::vector<int> * leids)
{
  ErrorCode error;
  EntityType ctype = mb->type_from_handle(*_cells.begin());
  EntityHandle start_cell = *_cells.begin();
  int index = get_index_in_lmap(start_cell);
  int nvpc = lConnMap3D[index].num_verts_in_cell;
  int nfpc = lConnMap3D[index].num_faces_in_cell;

   adjents.reserve(20);
   if (leids != NULL)
     leids->reserve(20);

  // Find the edge vertices
  const EntityHandle* econn;
  int num_conn = 0;
  error = mb->get_connectivity(eid, econn, num_conn, true);MB_CHK_ERR(error);

  EntityHandle v_start = econn[0], v_end = econn[1];
  int v1idx = ID_FROM_HANDLE(v_start)-1;
  int v2idx = ID_FROM_HANDLE(v_end)-1;

  // Find an half-facets incident to each end vertex of the edge
  std::vector<EntityHandle> start_cells;
  HFacet hf1 = v2hf[v1idx];
  HFacet hf2 = v2hf[v2idx];

  if (hf1 == 0 && !v2hfs.empty())
    {
      std::pair <std::multimap<EntityHandle, HFacet>::iterator, std::multimap<EntityHandle, HFacet>::iterator> it_hes;
      it_hes = v2hfs.equal_range(v_start);

      for (std::multimap<EntityHandle, HFacet>::iterator it = it_hes.first; it != it_hes.second; ++it)
        {
          start_cells.push_back(fid_from_halfacet(it->second, ctype));
        }
    }
  else if (hf1 != 0)
    {
      start_cells.push_back( fid_from_halfacet(hf1, ctype));
    }

  if (hf2 == 0 && !v2hfs.empty())
    {
      std::pair <std::multimap<EntityHandle, HFacet>::iterator, std::multimap<EntityHandle, HFacet>::iterator> it_hes;
      it_hes = v2hfs.equal_range(v_end);

      for (std::multimap<EntityHandle, HFacet>::iterator it = it_hes.first; it != it_hes.second; ++it)
        {
          start_cells.push_back(fid_from_halfacet(it->second, ctype));
        }
    }
  else if (hf2 != 0)
    {
      start_cells.push_back( fid_from_halfacet(hf2, ctype));
    }

  if (start_cells.empty())
    return MB_SUCCESS;

  std::sort(start_cells.begin(), start_cells.end());
  std::vector<EntityHandle>::iterator last = std::unique(start_cells.begin(), start_cells.end());
  start_cells.erase(last, start_cells.end());

  for (int i=0; i<(int)start_cells.size(); i++)
    cellq[i] = start_cells[i];

  int qsize = start_cells.size();
  int num_qvals = 0;

  while (num_qvals < qsize){
      EntityHandle cell_id = cellq[num_qvals];
      num_qvals += 1;

      const EntityHandle* conn;
      error = mb->get_connectivity(cell_id, conn, nvpc, true);MB_CHK_ERR(error);

      int lv0 = -1, lv1 = -1, lv = -1;

      //locate v_origin in poped out tet, check if v_end is in
      for (int i = 0; i<nvpc; i++){
          if (v_start == conn[i]){
              lv0 = i;
              lv = lv0;
            }
          else if (v_end == conn[i]){
              lv1 = i;
              lv = lv1;
            }
        }

      if ((lv0 >= 0) && (lv1 >= 0))
        {
          adjents.push_back(cell_id);
          if (leids != NULL)
            leids->push_back( lConnMap3D[index].lookup_leids[lv0][lv1]);
        }

      //push back new found unchecked incident tets of v_start
      int cidx = ID_FROM_HANDLE(cell_id)-1;
      int nhf_thisv = lConnMap3D[index].v2hf_num[lv];

      for (int i = 0; i < nhf_thisv; i++){
          int ind = lConnMap3D[index].v2hf[lv][i];
          HFacet hf = sibhfs[nfpc*cidx+ind];
          EntityHandle ngb = fid_from_halfacet(hf, ctype);

          if (ngb){
              bool found_ent = find_match_in_array(ngb, &cellq[0], qsize-1);

              if (!found_ent)
                {
                  cellq[qsize] = ngb;
                  qsize += 1;
                }
            }
        }
    }

  for (int i = 0; i<qsize; i++)
      cellq[i] = 0;


  return MB_SUCCESS;
}


  ErrorCode HalfFacetRep::get_up_adjacencies_edg_3d( EntityHandle cid,
                                                     int leid,
                                                     std::vector<EntityHandle> &adjents,
                                                     std::vector<int> * leids,
                                                     std::vector<int> *adj_orients)
  {
    ErrorCode error;
    EntityType ctype = mb->type_from_handle(cid);
    int index = get_index_in_lmap(cid);
    int nvpc = lConnMap3D[index].num_verts_in_cell;
    int nfpc = lConnMap3D[index].num_faces_in_cell;
    adjents.clear();
    adjents.reserve(20);

    if (leids != NULL)
      {
        leids->clear();
        leids->reserve(20);
      }
    if (adj_orients != NULL)
      {
        adj_orients->clear();
        adj_orients->reserve(20);
      }

    const EntityHandle* econn;
    error = mb->get_connectivity(cid, econn, nvpc, true);MB_CHK_ERR(error);

    // Get the end vertices of the edge <cid,leid>
    int id = lConnMap3D[index].e2v[leid][0];
    EntityHandle v_start = econn[id];
    id = lConnMap3D[index].e2v[leid][1];
    EntityHandle v_end = econn[id];

    int v1idx = ID_FROM_HANDLE(v_start)-1;
    int v2idx = ID_FROM_HANDLE(v_end)-1;

    // Find an half-facets incident to each end vertex of the edge
    std::vector<EntityHandle> start_cells;
    HFacet hf1 = v2hf[v1idx];
    HFacet hf2 = v2hf[v2idx];

    if (hf1 == 0 && !v2hfs.empty())
      {
        std::pair <std::multimap<EntityHandle, HFacet>::iterator, std::multimap<EntityHandle, HFacet>::iterator> it_hes;
        it_hes = v2hfs.equal_range(v_start);

        for (std::multimap<EntityHandle, HFacet>::iterator it = it_hes.first; it != it_hes.second; ++it)
          {
            start_cells.push_back(fid_from_halfacet(it->second, ctype));
          }
      }
    else if (hf1 != 0)
      {
        start_cells.push_back( fid_from_halfacet(hf1, ctype));
      }

    if (hf2 == 0 && !v2hfs.empty())
      {
        std::pair <std::multimap<EntityHandle, HFacet>::iterator, std::multimap<EntityHandle, HFacet>::iterator> it_hes;
        it_hes = v2hfs.equal_range(v_end);

        for (std::multimap<EntityHandle, HFacet>::iterator it = it_hes.first; it != it_hes.second; ++it)
          {
            start_cells.push_back(fid_from_halfacet(it->second, ctype));
          }
      }
    else if (hf2 != 0)
      {
        start_cells.push_back( fid_from_halfacet(hf2, ctype));
      }

    if (start_cells.empty())
      return MB_SUCCESS;

    std::sort(start_cells.begin(), start_cells.end());
    std::vector<EntityHandle>::iterator last = std::unique(start_cells.begin(), start_cells.end());
    start_cells.erase(last, start_cells.end());

    for (int i=0; i<(int)start_cells.size(); i++)
      cellq[i] = start_cells[i];

    int qsize = start_cells.size();
    int num_qvals = 0;

    while (num_qvals < qsize){
        EntityHandle cell_id = cellq[num_qvals];
        num_qvals += 1;

        const EntityHandle* conn;
        error = mb->get_connectivity(cell_id, conn, nvpc, true);MB_CHK_ERR(error);

        int lv0 = -1, lv1 = -1, lv = -1;

        //locate v_origin in poped out tet, check if v_end is in
        for (int i = 0; i<nvpc; i++){
            if (v_start == conn[i]){
                lv0 = i;
                lv = lv0;
              }
            else if (v_end == conn[i]){
                lv1 = i;
                lv = lv1;
              }
          }

        if ((lv0 >= 0) && (lv1 >= 0))
          {
            adjents.push_back(cell_id);
            if (leids != NULL)
              leids->push_back( lConnMap3D[index].lookup_leids[lv0][lv1]);

            if (adj_orients != NULL)
              {
                int cur_leid = lConnMap3D[index].lookup_leids[lv0][lv1];
                int id1 =  lConnMap3D[index].e2v[cur_leid][0];
                int id2 =  lConnMap3D[index].e2v[cur_leid][1];
                if ((v_start == conn[id1]) && (v_end == conn[id2]))
                  adj_orients->push_back(1);
                else if ((v_start == conn[id2]) && (v_end== conn[id1]))
                  adj_orients->push_back(0);
              }
          }

        //push back new found unchecked incident tets of v_start
        int cidx = ID_FROM_HANDLE(cell_id)-1;
        int nhf_thisv = lConnMap3D[index].v2hf_num[lv];

        for (int i = 0; i < nhf_thisv; i++){
            int ind = lConnMap3D[index].v2hf[lv][i];
            HFacet hf = sibhfs[nfpc*cidx+ind];
            EntityHandle ngb = fid_from_halfacet(hf, ctype);

            if (ngb){
                bool found_ent = find_match_in_array(ngb, &cellq[0], qsize-1);

                if (!found_ent)
                  {
                    cellq[qsize] = ngb;
                    qsize += 1;
                  }
              }
          }
      }

    for (int i = 0; i<qsize; i++)
        cellq[i] = 0;

    return MB_SUCCESS;
  }
 
  ErrorCode HalfFacetRep::get_up_adjacencies_edg_3d_comp( EntityHandle cid,
                                                     int leid,
                                                     std::vector<EntityHandle> &adjents,
                                                     std::vector<int> *leids,
                                                     std::vector<int> *adj_orients)
  {
    ErrorCode error;
    EntityType ctype = mb->type_from_handle(cid);
    int index = get_index_in_lmap(cid);
    int nvpc = lConnMap3D[index].num_verts_in_cell;
    int nfpc = lConnMap3D[index].num_faces_in_cell;
    adjents.clear();
    adjents.reserve(20);

    if (leids != NULL)
      {
        leids->clear();
        leids->reserve(20);
      }
    if (adj_orients != NULL)
      {
        adj_orients->clear();
        adj_orients->reserve(20);
      }

    const EntityHandle* econn;
    error = mb->get_connectivity(cid, econn, nvpc);MB_CHK_ERR(error);

    // Get the end vertices of the edge <cid,leid>
    int id = lConnMap3D[index].e2v[leid][0];
    EntityHandle v_start = econn[id];
    id = lConnMap3D[index].e2v[leid][1];
    EntityHandle v_end = econn[id];

    int v1idx = ID_FROM_HANDLE(v_start)-1;
    int v2idx = ID_FROM_HANDLE(v_end)-1;

    // Find an half-facets incident to each end vertex of the edge
    std::vector<EntityHandle> start_cells;
    HFacet hf1 = v2hf[v1idx];
    HFacet hf2 = v2hf[v2idx];

    if ((hf1 == 0) && (v2hfs.find(v_start) != v2hfs.end()) && (hf2 == 0)  && (v2hfs.find(v_end) != v2hfs.end()))
      {
        std::pair <std::multimap<EntityHandle, HFacet>::iterator, std::multimap<EntityHandle, HFacet>::iterator> it_hes;
        it_hes = v2hfs.equal_range(v_start);

        for (std::multimap<EntityHandle, HFacet>::iterator it = it_hes.first; it != it_hes.second; ++it)
          {
            start_cells.push_back(fid_from_halfacet(it->second, ctype));
          }
      }
    else
      return MB_SUCCESS;

    if (start_cells.empty())
      return MB_SUCCESS;


   // std::sort(start_cells.begin(), start_cells.end());
  //  std::vector<EntityHandle>::iterator last = std::unique(start_cells.begin(), start_cells.end());
  //  start_cells.erase(last, start_cells.end());

    for (int c=0; c<(int)start_cells.size(); c++)
      {
        cellq[0] = start_cells[c];

        int qsize = 1;
        int num_qvals = 0;

        while (num_qvals < qsize){
            EntityHandle cell_id = cellq[num_qvals];
            num_qvals += 1;

            const EntityHandle* conn;
            error = mb->get_connectivity(cell_id, conn, nvpc);MB_CHK_ERR(error);

            int lv0 = -1, lv1 = -1, lv = -1;

            //locate v_origin in poped out tet, check if v_end is in
            for (int i = 0; i<nvpc; i++){
                if (v_start == conn[i]){
                    lv0 = i;
                    lv = lv0;
                  }
                else if (v_end == conn[i]){
                    lv1 = i;
                    lv = lv1;
                  }
              }

            if ((lv0 >= 0) && (lv1 >= 0))
              {
                adjents.push_back(cell_id);
                if (leids != NULL)
                  leids->push_back( lConnMap3D[index].lookup_leids[lv0][lv1]);

                if (adj_orients != NULL)
                  {
                    int cur_leid = lConnMap3D[index].lookup_leids[lv0][lv1];
                    int id1 =  lConnMap3D[index].e2v[cur_leid][0];
                    int id2 =  lConnMap3D[index].e2v[cur_leid][1];
                    if ((v_start == conn[id1]) && (v_end == conn[id2]))
                      adj_orients->push_back(1);
                    else if ((v_start == conn[id2]) && (v_end== conn[id1]))
                      adj_orients->push_back(0);
                  }

                for (int i = 0; i<qsize; i++)
                  cellq[i] = 0;

                break;
              }

            //push back new found unchecked incident tets of v_start
            int cidx = ID_FROM_HANDLE(cell_id)-1;
            int nhf_thisv = lConnMap3D[index].v2hf_num[lv];

            for (int i = 0; i < nhf_thisv; i++){
                int ind = lConnMap3D[index].v2hf[lv][i];
                HFacet hf = sibhfs[nfpc*cidx+ind];
                EntityHandle ngb = fid_from_halfacet(hf, ctype);

                if (ngb){
                    bool found_ent = find_match_in_array(ngb, &cellq[0], qsize-1);

                    if (!found_ent)
                      {
                        cellq[qsize] = ngb;
                        qsize += 1;
                      }
                  }
              }
          }

        for (int i = 0; i<qsize; i++)
          cellq[i] = 0;
      }

    return MB_SUCCESS;
  }



  ErrorCode  HalfFacetRep::get_up_adjacencies_face_3d( EntityHandle fid,
                                                       std::vector<EntityHandle> &adjents,
                                                       std::vector<int> * lfids)
  {
    ErrorCode error;

    EntityHandle cid = 0;
    int lid = 0;
    bool found = find_matching_halfface(fid, &cid, &lid);

    if (found){
      error = get_up_adjacencies_face_3d(cid, lid, adjents,  lfids); MB_CHK_ERR(error);
      }

    return MB_SUCCESS;
  }

  ErrorCode HalfFacetRep::get_up_adjacencies_face_3d( EntityHandle cid,
                                                      int lfid,
                                                      std::vector<EntityHandle> &adjents,
                                                      std::vector<int> * lfids)
  {

    EntityHandle start_cell = *_cells.begin();
    EntityType ctype = mb->type_from_handle(start_cell);
    int index = get_index_in_lmap(start_cell);
    int nfpc = lConnMap3D[index].num_faces_in_cell;

    adjents.reserve(4);
    adjents.push_back(cid);

    if (lfids != NULL)
      {
        lfids->reserve(4);
        lfids->push_back(lfid);
      }

    int cidx = ID_FROM_HANDLE(cid)-1;
    HFacet hf = sibhfs[nfpc*cidx+lfid];
    EntityHandle sibcid = fid_from_halfacet(hf, ctype);
    int siblid = lid_from_halffacet(hf);

    if (sibcid !=  0)
      {
        adjents.push_back(sibcid);
        if (lfids != NULL)
          lfids->push_back(siblid);
      }

    return MB_SUCCESS;
  }

 bool HalfFacetRep::find_matching_implicit_edge_in_cell( EntityHandle eid,
                                                         std::vector<EntityHandle> &cid,
                                                         std::vector<int> &leid)
  {
    ErrorCode error;
    EntityType ctype = mb->type_from_handle(*_cells.begin());
    EntityHandle start_cell = *_cells.begin();
    int index = get_index_in_lmap(start_cell);
    int nvpc = lConnMap3D[index].num_verts_in_cell;
    int nfpc = lConnMap3D[index].num_faces_in_cell;

    // Find the edge vertices
    const EntityHandle* econn;
    int num_conn = 0;
    error = mb->get_connectivity(eid, econn, num_conn, true);MB_CHK_ERR(error);

    EntityHandle v_start = econn[0], v_end = econn[1];
    int v1idx = ID_FROM_HANDLE(v_start)-1;
    int v2idx = ID_FROM_HANDLE(v_end)-1;

    // Find an incident cell to v_start
    std::vector<EntityHandle> start_cells;
    HFacet hf1 = v2hf[v1idx];
    HFacet hf2 = v2hf[v2idx];

    int ncomps1=0, ncomps2 = 0, ncomp;
    if (hf1 == 0 && !v2hfs.empty())
      {
        std::pair <std::multimap<EntityHandle, HFacet>::iterator, std::multimap<EntityHandle, HFacet>::iterator> it_hes;
        it_hes = v2hfs.equal_range(v_start);

        for (std::multimap<EntityHandle, HFacet>::iterator it = it_hes.first; it != it_hes.second; ++it)
          {
            start_cells.push_back(fid_from_halfacet(it->second, ctype));
            ncomps1 += 1;
          }
      }
    else if (hf1 != 0)
      {
        start_cells.push_back( fid_from_halfacet(hf1, ctype));
        ncomps1 += 1;
      }

    if (hf2 == 0 && !v2hfs.empty())
      {
        std::pair <std::multimap<EntityHandle, HFacet>::iterator, std::multimap<EntityHandle, HFacet>::iterator> it_hes;
        it_hes = v2hfs.equal_range(v_end);

        for (std::multimap<EntityHandle, HFacet>::iterator it = it_hes.first; it != it_hes.second; ++it)
          {
            start_cells.push_back(fid_from_halfacet(it->second, ctype));
            ncomps2 += 1;
          }
      }
    else if (hf2 != 0)
      {
        start_cells.push_back( fid_from_halfacet(hf2, ctype));
        ncomps2 += 1;
      }

    ncomp = std::min(ncomps1,ncomps2);

    bool found = false;
    if (start_cells.empty())
      return found;

    for (int i=0; i<(int)start_cells.size(); i++)
      cellq[i] = start_cells[i];

    int qsize = start_cells.size();
    int num_qvals = 0;

    while (num_qvals < qsize){
        EntityHandle cell_id = cellq[num_qvals];
        num_qvals += 1;

        const EntityHandle* conn;
        error = mb->get_connectivity(cell_id, conn, nvpc, true);MB_CHK_ERR(error);

        int lv0 = -1, lv1 = -1, lv = -1;

        //locate v_origin in poped out tet, check if v_end is in
        for (int i = 0; i<nvpc; i++){
            if (v_start == conn[i]){
                lv0 = i;
                lv = lv0;
              }
            else if (v_end == conn[i]){
                lv1 = i;
                lv = lv1;
              }
          }

        if ((lv0 >= 0) && (lv1 >= 0))
          {
            found = true;
            cid.push_back(cell_id);
            leid.push_back(lConnMap3D[index].lookup_leids[lv0][lv1]);

            if ((int)cid.size() == ncomp)
              break;
          }

        //push back new found unchecked incident tets of v_start
        int cidx = ID_FROM_HANDLE(cell_id)-1;
        int nhf_thisv = lConnMap3D[index].v2hf_num[lv];

        for (int i = 0; i < nhf_thisv; i++){
            int ind = lConnMap3D[index].v2hf[lv][i];
            HFacet hf = sibhfs[nfpc*cidx+ind];
            EntityHandle ngb = fid_from_halfacet(hf, ctype);

            if (ngb){
                bool found_ent = find_match_in_array(ngb, &cellq[0], qsize-1);

                if (!found_ent)
                  {
                    cellq[qsize] = ngb;
                    qsize += 1;
                  }
              }
          }
      }

    for (int i = 0; i<qsize; i++)
        cellq[i] = 0;

    
    return found;
 }

  bool HalfFacetRep::find_matching_halfface(EntityHandle fid, EntityHandle *cid, int *lid)
  {    
    ErrorCode error;
    EntityHandle start_cell = *_cells.begin();
    EntityType ctype = mb->type_from_handle(start_cell);
    int index = get_index_in_lmap(start_cell);
    int nvpc = lConnMap3D[index].num_verts_in_cell;
    int nfpc = lConnMap3D[index].num_faces_in_cell;
    EntityType ftype = mb->type_from_handle(fid);
    int nvF = lConnMap2D[ftype-2].num_verts_in_face;

    const EntityHandle* fid_verts;
    error = mb->get_connectivity(fid, fid_verts, nvF, true);MB_CHK_ERR(error);

    std::vector<EntityHandle> start_cells;
    int vidx, locfv0=-1;
    HFacet hf = 0;

    for (int i=0; i<nvF; i++)
      {
        vidx = ID_FROM_HANDLE(fid_verts[i])-1;
        hf = v2hf[vidx];
        if (hf != 0)
          {
            start_cells.push_back(fid_from_halfacet(hf,ctype));
            locfv0 = i;
            break;
          }
        else if (hf==0 && v2hfs.find(fid_verts[i]) != v2hfs.end())
          {
            std::pair <std::multimap<EntityHandle, HFacet>::iterator, std::multimap<EntityHandle, HFacet>::iterator> it_hfs;
            it_hfs = v2hfs.equal_range(fid_verts[i]);

            for (std::multimap<EntityHandle, HFacet>::iterator it = it_hfs.first; it != it_hfs.second; ++it)
              {
                start_cells.push_back(fid_from_halfacet(it->second, ctype));
              }
            locfv0 = i;
            break;
          }
      }

    if (start_cells.empty())
      return false;

    EntityHandle cur_cid;
    bool found = false;

    int Stksize = 0, count = -1;

    for (int i=0; i<(int)start_cells.size(); i++)
        Stkcells[i] = start_cells[i];

    Stksize = start_cells.size() - 1;

    while (Stksize >= 0 ){
        cur_cid = Stkcells[Stksize];
        Stksize -= 1 ;
        count += 1;
        trackcells[count] = cur_cid;

        const EntityHandle* conn;
        error = mb->get_connectivity(cur_cid, conn, nvpc, true);MB_CHK_ERR(error);

        int lv[4] = {-1,-1,-1,-1};
        int cnt = 0;
        for (int i=0; i<nvpc; i++)
          {
            for (int j=0; j<nvF; j++)
              if (conn[i] == fid_verts[j])
                {
                  lv[j] = i;
                  cnt += 1;
                }
          }
        if (cnt == nvF) //All face verts are part of the cell
          {
            found = true;
            int nhf_thisv = lConnMap3D[index].v2hf_num[lv[locfv0]];
            int lfid=-1;
            for(int i = 0; i < nhf_thisv; ++i){
                lfid = lConnMap3D[index].v2hf[lv[locfv0]][i];
                int lcnt = 0;
                for(int j = 0; j < nvF; ++j){
                    for(int k = 0; k < nvF; ++k){
                        if ( lv[k] == lConnMap3D[index].hf2v[lfid][j])
                          lcnt += 1;
                      }
                  }
                if (lcnt == nvF)
                  break;
              }
            cid[0] = cur_cid;
            lid[0] = lfid;

            break;
          }
        else
          {
            // Add other cells that are incident on fid_verts[0]
            int nhf_thisv = lConnMap3D[index].v2hf_num[lv[locfv0]];
            int cidx = ID_FROM_HANDLE(cur_cid)-1;

            // Add new cells into the stack
            EntityHandle ngb;
            for (int i = 0; i < nhf_thisv; ++i){
                int ind = lConnMap3D[index].v2hf[lv[locfv0]][i];
                hf = sibhfs[nfpc*cidx+ind];
                ngb = fid_from_halfacet(hf, ctype);

                if (ngb) {

                    bool found_ent = find_match_in_array(ngb, trackcells, count);

                    if (!found_ent){
                        Stksize += 1;
                        Stkcells[Stksize] = ngb;
                      }
                  }
              }
          }
      }

    //Change the visited faces to false
    for (int i = 0; i<Stksize; i++)
      Stkcells[i] = 0;

    for (int i = 0; i <= count; i++)
      trackcells[i] = 0;

    return found;
  }

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  ErrorCode HalfFacetRep::get_neighbor_adjacencies_3d( EntityHandle cid, std::vector<EntityHandle> &adjents)
  {
    adjents.reserve(20);
    EntityType ctype = mb->type_from_handle(cid);
    int index = get_index_in_lmap(cid);
    int nfpc = lConnMap3D[index].num_faces_in_cell;
    int cidx = ID_FROM_HANDLE(cid)-1;

    if (cid != 0 ){
      for (int lfid = 0; lfid < nfpc; ++lfid){
          HFacet hf = sibhfs[nfpc*cidx+lfid];
          EntityHandle sibcid = fid_from_halfacet(hf, ctype);
          if (sibcid != 0)
            adjents.push_back(sibcid);
      }    
    }

    return MB_SUCCESS; 
  }

  /////////////////////////////////////////////////////////////////////////////////////////////////
  ErrorCode HalfFacetRep::get_down_adjacencies_edg_3d(EntityHandle cid, std::vector<EntityHandle> &adjents)
  {
    //TODO: Try intersection without using std templates
      //Returns explicit edges, if any, of the face
      ErrorCode error;
      adjents.reserve(20);
      int index = get_index_in_lmap(cid);
      int nvpc = lConnMap3D[index].num_verts_in_cell;

      const EntityHandle* conn;
      error = mb->get_connectivity(cid, conn, nvpc, true);MB_CHK_ERR(error);

      //Gather all the incident edges on each vertex of the face
      int ns = lConnMap3D[index].search_everts[0];
      std::vector<EntityHandle> temp;
      for (int i=0; i<ns; i++)
      {
          temp.clear();
          int lv0 = lConnMap3D[index].search_everts[i+1];
          error = get_up_adjacencies_1d(conn[lv0], temp);MB_CHK_ERR(error);

          int nle = lConnMap3D[index].v2le[i][0];
          int count =0;
          for (int j=0; j<(int)temp.size(); j++)
            {
              const EntityHandle* econn;
              int nvpe = 0;
              error = mb->get_connectivity(temp[j], econn, nvpe, true);MB_CHK_ERR(error);

              for (int k=0; k<nle; k++)
                {
                  int lv1 = lConnMap3D[index].v2le[i][k+1];
                  if (((econn[0] == conn[lv0]) && (econn[1] == conn[lv1]))||((econn[1] == conn[lv0]) && (econn[0] == conn[lv1])))
                    {
                      adjents.push_back(temp[j]);
                      count += 1;
                    }
                }
              if (count == nle)
                break;
            }
      }   
      return MB_SUCCESS;
  }


  ErrorCode HalfFacetRep::get_down_adjacencies_face_3d(EntityHandle cid, std::vector<EntityHandle> &adjents)
  {
      //Returns explicit face, if any of the cell
      ErrorCode error;
      adjents.reserve(10);
      int index = get_index_in_lmap(cid);
      int nvpc = lConnMap3D[index].num_verts_in_cell;
      int nfpc = lConnMap3D[index].num_faces_in_cell;

      //Get the connectivity of the input cell
      const EntityHandle* conn;
      error = mb->get_connectivity(cid, conn, nvpc, true);MB_CHK_ERR(error);

      //Collect all the half-faces of the cell
      EntityHandle half_faces[6][4];
      for (int i=0; i<nfpc; i++)
        {
          int nvf = lConnMap3D[index].hf2v_num[i];
          for (int j=0; j< nvf; j++)
            {
              int ind = lConnMap3D[index].hf2v[i][j];
              half_faces[i][j] = conn[ind];
            }
        }

      //Add two vertices for which the upward adjacencies are computed
      int search_verts[2];
      search_verts[0] = lConnMap3D[index].search_fverts[0];
      search_verts[1] = lConnMap3D[index].search_fverts[1];

      std::vector<EntityHandle> temp;
      temp.reserve(20);
      for (int i=0; i<2; i++)
        {
          //Get the incident faces on the local vertex
          int lv = search_verts[i];
          temp.clear();
          error = get_up_adjacencies_vert_2d(conn[lv], temp);MB_CHK_ERR(error);

          if (temp.size() == 0)
              continue;

          //Get the half-faces incident on the local vertex and match it with the obtained faces
          int nhfthisv =  lConnMap3D[index].v2hf_num[lv];
          for (int k=0; k<(int)temp.size(); k++)
            {
              const EntityHandle* fid_verts;
              int fsize = 0;
              error = mb->get_connectivity(temp[k], fid_verts, fsize, true);MB_CHK_ERR(error);

              for (int j=0; j<nhfthisv; j++)
                {
                  //Collect all the vertices of this half-face
                  int idx = lConnMap3D[index].v2hf[lv][j];
                  int nvF = lConnMap3D[index].hf2v_num[idx];

                  if  (fsize != nvF)
                    continue;

                  int direct,offset;
                  bool they_match = CN::ConnectivityMatch(&half_faces[idx][0],&fid_verts[0],nvF,direct,offset);

                  if (they_match)
                    {
                      bool found = false;
                      for (int p=0; p<(int)adjents.size(); p++)
                        {
                          if (adjents[p] == temp[k])
                            {
                              found = true;
                              break;
                            }
                        }
                      if (!found)
                        adjents.push_back(temp[k]);
                    }
                }
            }
        }

      return MB_SUCCESS;
  }
  ////////////////////////////////////////////////////////////////////////////////
  ErrorCode HalfFacetRep::find_total_edges_faces_3d(Range cells, int *nedges, int *nfaces)
  {
    ErrorCode error;
    int index = get_index_in_lmap(*cells.begin());
    int nepc = lConnMap3D[index].num_edges_in_cell;
    int nfpc = lConnMap3D[index].num_faces_in_cell;
    int ncells = cells.size();
    int total_edges = nepc*ncells;
    int total_faces = nfpc*ncells;

    std::vector<int> trackE(total_edges, 0);
    std::vector<int> trackF(total_faces,0);

    std::vector<EntityHandle> inc_cids, sib_cids;
    std::vector<int> inc_leids, sib_lfids;

    for (Range::iterator it = cells.begin(); it != cells.end(); ++it)
      {
        //Count edges
        for (int i=0; i<nepc; i++)
          {
            inc_cids.clear();
            inc_leids.clear();

            int id = nepc*(cells.index(*it))+i;
            if (!trackE[id])
              {
                error = get_up_adjacencies_edg_3d(*it, i, inc_cids, &inc_leids);MB_CHK_ERR(error);

                total_edges -= inc_cids.size() -1;
                for (int j=0; j < (int)inc_cids.size(); j++)
                  trackE[nepc*(cells.index(inc_cids[j]))+inc_leids[j]] = 1;
              }
          }

        //Count faces
        for (int i=0; i<nfpc; i++)
          {
            sib_cids.clear();
            sib_lfids.clear();

            int id = nfpc*(cells.index(*it))+i;
            if (!trackF[id])
              {
                error = get_up_adjacencies_face_3d(*it, i, sib_cids, &sib_lfids);MB_CHK_ERR(error);

                if (sib_cids.size() ==1)
                  continue;

                total_faces -= sib_cids.size() -1;
                trackF[nfpc*(cells.index(sib_cids[1]))+sib_lfids[1]] = 1;
              }
          }
      }

    nedges[0] = total_edges;
    nfaces[0] = total_faces;

    return MB_SUCCESS;

  }


  bool HalfFacetRep::find_match_in_array(EntityHandle ent, EntityHandle *ent_list, int count, bool get_index, int *index)
  {
    bool found = false;
    for (int i = 0; i<= count; i++)
      {
        if (ent == ent_list[i])
          {
            found = true;
            if (get_index)
              *index = i;
            break;
          }
      }

    return found;
  }

  ErrorCode HalfFacetRep::get_half_facet_in_comp(EntityHandle cid, int leid, std::vector<EntityHandle> &ents, std::vector<int> &lids, std::vector<int> &lfids)
  {
    ErrorCode error;
    ents.clear(); lids.clear();
    EntityType ctype = mb->type_from_handle(cid);
    int index = get_index_in_lmap(*_cells.begin());
    int nfpc = lConnMap3D[index].num_faces_in_cell;
    int nvpc = lConnMap3D[index].num_verts_in_cell;

    //Get all incident cells
    std::vector<EntityHandle> adjents;
    std::vector<int> adjlids;
    error = get_up_adjacencies_edg_3d(cid, leid, adjents, &adjlids);MB_CHK_ERR(error);

    // Get the end vertices of the edge <cid,leid>
    const EntityHandle* econn;
    error = mb->get_connectivity(cid, econn, nvpc, true);MB_CHK_ERR(error);
    int id = lConnMap3D[index].e2v[leid][0];
    EntityHandle vstart = econn[id];
    id = lConnMap3D[index].e2v[leid][1];
    EntityHandle vend = econn[id];


    std::vector<int> mark(adjents.size(), 0);
    int count = 0;

    for (int k=0; k<(int)adjents.size(); k++){

        if (mark[k] != 0)
          continue;

        count += 1;
        mark[k] = count;

        //Loop over each half-face incident on this local edge
        for (int i =0; i< 2; i++ )
        {
            EntityHandle cur_cell = adjents[k];
            int cur_leid = adjlids[k];

            int lface = i;

            while(true){
                int cidx = ID_FROM_HANDLE(cur_cell)-1;
                int lfid =  lConnMap3D[index].e2hf[cur_leid][lface];

                HFacet hf = sibhfs[nfpc*cidx+lfid];
                cur_cell = fid_from_halfacet(hf, ctype);
                lfid = lid_from_halffacet(hf);

                //Check if loop reached starting cell or a boundary
                if ((cur_cell == adjents[k]) || ( cur_cell==0))
                  break;

                const EntityHandle* sib_conn;
                error = mb->get_connectivity(cur_cell, sib_conn, nvpc, true);MB_CHK_ERR(error);

                //Find the local edge id wrt to sibhf
                int nv_curF = lConnMap3D[index].hf2v_num[lfid];
                int lv0 = -1, lv1 = -1, idx = -1 ;
                for (int j = 0; j<nv_curF; j++)
                  {
                    idx = lConnMap3D[index].hf2v[lfid][j];
                    if (vstart== sib_conn[idx])
                      lv0 = idx;
                    if (vend == sib_conn[idx])
                      lv1 = idx;
                  }

                assert((lv0 >= 0) && (lv1 >= 0));
                cur_leid = lConnMap3D[index].lookup_leids[lv0][lv1];

                int chk_lfid = lConnMap3D[index].e2hf[cur_leid][0];

                if (lfid == chk_lfid)
                  lface = 1;
                else
                  lface = 0;

                int ind = std::find(adjents.begin(), adjents.end(), cur_cell) - adjents.begin();
                mark[ind] = count;
              }


           //Loop back
           if (cur_cell != 0)
             break;
         }
      }

    //Loop over again to find cells on the boundary
    for (int c = 0; c<count; c++){
        for (int i=0; i<(int)adjents.size(); i++)
          {
            if (mark[i] == c+1) {
                int cidx = ID_FROM_HANDLE(adjents[i])-1;
                for (int j=0; j<nfpc; j++){
                    HFacet hf = sibhfs[nfpc*cidx+j];
                    EntityHandle  cell = fid_from_halfacet(hf, ctype);
                    if (cell == 0)
                      {
                        ents.push_back(adjents[i]);
                        lids.push_back(adjlids[i]);
                        lfids.push_back(j);
                        break;
                      }
                  }
              }
          }
      }



    return MB_SUCCESS;
  }

  ///////////////////////////////////////////////////////////////////////////////////////////
  ErrorCode HalfFacetRep::resize_hf_maps(EntityHandle start_vert, int nverts, EntityHandle start_edge, int nedges, EntityHandle start_face, int nfaces, EntityHandle start_cell, int ncells)
  {
    int nwsz=0, insz=0;
    if (nedges)
      {
        if (ID_FROM_HANDLE((*(_edges.end()-1)+1)) != ID_FROM_HANDLE(start_edge))
          nwsz = (ID_FROM_HANDLE(start_edge)-ID_FROM_HANDLE(*_edges.end())+nedges)*2;
        else
          nwsz = nedges*2;
        insz = sibhvs.size();
        sibhvs.resize(insz+nwsz,0);

        if (v2hv.empty())
          {
            if ((!v2he.empty()))
              insz = v2he.size();
            else if ((!v2hf.empty()))
              insz = v2hf.size();
            else
              MB_SET_ERR(MB_FAILURE,"Trying to resize ahf maps for a mesh with no edges, faces and cells");
          }
        else
          insz = v2hv.size();

        if (ID_FROM_HANDLE(*(_verts.end()-1)+1) != ID_FROM_HANDLE(start_vert))
          nwsz = ID_FROM_HANDLE(start_vert)-ID_FROM_HANDLE(*_verts.end())+nverts;
        else
          nwsz = nverts;
        v2hv.resize(insz+nwsz,0);

      }

   if (nfaces)
     {
       EntityType ftype = mb->type_from_handle(*_faces.begin());
       int nepf = lConnMap2D[ftype-2].num_verts_in_face;

       if (ID_FROM_HANDLE((*(_faces.end()-1)+1)) != ID_FROM_HANDLE(start_face))
         nwsz = (ID_FROM_HANDLE(start_face)-ID_FROM_HANDLE(*_faces.end())+nfaces)*nepf;
       else
         nwsz = nfaces*nepf;
       insz = sibhes.size();
       sibhes.resize(insz+nwsz,0);

       if (ID_FROM_HANDLE(*(_verts.end()-1)+1) != ID_FROM_HANDLE(start_vert))
         nwsz = ID_FROM_HANDLE(start_vert)-ID_FROM_HANDLE(*_verts.end())+nverts;
       else
         nwsz = nverts;
       insz = v2he.size();
       v2he.resize(insz+nwsz,0);
     }

   if (ncells)
     {
       int index = get_index_in_lmap(*_cells.begin());
       int nfpc = lConnMap3D[index].num_faces_in_cell;

       if (ID_FROM_HANDLE((*(_cells.end()-1)+1)) != ID_FROM_HANDLE(start_cell))
         nwsz = (ID_FROM_HANDLE(start_cell)-ID_FROM_HANDLE(*_cells.end())+ncells)*nfpc;
       else
         nwsz = ncells*nfpc;
       insz = sibhfs.size();
       sibhfs.resize(insz+nwsz,0);

       if (ID_FROM_HANDLE(*(_verts.end()-1)+1) != ID_FROM_HANDLE(start_vert))
         nwsz = ID_FROM_HANDLE(start_vert)-ID_FROM_HANDLE(*_verts.end())+nverts;
       else
         nwsz = nverts;
       insz = v2hf.size();
       v2hf.resize(insz+nwsz,0);
     }

   return MB_SUCCESS;

  }

  bool HalfFacetRep::check_nonmanifold_vertices(EntityType type, EntityHandle vid)
  {
    bool status = false;
    if (type == MBTRI || type == MBQUAD)
      {
        HFacet hf = v2he[ID_FROM_HANDLE(vid)-1];
        if (hf==0 && (v2hes.find(vid) != v2hes.end()))
          status = true;
      }
    else if (type == MBTET || type == MBHEX)
      {
        HFacet hf = v2hf[ID_FROM_HANDLE(vid)-1];
        if (hf==0 && (v2hfs.find(vid) != v2hfs.end()))
          status = true;
      }
    else
      MB_SET_ERR(MB_FAILURE,"Requesting non-manifold vertex checks for either (1) 1D mesh or (2) not-implemented entity types");

    return status;
  }

  ErrorCode HalfFacetRep::get_sibling_map(EntityType type, EntityHandle ent, EntityHandle *sib_entids, int *sib_lids,  int num_halffacets)
  {

    if (type == MBEDGE)
      {
        if (num_halffacets != 2)  MB_SET_ERR(MB_FAILURE, "Incorrect number of halfvertices.");

        int eidx = ID_FROM_HANDLE(ent)-1;
        for (int i=0; i<2; i++)
          {
            HFacet hf = sibhvs[2*eidx+i];
            sib_entids[i] = fid_from_halfacet(hf, MBEDGE);
            sib_lids[i] = lid_from_halffacet(hf);
          }
      }
    else if (type == MBTRI || type == MBQUAD)
      {
       int nepf = lConnMap2D[type-2].num_verts_in_face;

       if(num_halffacets != nepf) MB_SET_ERR(MB_FAILURE, "Incorrect number of halfedges.");

       int fidx = ID_FROM_HANDLE(ent)-1;
        for (int i=0; i<nepf; i++)
          {
            HFacet hf = sibhes[nepf*fidx+i];
            sib_entids[i] = fid_from_halfacet(hf, type);
            sib_lids[i] = lid_from_halffacet(hf);
          }
      }
    else
      {
        int idx = get_index_in_lmap(*_cells.begin());
        int nfpc = lConnMap3D[idx].num_faces_in_cell;

        if(num_halffacets != nfpc) MB_SET_ERR(MB_FAILURE, "Incorrect number of halffaces.");

        int cidx = ID_FROM_HANDLE(ent)-1;
        for (int i=0; i<nfpc; i++)
          {
            HFacet hf = sibhfs[nfpc*cidx+i];
            sib_entids[i] = fid_from_halfacet(hf, type);
            sib_lids[i] = lid_from_halffacet(hf);
          }

      }
    return MB_SUCCESS;
  }

  ErrorCode HalfFacetRep::get_sibling_map(EntityType type, EntityHandle ent, int lid,  EntityHandle &sib_entid, int &sib_lid)
  {

    if (type == MBEDGE)
      {
        int eidx = ID_FROM_HANDLE(ent)-1;
        HFacet hf = sibhvs[2*eidx+lid];
        sib_entid = fid_from_halfacet(hf, MBEDGE);
        sib_lid = lid_from_halffacet(hf);
      }
    else if (type == MBTRI || type == MBQUAD)
      {
       int nepf = lConnMap2D[type-2].num_verts_in_face;
       int fidx = ID_FROM_HANDLE(ent)-1;
       HFacet hf = sibhes[nepf*fidx+lid];
       sib_entid = fid_from_halfacet(hf, type);
       sib_lid = lid_from_halffacet(hf);
      }
    else
      {
        int idx = get_index_in_lmap(*_cells.begin());
        int nfpc = lConnMap3D[idx].num_faces_in_cell;
        int cidx = ID_FROM_HANDLE(ent)-1;
        HFacet hf = sibhfs[nfpc*cidx+lid];
        sib_entid = fid_from_halfacet(hf, type);
        sib_lid = lid_from_halffacet(hf);
      }
    return MB_SUCCESS;
  }

  ErrorCode HalfFacetRep::set_sibling_map(EntityType type, EntityHandle ent, EntityHandle *set_entids, int *set_lids, int num_halffacets)
  {

    if (type == MBEDGE)
      {
        if (num_halffacets != 2) MB_SET_ERR(MB_FAILURE, "Incorrect number of halfvertices");

        int eidx = ID_FROM_HANDLE(ent)-1;
        for (int i=0; i<2; i++)
          {
            sibhvs[2*eidx+i] = create_halffacet(set_entids[i], set_lids[i]);
          }
      }
    else if (type == MBTRI || type == MBQUAD)
      {
        int nepf = lConnMap2D[type-2].num_verts_in_face;
        if (num_halffacets != nepf) MB_SET_ERR(MB_FAILURE, "Incorrect number of halfedges.");

        int fidx = ID_FROM_HANDLE(ent)-1;
         for (int i=0; i<nepf; i++)
           {
             sibhes[nepf*fidx+i] = create_halffacet(set_entids[i], set_lids[i]);
           }
      }
    else
      {
        int idx = get_index_in_lmap(*_cells.begin());
        int nfpc = lConnMap3D[idx].num_faces_in_cell;
        if (num_halffacets != nfpc) MB_SET_ERR(MB_FAILURE, "Incorrect number of halffaces.");

        int cidx = ID_FROM_HANDLE(ent)-1;
        for (int i=0; i<nfpc; i++)
          {
            sibhfs[nfpc*cidx+i] = create_halffacet(set_entids[i], set_lids[i]);
          }
      }

    return MB_SUCCESS;
  }

  ErrorCode HalfFacetRep::set_sibling_map(EntityType type, EntityHandle ent, int lid, EntityHandle &set_entid, int &set_lid)
  {

    if (type == MBEDGE)
      {
        int eidx = ID_FROM_HANDLE(ent)-1;
        sibhvs[2*eidx+lid] = create_halffacet(set_entid, set_lid);
      }
    else if (type == MBTRI || type == MBQUAD)
      {
        int nepf = lConnMap2D[type-2].num_verts_in_face;
        int fidx = ID_FROM_HANDLE(ent)-1;
        sibhes[nepf*fidx+lid] = create_halffacet(set_entid, set_lid);
      }
    else
      {
        int idx = get_index_in_lmap(*_cells.begin());
        int nfpc = lConnMap3D[idx].num_faces_in_cell;
        int cidx = ID_FROM_HANDLE(ent)-1;
        sibhfs[nfpc*cidx+lid] = create_halffacet(set_entid, set_lid);
      }

    return MB_SUCCESS;
  }

  ErrorCode HalfFacetRep::get_incident_map(EntityType type, EntityHandle vid, std::vector<EntityHandle> &inci_entid, std::vector<int> &inci_lid)
  {
    inci_entid.clear(); inci_lid.clear();

    if (type == MBEDGE)
      {   
        HFacet hf = v2hv[ ID_FROM_HANDLE(vid)-1];
        inci_entid.push_back(fid_from_halfacet(hf, type));
        inci_lid.push_back(lid_from_halffacet(hf));
      }
    else if (type == MBTRI || type == MBQUAD)
      {
        HFacet hf = v2he[ID_FROM_HANDLE(vid)-1];
        if (hf==0 && (v2hes.find(vid) != v2hes.end()))
          {
            std::pair <std::multimap<EntityHandle, HFacet>::iterator, std::multimap<EntityHandle, HFacet>::iterator> it_hes;
            it_hes = v2hes.equal_range(vid);

            for (std::multimap<EntityHandle, HFacet>::iterator it = it_hes.first; it != it_hes.second; ++it)
              {
                inci_entid.push_back(fid_from_halfacet(it->second, type));
                inci_lid.push_back(lid_from_halffacet(it->second));
              }
          }
        else if (hf != 0){
            inci_entid.push_back(fid_from_halfacet(hf, type));
            inci_lid.push_back(lid_from_halffacet(hf));
          }
        else if (hf==0 && (v2hes.find(vid) == v2hes.end()))
          {
            inci_entid.push_back(fid_from_halfacet(hf, type));
            inci_lid.push_back(lid_from_halffacet(hf));
          }
      }
    else
      {
        HFacet hf = v2hf[ID_FROM_HANDLE(vid)-1];
        if (hf==0 && (v2hfs.find(vid) != v2hfs.end()))
          {
            std::pair <std::multimap<EntityHandle, HFacet>::iterator, std::multimap<EntityHandle, HFacet>::iterator> it_hes;
            it_hes = v2hfs.equal_range(vid);

            for (std::multimap<EntityHandle, HFacet>::iterator it = it_hes.first; it != it_hes.second; ++it)
              {
                inci_entid.push_back(fid_from_halfacet(it->second, type));
                inci_lid.push_back(lid_from_halffacet(it->second));
              }
          }
        else if (hf != 0){
            inci_entid.push_back(fid_from_halfacet(hf, type));
            inci_lid.push_back(lid_from_halffacet(hf));
          }
        else if (hf==0 && (v2hfs.find(vid) == v2hfs.end()))
          {
            inci_entid.push_back(fid_from_halfacet(hf, type));
            inci_lid.push_back(lid_from_halffacet(hf));
          }
      }

    return MB_SUCCESS;
  }

  ErrorCode HalfFacetRep::set_incident_map(EntityType type, EntityHandle vid, std::vector<EntityHandle> &set_entid, std::vector<int> &set_lid)
  {
    if (type == MBEDGE)
      {
        v2hv[ID_FROM_HANDLE(vid)-1] = create_halffacet(set_entid[0], set_lid[0]);
      }
    else if (type == MBTRI || type == MBQUAD)
      {
        if (set_entid.size() == 1)
          v2he[ID_FROM_HANDLE(vid)-1] = create_halffacet(set_entid[0], set_lid[0]);
        else
          {
            HFacet hf = 0;
            for (int i=0; i<(int)set_entid.size(); i++)
              {
                hf = create_halffacet(set_entid[i], set_lid[i]);
                v2hes.insert(std::pair<EntityHandle, HFacet>(vid, hf));
              }
          }
      }
    else
      {
        if  (set_entid.size() == 1)
          v2hf[ID_FROM_HANDLE(vid)-1] = create_halffacet(set_entid[0], set_lid[0]);
        else
          {
            HFacet hf = v2hf[ID_FROM_HANDLE(vid)-1];
            if (hf != 0)
              {
                v2hf[ID_FROM_HANDLE(vid)-1] = 0;
              }
            for (int i=0; i<(int)set_entid.size(); i++)
              {
                hf = create_halffacet(set_entid[i], set_lid[i]);
                v2hfs.insert(std::pair<EntityHandle, HFacet>(vid, hf));
              }
          }
      }

    return MB_SUCCESS;
  }

  ErrorCode HalfFacetRep::get_entity_ranges(Range &verts, Range &edges, Range &faces, Range &cells)
  {
    verts = _verts;
    edges = _edges;
    faces = _faces;
    cells = _cells;
    return MB_SUCCESS;
  }

  ErrorCode HalfFacetRep::update_entity_ranges(EntityHandle fileset)
  {
   ErrorCode error;

   error = mb->get_entities_by_dimension(fileset, 0, _verts, true);MB_CHK_ERR(error);

   error = mb->get_entities_by_dimension(fileset, 1, _edges, true);MB_CHK_ERR(error);

   error = mb->get_entities_by_dimension(fileset, 2, _faces, true);MB_CHK_ERR(error);

   error = mb->get_entities_by_dimension(fileset, 3, _cells, true);MB_CHK_ERR(error);

   return MB_SUCCESS;
  }

  void HalfFacetRep::get_memory_use(unsigned long long &entity_total, unsigned long long &memory_total)
  {
    entity_total = memory_total = 0;
    //1D
    if ( !v2hv.empty())
      entity_total += v2hv.capacity()*sizeof(HFacet) +sizeof(v2hv);
    if (!sibhvs.empty())
      entity_total += sibhvs.capacity()*sizeof(HFacet) + sizeof(sibhvs);

    //2D
    if (!v2he.empty())
      entity_total += v2he.capacity()*sizeof(HFacet) + sizeof(v2he);
    if (!sibhes.empty())
      entity_total += sibhes.capacity()*sizeof(HFacet) + sizeof(sibhes);

    //3D
    if (!v2hf.empty())
      entity_total += v2hf.capacity()*sizeof(HFacet) + sizeof(v2hf);
    if (!sibhfs.empty())
      entity_total += sibhfs.capacity()*sizeof(HFacet) + sizeof(sibhfs);

    memory_total = entity_total;
  }

  HFacet HalfFacetRep::create_halffacet(EntityHandle handle, int lid)
  {
    EntityID fid = ID_FROM_HANDLE(handle);
    return CREATE_HALFFACET(lid, fid);
  }

  EntityHandle HalfFacetRep::fid_from_halfacet(const HFacet facet, EntityType type)
  {
    EntityID id = FID_FROM_HALFFACET(facet);
    EntityHandle handle = 0;
    if (id == 0)
      return handle;

    ErrorCode error = mb->handle_from_id(type, id, handle); MB_CHK_ERR(error);
    return handle;
  }

  int HalfFacetRep::lid_from_halffacet(const HFacet facet)
  {
    if (facet == 0)
      return  0;
    else
      return LID_FROM_HALFFACET(facet);
  }

} // namespace moab

