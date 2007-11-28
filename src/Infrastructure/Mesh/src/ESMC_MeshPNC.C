// $Id: ESMC_MeshPNC.C,v 1.1 2007/11/28 16:28:03 dneckels Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <mesh/ESMC_MeshPNC.h>
#include <mesh/ESMC_MeshDB.h>

#include <mesh/ESMC_Exception.h>
#include <mesh/ESMC_MeshObjTopo.h>
#include <mesh/ESMC_MeshSkin.h>
#include <mesh/ESMC_MeshField.h>
#include <mesh/ESMC_Mesh.h>
#include <mesh/ESMC_IOField.h>
#include <mesh/ESMC_ParEnv.h>
#include <mesh/ESMC_MeshObj.h>
#include <mesh/ESMC_MeshUtils.h>

#include <pnetcdf.h>

#include <cmath>
#include <iostream>
#include <algorithm>
#include <set>


namespace ESMC {


void LoadNCDualMeshPar(Mesh &mesh, const std::string fname, bool use_quad) {

  UInt rank = Par::Rank(), nproc = Par::Size();
  
  std::vector<double> xyz;
  int ncid, stat;
  std::vector<int> mask;
  std::map<int,int> node2idx;
  MPI_Offset grid_size;


    if ((stat = ncmpi_open(Par::Comm(), fname.c_str(), NC_NOWRITE, MPI_INFO_NULL, &ncid)) != NC_NOERR) {
      Throw() << "Trouble opening " << fname << ", ncerr=" <<
                 ncmpi_strerror(stat);
    }
  
    MPI_Offset n, grid_rank, grid_corners;
  
    // Get ni, nj; verify nv=4
    int dimid;
    // n
    if ((stat = ncmpi_inq_dimid(ncid, "grid_size", &dimid)) != NC_NOERR) {
      Throw() << "NCErr:" << ncmpi_strerror(stat);
    }
    ncmpi_inq_dimlen(ncid, dimid, &n);
  //std::cout << "grid_size=" << n << std::endl;
    grid_size = n;
  
    // ni
    if ((stat = ncmpi_inq_dimid(ncid, "grid_rank", &dimid)) != NC_NOERR) {
      Throw() << "NCErr:" << ncmpi_strerror(stat);
    }
    ncmpi_inq_dimlen(ncid, dimid, &grid_rank);
  //std::cout << "grid_rank=" << grid_rank << std::endl;
  
    ThrowRequire(grid_rank == 2); // for now
  
    // nv
    if ((stat = ncmpi_inq_dimid(ncid, "grid_corners", &dimid)) != NC_NOERR) {
      Throw() << "NCErr:" << ncmpi_strerror(stat);
    }
    ncmpi_inq_dimlen(ncid, dimid, &grid_corners);
  //std::cout << "grid_corners=" << grid_corners << std::endl;
  
  
    if (grid_corners != 4) Throw() << "unexpected nv=" << grid_corners << "; should be 4";
  
    // Get the grid dims
    int gdimid;
    if ((stat = ncmpi_inq_varid(ncid, "grid_dims", &gdimid)) != NC_NOERR) {
      Throw() << "NCErr:" << ncmpi_strerror(stat);
    }
    int grid_dims[2];
    ncmpi_get_var_int_all(ncid, gdimid, &grid_dims[0]);
    
    //std::cout << "grid_dims:" << grid_dims[0] << ", " << grid_dims[1] << std::endl;

    // Calc min/max row for this proc
    
    long min_row, max_row, my_start, my_num;
    
    decomp1d(grid_dims[1] - 1, Par::Size(), Par::Rank(), my_num, my_start);
    
    min_row = my_start;
    max_row = min_row + my_num;
    
    MPI_Offset local_grid_size;
    MPI_Offset local_grid_start;
    
    if (my_num == 0) {
      local_grid_size = local_grid_start = 0;
    } else {
      local_grid_size = (my_num+1)*grid_dims[0];
      local_grid_start = my_start*grid_dims[0];
    }
    
    int local_grid_dims[2] = {grid_dims[0], my_num > 0 ? my_num+1 : 0};

    Par::Out() << "min_row=" << min_row << ", max_row=" << max_row << std::endl;
    Par::Out() << "local_grid_start=" << local_grid_start << ", local_grid_size:" << local_grid_size << std::endl;
  
      // We read in all nodes.  We then sort and unique the nodes
    
      int gclatid, gclonid;
      if ((stat = ncmpi_inq_varid(ncid, "grid_center_lon", &gclonid)) != NC_NOERR) {
        Throw() << "NCErr:" << ncmpi_strerror(stat);
      }
      if ((stat = ncmpi_inq_varid(ncid, "grid_center_lat", &gclatid)) != NC_NOERR) {
        Throw() << "NCErr:" << ncmpi_strerror(stat);
      }
      
      // Get units
      MPI_Offset gclonattlen, gclatattlen;
      bool latdeg = false, londeg = false;
      char attbuf[1024];
      if ((stat = ncmpi_inq_attlen(ncid, gclatid, "units", &gclatattlen)) != NC_NOERR) {
        Throw() << "NCErr:" << ncmpi_strerror(stat);
      }
      if ((stat = ncmpi_inq_attlen(ncid, gclonid, "units", &gclonattlen)) != NC_NOERR) {
        Throw() << "NCErr:" << ncmpi_strerror(stat);
      }
      
      if ((stat = ncmpi_get_att_text(ncid, gclatid, "units", &attbuf[0])) != NC_NOERR) {
        Throw() << "NCErr:" << ncmpi_strerror(stat);
      }
      attbuf[gclatattlen] = '\0';
      if (std::string(attbuf) == std::string("degrees")) latdeg = true;
      //std::cout << "lat units:" << attbuf << std::endl;
      
      if ((stat = ncmpi_get_att_text(ncid, gclonid, "units", &attbuf[0])) != NC_NOERR) {
        Throw() << "NCErr:" << ncmpi_strerror(stat);
      }
      attbuf[gclonattlen] = '\0';
      if (std::string(attbuf) == std::string("degrees")) londeg = true;
      //std::cout << "lon units:" << attbuf << std::endl;
      
  
      /*-------------------------------------------------------------------*/
      // Read in the cell centers (which are, in reality, of the dual mesh the
      // cell vertices).
      /*-------------------------------------------------------------------*/
  
      xyz.resize(3*local_grid_size);
      std::vector<MeshObj*> nodevect; nodevect.reserve(local_grid_size);
      std::vector<MeshObj*> nodevect_r, nodevect_u, nodevect_c;
      if (use_quad) {
        nodevect_r.reserve(local_grid_size);
        nodevect_u.reserve(local_grid_size);
        nodevect_c.reserve(local_grid_size);
      }
      
      {
      std::vector<double> latcoord(local_grid_size);
      std::vector<double> loncoord(local_grid_size);
      
      /*
      std::cout << "Before lat read!!" << "P:" << Par::Rank() << "glatid=" << gclatid << ", glonid=" << gclonid << std::endl;
      
  std::vector<double> tst(grid_size);
  ncmpi_get_var_double_all(ncid, gclatid, &tst[0]);
  std::copy(tst.begin(), tst.end(), std::ostream_iterator<double>(Par::Out(), "\n"));
  */
  
  MPI_Offset starts[] = {local_grid_start, 0, 0, 0};
  MPI_Offset counts[] = {local_grid_size, 0, 0, 0};
  
      ncmpi_get_vara_double_all(ncid, gclatid, starts, counts, &latcoord[0]);
      ncmpi_get_vara_double_all(ncid, gclonid, starts, counts, &loncoord[0]);
  
      /*
      std::cout << "Past lat read!!" << "P:" << Par::Rank() << std::endl;
  
      std::copy(latcoord.begin(), latcoord.end(), std::ostream_iterator<double>(Par::Out(), "\n"));
  MPI_Barrier(Par::Comm());
  */
      UInt idx = 0;
      const double DEG2RAD = M_PI/180.0;
      
      for (UInt i = 0; i < local_grid_size; i++) {
        double lat = latdeg ? DEG2RAD*latcoord[i] : latcoord[i];
        double lon = londeg ? DEG2RAD*loncoord[i] : loncoord[i];
        
        double theta = lon, phi = (DEG2RAD*90-lat);
        
        double x = std::cos(theta)*std::sin(phi),
          y = std::sin(theta)*std::sin(phi),
          z = cos(phi);
          
        int three_i = 3*i;
        
        xyz[three_i] = x;
        xyz[three_i+1] = y;
        xyz[three_i+2] = z;
        
        // Create a node
        MeshObj *node = new MeshObj(MeshObj::NODE, local_grid_start + i+1, i);
  //Par::Out() << "node_id=" << local_grid_start+i+1 << ", " << std::endl;
        
        int nset = 1;
        
        node->set_owner(Par::Rank());
   
        // Set the owner of the last row to be the next proc.
        bool last_row = (i >= (local_grid_size - grid_dims[0]) &&
                   local_grid_start+local_grid_size < grid_size);
        if (last_row) node->set_owner(Par::Rank()+1);
  
  //if (rank < nproc-1 && last_row) Par::Out() << "owner=" << Par::Rank()+1 << std::endl;
        
        mesh.add_node(node,nset); // TODO: more reasonable block #
        nodevect.push_back(node);
        
        node2idx[node->get_id()] = i;
  
        // If quad, then create edge nodes (possibly too many, but we remove them later).
        if (use_quad) {
          MeshObj *node_r = new MeshObj(MeshObj::NODE, grid_size + (i+1), 0);
          MeshObj *node_u = new MeshObj(MeshObj::NODE, 2*grid_size + (i+1), 0);
          MeshObj *node_c = new MeshObj(MeshObj::NODE, 3*grid_size + (i+1), 0);
          node_r->set_owner(Par::Rank());
          node_u->set_owner(Par::Rank());
          node_c->set_owner(Par::Rank());
          if (rank < nproc-1 && last_row) {
            node_r->set_owner(Par::Rank()+1);
            node_u->set_owner(Par::Rank()+1);
            node_c->set_owner(Par::Rank()+1);
          }
          mesh.add_node(node_r, nset);
          mesh.add_node(node_u, nset);
          mesh.add_node(node_c, nset);
          nodevect_r.push_back(node_r);
          nodevect_u.push_back(node_u);
          nodevect_c.push_back(node_c);
          
          // Don't worry about node2idx, since we don't have any data to retrive there.
        }
      } // for i
    
      } // bye to xncoord, yncoord
      
      // Read mask
      mask.resize(local_grid_size);
      int maskid;
      if ((stat = ncmpi_inq_varid(ncid, "grid_imask", &maskid)) != NC_NOERR)
        Throw() << "no mask found!!";
      ncmpi_get_vara_int_all(ncid, maskid, &local_grid_start, &local_grid_size, &mask[0]);
      
      // Create cells (use striding info).  Assume periodicity in lon.
      MeshObjTopo *topo = use_quad ? GetTopo("SHELL9") : GetTopo("SHELL");
      UInt nnodes = topo->num_nodes;
      std::vector<MeshObj*> nvect(nnodes, static_cast<MeshObj*>(0));
      
      UInt cnt = 0;
      for (UInt j = 0; j < local_grid_dims[1]-1; j++) {
        for (UInt i = 0; i < local_grid_dims[0]; i++) {
  
          // lon periodicity
          int ip1 = (i == local_grid_dims[0] - 1) ? 0 : i+1;
          
          nvect[0] = nodevect[j*local_grid_dims[0]+i];
          nvect[1] = nodevect[j*local_grid_dims[0]+ip1];
          nvect[2] = nodevect[(j+1)*local_grid_dims[0]+ip1];
          nvect[3] = nodevect[(j+1)*local_grid_dims[0]+i];
          
          // If useing quadratics, create and add the interior nodes
          if (use_quad) {
            nvect[4] = nodevect_r[j*local_grid_dims[0]+i];
            nvect[5] = nodevect_u[j*local_grid_dims[0]+ip1];
            nvect[6] = nodevect_r[(j+1)*local_grid_dims[0]+i];
            nvect[7] = nodevect_u[j*local_grid_dims[0]+i];
            nvect[8] = nodevect_c[j*local_grid_dims[0] + i];
          }
  
          // Check: if mask is 0 for all nodes, then don't add element
          bool elem_ok = false;
          for (UInt m = 0; !elem_ok && m < 4; m++) {
            elem_ok = mask[nvect[m]->get_data_index()] == 1;
          } 
//elem_ok = true;
          // Add the element if at least one node is active
          if (elem_ok) {
            MeshObj *elem = new MeshObj(MeshObj::ELEMENT, local_grid_start+cnt+1);
            /*
    Par::Out() << "Create elem:" << local_grid_start + cnt+1 << std::endl;
    std::copy(nvect.begin(), nvect.end(), std::ostream_iterator<MeshObj*>(Par::Out(), " "));
    Par::Out() << std::endl;
    */
            elem->set_owner(Par::Rank());
            int blk = 1;
            mesh.add_element(elem, nvect, blk, topo);
          }
              
          ++cnt;
        } // for i
      } // for j
  
    // Run concurrently 
  mesh.set_spatial_dimension(3);
  UInt sdim = 3;
  mesh.set_parametric_dimension(2);
  mesh.set_filename(fname);
   
  IOField<NodalField> *node_coord = mesh.RegisterNodalField(mesh, "coordinates", mesh.spatial_dim());
  
  IOField<NodalField> *mask_f = mesh.RegisterNodalField(mesh, "mask");
  
  MeshDB::const_iterator Ni = mesh.node_begin(), Ne = mesh.node_end();
  {
    for (; Ni != Ne; ++Ni) {

      double *c = node_coord->data(*Ni);
      double *m = mask_f->data(*Ni);
      
      int id = Ni->get_id();
      
      if (id <= grid_size) {
        int idx = node2idx[id];
        int three_i = idx*3;
        c[0] = xyz[three_i]; c[1] = xyz[three_i+1]; c[2] = xyz[three_i+2];
  
        m[0] = mask[idx];
      }
      
    }

  }
  
  // Now set the quadratic node coordinates
  if (use_quad) {
    MeshDB::const_iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
    for (; ei != ee; ++ei) {
      const MeshObj &elem = *ei;
      
      MeshObj *node;
      
      const MeshObjTopo *topo = GetMeshObjTopo(elem);

      double cn[3] = {0.0, 0.0, 0.0};
      
      for (UInt e = 0; e < topo->num_edges; e++) {
        
        const int *edge_nodes = topo->get_edge_nodes(e);
        
        ThrowRequire(topo->get_num_edge_nodes() == 3);
        
        MeshObj *node = elem.Relations[edge_nodes[2]].obj;
        
        MeshObj *node0 = elem.Relations[edge_nodes[0]].obj;
        MeshObj *node1 = elem.Relations[edge_nodes[1]].obj;
        
        double *c = node_coord->data(*node);
        
        for (UInt d = 0; d < sdim; d++) {
          c[d] = 0.5*(node_coord->data(*node0)[d] + node_coord->data(*node1)[d]);
          cn[d] += node_coord->data(*node0)[d] + node_coord->data(*node1)[d];
        }
        
        // normalize to circle
        double nm = 1.0 / std::sqrt(c[0]*c[0]+c[1]*c[1]+c[2]*c[2]);
        c[0] *= nm; c[1] *= nm; c[2] *= nm;
        
        
      }
      
      // And, lastly, the center
      const MeshObj *cnode = elem.Relations[8].obj;
      
      cn[0] /= 4.0; cn[1] /= 4.0; cn[2] /= 4.0;
      
      double nm = 1.0 / std::sqrt(cn[0]*cn[0]+cn[1]*cn[1]+cn[2]*cn[2]);
      cn[0] *= nm; cn[1] *= nm; cn[2] *= nm;
      
    }
  }

  ncmpi_close(ncid);
//std::cout << "to end, P:" << Par::Rank() << std::endl;
  
  /*
   * The following is a bit contorted, but here is what is going on.  We
   * have used the rule of assigning the upper row (of nodes) to the next processor.
   * However, that next processor cell may be masked out, and so a node that
   * we have declared owned by the next proc may not be used on that proc.
   * So we do the following:
   * 1) build the sym spec while the node is still there (but unused).
   * 2) Mark the nodes for delete.
   * 3) Resolve a viable ownership, now knowing that the nodes will disappear.
   * 4) clear spec, delete nodes, and then rebuild the spec with new ownership.
   * 
   * There is probably a way to do this without having to build the spec twice,
   * but this approach was the simplest to code. --dn
   */
  {
    mesh.build_sym_comm_rel(MeshObj::NODE);
  
    // Mark all unused nodes to delete.  Don't actually remove them yet.
    mesh.remove_unused_nodes(false);
    
    // Update the spec with new and valid owners.
    mesh.resolve_cspec_delete_owners(MeshObj::NODE);
    
    // We now have the correct owners, so clear the spec, delete the objects,
    // and rebuild the spec.
    
    mesh.GetCommRel(MeshObj::NODE).clear();
    
    // Locally delete
    mesh.MeshDB::ResolvePendingDelete(MeshObj::NODE);
    
    // rebuild
    mesh.build_sym_comm_rel(MeshObj::NODE);
  
  }
  
  Skin(mesh);
}


} // namespace
