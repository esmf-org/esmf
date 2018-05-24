// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <Mesh/include/Legacy/ESMCI_MeshPNC.h>
#include <Mesh/include/Legacy/ESMCI_MeshDB.h>

#include <Mesh/include/Legacy/ESMCI_Exception.h>
#include <Mesh/include/Legacy/ESMCI_MeshObjTopo.h>
#include <Mesh/include/Legacy/ESMCI_MeshSkin.h>
#include <Mesh/include/Legacy/ESMCI_MeshField.h>
#include <Mesh/include/ESMCI_Mesh.h>
#include <Mesh/include/Legacy/ESMCI_IOField.h>
#include <Mesh/include/Legacy/ESMCI_ParEnv.h>
#include <Mesh/include/Legacy/ESMCI_MeshObj.h>
#include <Mesh/include/Legacy/ESMCI_MeshUtils.h>

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

#ifdef ESMF_PNETCDF
#include <pnetcdf.h>
typedef MPI_Offset MPI_OffType;
#else
typedef long long MPI_OffType;
#endif

#include <cmath>
#include <iostream>
#include <algorithm>
#include <set>


namespace ESMCI {


void LoadNCDualMeshPar(Mesh &mesh, const std::string fname) {
  Trace __trace("LoadNCDualMeshPar(Mesh &mesh, const std::string fname)");
#ifdef ESMF_PNETCDF

  UInt rank = Par::Rank(), nproc = Par::Size();
  
  std::vector<double> xyz;
  int ncid, stat;
  std::vector<int> mask;
  std::map<int,int> node2idx;
  MPI_OffType grid_size;


    if ((stat = ncmpi_open(Par::Comm(), fname.c_str(), NC_NOWRITE, MPI_INFO_NULL, &ncid)) != NC_NOERR) {
      Throw() << "Trouble opening " << fname << ", ncerr=" <<
                 ncmpi_strerror(stat);
    }
  
    MPI_OffType n, grid_rank, grid_corners;
  
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
    
    MPI_OffType local_grid_size;
    MPI_OffType local_grid_start;
    
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
      MPI_OffType gclonattlen, gclatattlen;
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
      
      {
      std::vector<double> latcoord(local_grid_size);
      std::vector<double> loncoord(local_grid_size);
      
      /*
      std::cout << "Before lat read!!" << "P:" << Par::Rank() << "glatid=" << gclatid << ", glonid=" << gclonid << std::endl;
      
  std::vector<double> tst(grid_size);
  ncmpi_get_var_double_all(ncid, gclatid, &tst[0]);
  std::copy(tst.begin(), tst.end(), std::ostream_iterator<double>(Par::Out(), "\n"));
  */
  
  MPI_OffType starts[] = {local_grid_start, 0, 0, 0};
  MPI_OffType counts[] = {local_grid_size, 0, 0, 0};
  
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
        
        int nset = 0;

        // If the bottom row make nset=1, if top, nset=2
        if ((local_grid_start+i) < grid_dims[0]) {
          nset = 1;
        } else if ((local_grid_start+i) >= (grid_size-grid_dims[0])) {
          nset = 2;
        }
        
        node->set_owner(Par::Rank());
   
        // Set the owner of the last row to be the next proc.
        bool last_row = (i >= (local_grid_size - grid_dims[0]) &&
                   local_grid_start+local_grid_size < grid_size);
        if (last_row) node->set_owner(Par::Rank()+1);
  
  //if (rank < nproc-1 && last_row) Par::Out() << "owner=" << Par::Rank()+1 << std::endl;
        
        mesh.add_node(node,nset); // TODO: more reasonable block #
        nodevect.push_back(node);
        
        node2idx[node->get_id()] = i;
  
      } // for i
    
      } // bye to xncoord, yncoord
      
      // Read mask
      mask.resize(local_grid_size);
      int maskid;
      if ((stat = ncmpi_inq_varid(ncid, "grid_imask", &maskid)) != NC_NOERR)
        Throw() << "no mask found!!";
      ncmpi_get_vara_int_all(ncid, maskid, &local_grid_start, &local_grid_size, &mask[0]);
      
      // Create cells (use striding info).  Assume periodicity in lon.
      MeshObjTopo *topo = GetTopo("SHELL");
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
  
  IOField<NodalField> *mask_f = mesh.RegisterNodalField(mesh, "MASK_IO");
  mask_f->set_output_status(true);
  
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
  
 // Skin(mesh); // don't bother skinning; takes too much time.
#else
  Throw() << "Please recompile with PNETCDF support";
#endif
}


} // namespace
