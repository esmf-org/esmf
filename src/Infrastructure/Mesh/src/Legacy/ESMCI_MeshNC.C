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
#include <Mesh/include/Legacy/ESMCI_MeshNC.h>
#include <Mesh/include/Legacy/ESMCI_MeshDB.h>

#include <Mesh/include/Legacy/ESMCI_Exception.h>
#include <Mesh/include/Legacy/ESMCI_MeshObjTopo.h>
#include <Mesh/include/Legacy/ESMCI_MeshSkin.h>
#include <Mesh/include/Legacy/ESMCI_MeshField.h>
#include <Mesh/include/ESMCI_Mesh.h>
#include <Mesh/include/Legacy/ESMCI_IOField.h>
#include <Mesh/include/Legacy/ESMCI_ParEnv.h>
#include <Mesh/include/Legacy/ESMCI_MeshObj.h>

#ifdef ESMC_NETCDF
#include <netcdf.h>
#endif

#include <cmath>
#include <iostream>
#include <algorithm>
#include <set>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

namespace ESMCI {

struct llnode {
  UInt idx;
  double lat, lon;
  double x,y,z;
  bool operator==(const llnode &rhs) const {
    if (std::abs(x - rhs.x) < 1e-5 &&
       std::abs(y - rhs.y) < 1e-5 &&
        std::abs(z - rhs.z) < 1e-5) return true;

    return false;
  }
  bool operator<(const llnode &rhs) const {
    return (std::abs(lat-rhs.lat) > 1e-5) ?
            lat < rhs.lat :
            lon < rhs.lon;
  }
};

void LoadNCMesh(Mesh &mesh, const std::string name) {
  Trace __trace("LoadNCMesh(Mesh &mesh, const std::string name)");
#ifdef ESMC_NETCDF

  
  int ncid, stat;
  std::size_t grid_size;
  std::map<MeshObj::id_type, llnode> nmap;
  std::map<MeshObj::id_type, int> emap;
  
  mesh.set_spatial_dimension(3);
  mesh.set_parametric_dimension(2);
  mesh.set_filename(name);

  // open the netcdf file and read on proc 0
  if (Par::Rank() == 0) {
    
  
      if ((stat = nc_open(name.c_str(), NC_WRITE, &ncid)) != NC_NOERR) {
        Throw() << "Trouble opening " << name << ", ncerr=" <<
                   nc_strerror(stat);
      }
    
      std::size_t n, grid_rank, grid_corners, ni, nj;
    
      // Get ni, nj; verify nv=4
      int dimid;
      // n
      if ((stat = nc_inq_dimid(ncid, "grid_size", &dimid)) != NC_NOERR) {
        Throw() << "NCErr:" << nc_strerror(stat);
      }
      nc_inq_dimlen(ncid, dimid, &n);
    std::cout << "grid_size=" << n << std::endl;
      grid_size = n;
    
      // ni
      if ((stat = nc_inq_dimid(ncid, "grid_rank", &dimid)) != NC_NOERR) {
        Throw() << "NCErr:" << nc_strerror(stat);
      }
      nc_inq_dimlen(ncid, dimid, &grid_rank);
    std::cout << "grid_rank=" << grid_rank << std::endl;
    
      //ThrowRequire(grid_rank == 2); // for now
    
      // nv
      if ((stat = nc_inq_dimid(ncid, "grid_corners", &dimid)) != NC_NOERR) {
        Throw() << "NCErr:" << nc_strerror(stat);
      }
      nc_inq_dimlen(ncid, dimid, &grid_corners);
    std::cout << "grid_corners=" << grid_corners << std::endl;
    
    
      if (grid_corners != 4) Throw() << "unexpected nv=" << grid_corners << "; should be 4";
    
      // Get the grid dims
      int gdimid;
      if ((stat = nc_inq_varid(ncid, "grid_dims", &gdimid)) != NC_NOERR) {
        Throw() << "NCErr:" << nc_strerror(stat);
      }
      int grid_dims[2];
      nc_get_var_int(ncid, gdimid, &grid_dims[0]);
      
      std::cout << "grid_dims:" << grid_dims[0] << ", " << grid_dims[1] << std::endl;
    
      ni = grid_dims[0]; nj = grid_dims[1];
      
      // We read in all nodes.  We then sort and unique the nodes
    
      int gclatid, gclonid;
      if ((stat = nc_inq_varid(ncid, "grid_corner_lon", &gclonid)) != NC_NOERR) {
        Throw() << "NCErr:" << nc_strerror(stat);
      }
      if ((stat = nc_inq_varid(ncid, "grid_corner_lat", &gclatid)) != NC_NOERR) {
        Throw() << "NCErr:" << nc_strerror(stat);
      }
      
      // Get units
      std::size_t gclonattlen, gclatattlen;
      bool latdeg = false, londeg = false;
      char attbuf[1024];
      if ((stat = nc_inq_attlen(ncid, gclatid, "units", &gclatattlen)) != NC_NOERR) {
        Throw() << "NCErr:" << nc_strerror(stat);
      }
      if ((stat = nc_inq_attlen(ncid, gclonid, "units", &gclonattlen)) != NC_NOERR) {
        Throw() << "NCErr:" << nc_strerror(stat);
      }
      
      if ((stat = nc_get_att_text(ncid, gclatid, "units", &attbuf[0])) != NC_NOERR) {
        Throw() << "NCErr:" << nc_strerror(stat);
      }
      attbuf[gclatattlen] = '\0';
      if (std::string(attbuf) == std::string("degrees")) latdeg = true;
      std::cout << "lat units:" << attbuf << std::endl;
      
      if ((stat = nc_get_att_text(ncid, gclonid, "units", &attbuf[0])) != NC_NOERR) {
        Throw() << "NCErr:" << nc_strerror(stat);
      }
      attbuf[gclonattlen] = '\0';
      if (std::string(attbuf) == std::string("degrees")) londeg = true;
      std::cout << "lon units:" << attbuf << std::endl;
      
    
    std::vector<llnode> ll; ll.reserve(grid_size*4); // all lat lons
    {
    std::vector<double> xncoord(4*grid_size);
    std::vector<double> yncoord(4*grid_size);
    nc_get_var_double(ncid, gclonid, &xncoord[0]);
    nc_get_var_double(ncid, gclatid, &yncoord[0]);
    UInt idx = 0;
    ThrowRequire(latdeg == londeg);
    double DEG2RAD = M_PI/180.0;
    double ninety = 90.0;
    if (!latdeg) {DEG2RAD = 1.0; ninety = M_PI/2.0;}
    for (UInt cnt = 0; cnt < grid_size; ++cnt) {
        for (UInt v = 0; v < grid_corners; v++) {
          llnode ln; ln.idx = idx++;
          ln.lon = xncoord[cnt*grid_corners+v];
          ln.lat = yncoord[cnt*grid_corners+v];
          double theta = DEG2RAD*ln.lon, phi = DEG2RAD*(ninety-ln.lat);
          ln.x = std::cos(theta)*std::sin(phi);
          ln.y = std::sin(theta)*std::sin(phi);
          ln.z = cos(phi);
          ll.push_back(ln);
        } // nv
    }
  
    } // bye to xncoord, yncoord
  
  std::cout << "size of ll:" << ll.size() << std::endl;
    std::sort(ll.begin(), ll.end(), std::less<llnode>());
  
    // We travel the list and build an index vector
    std::vector<UInt> new_idx(ll.size());
    { // indexing loop
    std::vector<llnode>::iterator li = ll.begin();
    const std::vector<llnode>::iterator le = ll.end();
   
    UInt nidx = 0;
    for (; li != le;) {
      const llnode &ln = *li;
  //std::cout << "different" << std::endl;
      new_idx[ln.idx] = nidx;
      
      while(li != le && *li == ln) {
        new_idx[li->idx] = nidx;
        ++li;
      }
      nidx++;
    }
  
  
    ll.erase(std::unique(ll.begin(), ll.end()), ll.end());
  
    if (nidx != ll.size()) Throw() << "nidx=" << nidx << " should be ll.size()=" << ll.size();
  
    } // indexing 
  
    
    // We can now build the nodes.
  
    std::vector<MeshObj*> nodevect; nodevect.reserve(ll.size());
    // Loop ll
  
    { //nodecreate
    std::vector<llnode>::iterator li = ll.begin();
    const std::vector<llnode>::iterator le = ll.end();
    UInt nnumber = 0;
    for (; li != le; ++li) {
      MeshObj *node = new MeshObj(MeshObj::NODE, ++nnumber);
      std::vector<double> coord(3);
      coord[0] = li->x; coord[1] = li->y; coord[2] = li->z;
      nmap[node->get_id()] = *li;
      //node->add_data("coord", coord);
      int nset =0;
      mesh.add_node(node,nset); // TODO: more reasonable block #
      nodevect.push_back(node);
    } // for li
    } // nodecreate
    
  
 
    // Loop through n, building 
    { // elements
    std::vector<int> mask(grid_size);
    int maskid;
    if ((stat = nc_inq_varid(ncid, "grid_imask", &maskid)) != NC_NOERR)
      Throw() << "no mask found!!";
    nc_get_var_int(ncid, maskid, &mask[0]);
    MeshObjTopo *topo= GetTopo("SHELL");
    MeshObjTopo *tri_topo= GetTopo("SHELL3");
    std::vector<MeshObj*> nconnect(4);
    std::set<MeshObj*> nset;
    int blk = 1;
    int blk2 = 2;
    UInt cnt = 0;
    for (UInt i = 0; i < grid_size; i++) {
        nset.clear();
        for (UInt v = 0; v < grid_corners; v++) {
          nconnect[v] = nodevect[new_idx[grid_corners*cnt+v]];
          nset.insert(nodevect[new_idx[grid_corners*cnt+v]]);
        } //nv
        // Check for something wierd.  If any two of the vertices are the
        // same, then we are probably at the pole (or some nasty coord singularity),
        // so make these triangles
          if (nset.size() != grid_corners) {
            //std::cout << "Must be tri, nset:" << nset.size() << std::endl;
            // move non uniques to end
            std::unique(nconnect.begin(), nconnect.end());
            // Build a triangle
            MeshObj *elem = new MeshObj(MeshObj::ELEMENT, cnt+1);
            //elem->add_data("block", blk2);
            //elem->add_data("MASK_IO", mask[j*ni+i]);
            emap[elem->get_id()] = mask[cnt];
            mesh.add_element(elem, nconnect, blk2, tri_topo);
            //std::cout << "add element tri:" << (cnt+1) << std::endl;
          } else {
            MeshObj *elem = new MeshObj(MeshObj::ELEMENT, cnt+1);
    /*
    if (cnt < 15)  {
            elem->add_data("block", blk2);
    } else
    */
        //    std::cout << "add element:" << (cnt+1) << std::endl;
            //elem->add_data("block", blk);
            //elem->add_data("mask", mask[j*ni+i]);
          if (elem->get_id() == 2426) {
            for (UInt h = 0; h < 4; h++) {
              std::cout << "node=" << nconnect[h]->get_id() << std::endl;
            }
          }
            emap[elem->get_id()] = mask[cnt];
            mesh.add_element(elem, nconnect, blk, topo);
          }
        cnt++;
      } // for i
    } // elements
  
    mesh.remove_unused_nodes();
    mesh.linearize_data_index();
    
    nc_close(ncid);
    
  } // if proc 0 
  IOField<NodalField> *node_coord = mesh.RegisterNodalField(mesh, "coordinates", mesh.spatial_dim());
  
  MeshDB::const_iterator Ni = mesh.node_begin(), Ne = mesh.node_end();
  {
    for (; Ni != Ne; ++Ni) {
      double *c = node_coord->data(*Ni);
      llnode &tc = nmap[Ni->get_id()];
      c[0] = tc.x; c[1] = tc.y; c[2] = tc.z;
    }
  }

  // apply mask
  IOField<ElementField> *elem_mask = mesh.RegisterElementField(mesh, "MASK_IO");
  elem_mask->set_output_status(true);
  MeshDB::const_iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
//std::cout << "ei=" << (*ei)->get_id() << ", ee=" << (*ee)->get_id() << std::endl;
//std::cout << "num elem:" << mesh.num_elems() << std::endl;
  for (; ei != ee; ++ei) {
    //int mask = ei->get_int("MASK_IO");
    int mask = emap[ei->get_id()];
    *((double*)elem_mask->data(*ei)) = mask;
    //*((double*) elem_mask->data(*ei)) = ei->get_id();
//std::cout << "el:" << (*ei)->get_id() << ", mask=" << mask << std::endl;
  }


#else
  Throw() << "Please recompile with NETCDF enabled";
#endif
}

void LoadNCDualMesh(Mesh &mesh, const std::string fname, bool use_quad) {
  Trace __trace("LoadNCDualMesh(Mesh &mesh, const std::string fname, bool use_quad");
#ifdef ESMC_NETCDF
  
  std::vector<double> xyz;
  int ncid, stat;
  std::vector<int> mask;
  std::vector<int> node2idx;
  std::size_t grid_size;


  // open the netcdf file and read on proc 0
  if (Par::Rank() == 0) {
    

    if ((stat = nc_open(fname.c_str(), NC_WRITE, &ncid)) != NC_NOERR) {
      Throw() << "Trouble opening " << fname << ", ncerr=" <<
                 nc_strerror(stat);
    }
  
    std::size_t n, grid_rank, grid_corners;
  
    // Get ni, nj; verify nv=4
    int dimid;
    // n
    if ((stat = nc_inq_dimid(ncid, "grid_size", &dimid)) != NC_NOERR) {
      Throw() << "NCErr:" << nc_strerror(stat);
    }
    nc_inq_dimlen(ncid, dimid, &n);
  std::cout << "grid_size=" << n << std::endl;
    grid_size = n;
  
    // ni
    if ((stat = nc_inq_dimid(ncid, "grid_rank", &dimid)) != NC_NOERR) {
      Throw() << "NCErr:" << nc_strerror(stat);
    }
    nc_inq_dimlen(ncid, dimid, &grid_rank);
  std::cout << "grid_rank=" << grid_rank << std::endl;
  
    ThrowRequire(grid_rank == 2); // for now
  
    // nv
    if ((stat = nc_inq_dimid(ncid, "grid_corners", &dimid)) != NC_NOERR) {
      Throw() << "NCErr:" << nc_strerror(stat);
    }
    nc_inq_dimlen(ncid, dimid, &grid_corners);
  std::cout << "grid_corners=" << grid_corners << std::endl;
  
  
    if (grid_corners != 4) Throw() << "unexpected nv=" << grid_corners << "; should be 4";
  
    // Get the grid dims
    int gdimid;
    if ((stat = nc_inq_varid(ncid, "grid_dims", &gdimid)) != NC_NOERR) {
      Throw() << "NCErr:" << nc_strerror(stat);
    }
    int grid_dims[2];
    nc_get_var_int(ncid, gdimid, &grid_dims[0]);
    
    std::cout << "grid_dims:" << grid_dims[0] << ", " << grid_dims[1] << std::endl;
  
    // We read in all nodes.  We then sort and unique the nodes
  
    int gclatid, gclonid;
    if ((stat = nc_inq_varid(ncid, "grid_center_lon", &gclonid)) != NC_NOERR) {
      Throw() << "NCErr:" << nc_strerror(stat);
    }
    if ((stat = nc_inq_varid(ncid, "grid_center_lat", &gclatid)) != NC_NOERR) {
      Throw() << "NCErr:" << nc_strerror(stat);
    }
    
    // Get units
    std::size_t gclonattlen, gclatattlen;
    bool latdeg = false, londeg = false;
    char attbuf[1024];
    if ((stat = nc_inq_attlen(ncid, gclatid, "units", &gclatattlen)) != NC_NOERR) {
      Throw() << "NCErr:" << nc_strerror(stat);
    }
    if ((stat = nc_inq_attlen(ncid, gclonid, "units", &gclonattlen)) != NC_NOERR) {
      Throw() << "NCErr:" << nc_strerror(stat);
    }
    
    if ((stat = nc_get_att_text(ncid, gclatid, "units", &attbuf[0])) != NC_NOERR) {
      Throw() << "NCErr:" << nc_strerror(stat);
    }
    attbuf[gclatattlen] = '\0';
    if (std::string(attbuf) == std::string("degrees")) latdeg = true;
    std::cout << "lat units:" << attbuf << std::endl;
    
    if ((stat = nc_get_att_text(ncid, gclonid, "units", &attbuf[0])) != NC_NOERR) {
      Throw() << "NCErr:" << nc_strerror(stat);
    }
    attbuf[gclonattlen] = '\0';
    if (std::string(attbuf) == std::string("degrees")) londeg = true;
    std::cout << "lon units:" << attbuf << std::endl;
    

    /*-------------------------------------------------------------------*/
    // Read in the cell centers (which are, in reality, of the dual mesh the
    // cell vertices).
    /*-------------------------------------------------------------------*/

    node2idx.resize(grid_size+1);
    xyz.resize(3*n);
    std::vector<MeshObj*> nodevect; nodevect.reserve(n);
    std::vector<MeshObj*> nodevect_r, nodevect_u, nodevect_c;
    if (use_quad) {
      nodevect_r.reserve(n);
      nodevect_u.reserve(n);
      nodevect_c.reserve(n);
    }
    
    {
    std::vector<double> latcoord(n);
    std::vector<double> loncoord(n);
    nc_get_var_double(ncid, gclatid, &latcoord[0]);
    nc_get_var_double(ncid, gclonid, &loncoord[0]);
    UInt idx = 0;
    const double DEG2RAD = M_PI/180.0;
    
    for (UInt i = 0; i < n; i++) {
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
      MeshObj *node = new MeshObj(MeshObj::NODE, i+1, i);

   
      
      int nset = 1;
      
      node->set_owner(Par::Rank());
      
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
    mask.resize(n);
    int maskid;
    if ((stat = nc_inq_varid(ncid, "grid_imask", &maskid)) != NC_NOERR)
      Throw() << "no mask found!!";
    nc_get_var_int(ncid, maskid, &mask[0]);
    
    // Create cells (use striding info).  Assume periodicity in lon.
    MeshObjTopo *topo = use_quad ? GetTopo("SHELL9") : GetTopo("SHELL");
    UInt nnodes = topo->num_nodes;
    std::vector<MeshObj*> nvect(nnodes, static_cast<MeshObj*>(0));
    
    UInt cnt = 0;
    for (UInt j = 0; j < grid_dims[1]-1; j++) {
      for (UInt i = 0; i < grid_dims[0]; i++) {

        // lon periodicity
        int ip1 = (i == grid_dims[0] - 1) ? 0 : i+1;
        
        nvect[0] = nodevect[j*grid_dims[0]+i];
        nvect[1] = nodevect[j*grid_dims[0]+ip1];
        nvect[2] = nodevect[(j+1)*grid_dims[0]+ip1];
        nvect[3] = nodevect[(j+1)*grid_dims[0]+i];
        
        // If useing quadratics, create and add the interior nodes
        if (use_quad) {
          nvect[4] = nodevect_r[j*grid_dims[0]+i];
          nvect[5] = nodevect_u[j*grid_dims[0]+ip1];
          nvect[6] = nodevect_r[(j+1)*grid_dims[0]+i];
          nvect[7] = nodevect_u[j*grid_dims[0]+i];
          nvect[8] = nodevect_c[j*grid_dims[0] + i];
        }

        // Check: if mask is 0 for all nodes, then don't add element
        bool elem_ok = false;
        for (UInt m = 0; !elem_ok && m < 4; m++) {
          elem_ok = mask[nvect[m]->get_data_index()] == 1;
        } 
        
        // Add the element if at least one node is active
        if (elem_ok) {
          MeshObj *elem = new MeshObj(MeshObj::ELEMENT, cnt+1);
  
          elem->set_owner(Par::Rank());
          int blk = 1;
          mesh.add_element(elem, nvect, blk, topo);
        }
            
        ++cnt;
      } // for i
    } // for j

    mesh.remove_unused_nodes();
    mesh.linearize_data_index();

  } // if rank == 0
  
  // Run concurrently 
  mesh.set_spatial_dimension(3);
  UInt sdim = 3;
  mesh.set_parametric_dimension(2);
  mesh.set_filename(fname);
   
  IOField<NodalField> *node_coord = mesh.RegisterNodalField(mesh, "coordinates", mesh.spatial_dim());
  
  IOField<NodalField> *mask_f = mesh.RegisterNodalField(mesh, "MASK_IO");
  
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

  // Only proc 0 closes file.
  if (Par::Rank() == 0) {

    nc_close(ncid);
    
  } // if rank == 0
  
  Skin(mesh);
#else
  Throw() << "Please recompile with netcdf enabled";
#endif
}

// Load a spectral mesh with only lat/lon, not cell info
void LoadNCTMesh(Mesh &mesh, const std::string name,
                 latlon_func *lf)
{
  Trace __trace("LoadNCTMesh(Mesh &mesh, const std::string name, latlon_func *lf)");
#ifdef ESMC_NETCDF

  // open the netcdf file
  int ncid, stat;
  if ((stat = nc_open(name.c_str(), NC_WRITE, &ncid)) != NC_NOERR) {
    Throw() << "Trouble opening " << name << ", ncerr=" <<
               nc_strerror(stat);
  }

  std::size_t nlat, nlon;

  int dimid;

  // ni
  if ((stat = nc_inq_dimid(ncid, "lat", &dimid)) != NC_NOERR) {
    Throw() << "NCErr:" << nc_strerror(stat);
  }
  nc_inq_dimlen(ncid, dimid, &nlat);
std::cout << "nlat=" << nlat << std::endl;

  // nj
  if ((stat = nc_inq_dimid(ncid, "lon", &dimid)) != NC_NOERR) {
    Throw() << "NCErr:" << nc_strerror(stat);
  }
  nc_inq_dimlen(ncid, dimid, &nlon);
std::cout << "nlon=" << nlon << std::endl;

  // We read in all nodes.  We then sort and unique the nodes

  int latid, lonid;
  if ((stat = nc_inq_varid(ncid, "lon", &lonid)) != NC_NOERR) {
    Throw() << "NCErr:" << nc_strerror(stat);
  }
  if ((stat = nc_inq_varid(ncid, "lat", &latid)) != NC_NOERR) {
    Throw() << "NCErr:" << nc_strerror(stat);
  }

  mesh.set_spatial_dimension(3);
  mesh.set_parametric_dimension(2);
  mesh.set_filename(name);

  UInt n = nlat*nlon;
  std::vector<llnode> ll; ll.reserve(n); // all lat lons
  {
  std::vector<double> latcoord(n);
  std::vector<double> loncoord(n);
  nc_get_var_double(ncid, lonid, &loncoord[0]);
  nc_get_var_double(ncid, latid, &latcoord[0]);
  UInt idx = 0;
  const double DEG2RAD = M_PI/180.0;
  for (UInt i = 0; i < nlon; i++) {
    for (UInt j = 0; j < nlat; j++) {
      llnode ln; ln.idx = idx++;
      ln.lon = loncoord[i];
      ln.lat = latcoord[j];
      double theta = DEG2RAD*ln.lon, phi = DEG2RAD*(90-ln.lat);
      ln.x = std::cos(theta)*std::sin(phi);
      ln.y = std::sin(theta)*std::sin(phi);
      ln.z = cos(phi);
      ll.push_back(ln);
    }
  } // for i

  } // bye to xncoord, yncoord

std::cout << "size of ll:" << ll.size() << std::endl;

  // Create nodes;
  std::vector<MeshObj*> nodevect; nodevect.reserve(ll.size()+nlat);
  // Loop ll
  std::map<MeshObj::id_type, llnode> nmap;
  { //nodecreate
  std::vector<llnode>::iterator li = ll.begin();
  const std::vector<llnode>::iterator le = ll.end();
  UInt nnumber = 0;
  for (; li != le; ++li) {
    MeshObj *node = new MeshObj(MeshObj::NODE, ++nnumber);
    std::vector<double> coord(3);
    coord[0] = li->x; coord[1] = li->y; coord[2] = li->z;
    nmap[node->get_id()] = *li;
    int nset =0;
    // see if a pole.
    mesh.add_node(node,nset); // TODO: more reasonable block #
    nodevect.push_back(node);
  } // for li
  } // nodecreate

  // Copy the last strip of lat onto end of list so the loop below
  // connects lon periodically
  std::copy(&nodevect[0], &nodevect[nlat], std::back_inserter(nodevect));

  // Now to construct the cells.  We wrap in lon, lat stops poleward
  std::vector<MeshObj*> nconnect(4);
  UInt cnt = 0;
  int blk = 1;
  MeshObjTopo *topo= GetTopo("SHELL");
  for (UInt i = 0; i < nlon; i++) {
    // nlat-1 to stop poleward
    for (UInt j = 0; j < nlat-1; j++) {
      nconnect[0] = nodevect[i*nlat+j];
      nconnect[1] = nodevect[(i+1)*nlat+j];
      nconnect[2] = nodevect[(i+1)*nlat+j+1];
      nconnect[3] = nodevect[i*nlat+j+1];

      // Check for inclusion with lat lon
      bool add_elem = true;
      // if a latlon selector, investigate element.
      if (lf) {
        llnode &tc = nmap[nconnect[0]->get_id()];
        add_elem = (*lf)(tc.lat, tc.lon);
      }

      if (add_elem) {
        MeshObj *elem = new MeshObj(MeshObj::ELEMENT, cnt+1);
        mesh.add_element(elem, nconnect, blk, topo);
        cnt++;
      } 
    }
  } // elements

  // Now, some nodes may not have parents, so remove them.
  mesh.remove_unused_nodes();
  mesh.linearize_data_index();

  IOField<NodalField> *node_coord = mesh.RegisterNodalField(mesh, "coordinates", mesh.spatial_dim());

  MeshDB::const_iterator Ni = mesh.node_begin(), Ne = mesh.node_end();
  {
    for (; Ni != Ne; ++Ni) {
      double *c = node_coord->data(*Ni);
      llnode &tc = nmap[Ni->get_id()];
      c[0] = tc.x; c[1] = tc.y; c[2] = tc.z;
    }
  }


  nc_close(ncid);
#else
  Throw() << "please recompile with netcdf enabled";
#endif

}

bool LoadNCTData(MeshDB &mesh, 
                 const std::string &filename,  // file to read from
                 const std::vector<std::string> &vnames, // the names for each component
                 const MEField<> &field, int timestep)
{
  Trace __trace(" LoadNCTData(MeshDB &mesh, const std::string &filename,  std::vector<std::string> &vnames, const MEField<> &field, int timestep)");
#ifdef ESMC_NETCDF

  if (vnames.size() != field.dim()) 
    Throw() << "NCTData, vnamesize=" << vnames.size() << ", but field dim=" << field.dim();
  // open the netcdf file
  int ncid, stat;
  if ((stat = nc_open(filename.c_str(), NC_WRITE, &ncid)) != NC_NOERR) {
    Throw() << "Trouble opening " << filename << ", ncerr=" <<
               nc_strerror(stat);
  }

  int ntstepid;
  std::size_t ntstep;
  if ((stat = nc_inq_unlimdim(ncid, &ntstepid)) != NC_NOERR) {
    Throw() << "NCErr:" << nc_strerror(stat);
  } 
  nc_inq_dimlen(ncid, ntstepid, &ntstep);
std::cout << "ntsteps:" << ntstep << std::endl;
  if (timestep > (int) ntstep) return false;

  std::size_t nlat, nlon, nnodes, n;
  nnodes = mesh.num_nodes();
  int dimid;

  if ((stat = nc_inq_dimid(ncid, "lat", &dimid)) != NC_NOERR) {
    Throw() << "NCErr:" << nc_strerror(stat);
  } 
  nc_inq_dimlen(ncid, dimid, &nlat);
  
  if ((stat = nc_inq_dimid(ncid, "lon", &dimid)) != NC_NOERR) {
    Throw() << "NCErr:" << nc_strerror(stat);
  }
  nc_inq_dimlen(ncid, dimid, &nlon);

  n = nlat*nlon;

/*
  if (nnodes != nlat*nlon)
    Throw() << "nnodes=" << nnodes << ", but nlat*nlon=" << nlat*nlon;
*/

  // We read in all nodes.  We then sort and unique the nodes
  std::vector<double> ddata(field.dim()*n);
  std::vector<std::size_t> starts(3);
  std::vector<std::size_t> counts(3);
  starts[0] = timestep-1; starts[1] = 0; starts[2] = 0;
  counts[0] = 1; counts[1] = nlat; counts[2] = nlon;
  for (UInt d = 0; d < field.dim(); d++) {
    int varid;
    if ((stat = nc_inq_varid(ncid, vnames[d].c_str(), &varid)) != NC_NOERR) {
      Throw() << "NCErr:" << nc_strerror(stat);
    }
    if ((stat = nc_get_vara_double(ncid, varid, &starts[0], &counts[0], &ddata[d*n]) != NC_NOERR)) {
      Throw() << "NCErr:" << nc_strerror(stat);
    }
  } // for d

  // Load into mesh.  Assuming here that node data index was loaded as
  // lat fastest, lon next (see routine above).
  Mesh::iterator ni = mesh.node_begin(), ne = mesh.node_end();
  for (; ni != ne; ++ni) {
    const MeshObj &node = *ni;
    double *fval = field.data(node);
    std::div_t dt = std::div((int)node.get_id(), (int)nlat);
    UInt lt = dt.rem, ln = dt.quot;
//std::cout << "lt=" << lt << ", ln=" << ln << ", d=" <<  ddata[0*nnodes+lt*nlon+ln] << std::endl;
    for (UInt d = 0; d < field.dim(); d++) {
      fval[d] = ddata[(d*nlat+lt)*nlon+ln];
    }
  }

  // Loop field dims
  nc_close(ncid);
#else
  Throw() << "Please recompile with netcdf enabled";
#endif

  return true;
}

} // namespace
