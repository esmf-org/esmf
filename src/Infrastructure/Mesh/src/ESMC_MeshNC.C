// $Id: ESMC_MeshNC.C,v 1.3.2.2 2009/01/21 21:25:23 cdeluca Exp $
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
#include "ESMC_MeshNC.h"
#include "ESMC_MeshDB.h"

#include "ESMC_Exception.h"
#include "ESMC_MeshObjTopo.h"
#include <ESMC_MeshSkin.h>
#include "ESMC_MeshField.h"
#include <ESMC_Mesh.h>
#include <ESMC_IOField.h>

#ifdef ESMC_NETCDF
#include <netcdf.h>
#endif
#include <cmath>
#include <iostream>
#include <algorithm>
#include <set>

namespace ESMCI {
namespace MESH {

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

void LoadNCMesh(MeshDB &mesh, const std::string name, bool land, pole_func *pf, latlon_func *lf) {
#ifdef ESMC_NETCDF

  // open the netcdf file
  int ncid, stat;
  if ((stat = nc_open(name.c_str(), NC_WRITE, &ncid)) != NC_NOERR) {
    Throw() << "Trouble opening " << name << ", ncerr=" <<
               nc_strerror(stat);
  }

  std::size_t ni, nj, nv, n;

  // Get ni, nj; verify nv=4
  int dimid;
  // n
  if ((stat = nc_inq_dimid(ncid, "n", &dimid)) != NC_NOERR) {
    Throw() << "NCErr:" << nc_strerror(stat);
  }
  nc_inq_dimlen(ncid, dimid, &n);
std::cout << "n=" << n << std::endl;

  // ni
  if ((stat = nc_inq_dimid(ncid, "ni", &dimid)) != NC_NOERR) {
    Throw() << "NCErr:" << nc_strerror(stat);
  }
  nc_inq_dimlen(ncid, dimid, &ni);
std::cout << "ni=" << ni << std::endl;

  // nj
  if ((stat = nc_inq_dimid(ncid, "nj", &dimid)) != NC_NOERR) {
    Throw() << "NCErr:" << nc_strerror(stat);
  }
  nc_inq_dimlen(ncid, dimid, &nj);
std::cout << "nj=" << nj << std::endl;

  // nv
  if ((stat = nc_inq_dimid(ncid, "nv", &dimid)) != NC_NOERR) {
    Throw() << "NCErr:" << nc_strerror(stat);
  }
  nc_inq_dimlen(ncid, dimid, &nv);

  if (nv != 4) Throw() << "unexpected nv=" << nv << "; should be 4";

  // We read in all nodes.  We then sort and unique the nodes

  int xvid, yvid;
  if ((stat = nc_inq_varid(ncid, "xv", &xvid)) != NC_NOERR) {
    Throw() << "NCErr:" << nc_strerror(stat);
  }
  if ((stat = nc_inq_varid(ncid, "yv", &yvid)) != NC_NOERR) {
    Throw() << "NCErr:" << nc_strerror(stat);
  }

/*
  nc_type xtype;
  if ((stat = nc_inq_vartype(ncid, xvid, &xtype)) != NC_NOERR) {
    Throw() << "NCErr:" << nc_strerror(stat);
  }
*/
  
  std::vector<llnode> ll; ll.reserve(n*4); // all lat lons
  {
  std::vector<double> xncoord(4*n);
  std::vector<double> yncoord(4*n);
  nc_get_var_double(ncid, xvid, &xncoord[0]);
  nc_get_var_double(ncid, yvid, &yncoord[0]);
  UInt idx = 0;
  const double DEG2RAD = M_PI/180.0;
  for (UInt i = 0; i < ni; i++) {
    for (UInt j = 0; j < nj; j++) {
      for (UInt v = 0; v < nv; v++) {
        llnode ln; ln.idx = idx++;
        ln.lon = xncoord[(j*ni + i)*nv+v];
        ln.lat = yncoord[(j*ni + i)*nv+v];
        double theta = DEG2RAD*ln.lon, phi = DEG2RAD*(90-ln.lat);
        ln.x = std::cos(theta)*std::sin(phi);
        ln.y = std::sin(theta)*std::sin(phi);
        ln.z = cos(phi);
        ll.push_back(ln);
      } // nv
    } // for j
  } // for i

  } // bye to xncoord, yncoord

std::cout << "size of ll:" << ll.size() << std::endl;
  std::sort(ll.begin(), ll.end(), std::less<llnode>());

  // We travel the list and build an index vector
  std::vector<UInt> new_idx(ll.size());
  { // indexing loop
  std::vector<llnode>::iterator li = ll.begin();
  const std::vector<llnode>::iterator le = ll.end();
 
  UInt nidx = 0;
  for (; li != le; ++li) {
    const llnode &ln = *li;
//std::cout << "different" << std::endl;
    new_idx[ln.idx] = nidx;
    while (li != le && *(li + 1) == ln) {
      ++li;
      new_idx[li->idx] = nidx;
//std::cout << "\tsame" << std::endl;
    }
    nidx++;
    if (li == le) break;
  }


  ll.erase(std::unique(ll.begin(), ll.end()), ll.end());

  if (nidx != ll.size()) Throw() << "nidx=" << nidx << " should be ll.size()=" << ll.size();

  } // indexing 

  // We can now build the nodes.

  mesh.set_spatial_dimension(3);
  mesh.set_parametric_dimension(2);
  mesh.set_filename(name);

  std::vector<MeshObj*> nodevect; nodevect.reserve(ll.size());
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
    //node->add_data("coord", coord);
    int nset =0;
    // see if a pole.
    if (pf && (*pf)(&coord[0])) { std::cout << "node:" << node->get_id() << " a pole" << std::endl;  nset = 1; }
    mesh.add_node(node,nset); // TODO: more reasonable block #
    nodevect.push_back(node);
  } // for li
  } // nodecreate
  

  std::map<MeshObj::id_type, int> emap;
  // Loop through n, building 
  { // elements
  std::vector<int> mask(n);
  int maskid;
  if ((stat = nc_inq_varid(ncid, "mask", &maskid)) != NC_NOERR)
    Throw() << "no mask found!!";
  nc_get_var_int(ncid, maskid, &mask[0]);
  MeshObjTopo *topo= GetTopo("SHELL");
  MeshObjTopo *tri_topo= GetTopo("SHELL3");
  std::vector<MeshObj*> nconnect(4);
  std::set<MeshObj*> nset;
  int blk = 1;
  int blk2 = 2;
  UInt cnt = 0;
  for (UInt i = 0; i < ni; i++) {
    for (UInt j = 0; j < nj; j++) {
      nset.clear();
      for (UInt v = 0; v < nv; v++) {
        nconnect[v] = nodevect[new_idx[nv*cnt+v]];
        nset.insert(nodevect[new_idx[nv*cnt+v]]);
      } //nv
      // Check for something wierd.  If any two of the vertices are the
      // same, then we are probably at the pole (or some nasty coord singularity),
      // so make these triangles
      bool add_elem = false;
      if (land || (mask[j*ni+i] > 0.01)) {
        if (lf) {
          llnode &tc = nmap[nconnect[0]->get_id()];
          add_elem = (*lf)(tc.lat, tc.lon);
        }
        if (add_elem)
        if (nset.size() != nv) {
          //std::cout << "Must be tri, nset:" << nset.size() << std::endl;
          // move non uniques to end
          std::unique(nconnect.begin(), nconnect.end());
          // Build a triangle
          MeshObj *elem = new MeshObj(MeshObj::ELEMENT, cnt+1);
          //elem->add_data("block", blk2);
          //elem->add_data("mask", mask[j*ni+i]);
          emap[elem->get_id()] = mask[j*ni+i];
          mesh.add_element(elem, nconnect, blk2, tri_topo);
        } else {
          MeshObj *elem = new MeshObj(MeshObj::ELEMENT, cnt+1);
  /*
  if (cnt < 15)  {
          elem->add_data("block", blk2);
  } else
  */
          //elem->add_data("block", blk);
          //elem->add_data("mask", mask[j*ni+i]);
          emap[elem->get_id()] = mask[j*ni+i];
          mesh.add_element(elem, nconnect, blk, topo);
        }
        }
      cnt++;
    } // for j
  } // for i
  } // elements

  mesh.remove_unused_nodes();
  mesh.linearize_data_index();

  IOField<NodalField> *node_coord = new IOField<NodalField>(mesh, "coordinates", mesh.spatial_dim());

  MeshDB::const_iterator Ni = mesh.node_begin(), Ne = mesh.node_end();
  {
    for (; Ni != Ne; ++Ni) {
      double *c = node_coord->data(*Ni);
      llnode &tc = nmap[Ni->get_id()];
      c[0] = tc.x; c[1] = tc.y; c[2] = tc.z;
    }
  }

  // apply mask
  IOField<ElementField> *elem_mask = new IOField<ElementField>(mesh, "mask");
  elem_mask->set_output_status(true);
  MeshDB::const_iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
//std::cout << "ei=" << (*ei)->get_id() << ", ee=" << (*ee)->get_id() << std::endl;
//std::cout << "num elem:" << mesh.num_elems() << std::endl;
  for (; ei != ee; ++ei) {
    //int mask = ei->get_int("mask");
    int mask = emap[ei->get_id()];
    *(elem_mask->data(*ei)) = mask;
//std::cout << "el:" << (*ei)->get_id() << ", mask=" << mask << std::endl;
  }

  nc_close(ncid);

#endif
}

// Load a spectral mesh with only lat/lon, not cell info
void LoadNCTMesh(Mesh &mesh, const std::string name,
                 latlon_func *lf)
{
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

#endif
}

bool LoadNCTData(MeshDB &mesh, 
                 const std::string &filename,  // file to read from
                 const std::vector<std::string> &vnames, // the names for each component
                 const MEField<> &field, int timestep)
{
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

  return true;
#endif
}

} //namespace
} //namespace
