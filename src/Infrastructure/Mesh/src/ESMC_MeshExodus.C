// $Id: ESMC_MeshExodus.C,v 1.4.2.2 2009/01/21 21:25:23 cdeluca Exp $
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
#include <ESMC_MeshExodus.h>

#include <ESMC_MeshDB.h>
#include <ESMC_MeshObjTopo.h>
#include <iostream>
#include <stdexcept>
#include <sstream>
#include <iomanip>
#include <iterator>
#include <string>

#include <ESMC_MeshField.h>
#include <ESMC_MeshUtils.h>
#include <ESMC_MeshSkin.h>

#include <ESMC_Exception.h>
#include <ESMC_ParEnv.h>


#ifdef ESMC_EXODUS
extern "C" {
#include "exodusII.h"
}
#endif


namespace ESMCI {
namespace MESH {


static UInt share_parametric_dim(UInt pdim) {

  UInt nproc = Par::Size();

  if (nproc == 1) return pdim;

  int rank = pdim == 0 ? nproc : Par::Rank();
  int lowest_rank = nproc;

  // Find the minimum processor number that has a valid pdim

  MPI_Allreduce(&rank, &lowest_rank, 1, MPI_INT, MPI_MIN, Par::Comm());

  // Someone needs to have a valid pdim.
  ThrowRequire(lowest_rank < nproc);

  // Let that rank scatter the pdim
  int i_pdim = pdim;
  int i_pdim_r = 0;

  MPI_Scatter(&i_pdim, 1, MPI_INT, &i_pdim_r, 1, MPI_INT, lowest_rank, Par::Comm());

  return i_pdim_r;

}
 
void LoadExMesh(Mesh &mesh, const std::string &filename, int nstep) {
  Trace __trace("LoadExMesh(Mesh &mesh, const std::string &filename, int nstep)");
#ifdef ESMC_EXODUS

  char   title[250];
  int    ws1, ws2, exoid, exoerr;
  int    i, j, k, m, nblocks, nns, nss, nelems,
         npe, nattrs, *c0, *blk_id;
  float  version;
  int    *gg, *mm, num_nodes;
  double *xx, *yy, *zz;
  int ndim, nnodes;
  
  ws1    = sizeof(double);
  ws2    = 0;
  ex_opts(EX_VERBOSE);
  exoid  = ex_open(filename.c_str(), EX_READ, &ws1, &ws2, &version);
  if (exoid <= 0) {
    Throw() << "Could not open exodus file:" << filename << ", exoid=" << std::setw(3) << std::setfill('0')
               << exoid << std::endl;
  }

  exoerr = ex_get_init(exoid,title,&ndim,&nnodes,
                       &nelems,&nblocks,&nns, &nss);
  if (exoerr) {
    ex_err(filename.c_str(), "Exodus read error", exoerr);
    throw("LoadMeshDB exodus error");
  }
  num_nodes = nnodes;
/*
  printf("Title:%s\n", title);
  printf("\tnum_nodes:%d, ndim:%d, nblocks:%d\n", nnodes,
         ndim, nblocks);
  printf("\tnelem:%d, nside:%d, nns:%d\n", nelems, nss, nns);
*/

  mesh.set_spatial_dimension(ndim);

  std::vector<int> sids(nss,0);
  // find out how many cells in side sets
  int nscells = 0;
  int nsc, nsd;
  if (nss > 0) {
    ex_get_side_set_ids(exoid, &sids[0]);
    for (int l = 0; l < nss; l++) {
      ex_get_side_set_param(exoid, sids[l], &nsc, &nsd);
      nscells += nsc;
    }
  }

 // printf("\tnsidecell:%d\n", nscells);

  // Read the nodesets
  std::vector<int> nids(nns,0);
  if (nns > 0)
    ex_get_node_set_ids(exoid, &nids[0]);
  std::map<UInt, int> nodesets;
  for (int i = 0; i < nns; i++) {
   // std::cout << "Nodeset:" << nids[i] << std::endl;
    int num_nd, num_df;
    ex_get_node_set_param(exoid, nids[i], &num_nd, &num_df);
    std::vector<int> node_list(num_nd, 0);
    ex_get_node_set(exoid, nids[i], &node_list[0]);
    // Loop nodes.  Put them in a map
    for (int j = 0; j < num_nd; j++) {
      nodesets[node_list[j]] = nids[i];
/*
std::cout << "node_list:" << node_list[j] << " has members:";
std::copy(nids.begin(), nids.end(), std::ostream_iterator<int>(std::cout, " "));
std::cout << std::endl;
*/
    }
  }


  // Done Reading node vars



  /* READ THE NODE COORDINATES */
  xx = new double[nnodes];
  yy = new double[nnodes];
  zz = new double[nnodes];
  for (i=0; i<num_nodes; i++) {
    xx[i] = 0.0;
    yy[i] = 0.0;
    zz[i] = 0.0;
  }
  exoerr = ex_get_coord(exoid,xx,yy,zz);

  mm     = new int[nnodes+1];
  exoerr = ex_get_node_num_map(exoid, mm);
  std::vector<MeshObj*> nodevect;  // temporary indexer
  nodevect.reserve(num_nodes);

  for (i=0; i < num_nodes; i++) {
    std::vector<double> coords(3);
    coords[0] = xx[i]; coords[1] = yy[i]; coords[2] = zz[i];
    // ,i here is to set aside the original numbering, used to load
    // fields below.
    MeshObj *node = new MeshObj(MeshObj::NODE, mm[i],i);
    //node->add_data("coord", coords);
    int nset = 0;
    std::map<UInt, int>::iterator nsi =
      nodesets.find(i+1);
    if (nsi != nodesets.end()) nset = nsi->second;
    mesh.add_node(node, nset);
    nodevect.push_back(node);
  }
  // clear memory
  std::map<UInt, int>().swap(nodesets);

  gg     = new int[nelems+1];
  exoerr = ex_get_elem_num_map(exoid, gg);

  /* READ THE ELEM CONNECTIVITIES */
  blk_id = new int[nblocks];
  exoerr = ex_get_elem_blk_ids (exoid, blk_id);
  int eid = 0;
  //cc = new int[8*nelems]; // more than necessary
  std::vector<MeshObj*> elemvect;  // temporary indexer
  elemvect.reserve(nelems);
  std::vector<int> nbelems(nblocks);
  for (m=0, i=0; i<nblocks; i++) {
    //printf("Processing block:%d\n", blk_id[i]);
    char eltype[512];
    exoerr = ex_get_elem_block (exoid, blk_id[i], eltype,
                                &nbelems[i], &npe, &nattrs);
    //printf("Element type:%s\n", eltype);
    //printf("\tnpe:%d, nel:%d\n", npe, nbelems[i]);
    c0 = new int[npe*nbelems[i]];
    exoerr = ex_get_elem_conn(exoid,blk_id[i],c0);

    for (j = 0; j < nbelems[i]; j++) {
      MeshObjTopo *topo = GetTopo(eltype);
      MeshObj *elem = new MeshObj(MeshObj::ELEMENT, gg[eid], eid);
      //elem->add_data("block", blk_id[i]);
      elemvect.push_back(elem);
      std::vector<MeshObj*> nconnect;
      eid++;
      nconnect.clear();
      for (k = 0; k < npe; k++) {
        //cc[(eid-1)*npe+k] = mm[c0[j*npe+k]-1];
        nconnect.push_back(nodevect[c0[j*npe+k]-1]);
      }
      mesh.add_element(elem, nconnect, blk_id[i], topo);
    }
    delete [] c0;
  }

  delete [] gg;

  UInt pdim_tmp = 0;
  if (nelems > 0) {
    pdim_tmp = GetMeshObjTopo(*mesh.elem_begin())->parametric_dim;
  }

  // In case someone doesn't have an element, share this
  pdim_tmp = share_parametric_dim(pdim_tmp);

  mesh.set_parametric_dimension(pdim_tmp);

  // Now popoulate existing sidesets
  int *elist, *esides;
  for (int l = 0; l < nss; l++) {
    ex_get_side_set_param(exoid, sids[l], &nsc, &nsd);
    
    // print the side set
    elist = new int[nsc];
    esides = new int[nsc];
    ex_get_side_set(exoid, sids[l], elist, esides);

    for (int q = 0; q < nsc; q++) {
//std::cout << "side elem:"<< elist[q] << std::endl;
    MeshObj *element = elemvect[elist[q] - 1];
    const MeshObjTopo * const topo = GetMeshObjTopo(*element);
    const MeshObjTopo * const stopo = topo->side_topo(esides[q]-1);
    
    MeshObj::MeshObjType tp = topo->parametric_dim == 2 ? MeshObj::EDGE : MeshObj::FACE;
    // side will be globally consistent, but needs a global number
    MeshObj *side = new MeshObj(tp, mesh.get_new_local_id(tp));
//Par::Out() << "create side, id=" << side->get_id() << std::endl;

    // Must add sides local, because they require global numbering;
    // this is not provided in exodus file.
    mesh.add_side_local(*side, *element, esides[q] - 1, sids[l], stopo);
    }
    delete [] elist;
    delete [] esides;
  }



  // Set up a coordinates field
  IOField<NodalField> *node_coord = mesh.RegisterNodalField(mesh, "coordinates", mesh.spatial_dim());

  // Use data_index, since we have stored the original orderinng there.
  MeshDB::const_iterator ni = mesh.node_begin(), ne = mesh.node_end();
  {
  UInt i = 0;
    for (; ni != ne; ++ni) {
      //double *c = &node_coord->raw_data()[i*node_coord->dim()];
      double *c = node_coord->data(*ni);
      //UInt idx = id2idx[(*ni)->get_id()];
      //UInt idx = i;
      UInt idx = ni->get_data_index();
      c[0] = xx[idx];
      if (mesh.spatial_dim() >= 2) c[1] = yy[idx];
      if (mesh.spatial_dim() >= 3) c[2] = zz[idx];
      i++;
    }
  }

  // Don't worry, the field is stored in the registry

  delete [] xx; delete [] yy; delete [] zz;


  // Read the node variables.  Will use these to set owner attribute of node
  {
    int num_nodal_vars;
    ex_get_var_param(exoid, "n", &num_nodal_vars);
 // std::cout << "Num nodal vars:" << num_nodal_vars << std::endl;
  
    if (num_nodal_vars > 0) { 
      std::vector<char*> snames(num_nodal_vars);
    
      for (UInt i = 0; i < (UInt) num_nodal_vars; i++) {
        snames[i]= new char[MAX_STR_LENGTH];
      }
    
      ex_get_var_names(exoid, "n", num_nodal_vars, &snames[0]);
    
  /*  std::cout << "var names:";
      std::copy(snames.begin(), snames.end(), std::ostream_iterator<char*>(std::cout, ","));
    std::cout <<std::endl;
*/
    
      // For now, we merely copy these variables into fields.  TODO: we
      // should detect vector variables (_1,_2,_3) and read them into
      // a single field.
     
      // We stored the original index in data_index for the time being.
      std::vector<double> tdata(num_nodes, 0);
      for (UInt i = 0; i < (UInt) num_nodal_vars; i++) {
        // Load this field too!! if (snames[i] != "_OWNER") {
          IOField<NodalField> *nfield = mesh.RegisterNodalField(mesh, snames[i]);
          nfield->set_output_status(true);
          //ex_get_nodal_var(exoid, time, i+1, num_nodes, nfield->raw_data());
          ex_get_nodal_var(exoid, nstep, i+1, num_nodes, &tdata[0]);
          // Now assign to field
          MeshDB::iterator ni = mesh.node_begin(), ne = mesh.node_end();
          UInt j = 0;
  // TODO: don't save owner as a field, but write it out later as one.
          bool owner_field = (snames[i] == std::string("_OWNER"));
          for (; ni != ne; ++ni) {
            if (owner_field) ni->set_owner((UInt) (tdata[ni->get_data_index()] + 0.001));
            double *d = nfield->data(*ni);
            *d = tdata[ni->get_data_index()];
            j++;
          }
        // }
      }
      
    
      for (UInt i = 0; i < (UInt) num_nodal_vars; i++) {
        delete [] snames[i];
      }
  
    } // num nodal vars > 0
  
  } // nodal vvars

  // Read the element variables.  
  {
    int num_elem_vars;
    ex_get_var_param(exoid, "e", &num_elem_vars);
 // std::cout << "Num elem vars:" << num_elem_vars << std::endl;
  
    if (num_elem_vars > 0) {
    
      std::vector<char*> snames(num_elem_vars);
    
      for (UInt i = 0; i < (UInt) num_elem_vars; i++) {
        snames[i]= new char[MAX_STR_LENGTH];
      }
    
      ex_get_var_names(exoid, "e", num_elem_vars, &snames[0]);
    
/*
    std::cout << "elem var names:";
      std::copy(snames.begin(), snames.end(), std::ostream_iterator<char*>(std::cout, ","));
    std::cout <<std::endl;
*/
    
      // For now, we merely copy these variables into fields.  TODO: we
      // should detect vector variables (_1,_2,_3) and read them into
      // a single field.
     
      // We stored the original index in data_index for the time being.
      std::vector<double> tdata(nelems, 0);
      for (UInt i = 0; i < (UInt) num_elem_vars; i++) {
        // Load this field too!! if (snames[i] != "_OWNER") {
          IOField<ElementField> *efield = mesh.RegisterElementField(mesh, snames[i]);
          efield->set_output_status(true);
          //ex_get_nodal_var(exoid, time, i+1, num_nodes, nfield->raw_data());
          UInt cur_elem = 0;
          for (UInt b = 0; b < (UInt) nblocks; b++) {
            ex_get_elem_var(exoid, nstep, i+1, blk_id[b], nbelems[b], &tdata[cur_elem]);
            cur_elem += nbelems[b];
          }
          // Now assign to field
          MeshDB::const_iterator ni = mesh.elem_begin(), ne = mesh.elem_end();
          UInt j = 0;
          for (; ni != ne; ++ni) {
            double *d = efield->data(*ni);
            d[0] = tdata[ni->get_data_index()];
            j++;
          }
        // }
      }
      
    
      for (UInt i = 0; i < (UInt) num_elem_vars; i++) {
        delete [] snames[i];
      }
    } // num elem vars > 0
  
  } // elem vvars

  ex_close(exoid);

  std::cout << "MeshExodus Load file " << filename << " complete." << std::endl;

#endif
}

// Load data into an array from the mesh, switching on the correct typeid
template<typename iter, typename FIELD>
void get_data(iter ni, iter ne, const FIELD &llf, double data[], UInt d) {
  Trace __trace("get_data(iter ni, iter ne, const FIELD &llf, double data[], UInt d)");

  UInt i = 0;
  if (llf.tinfo() == typeid(double)) {
    for (; ni != ne; ++ni) {
      if (llf.OnObj(*ni))
        data[i] = ((double*)llf.data(*ni))[d];
      else data[i] = 0;
      i++;
    }
  } else if (llf.tinfo() == typeid(int)) {
    for (; ni != ne; ++ni) {
      if (llf.OnObj(*ni))
      data[i] = ((int*)llf.data(*ni))[d];
      else data[i] = 0;
      i++;
    }
  } else if (llf.tinfo() == typeid(float)) {
    for (; ni != ne; ++ni) {
      if (llf.OnObj(*ni))
      data[i] = ((float*)llf.data(*ni))[d];
      else data[i] = 0;
      i++;
    }
  } else if (llf.tinfo() == typeid(long)) {
    for (; ni != ne; ++ni) {
      if (llf.OnObj(*ni))
      data[i] = ((long*)llf.data(*ni))[d];
      else data[i] = 0;
      i++;
    }
  } else if (llf.tinfo() == typeid(char)) {
    for (; ni != ne; ++ni) {
      if (llf.OnObj(*ni))
      data[i] = ((char*)llf.data(*ni))[d];
      else data[i] = 0;
      i++;
    }
  } else if (llf.tinfo() == typeid(UChar)) {
    for (; ni != ne; ++ni) {
      if (llf.OnObj(*ni))
      data[i] = ((UChar*)llf.data(*ni))[d];
      else data[i] = 0;
      i++;
    }
  } else std::cout << "Unknown data type, skipping ";
}

void WriteExMesh(const Mesh &mesh, const std::string &filename, int nstep, double tstep) {
  Trace __trace("WriteExMesh(const Mesh &mesh, const std::string &filename, int nstep, double tstep)");
#ifdef ESMC_EXODUS

  int wsize = sizeof(double);
  int ex_id = ex_create(filename.c_str(), EX_CLOBBER, &wsize, &wsize);

  char *cnames[3] = {"x", "y","z"};
  ex_put_init(ex_id, filename.c_str(),
                      mesh.spatial_dim(),
                      mesh.num_nodes(),
                      mesh.num_elems(),
                      mesh.num_blocks(),
                      mesh.num_node_sets(),
                      mesh.num_side_sets()
                      );

  ex_put_coord_names(ex_id,cnames);

  std::vector<std::vector<double> > coords(3);
  GetMeshCoords(mesh, coords[0], coords[1], coords[2]);

  ex_put_coord(ex_id, &(coords[0])[0], &(coords[1])[0], &(coords[2])[0]);

  // Node number and element number maps
  {
  std::vector<int> node_ids(mesh.num_nodes());
  MeshDB::const_iterator ni = mesh.node_begin(), ne = mesh.node_end();
  for (UInt q = 0; ni != ne; ni++) {
    node_ids[q] = ni->get_id();
    q++;
  }

  ex_put_node_num_map(ex_id, &node_ids[0]);

  std::vector<int> elem_ids(mesh.num_elems());
  MeshDB::const_iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
  for (UInt q = 0; ei != ee; ei++) {
    elem_ids[q] = ei->get_id();
    q++;
  }

  ex_put_elem_num_map(ex_id, &elem_ids[0]);
  }
  

  // Since relations are stored by id, but exodus wants connectivity by index,
  // store a map from one to other.
  std::map<int, int> id2ord;
  int i = 0;
  MeshDB::const_iterator nd = mesh.node_begin(), ne = mesh.node_end();
  for (; nd != ne; ++nd) {
    id2ord[nd->get_id()] = i++;
  }

  for (KernelList::const_iterator mi = mesh.set_begin(); mi != mesh.set_end(); mi++) {
    //if (mi->empty()) continue;
    if (mi->type() != MeshObj::ELEMENT || !mi->is_active()) continue;
    const std::string &elname = GetMeshObjTopo(*mi->objects.begin())->name;
    int npe = GetMeshObjTopo(*mi->objects.begin())->num_nodes;
    int blockid = mi->key();
    int nelems = mesh.num_elems(blockid);
  
    // Only report new blocks
    ex_put_elem_block(ex_id, blockid, elname.c_str(), nelems, npe, 0);

    // Build the connectivity list
    int el = 0;
    std::vector<int> conn(npe*nelems);
    for (; el < nelems; ) {
      for (MeshObjList::const_iterator moi = mi->objects.begin(); moi != mi->objects.end(); moi++) {
        const MeshObj &elem = *moi;
        for (int n = 0; n < npe; n++) {
          const MeshObj &node = *(elem.Relations[n].obj);
          conn[el*npe+n] = id2ord[node.get_id()] + 1; // make 1 basec
        }
        el++;
      }
      if (el < nelems) mi++;  // move to the next set, which must be of same key
    }

    ex_put_elem_conn(ex_id, blockid, &conn[0]);

  }

  // Process node sets.
  {
    KernelList::const_iterator mi = mesh.set_begin(), me = mesh.set_end();
    for (; mi != me; ++mi) {
      //if (mi->empty()) continue;
      if (mi->type() != (UInt)MeshObj::NODE || !mi->is_active()) continue;
      int nodeid = mi->key();
      // Don't save sideset 0, since this is an internal
      // set.
      if (nodeid == 0) continue;
  
      int nnd = mesh.num_nodes(nodeid);
//std::cout << "Saving nodeset:" << nodeid << ", with " << nnd << " members" << std::endl;
      int errval = ex_put_node_set_param(ex_id, nodeid, nnd, 0);
      ex_err("david", "err:", errval);
      // Build the connectivity list
      std::vector<int> nod(nnd);
      int nd = 0;
      for (; nd < nnd; ) {
        for (MeshObjList::const_iterator moi = mi->objects.begin(); moi != mi->objects.end(); moi++) {
          const MeshObj &node = *moi;
          nod[nd] = id2ord[node.get_id()]+1;
          nd++;
        } 
        if (nd < nnd) mi++;  // move to the next set, which must be of same key
      }
  
      errval = ex_put_node_set(ex_id, nodeid, &nod[0]);
      ex_err("david", "err:", errval);
    }
  }

  // Wonderful.  Now process the side sets
  std::map<int, int> el_id2ord;
  i = 0;
  {
  MeshDB::const_iterator nd = mesh.elem_begin(), ne = mesh.elem_end();
  for (; nd != ne; ++nd) {
    el_id2ord[nd->get_id()] = i++;
  }
  }

  KernelList::const_iterator mi = mesh.set_begin(), me = mesh.set_end();
  for (; mi != me; ++mi) {
      //if (mi->empty()) continue;
    if (mi->type() != (UInt)mesh.side_type() || !mi->is_active()) continue;
    int sideid = mi->key();
    // Don't save sideset 0, since this is an internal
    // set.
    if (sideid == 0) continue;

    int nfaces = mesh.num_sides(sideid);
    ex_put_side_set_param(ex_id, sideid, nfaces, 0);

    // Build the connectivity list
    std::vector<int> elem(nfaces);
    std::vector<int> side(nfaces);
    int el = 0;
    for (; el < nfaces; ) {
      for (MeshObjList::const_iterator moi = mi->objects.begin(); moi != mi->objects.end(); moi++) {
        const MeshObj &face = *moi;
        // Get first element relation (we use first element for face).
        const MeshObj *elptr = NULL;
        int ordinal;
        for (MeshObjRelationList::const_iterator rel = face.Relations.begin(); rel != face.Relations.end(); rel++) {
          if ((*rel).obj->get_type() == MeshObj::ELEMENT) {
            elptr = (*rel).obj;
            ordinal = (*rel).ordinal;
            break;
          }
        }
        if (elptr == NULL) {
          Par::Out() << "MeshWrite, couldnt get element from side:" << face.get_id();
          Par::Out() << face;
          Throw() << "MeshWrite, couldnt get element from side:" << face.get_id();
        }
        elem[el] = el_id2ord[elptr->get_id()] + 1;
        side[el] = ordinal + 1;
        
        el++;
      } 
      if (el < nfaces) mi++;  // move to the next set, which must be of same key
    }

    ex_put_side_set(ex_id, sideid, &elem[0], &side[0]);

  }


  if (nstep != 0) ex_put_time(ex_id, nstep, &tstep);
//std::cout << "nstep=" << nstep << ", tstep=" << tstep << std::endl;
  // Now look into nodal variables.  If a variable is not element, then
  // project it onto the std lagrange variable for saving to file.( TODO)
  // For now, just save all nodal variables.
  {
  FieldReg::MEField_const_iterator nv = mesh.Field_begin(), ne = mesh.Field_end();
  UInt nvars_to_output = 0;
  std::vector<char*> var_names_ptr;
  UInt cur_var = 0;
  for (; nv != ne; ++nv) {
    const MEField<> &mf = *nv;
    if (mf.Output() && mf.is_nodal()) {
      nvars_to_output += mf.dim();
      if (mf.dim() > 1) {
        for (UInt d = 0; d < mf.dim(); d++) {  
          char *buf = new char[1024];
          std::sprintf(buf, "%s_%d", mf.name().c_str(), d);
          var_names_ptr.push_back(buf);
          //std::cout << "will output variable:" << buf << std::endl;
        }
      } else {
          char *buf = new char[1024];
          std::strcpy(buf, mf.name().c_str());
          var_names_ptr.push_back(buf);
          //std::cout << "will output variable:" << mf.name() << " =? " << var_names_ptr.back() << std::endl;
      }
    }
  }

/*
for (UInt i = 0; i < nvars_to_output; i++) {
  std::cout << "P:" << Par::Rank() << ", var=<" << var_names_ptr[i] << ">" << std::endl;
}
*/

  if (nvars_to_output > 0) {
    UInt nnodes = mesh.num_nodes();
    std::vector<double> data(nnodes);
    ex_put_var_param(ex_id, "n", nvars_to_output);
    ex_put_var_names(ex_id, "n", nvars_to_output, &var_names_ptr[0]);
    // Write data if nstep != 0
    if (nstep != 0) {
    cur_var = 1;
    for (nv = mesh.Field_begin(); nv != ne; ++nv) {
      const MEField<> &mf = *nv;
      if (mf.Output() && mf.is_nodal()) {
        const _field &llf = mf();
        if (mf.dim() > 1) {
          for (UInt d = 0; d < mf.dim(); d++) {  
            // We have to slice the data
            MeshDB::const_iterator ni = mesh.node_begin(), ne = mesh.node_end();
            get_data(ni, ne, llf, &data[0], d);
            ex_put_nodal_var(ex_id, nstep, cur_var++, nnodes, &data[0]);
          }
        } else {
            MeshDB::const_iterator ni = mesh.node_begin(), ne = mesh.node_end();
            get_data(ni, ne, llf, &data[0], 0);
            ex_put_nodal_var(ex_id, 1, cur_var++, nnodes, &data[0]);
        }
      }
    }
  }
  
    for (UInt i = 0; i < var_names_ptr.size(); i++)
      delete [] var_names_ptr[i];
  } // if nstep != 0
  }

  // Now look into Element variables
  {
  FieldReg::MEField_const_iterator nv = mesh.Field_begin(), ne = mesh.Field_end();
  UInt nvars_to_output = 0;
  std::vector<char*> var_names_ptr;
  UInt cur_var = 0;
  for (; nv != ne; ++nv) {
    const MEField<> &mf = *nv;
    if (mf.Output() && mf.is_elemental()) {
      nvars_to_output += mf.dim();
      if (mf.dim() > 1) {
        for (UInt d = 0; d < mf.dim(); d++) {  
          char *buf = new char[1024];
          std::sprintf(buf, "%s_%d", mf.name().c_str(), d);
          var_names_ptr.push_back(buf);
          //std::cout << "will output element variable:" << buf << std::endl;
        }
      } else {
          char *buf = new char[1024];
          std::strcpy(buf, mf.name().c_str());
          var_names_ptr.push_back(buf);
          //std::cout << "will output element variable:" << mf.name() << std::endl;
      }
    }
  }

  if (nvars_to_output > 0) {
    UInt nelem = mesh.num_elems();
    std::vector<double> data(nelem);
    ex_put_var_param(ex_id, "e", nvars_to_output);
    ex_put_var_names(ex_id, "e", nvars_to_output, &var_names_ptr[0]);

    // Write data && nstep != 0
    if (nstep != 0) {

    // Get a list of the blocks in mesh
    std::vector<UInt> blocks;
    {
       KernelList::const_iterator bi = mesh.set_begin(), be = mesh.set_end();
       for (; bi != be; ++bi) {
         if (bi->type() != MeshObj::ELEMENT) continue;
         std::vector<UInt>::iterator lb =
           std::lower_bound(blocks.begin(), blocks.end(), bi->key());
         if (lb == blocks.end() || *lb != bi->key())
           blocks.insert(lb, bi->key());
       }
    }

    for (UInt b = 0; b < blocks.size(); b++) {
      UInt block_num = blocks[b];
      UInt nelems_blk = mesh.num_elems(block_num);

      cur_var = 1;

      // Loop fields, write out for this block
      for (nv = mesh.Field_begin(); nv != ne; ++nv) {
        const MEField<> &mf = *nv;

        for (UInt d = 0; d < mf.dim(); d++) {
          if (mf.Output() && mf.is_elemental()) {
            const _field &llf = mf();
  
            KernelList::const_iterator bi = mesh.set_begin(), be = mesh.set_end();
  
            // Gather data for variable
            UInt cur_off = 0;
            for (; bi != be; ++bi){
              if (!bi->is_active() || bi->type() != MeshObj::ELEMENT || bi->key() != block_num) continue;

              get_data(bi->obj_begin(), bi->obj_end(), llf, &data[cur_off], d);
              cur_off += bi->Count();

              ThrowRequire(cur_off <= nelems_blk);

            } // blocks

            ex_put_elem_var(ex_id, nstep, cur_var, block_num, nelems_blk, &data[0]);
  
            cur_var++;
          } // variable fits
        } // for dim
      } // nv
    } // blocks
  } // nstep != 0
  
    for (UInt i = 0; i < nvars_to_output; i++)
      delete [] var_names_ptr[i];
  }

  } // element vars

  ex_close(ex_id);
#endif
}

void WriteExMeshTimeStep(int nstep, double tstep, const Mesh &mesh, const std::string &filename) {
  Trace __trace("WriteExMeshTimeStep(int nstep, double tstep, const Mesh &mesh, const std::string &filename)");
#ifdef ESMC_EXODUS

  int    ws1, ws2, ex_id;
  float  version;

  ws1    = sizeof(double);
  ws2    = 0;
  ex_opts(EX_VERBOSE);
  ex_id  = ex_open(filename.c_str(), EX_WRITE, &ws1, &ws2, &version);
  if (ex_id <= 0) {
    Throw() << "Could not open exodus file:" << filename << ", exoid=" << std::setw(3) << std::setfill('0')
               << ex_id << std::endl;
  }

  ex_put_time(ex_id, nstep, &tstep);
  // Now look into nodal variables
  {
  FieldReg::MEField_const_iterator nv = mesh.Field_begin(), ne = mesh.Field_end();
  UInt nvars_to_output = 0;
  std::vector<char*> var_names_ptr;
  UInt cur_var = 0;
  for (; nv != ne; ++nv) {
    const MEField<> &mf = *nv;
    if (mf.Output() && mf.is_nodal()) {
      nvars_to_output += mf.dim();
      if (mf.dim() > 1) {
        for (UInt d = 0; d < mf.dim(); d++) {  
          char *buf = new char[1024];
          std::sprintf(buf, "%s_%d", mf.name().c_str(), d);
          var_names_ptr.push_back(buf);
          //std::cout << "will output variable:" << buf << std::endl;
        }
      } else {
          char *buf = new char[1024];
          std::strcpy(buf, mf.name().c_str());
          var_names_ptr.push_back(buf);
          //std::cout << "will output variable:" << mf.name() << std::endl;
      }
    }
  }

  if (nvars_to_output > 0) {
    UInt nnodes = mesh.num_nodes();
    std::vector<double> data(nnodes);
    //ex_put_var_param(ex_id, "n", nvars_to_output);
    //ex_put_var_names(ex_id, "n", nvars_to_output, &var_names_ptr[0]);
    // Write data
    cur_var = 1;
    for (nv = mesh.Field_begin(); nv != ne; ++nv) {
      const MEField<> &mf = *nv;
      if (mf.Output() && mf.is_nodal()) {
        const _field &llf = mf();
        if (mf.dim() > 1) {
          for (UInt d = 0; d < mf.dim(); d++) {  
            // We have to slice the data
            MeshDB::const_iterator ni = mesh.node_begin(), ne = mesh.node_end();
            get_data(ni, ne, llf, &data[0], d);
            ex_put_nodal_var(ex_id, nstep, cur_var++, nnodes, &data[0]);
          }
        } else {
            MeshDB::const_iterator ni = mesh.node_begin(), ne = mesh.node_end();
            get_data(ni, ne, llf, &data[0], 0);
            ex_put_nodal_var(ex_id, nstep, cur_var++, nnodes, &data[0]);
        }
      }
    }
    for (UInt i = 0; i < nvars_to_output; i++) 
      delete [] var_names_ptr[i];
  }
  }

  // Now look into Element variables
  {
  FieldReg::MEField_const_iterator nv = mesh.Field_begin(), ne = mesh.Field_end();
  UInt nvars_to_output = 0;
  std::vector<char*> var_names_ptr;
  UInt cur_var = 0;
  for (; nv != ne; ++nv) {
    const MEField<> &mf = *nv;
    if (mf.Output() && mf.is_elemental()) {
      nvars_to_output += mf.dim();
      if (mf.dim() > 1) {
        for (UInt d = 0; d < mf.dim(); d++) {  
          char *buf = new char[1024];
          std::sprintf(buf, "%s_%d", mf.name().c_str(), d);
          var_names_ptr.push_back(buf);
          //std::cout << "will output element variable:" << buf << std::endl;
        }
      } else {
          char *buf = new char[1024];
          std::strcpy(buf, mf.name().c_str());
          var_names_ptr.push_back(buf);
          //std::cout << "will output element variable:" << mf.name() << std::endl;
      }
    }
  }

  if (nvars_to_output > 0) {
    UInt nelem = mesh.num_elems();
    std::vector<double> data(nelem);
    //ex_put_var_param(ex_id, "e", nvars_to_output);
    //ex_put_var_names(ex_id, "e", nvars_to_output, &var_names_ptr[0]);
    // Write data
    cur_var = 1;
    for (nv = mesh.Field_begin(); nv != ne; ++nv) {
      const MEField<> &mf = *nv;
      if (mf.Output() && mf.is_elemental()) {
        const _field &llf = mf();
        // Loop the blocks
        KernelList::const_iterator bi = mesh.set_begin(), be = mesh.set_end();
        UInt cur_elem = 0;
        UInt cur_var_save = cur_var;
        for (; bi != be; ++bi){
          if (bi->type() != MeshObj::ELEMENT) continue;
          cur_var = cur_var_save; // go through var(s) again for new block
          UInt block_num = bi->key();
          UInt nelems_blk = mesh.num_elems(block_num);
          //UInt block_num = (*bi->second->start)->get_int("block");
          if (mf.dim() > 1) {
            for (UInt d = 0; d < mf.dim(); d++) {  
              // We have to slice the data
              get_data(bi->obj_begin(), bi->obj_end(), llf, &data[0], d);
              ex_put_elem_var(ex_id, nstep, cur_var++, block_num, nelems_blk, &data[0]);
            }
          } else {
              get_data(bi->obj_begin(), bi->obj_end(), llf, &data[0], 0);
              ex_put_elem_var(ex_id, nstep, cur_var++, block_num, nelems_blk, &data[0]);
          }
          cur_elem += nelems_blk;
          // Forward to next block.  TODO: correct handling once fields are on kernels
          KernelList::const_iterator bip1 = bi;
          bip1++;
          if ((bip1) != mesh.set_end() &&
              bip1->key() == block_num) bi++;
        } // blocks
      }
    }
    for (UInt i = 0; i < nvars_to_output; i++)
      delete [] var_names_ptr[i];
  }
  }

  ex_close(ex_id);

#endif
}

} //namespace
} //namespace
