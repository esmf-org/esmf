// $Id: ESMC_MeshPartition.C,v 1.4.2.2 2009/01/21 21:25:23 cdeluca Exp $
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
#include <ESMC_MeshPartition.h>
#include <ESMC_MeshObjConn.h>

#include <ESMC_MeshDB.h>
#include <ESMC_MeshObjTopo.h>

#include <ESMC_MeshField.h>
#include <ESMC_MeshExodus.h>
#include <ESMC_MeshUtils.h>
#include <ESMC_MeshRead.h>

#include <iostream>
#include <iterator>

#include <limits>
#include <iomanip>
#include <sstream>
#include <set>

// Easier than tracking down prototype
extern "C" {
typedef int idxtype;
void METIS_PartGraphKway(int *, idxtype *, idxtype *, idxtype *, idxtype *, int *, int *, int *, int *, int *, idxtype *);
}

namespace ESMCI {
namespace MESH {

void GetMetisPartition(const Mesh &mesh, UInt npart, const MEField<> &ep);

void MeshMetisPartition(const Mesh &mesh, UInt npart, const MEField<> &ep, const MEField<> &np) {

  GetMetisPartition(mesh, npart, ep);
  std::cout << "MeshDB Partitioned, saving partitions..." << std::endl;

  SavePartition(mesh, npart, ep, np);
}


void GetMetisPartition(const Mesh &mesh, UInt npart, const MEField<> &ep) {
#ifdef ESMC_METIS
//mesh.Print();


  // Step One is to get the partition array
  int nelem = mesh.num_elems();

  std::vector<int> xadj(nelem+1, 0);
  std::vector<int> adjncy;
  adjncy.reserve(2*nelem);  // use push back, not sure of size
  MeshDB::const_iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
  long i = 0, next_elem = 0, lcount = 0;
  for (; ei != ee; ei++) {
    xadj[i] = next_elem;
    const MeshObj &elem = *ei;
//std::cout << "i=" << i << ", get data index=" << elem.get_data_index() << std::endl;
    if (i != elem.get_data_index()) 
      throw("MeshDB partition assumes order=get_data_index order");

    // Find the neighbor elements
    const MeshObjTopo &topo = *GetMeshObjTopo(elem);

    // Loop sides, see if an element is connected next door
    for (UInt s = 0; s < topo.num_sides; s++) {
      // Is the an element next door?
      const MeshObj *neighbor = MeshObjConn::opposite_element(elem, s);
      if (neighbor == NULL) continue; // nothing
      
      // Otherwise add to adjncy list
      adjncy.push_back(neighbor->get_data_index());
      lcount++;
    }

    next_elem += lcount; lcount = 0;
    i++;
  } // for elem
  xadj[nelem] = adjncy.size(); // not sure why needed

/*
  std::cout << "MeshDB xadj:" << std::endl;
  std::copy(xadj.begin(), xadj.end(), std::ostream_iterator<int>(std::cout, ", "));
  std::cout << std::endl;
  std::cout << "Edges:" << std::endl;
  std::copy(adjncy.begin(), adjncy.end(), std::ostream_iterator<int>(std::cout, ", "));
  std::cout << std::endl;
*/

  std::vector<int> part;
  part.resize(nelem,-1);
  // Call Metis
  {
    int wgtflag = 0, numflag = 0, options[5], edgecut = 0;
    options[0] = 0;

    int npart1 = npart;
    METIS_PartGraphKway(&nelem, &xadj[0], &adjncy[0], NULL, NULL, &wgtflag,
               &numflag, &npart1, options, &edgecut, &part[0]);
  }

  // Load data onto elements
  i = 0;
  ei = mesh.elem_begin();
  for (;ei != ee; ++ei) {
    const MeshObj &elem = *ei;
    int *epdata = ep.data(elem);
    epdata[0] = part[i];
    i++;
  }

#endif
}


void SavePartition(const Mesh &mesh, UInt npart, const MEField<> &ep, const MEField<> &np) {
  // Declare an element field
  UInt num_nodes = mesh.num_nodes();

  // Step Two: loop nodes, determine an owner.  To do this, we loop all element
  // used_by relations and take the one with the lowest Part val
  MEField<> &coord_field = *mesh.GetCoordField();

  MeshDB::const_iterator nb = mesh.node_begin(), ne = mesh.node_end();
  for (; nb != ne; ++nb) {
    const MeshObj &node = *nb;
//std::cout << "node:" << node.get_id() << std::endl;

    // Loop elements
    MeshObjRelationList::const_iterator ei , ee = node.Relations.end();
    // Seek to elements
    ei = MeshObjConn::find_relation(node, MeshObj::ELEMENT);
    UInt owner = std::numeric_limits<UInt>::max();
    for (; ei != ee && ei->obj->get_type() == MeshObj::ELEMENT; ++ei) {
      const int proc = ep.data(*ei->obj);
//std::cout << "elem:" << ei->obj->get_id() << " owned by " << proc << std::endl;
      if ((UInt) proc < owner) owner = proc;
    }
    int &nowner = np.data(node); nowner = owner;
  }
WriteMesh(mesh, "partition");

  // ************* We are now ready to split the mesh *************
  // Set up a temporary vector of the nodes (for fast sequential access)
  std::vector<const MeshObj*> nodes(num_nodes,NULL);
  {
    MeshDB::const_iterator ni = mesh.node_begin(), ne = mesh.node_end();
    UInt i = 0;
    for (; ni != ne; ++ni) {
      nodes[ni->get_data_index()] = &*ni;
      i++;
    }
  //std::copy(mesh.node_begin(), mesh.node_end(), back_inserter(nodes));
  }

  // the index array
  std::vector<int> node_index(num_nodes, -1);

  for (UInt nm = 0; nm < npart; nm++) {
    Mesh piece;

    std::ostringstream newname_str;
    UInt ndec = numDecimal(npart);
    char buf[512];
    sprintf(buf, "%s", mesh.filename().c_str());
/*
    cptr = std::strstr(buf, ".g");
    if (!cptr)
      throw("Expected a .g in filename");
    *cptr = '\0';
*/
    newname_str << buf << "." << npart << ".";
    newname_str << std::setw(ndec) << std::setfill('0') << nm;
    std::string newname = newname_str.str();
std::cout << "Filename:" << newname << std::endl;
    piece.set_filename(newname);

    piece.set_spatial_dimension(mesh.spatial_dim());

    // To see if a node appears on this proc, we must loop the elements
    // and paint the nodes that go
    MeshDB::const_iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
    for (; ei != ee; ++ei) {
      int proc = ep.data(*ei);
      // Mark nodes
      if ((UInt) proc==nm) {
        const MeshObj &elem = *ei;
        for (UInt n = 0; n < GetMeshObjTopo(elem)->num_nodes; n++) {
          const MeshObj &node = *elem.Relations[n].obj;
          node_index[node.get_data_index()] = node.get_data_index();
        }
      }
    }
    
    std::vector<MeshObj*> new_nodes;
    new_nodes.reserve((int)((double)(1.5/npart)*num_nodes));
    UInt cur_node = 0;
    for (UInt n = 0; n < num_nodes; n++) {
      if (node_index[n] != -1) {
        const MeshObj &node = *nodes[node_index[n]];  // our handy sequential list above
        node_index[n] = cur_node;  // numbering in new list
        MeshObj *newnode = new MeshObj(MeshObj::NODE, node.get_id(), cur_node);
        //const std::vector<double> &coords = node.get_double_data("coord");
        //newnode->add_data("coord", coords);
        piece.add_node(newnode, GetAttr(node));
        new_nodes.push_back(newnode);
        //double owner = *node_owner->data(node);
        //newnode->add_data("owner", (int)(owner + 0.01));
        cur_node++;
      }
    }

    // And now, the elements
    ei = mesh.elem_begin(), ee = mesh.elem_end();
    UInt cur_elem = 0;
    for (; ei != ee; ++ei) {
      int proc = ep.data(*ei);
      // Mark nodes
      if ((UInt) proc==nm) {
        std::vector<MeshObj*> elem_nodes;
        const MeshObj &elem = *ei;
        for (UInt n = 0; n < GetMeshObjTopo(elem)->num_nodes; n++) {
          const MeshObj &node = *elem.Relations[n].obj;
          elem_nodes.push_back(new_nodes[node_index[node.get_data_index()]]);
/*
std::cout << "node_index=" << node_index[node.get_data_index()] << std::endl;
std::cout << "new_nodeid=" << new_nodes[node_index[node.get_data_index()]]->get_id() << std::endl;
*/
        }
        MeshObj *new_elem = new MeshObj(MeshObj::ELEMENT, elem.get_id(), cur_elem);
        //new_elem->add_data("block", elem.get_int("block"));
        piece.add_element(new_elem, elem_nodes, GetAttr(elem),
             GetMeshObjTopo(elem));
        cur_elem++;
      }
    } // for ei

    piece.set_parametric_dimension(mesh.parametric_dim());
//std::cout << "Piece:" << nm << std::endl;
//piece.Print();

    // Create the coordinate field
    Context ctxt; ctxt.flip();
    MEField<> *tcfield = piece.RegisterField("coordinates", MEFamilyStd::instance(),
                   MeshObj::ELEMENT, ctxt, piece.spatial_dim());
    // Save the node ownership
    MEField<> *pn = piece.RegisterField("_OWNER", MEFamilyStd::instance(),
                   MeshObj::ELEMENT, ctxt, 1, true, false, _fieldType<int>::instance());

    piece.Commit();
    MeshDB::const_iterator ni = piece.node_begin(), ne = piece.node_end();
    for (; ni != ne; ni++) {
      MeshDB::MeshObjIDMap::const_iterator nmi =
        mesh.map_find(MeshObj::NODE, ni->get_id());
      if (nmi == mesh.map_end(MeshObj::NODE))
        Throw() << "Cannot find node id:" << ni->get_id() << std::endl;
      const MeshObj &onode = *nmi;
      int &pno = pn->data(*ni);  pno = np.data(onode);
      const double *cd = coord_field.data(onode);
      double *c = tcfield->data(*ni);
      for (UInt d = 0; d < (UInt) piece.spatial_dim(); d++) c[d] = cd[d];
    }

    
    // write the mesh
    WriteMesh(piece, piece.filename());


    for (UInt i = 0; i < num_nodes; i++) node_index[i] = -1;
    //node_index.resize(num_nodes, 0);  // set to zero
  } // for nm (mesh partitions)
 
}

class id_less : public std::binary_function<std::pair<MeshObj*,Attr>,std::pair<MeshObj*,Attr>,bool> {
public:
  id_less() {}
  bool operator()(const std::pair<MeshObj*,Attr> &l, const std::pair<MeshObj*,Attr> &r) {
    return l.first->get_id() < r.first->get_id();
  }
};

class id_equal : public std::binary_function<std::pair<MeshObj*,Attr>,std::pair<MeshObj*,Attr>,bool> {
public:
  id_equal() {}
  bool operator()(const std::pair<MeshObj*,Attr> &l, const std::pair<MeshObj*,Attr> &r) {
    return l.first->get_id() == r.first->get_id();
  }
};



void MeshConcat(Mesh &mesh, std::vector<Mesh*> &srcmesh) {
  UInt npart = srcmesh.size();

  mesh.set_spatial_dimension(srcmesh[0]->spatial_dim());

  // Step 1, as always:  Get the nodes
  {
  std::vector<std::pair<MeshObj*, Attr> > nodes;
  for (UInt i = 0; i < npart; i++) {
    Mesh &src = *srcmesh[i];
//std::cout << "Mesh:" << i << std::endl;
//srcmesh[i].Print();

    MeshDB::const_iterator ni = src.node_begin(), ne = src.node_end();
    for (; ni != ne; ++ni) {
      MeshObj *newnode = new MeshObj(MeshObj::NODE, ni->get_id());
      nodes.push_back(std::make_pair(newnode, GetAttr(*ni)));
    }
  }

  // Unique the list
  std::sort(nodes.begin(), nodes.end(), id_less());
  std::vector<MeshObj*> delete_list;
  for (UInt i = 1; i < nodes.size(); i++) {
    if (nodes[i].first == nodes[i-1].first) delete_list.push_back(nodes[i].first);
  }
  std::vector<std::pair<MeshObj*,Attr> >::iterator ri =
              std::unique(nodes.begin(), nodes.end(), id_equal());
  nodes.erase(ri, nodes.end());

  for (UInt i = 0; i < delete_list.size(); i++) delete delete_list[i];


  // Now add these guys
  for (UInt i = 0; i < nodes.size(); i++) {
    
    mesh.add_node(nodes[i].first, nodes[i].second); // TODO: use correct attr
  }

  }
  // Step 2, the elements.  Assuming unique here.
  {
  for (UInt i = 0; i < npart; i++) {
    Mesh &src = *srcmesh[i];

    MeshDB::const_iterator ni = src.elem_begin(), ne = src.elem_end();
    std::vector<MeshObj*> nconnect;
    for (; ni != ne; ++ni) {
      const MeshObj &elem = *ni;
      MeshObj *newelem = new MeshObj(MeshObj::ELEMENT, elem.get_id());
      //int block = (*ni)->get_int("block");
      //newelem->add_data("block", block);
      UInt npe = GetMeshObjTopo(elem)->num_nodes;
      nconnect.resize(npe);
//std::cout << "elem:" << newelem->get_id() << ":connect:" << std::endl;
      for (UInt n = 0; n < npe; n++) {
        MeshObj::id_type id = elem.Relations[n].obj->get_id();
//std::cout << "\tnode id:" << id << std::endl;
        MeshDB::MeshObjIDMap::iterator mni = mesh.map_find(MeshObj::NODE, id);
        if (mni == mesh.map_end(MeshObj::NODE)) throw("MeshDB Concat, node should be there!!!!");
        nconnect[n] = const_cast<MeshObj*>(&*mni);
      }
      // If element already there, don't do this:
      MeshDB::MeshObjIDMap::iterator efi = mesh.map_find(MeshObj::ELEMENT, elem.get_id());
      if (efi != mesh.map_end(MeshObj::ELEMENT)) continue;
      mesh.add_element(newelem, nconnect, GetAttr(elem).get_key(), GetMeshObjTopo(elem));
    }
  }
  }

  mesh.linearize_data_index();
//mesh.Print();

  // And now, fields!!
  // Create fields
  typedef MEField<>* nField;
  std::vector<nField> fields;

  // Use mesh 1
  Context ctxt; ctxt.flip();
  
  // Loop all fields (in case some files are empty)
  for (UInt i = 0; i < npart; i++) 
  {
    FieldReg::MEField_const_iterator fi = srcmesh[i]->Field_begin(), fe = srcmesh[i]->Field_end();
  
    for (; fi != fe; ++fi) {
      const MEField<> &ofield = *fi;
      if (!ofield.is_nodal()) continue;
  //std::cout << "Creating field:" << ofield.name() << std::endl;
      MEField<> *nfield = mesh.RegisterField(ofield.name(), MEFamilyStd::instance(), MeshObj::ELEMENT, ctxt, ofield.dim(), true);
      if (nfield->name() == "coordinates") nfield->SetOutput(false);
      std::vector<nField>::iterator lb =
        std::lower_bound(fields.begin(), fields.end(), nfield);
      if (lb == fields.end() || *lb != nfield)
        fields.insert(lb, nfield);
    }
  }

  typedef MEField<>* eField;
  std::vector<eField> efields;

  for (UInt i = 0; i < npart; i++)
  {
    FieldReg::MEField_const_iterator fi = srcmesh[i]->Field_begin(), fe = srcmesh[i]->Field_end();
  
    for (; fi != fe; ++fi) {
      const MEField<> &ofield = *fi;
      if (!ofield.is_elemental()) continue;
  //std::cout << "Creating field:" << ofield.name() << std::endl;
      MEField<> *efield = mesh.RegisterField(ofield.name(), MEFamilyDG0::instance(), MeshObj::ELEMENT, ctxt, ofield.dim(), true);
      
      std::vector<eField>::iterator lb =
        std::lower_bound(efields.begin(), efields.end(), efield);
        
      if (lb == efields.begin() || *lb != efield)
        efields.insert(lb, efield);
    }
  }

  
  // ONe last niftiness.  Add processor as an element var
  MEField<> *eown = mesh.RegisterField("_processor", MEFamilyDG0::instance(), MeshObj::ELEMENT,
              ctxt, 1, true, false, _fieldType<int>::instance());

  // Build storage
  mesh.Commit();

  // Now set the values
  for (UInt i = 0; i < npart; i++) {
    Mesh &src = *srcmesh[i];
    
    if (src.num_elems() == 0) continue;
    std::vector<nField> ofield;
    std::vector<eField> oefield;

    // Get this meshes field list
    ofield.clear();
    oefield.clear();
    for (UInt f = 0; f < fields.size(); f++) {
      MEField<> *of = src.GetField(fields[f]->name());
      if (!of) throw("didnt find field in other registrar!!!");
      ofield.push_back(of);
    }
    for (UInt f = 0; f < efields.size(); f++) {
      MEField<> *of = src.GetField(efields[f]->name());
      if (!of) throw("didnt find field in other registrar!!!");
      oefield.push_back(of);
    }

    // Nodal fields
    {
      MeshDB::const_iterator ni = src.node_begin(), ne = src.node_end();
      for (; ni != ne; ++ni) {
        const MeshObj &onode = *ni;
        MeshDB::MeshObjIDMap::iterator mni = mesh.map_find(MeshObj::NODE, onode.get_id()); 
        if (mni == mesh.map_end(MeshObj::NODE)) throw("MeshDB Concat, field build; node should be there!!!!");
        for (UInt f = 0; f < fields.size(); f++) {
          double *da = fields[f]->data(*mni);
          double *s = ofield[f]->data(onode);
          for (UInt d= 0; d < fields[f]->dim(); d++) {
            da[d] = s[d];
          }
        }
      }
    }

    // Element fields
    {
      MeshDB::const_iterator ni = src.elem_begin(), ne = src.elem_end();
      for (; ni != ne; ++ni) {
        const MeshObj &oelem = *ni;
        MeshDB::MeshObjIDMap::iterator mni = mesh.map_find(MeshObj::ELEMENT, oelem.get_id()); 
        if (mni == mesh.map_end(MeshObj::ELEMENT)) throw("MeshDB Concat, field build; node should be there!!!!");
        for (UInt f = 0; f < efields.size(); f++) {
          double *da = efields[f]->data(*mni);
          double *s = oefield[f]->data(oelem);
          for (UInt d= 0; d < efields[f]->dim(); d++) {
            da[d] = s[d];
          }
        }
      }
    }

    // Processor
    MeshDB::const_iterator ei = src.elem_begin(), ee = src.elem_end();
    for (; ei != ee; ++ei) {
      const MeshObj &elem = *ei;
      MeshObj::id_type id = elem.get_id();
      MeshDB::MeshObjIDMap::iterator mni = mesh.map_find(MeshObj::ELEMENT, id);
      if (mni == mesh.map_end(MeshObj::ELEMENT)) throw("MeshDB Concat, processor: couldn't find elem");
      int &eo = eown->data(*mni); eo = i;
    }
 
  }

  
}

} // namespace
} // namespace
