// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <Mesh/include/Legacy/ESMCI_MeshVTK.h>
#include <Mesh/include/ESMCI_Mesh.h>
#include <Mesh/include/Legacy/ESMCI_MEField.h>
#include <Mesh/include/Legacy/ESMCI_MeshObjTopo.h>
#include <Mesh/include/Legacy/ESMCI_ParEnv.h>
#include <Mesh/include/Legacy/ESMCI_IOField.h>
#include "ESMCI_Array.h"
#include "ESMCI_LocalArray.h"

#include <iostream>
#include <fstream>
#include <map>
#include <cstdio>

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

namespace ESMCI {

enum {VTK_TRIANGLE=5, VTK_QUAD=9, VTK_TETRA=10, VTK_HEXAHEDRON=12};

static UInt vtk_type_dim(UInt type) {
  switch (type) {
    case VTK_TRIANGLE:
    case VTK_QUAD:
    return 2;

    default:
    return 3;
  }
}

static const MeshObjTopo *vtk2topo(UInt vtk_type, UInt sdim) {
  if (sdim == 2) {
    switch(vtk_type) {

      case VTK_TRIANGLE:
        return GetTopo("TRI3");

      case VTK_QUAD:
        return GetTopo("QUAD");

      default:
        Throw() "vtk type unknown:" << vtk_type;
    }
  } else if (sdim == 3) {
    switch(vtk_type) {

      case VTK_TETRA:
        return GetTopo("TETRA");

      case VTK_HEXAHEDRON:
        return GetTopo("HEX");

      case VTK_QUAD:
        return GetTopo("SHELL");

      case VTK_TRIANGLE:
        return GetTopo("SHELL3");

      default:
        Throw() "vtk type unknown:" << vtk_type;
    }
  } else Throw() << "sdim not valid:" << sdim;
}

const MeshObjTopo *Vtk2Topo(UInt sdim, UInt vtk_type) {

  return vtk2topo(vtk_type, sdim);

}

  // Get node to array data index map
  //// TODO: This will work for Meshes without custom distgrids. However, to 
  ////        be  completely general. We need to loop through the distgrid getting the sequence
  ////        indices and create a map from those to where in the localDE, index space we are in the array. 
void get_node_to_array_index_map(const Mesh &mesh, std::map<MeshObj::id_type, int> &gid_to_index) {

  UInt num_nodes = mesh.num_nodes();

  std::vector<std::pair<int,int> > gids;
  gids.reserve(num_nodes);

  Mesh::const_iterator ni = mesh.node_begin(), ne = mesh.node_end();
  for (; ni != ne; ++ni) {

    const MeshObj &node = *ni;

    if (!GetAttr(node).is_locally_owned()) continue;

    int idx = node.get_data_index();

    gids.push_back(std::make_pair(idx, node.get_id()));

  }

  std::sort(gids.begin(), gids.end());

  gid_to_index.clear();
  for (UInt i = 0; i < gids.size(); ++i) {
    gid_to_index[gids[i].second]=i;
  }
}

  // Get node to array data index map
  //// TODO: This will work for Meshes without custom distgrids. However, to 
  ////        be  completely general. We need to loop through the distgrid getting the sequence
  ////        indices and create a map from those to where in the localDE, index space we are in the array. 
void get_elem_to_array_index_map(const Mesh &mesh, std::map<MeshObj::id_type, int> &gid_to_index) {

  UInt num_elems = mesh.num_elems();

  std::vector<std::pair<int,int> > gids;
  gids.reserve(num_elems);

  Mesh::const_iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
  for (; ei != ee; ++ei) {

    const MeshObj &elem = *ei;

    if (!GetAttr(elem).is_locally_owned()) continue;

    int idx = elem.get_data_index();

    gids.push_back(std::make_pair(idx, elem.get_id()));

  }

  std::sort(gids.begin(), gids.end());

  gid_to_index.clear();
  for (UInt i = 0; i < gids.size(); ++i) {
    gid_to_index[gids[i].second]=i;
  }
}


// Write mesh data to stream, taking into account the correct typeid
template<typename T, typename iter>
void append_data_from_Array(iter ni, iter ne, std::map<MeshObj::id_type, int> gid_to_index, 
                        ESMCI::Array *array, std::ofstream &out) {

  // Write header
  out << "LOOKUP_TABLE default" << std::endl;

  // Get local DE count
  int localDECount=array->getDELayout()->getLocalDeCount();

  // If there are no local DEs, then leave
  if (localDECount == 0) return;

  // If there is greater than 1 local DE, then complain
  if (localDECount > 1) Throw() << "this call currently can't handle > 1 localDE per PET";


  // Complain if the array has more than rank 1
  if (array->getRank() != 1) Throw() << "this call currently can't handle Array rank != 1";


  // Get local Array
  int localDE=0; // for now just 0
  LocalArray *localArray=(array->getLocalarrayList())[localDE];
  
  // Get localDE lower bound
  int lbound=(array->getComputationalLBound())[localDE]; // (assumes array rank is 1)

  // Loop writing out data in Array that corresponds to objects
  for (; ni != ne; ++ni) {
    const MeshObj &obj=*ni;

    // If it's not owned write out 0
    if (!GetAttr(obj).is_locally_owned()) {

      // Get 0 of this type
      T d=(T)0;

      // Write it out
      out << d << " ";

      // Go to next item in loop
      continue;
    }

    // Get index from id
    std::map<MeshObj::id_type,int>::iterator mi = gid_to_index.find(obj.get_id());
    if (mi == gid_to_index.end()) {
      Throw() << "object id not found.";
    }
    int index=mi->second+lbound; 

    // Get data
    T d;
    localArray->getDataInternal(&index, &d);

    // Write data
    out << d << " ";
  }
}

// Write mesh data to stream, taking into account the correct typeid
template<>
void append_data_from_Array<ESMC_R8>(Mesh::const_iterator ni, Mesh::const_iterator ne, std::map<MeshObj::id_type, int> gid_to_index, 
                        ESMCI::Array *array, std::ofstream &out) {

  // Write header
  out << "LOOKUP_TABLE default" << std::endl;

  // Get local DE count
  int localDECount=array->getDELayout()->getLocalDeCount();

  // If there are no local DEs, then leave
  if (localDECount == 0) return;

  // If there is greater than 1 local DE, then complain
  if (localDECount > 1) Throw() << "this call currently can't handle > 1 localDE per PET";


  // Complain if the array has more than rank 1
  if (array->getRank() != 1) Throw() << "this call currently can't handle Array rank != 1";


  // Get local Array
  int localDE=0; // for now just 0
  LocalArray *localArray=(array->getLocalarrayList())[localDE];
  
  // Get localDE lower bound
  int lbound=(array->getComputationalLBound())[localDE]; // (assumes array rank is 1)

  // Loop writing out data in Array that corresponds to objects
  for (; ni != ne; ++ni) {
    const MeshObj &obj=*ni;

    // If it's not owned write out 0
    if (!GetAttr(obj).is_locally_owned()) {

      // Get 0 of this type
      ESMC_R8 d=0.0;

      // Write it out
      out << d << " ";

      // Go to next item in loop
      continue;
    }

    // Get index from id
    std::map<MeshObj::id_type,int>::iterator mi = gid_to_index.find(obj.get_id());
    if (mi == gid_to_index.end()) {
      Throw() << "object id not found.";
    }
    int index=mi->second+lbound; 

    // Get data
    ESMC_R8 d;
    localArray->getDataInternal(&index, &d);

    // It seems to irritate visit if we have a number < 1.0E-300, so round down to 0.0
    if (d < 1.0E-300) d=0.0;

    // Write data
    out << d << " ";
  }
}


template<typename iter>
static void write_data_from_Array(iter ni, iter ne, std::map<MeshObj::id_type, int> &gid_to_index, 
                                    ESMCI::Array *array, const std::string &vname, std::ofstream &out) {




  // Get Array data type
  ESMC_TypeKind_Flag typekind=array->getTypekind();
    
  // Output based on type
  if (typekind == ESMC_TYPEKIND_R8) {
    out << "SCALARS " << vname << " double 1" << std::endl;
    append_data_from_Array<ESMC_R8>(ni, ne, gid_to_index, array, out);
  } else if (typekind == ESMC_TYPEKIND_I4) {
    out << "SCALARS " << vname << " int 1" << std::endl;
    append_data_from_Array<ESMC_I4>(ni, ne, gid_to_index, array, out);
  } else if (typekind == ESMC_TYPEKIND_R4) {
    out << "SCALARS " << vname << " float 1" << std::endl;
    append_data_from_Array<ESMC_R4>(ni, ne, gid_to_index, array, out);
  } else {
    std::cout << "Unknown data type, skipping " << std::endl;
  }
}

// Write mesh data to stream, taking into account the correct typeid
template<typename T, typename iter, typename FIELD>
void append_data(iter ni, iter ne, const FIELD &llf, UInt d, std::ofstream &out) {
  out << "LOOKUP_TABLE default" << std::endl;
  for (; ni != ne; ++ni) {
    if (llf.OnObj(*ni))
      out << ((T*)llf.data(*ni))[d] << " ";
  }
}

template<>
void append_data<double>(Mesh::const_iterator ni, Mesh::const_iterator ne, const _field &llf, UInt d, std::ofstream &out) {
  out << "LOOKUP_TABLE default" << std::endl;
  for (; ni != ne; ++ni) {
    if (llf.OnObj(*ni)) {
      // It seems to irritate visit if a value is <1.0E-300, so round down if that's the case
      if (((double *)llf.data(*ni))[d] > 1.0E-300) {
        out << ((double *)llf.data(*ni))[d] << " ";
      } else {
        out << "0 ";
      }
    }
  }
}


template<typename iter, typename FIELD>
static void write_data(iter ni, iter ne, const FIELD &llf, const std::string &vname, UInt d, std::ofstream &out) {

  if (llf.tinfo() == typeid(double)) {
    out << "SCALARS " << vname << " double 1" << std::endl;
    append_data<double>(ni, ne, llf, d, out);
  } else if (llf.tinfo() == typeid(int)) {
    out << "SCALARS " << vname << " int 1" << std::endl;
    append_data<int>(ni, ne, llf, d, out);
  } else if (llf.tinfo() == typeid(float)) {
    out << "SCALARS " << vname << " float 1" << std::endl;
    append_data<float>(ni, ne, llf, d, out);
  } else if (llf.tinfo() == typeid(long)) {
    out << "SCALARS " << vname << " long 1" << std::endl;
    append_data<long>(ni, ne, llf, d, out);
  } else if (llf.tinfo() == typeid(char)) {
    out << "SCALARS " << vname << " char 1" << std::endl;
    append_data<char>(ni, ne, llf, d, out);
  } else if (llf.tinfo() == typeid(UChar)) {
    out << "SCALARS " << vname << " unsigned char 1" << std::endl;
    append_data<UChar>(ni, ne, llf, d, out);
  } else {
    std::cout << "Unknown data type, skipping ";
  }
}

  void WriteVTKMesh(const Mesh &mesh, const std::string &filename,
                    int num_nodeArrays, ESMCI::Array **nodeArrays, 
                    int num_elemArrays, ESMCI::Array **elemArrays) {
    Trace __trace("WriteVTKMesh(const Mesh &mesh, const std::string &filename)");

    std::ofstream out(filename.c_str(), std::ios::out);

  out << "# vtk DataFile Version 3.0" << std::endl;
  out << "This file was generated by ESMC " << std::endl;
  out << "ASCII" << std::endl;
  out << "DATASET UNSTRUCTURED_GRID" << std::endl;
  out << "POINTS " << mesh.num_nodes() << " double" << std::endl;
  

  std::map<MeshObj::id_type, int> id2ord;

  {
    MEField<> &coord = *mesh.GetCoordField();
    Mesh::const_iterator ni = mesh.node_begin(), ne = mesh.node_end();
    for (UInt i = 0; ni != ne; ++ni) {
      const MeshObj &node = *ni;
      double *cd = coord.data(node);
      // Write coordinates
      out << cd[0] << " " << cd[1] << " " << (mesh.spatial_dim() == 3 ? cd[2] : 0.0) << std::endl;
  
      // Increment ordinal
      id2ord[node.get_id()] = i++;
    }
  }

  // Now figure out how large the cell index list is:
  int cell_size = 0, num_elem = 0, num_nodes = mesh.num_nodes();
  {
    Mesh::const_iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
    for (; ei != ee; ++ei) {
      const MeshObj &elem = *ei;
      const MeshObjTopo *topo = GetMeshObjTopo(elem);
      cell_size += 1+topo->num_nodes; // 1 for the num connectivity
      num_elem++;
    }
  }

  out << "CELLS " << num_elem << " " << cell_size << std::endl;

  // Now print the connectivity
  {
    Mesh::const_iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
    for (; ei != ee; ++ei) {
      const MeshObj &elem = *ei;
      const MeshObjTopo *topo = GetMeshObjTopo(elem);

      out << topo->num_nodes << " ";
      for (UInt n = 0; n < topo->num_nodes; n++) {

        MeshObj::id_type id = elem.Relations[n].obj->get_id();
        std::map<MeshObj::id_type,int>::iterator imi = id2ord.find(id);
        ThrowRequire(imi != id2ord.end());

         out << imi->second << " ";

      }

      out << std::endl;

    } // for ei

  } // print connectivity

  // Now print the cell types
  out << "CELL_TYPES " << num_elem << std::endl; 
  {
    Mesh::const_iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
    for (; ei != ee; ++ei) {
      const MeshObj &elem = *ei;
      const MeshObjTopo *topo = GetMeshObjTopo(elem);

      switch(topo->num_nodes) {
        case 3:
          out << VTK_TRIANGLE << std::endl;
        break;

        case 4:
          if (topo->parametric_dim == 3) {
//std::cout << "tetra!@!" << std::endl;
            out << VTK_TETRA << std::endl;
          } else {
//std::cout << "quad!@! " << mesh.parametric_dim() << std::endl;
            out << VTK_QUAD << std::endl;
          }
        break;

        case 8:
          out << VTK_HEXAHEDRON << std::endl;
        break;
 
        default:
          Throw() << "Unsupported VTK element type:" << topo->name << std::endl;
      }

    }

  } // print cell types

  // Unfortunately this file format is quite dumb (mean this literally, not in any condescending way).
  // Therefore we must save off the node and element numbers as fields.
  {
    // Element numbers
    out << "CELL_DATA " << num_elem << std::endl;
    out << "SCALARS " << "_ELEM_NUM" << " long 1" << std::endl;
    out << "LOOKUP_TABLE default" << std::endl;

    Mesh::const_iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
    for (UInt i = 0; ei != ee; ++ei) {
      const MeshObj &elem = *ei;
      out << elem.get_id() << " ";
    }

    out << std::endl;

  }

  // Now element variables
  {
     FieldReg::MEField_const_iterator nv = mesh.Field_begin(), ne = mesh.Field_end();
     for (; nv != ne; ++nv) {
       const MEField<> &mf = *nv;
       if (mf.Output() && mf.is_elemental()) {

         for (UInt d = 0; d < mf.dim(); d++) {
           char buf[512];
           std::sprintf(buf, "_%d", d);
           std::string vname = mf.name() + (mf.dim() == 1? "" : std::string(buf));

           const _field &llf = mf();
           write_data(mesh.elem_begin(), mesh.elem_end(), llf, vname, d, out);
           
           out << std::endl;

         } //for d

       } // is elemental/output

     }// fields

  } // elem vars

  // Now Arrays stored on elems
  {

    // Get map from node index to index in Array
    std::map<MeshObj::id_type, int> gid_to_index;
    get_elem_to_array_index_map(mesh, gid_to_index);


    // Loop over Arrays to be output
    for (int i=0; i<num_elemArrays; i++) {

      // Generate name
      char buf[512];
      std::sprintf(buf, "elemArray%d_%s",i+1,elemArrays[i]->getName());
      std::string vname(buf);

      // Write data to file
      write_data_from_Array(mesh.elem_begin(), mesh.elem_end(), gid_to_index, elemArrays[i], vname, out);

      // End line of data
      out << std::endl;
    }

  } // Arrays stored on elems


  // Save off the node numbering
  {
    // Node numbers
    out << "POINT_DATA " << num_nodes << std::endl;
    out << "SCALARS " << "_NODE_NUM" << " long 1" << std::endl;
    out << "LOOKUP_TABLE default" << std::endl;

    Mesh::const_iterator ni = mesh.node_begin(), ne = mesh.node_end();
    for (UInt i = 0; ni != ne; ++ni) {
      const MeshObj &node = *ni;
 
      out << node.get_id() << " ";
    }

    out << std::endl;

  }

  // Now node variables
  {
     FieldReg::MEField_const_iterator nv = mesh.Field_begin(), ne = mesh.Field_end();
     for (; nv != ne; ++nv) {
       const MEField<> &mf = *nv;
       if (mf.Output() && mf.is_nodal()) {

         for (UInt d = 0; d < mf.dim(); d++) {
           char buf[512];
           std::sprintf(buf, "_%d", d);
           std::string vname = mf.name() + (mf.dim() == 1? "" : std::string(buf));

           const _field &llf = mf();
           write_data(mesh.node_begin(), mesh.node_end(), llf, vname, d, out);

           out << std::endl;

         } //for d

       } // is nodal/output

     }// fields

  } // node vars


  // Now Arrays stored on nodes
  {

    // Get map from node index to index in Array
    std::map<MeshObj::id_type, int> gid_to_index;
    get_node_to_array_index_map(mesh, gid_to_index);


    // Loop over Arrays to be output
    for (int i=0; i<num_nodeArrays; i++) {

      // Generate name
      char buf[512];
      std::sprintf(buf, "nodeArray%d_%s",i+1,nodeArrays[i]->getName());
      std::string vname(buf);

      // Write data to file
      write_data_from_Array(mesh.node_begin(), mesh.node_end(), gid_to_index, nodeArrays[i], vname, out);

      // End line of data
      out << std::endl;
    }

  } // Arrays stored on nodes

}

// Manager the file resource so that early exit still closes the file...
struct FileManager {
  FileManager(const char *_f) : fp(NULL) {
    fp = fopen(_f, "r");
    if (!fp) Throw() << "Could not open infile:" << _f;
  }
  ~FileManager() {
     if (fp) fclose(fp);
   }
  FILE *operator()() { return fp;}
  FILE *fp;
};


/*--------------------------------------------------------------------------*/
// Read a vtk mesh.  This function is EXTREMELY primitive and will not tolerate
// any deviations from a very STRICT format.  It is provided just as a method
// to read at least some kind of file.  It will, for instance, read in vtk
// files that are generate by the write command above.  This allows some simple
// things like providing a mesh concatenator.
// The correct solution would be to use the EXODUS file format or compile a
// fully operation VTK paraser into ESMF.  Both are not allowed currently due
// to our reluctance to compile agains 3rd party libraries.
/*--------------------------------------------------------------------------*/
void ReadVTKMesh(Mesh &mesh, const std::string &filename) {


  FileManager fp(filename.c_str());

  // I'm sorry, but fscanf seems to be more powerful than ifstream...


  UInt sdim(0), pdim(0);

  // This is a rough reader... robustness is not guaranteed at this time.
  // What IS guaranteed is that this routine will read files written by the framework.
  {

    // The version line
    char linebuf[4096];
    float version;
    UInt ret = fscanf(fp(), "# vtk DataFile Version %f\n", &version);
    ThrowRequire(ret == 1);
  
    fgets(linebuf, 1000, fp());
  //std::cout << "linebuf=" << linebuf << std::endl;
  
    ThrowRequire(0 == fscanf(fp(), "ASCII\n"));
  
    ThrowRequire(0 == fscanf(fp(), "DATASET UNSTRUCTURED_GRID\n"));
  
    UInt npoints;
    ThrowRequire(1 == fscanf(fp(), "POINTS %u double\n", &npoints));
  
  //std::cout << "npoints = " << npoints << std::endl;
  
    /*------------------------------------------------------------*/
    // Now read the coords
    /*------------------------------------------------------------*/
    std::vector<double> coord(3*npoints);
    for (UInt i = 0; i < npoints; i++) {
      int t_i = 3*i;
      ThrowRequire(3 == fscanf(fp(), "%lf %lf %lf\n", &coord[t_i], &coord[t_i+1], &coord[t_i+2]));
    }
  
#ifdef DBG_VTKREAD
  for (UInt i = 0; i < npoints; i++) {
  std::cout << coord[3*i] << " " << coord[3*i+1] << " " << coord[3*i+2] << std::endl;
  }
#endif
  
    UInt nelem, width;
    ThrowRequire(2 == fscanf(fp(), "CELLS %u %u\n", &nelem, &width));
  
  //std::cout << "nelem:" << nelem << ", w=" << width << std::endl;
  
    /*------------------------------------------------------------*/
    // Read the connectivity for elements
    /*------------------------------------------------------------*/
    std::vector<UInt> conn(width);
  
    UInt wi = 0;
    for (UInt i = 0; i < nelem; i++) {
  
      ThrowRequire(1 == fscanf(fp(), "%u ", &conn[wi]));
  
      for (UInt n = 0; n < conn[wi]; n++) {
        ThrowRequire(1 == fscanf(fp(), "%u ", &conn[wi+n+1]));
      } 
      fscanf(fp(), "\n");

      wi += conn[wi] + 1; // skip forward
    }
  
  
    /*------------------------------------------------------------*/
    // Read the element types
    /*------------------------------------------------------------*/
    UInt nctype;
    ThrowRequire(1 == fscanf(fp(), "CELL_TYPES %u", &nctype));
  
    ThrowRequire(nctype == nelem);
  
    std::vector<UInt> ctypes(nctype);
    
    for (UInt i = 0; i < nctype; i++) {
      ThrowRequire(1 == fscanf(fp(), "%u\n", &ctypes[i]));
    }

   
    /*------------------------------------------------------------*/
    // Take a moment to actually build the mesh
    /*------------------------------------------------------------*/
    
    // Nodes first
    std::vector<MeshObj*> nodevect; nodevect.reserve(npoints);
    for (UInt i = 0; i < npoints; i++) {
      MeshObj *node = new MeshObj(MeshObj::NODE, -(i+1), i);
      nodevect.push_back(node);

      node->set_owner(Par::Rank());

      mesh.add_node(node, 0);
    }

    // Now, build an element_type -> block array.

    typedef std::map<UInt, UInt> bmap;
    bmap etype2blk;

    UInt cur_blk = 1;

    for (UInt i = 0; i < nelem; i++) {
      std::pair<bmap::iterator, bool> lk =
        etype2blk.insert(std::make_pair(ctypes[i], cur_blk));

      // If insert was successful, this was a new block
      if (lk.second == true) cur_blk++;
      
    }


    // Parametric dim??

    ThrowRequire(ctypes.size() > 0);
    pdim = vtk_type_dim(ctypes[0]);

    // And another administrative detail: are we 3D physical or 2D space ?
    // Assum that if we are 2d the last coord is fixed...
 

    // Spatial dim??

    if (pdim == 2) {
      double c3 = coord[2];

      bool coorddiff = false;

      for (UInt i = 0; !coorddiff && i < npoints; i++) {
        if (std::abs(coord[3*i + 2] - c3) > 1e-5) coorddiff = true;
      }

      sdim = (coorddiff ? 3 : 2);
 
    } else sdim = 3;

    mesh.set_parametric_dimension(pdim);
    mesh.set_spatial_dimension(sdim);

    // Add the elements
    UInt cur_off = 0;
    for (UInt i = 0; i < nelem; i++) {

      const MeshObjTopo *topo = vtk2topo(ctypes[i], sdim);

      MeshObj *elem = new MeshObj(MeshObj::ELEMENT, -(i+1), i);

      // Collect an array of the nodes.
      cur_off += 1; // skip num_nodes field
      std::vector<MeshObj*> elemnodes; elemnodes.reserve(topo->num_nodes);
      for (UInt n = 0; n < topo->num_nodes; n++)
        elemnodes.push_back(nodevect[conn[cur_off+n]]);

      elem->set_owner(Par::Rank());

      mesh.add_element(elem, elemnodes, topo->number, topo);

      cur_off += topo->num_nodes;
    }

    // Register coords
    IOField<NodalField> *node_coord = mesh.RegisterNodalField(mesh, "coordinates", mesh.spatial_dim());

    // Copy coordinates into field
    {
      MeshDB::const_iterator ni = mesh.node_begin(), ne = mesh.node_end();
      for (; ni != ne; ++ni) {
        double *c = node_coord->data(*ni);
        UInt idx = ni->get_data_index();
        c[0] = coord[3*idx];
        if (mesh.spatial_dim() >= 2) c[1] = coord[3*idx+1];
        if (mesh.spatial_dim() >= 3) c[2] = coord[3*idx+2];
      }
    }

  } // close read mesh objects

  // Field data.  Pick of _ELEM_NUM, _NODE_NUM as special cases.
  UInt num_nodes = mesh.num_nodes(), num_elems = mesh.num_elems();
  
  // Try reading POINT_DATA
  bool done = false;

  enum {VTK_NO_DATA = -1, VTK_CELL_DATA=0, VTK_NODE_DATA=1};

  int data_type = VTK_NO_DATA;
  while (!done) {
    UInt ndata;
    int nread = fscanf(fp(), "POINT_DATA %u\n", &ndata);
    if (nread == EOF) {
      done = true; continue;
    }

    if (1 == nread) {
      data_type = VTK_NODE_DATA;
    } else { // Try to read cell data
      nread = fscanf(fp(), "CELL_DATA %u\n", &ndata);
      if (nread == EOF) {
        done = true; continue;
      }
     
      if (1 == nread) {
        data_type = VTK_CELL_DATA;
      } else {
        // If data_type is already defined, just use what we have.
        // Otherwise we have a parsing error.
        if (data_type == VTK_NO_DATA)
          Throw() << "Confusion parsing VTK data type!";
      }
    }

//std::cout << "data_type=" << data_type << std::endl;

    // Process the data.  data_type is one of elem or node
    char vname[512];
    char vtype[15];
    ThrowRequire(2 == fscanf(fp(), "SCALARS %s %s 1\n", vname, vtype));
//std::cout << "Reading var:" << vname << std::endl;

    fscanf(fp(), "LOOKUP_TABLE default\n");

    // Now read the data
    UInt num_data = data_type == VTK_NODE_DATA ? num_nodes : num_elems;
    std::vector<double> data(num_data, 0);

 
    for (UInt i = 0; i < num_data; i++) {
      ThrowRequire(1 == fscanf(fp(), "%lf ", &data[i]));
//std::cout << "Read " << data[i] << std::endl;
    }
    
    fscanf(fp(), "\n");

    // If NUM, process specially
    if (data_type == VTK_CELL_DATA && std::string(vname) == "_ELEM_NUM") {

      MeshDB::iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
      for (; ei != ee; ++ei) {

        MeshObj &obj = *ei;

        // Remove object from old map
        Mesh::MeshObjIDMap &omap = mesh.get_map(MeshObj::ELEMENT);
        omap.erase(&obj);

        // Insert with new id
        obj.key = static_cast<long>(data[obj.get_data_index()]);
        std::pair<Mesh::MeshObjIDMap::iterator,bool> iu =
            omap.insert(obj);
        if (!iu.second)
              Throw() << "Error inserting. Key already exists!!! Obj:" << obj << std::endl;
      }

      
    } else if (data_type == VTK_NODE_DATA && std::string(vname) == "_NODE_NUM") {
      MeshDB::iterator ei = mesh.node_begin(), ee = mesh.node_end();
      for (; ei != ee; ++ei) {

        MeshObj &obj = *ei;

        // Remove object from old map
        Mesh::MeshObjIDMap &omap = mesh.get_map(MeshObj::NODE);
        omap.erase(&obj);

        // Insert with new id
        obj.key = static_cast<long>(data[obj.get_data_index()]);
        std::pair<Mesh::MeshObjIDMap::iterator,bool> iu =
            omap.insert(obj);
        if (!iu.second)
              Throw() << "Error inserting. Key already exists!!! Obj:" << obj << std::endl;
      }
    } else {

      // Regular variable

      if (data_type == VTK_NODE_DATA) {
        IOField<NodalField> *nfield = mesh.RegisterNodalField(mesh, vname);

        MeshDB::iterator ei = mesh.node_begin(), ee = mesh.node_end();
        for (; ei != ee; ++ei) {

          MeshObj &obj = *ei;

          double *d = nfield->data(obj);
          d[0] = data[obj.get_data_index()];
 
        }

      } else {
      }
    }

  } // while processing data

}

/**
 * Read the header info from the VTK file, specifically how many nodes, and
 * how many elements.
 */
void ReadVTKMeshHeader(const std::string &filename, int &num_elems, int &num_nodes, int &conn_size) {

  FileManager fp(filename.c_str());

  UInt sdim(0), pdim(0);

  {

    // The version line
    char linebuf[4096];
    float version;
    UInt ret = fscanf(fp(), "# vtk DataFile Version %f\n", &version);
    ThrowRequire(ret == 1);
  
    fgets(linebuf, 1000, fp());
  //std::cout << "linebuf=" << linebuf << std::endl;
  
    ThrowRequire(0 == fscanf(fp(), "ASCII\n"));
  
    ThrowRequire(0 == fscanf(fp(), "DATASET UNSTRUCTURED_GRID\n"));
  
    UInt npoints;
    ThrowRequire(1 == fscanf(fp(), "POINTS %u double\n", &npoints));
  
  //std::cout << "npoints = " << npoints << std::endl;
  
    /*------------------------------------------------------------*/
    // Now skip past the coords
    /*------------------------------------------------------------*/
    double coord[3];
    for (UInt i = 0; i < npoints; i++) {
      ThrowRequire(3 == fscanf(fp(), "%lf %lf %lf\n", &coord[0], &coord[1], &coord[2]));
    }
  
    UInt nelem, width;
    ThrowRequire(2 == fscanf(fp(), "CELLS %u %u\n", &nelem, &width));

    // Set the output arguments
    num_nodes = npoints;
    num_elems = nelem;
    conn_size = width-nelem;  // width includes an entry for num_nodes at beginning
                              // of each node list.  We will not forward this.
  }
}

/**
 * Fill out the arrays with mesh information.  Useful for creating an array in Fortran.
 */
void ReadVTKMeshBody(const std::string &filename, int *nodeId, double *nodeCoord, int *nodeOwner,
                     int *elemId, int *elemType, int *elemConn)
{

  FileManager fp(filename.c_str());

  // I'm sorry, but fscanf seems to be more powerful than ifstream...

  UInt sdim(0), pdim(0);


  UInt num_nodes, num_elems;

  // This is a rough reader... robustness is not guaranteed at this time.
  // What IS guaranteed is that this routine will read files written by the framework.
  {

    // The version line
    char linebuf[4096];
    float version;
    UInt ret = fscanf(fp(), "# vtk DataFile Version %f\n", &version);
    ThrowRequire(ret == 1);
  
    fgets(linebuf, 1000, fp());
  //std::cout << "linebuf=" << linebuf << std::endl;
  
    ThrowRequire(0 == fscanf(fp(), "ASCII\n"));
  
    ThrowRequire(0 == fscanf(fp(), "DATASET UNSTRUCTURED_GRID\n"));
  
    UInt npoints;
    ThrowRequire(1 == fscanf(fp(), "POINTS %u double\n", &npoints));
  
  //std::cout << "npoints = " << npoints << std::endl;
  
    /*------------------------------------------------------------*/
    // Now read the coords
    /*------------------------------------------------------------*/
    for (UInt i = 0; i < npoints; i++) {
      int t_i = 3*i;
      ThrowRequire(3 == fscanf(fp(), "%lf %lf %lf\n", &nodeCoord[t_i], &nodeCoord[t_i+1], &nodeCoord[t_i+2]));
    }
  
    UInt nelem, width;
    ThrowRequire(2 == fscanf(fp(), "CELLS %u %u\n", &nelem, &width));

    num_nodes = npoints;
    num_elems = nelem;
  
  //std::cout << "nelem:" << nelem << ", w=" << width << std::endl;
  
    /*------------------------------------------------------------*/
    // Read the connectivity for elements
    /*------------------------------------------------------------*/
  
    UInt wi = 0;
    UInt nnode;
    for (UInt i = 0; i < nelem; i++) {
  
      ThrowRequire(1 == fscanf(fp(), "%u ", &nnode));
  
      for (UInt n = 0; n < nnode; n++) {
        ThrowRequire(1 == fscanf(fp(), "%u ", &elemConn[wi+n]));
      } 
      fscanf(fp(), "\n");

      wi += nnode; // skip forward
    }
  
  
    /*------------------------------------------------------------*/
    // Read the element types
    /*------------------------------------------------------------*/
    UInt nctype;
    ThrowRequire(1 == fscanf(fp(), "CELL_TYPES %u", &nctype));
  
    ThrowRequire(nctype == nelem);
  
    for (UInt i = 0; i < nctype; i++) {
      ThrowRequire(1 == fscanf(fp(), "%u\n", &elemType[i]));
    }

  } // close read mesh objects

  // Try reading POINT_DATA
  bool done = false;

  enum {VTK_NO_DATA = -1, VTK_CELL_DATA=0, VTK_NODE_DATA=1};

  int data_type = VTK_NO_DATA;
  while (!done) {
    UInt ndata;
    int nread = fscanf(fp(), "POINT_DATA %u\n", &ndata);
    if (nread == EOF) {
      done = true; continue;
    }

    if (1 == nread) {
      data_type = VTK_NODE_DATA;
    } else { // Try to read cell data
      nread = fscanf(fp(), "CELL_DATA %u\n", &ndata);
      if (nread == EOF) {
        done = true; continue;
      }
     
      if (1 == nread) {
        data_type = VTK_CELL_DATA;
      } else {
        // If data_type is already defined, just use what we have.
        // Otherwise we have a parsing error.
        if (data_type == VTK_NO_DATA)
          Throw() << "Confusion parsing VTK data type!";
      }
    }

    // Process the data.  data_type is one of elem or node
    char vname[512];
    char vtype[15];
    ThrowRequire(2 == fscanf(fp(), "SCALARS %s %s 1\n", vname, vtype));
//std::cout << "Reading var:" << vname << std::endl;

    fscanf(fp(), "LOOKUP_TABLE default\n");

    // Now read the data
    UInt num_data = data_type == VTK_NODE_DATA ? num_nodes : num_elems;

    std::vector<double> data(num_data, 0);
 
    for (UInt i = 0; i < num_data; i++) {
      ThrowRequire(1 == fscanf(fp(), "%lf ", &data[i]));
//std::cout << "Read " << data[i] << std::endl;
    }
    
    fscanf(fp(), "\n");

    // If NUM, process specially
    if (data_type == VTK_CELL_DATA && std::string(vname) == "_ELEM_NUM") {

      for (UInt i = 0; i < data.size(); ++i) elemId[i] = (int)(data[i]+0.0001);
      //std::copy(data.begin(), data.end(), elemId); causes double to int warnings

    } else if (data_type == VTK_NODE_DATA && std::string(vname) == "_NODE_NUM") {

      for (UInt i = 0; i < data.size(); ++i) nodeId[i] = (int)(data[i]+0.0001);
      //std::copy(data.begin(), data.end(), nodeId); causes double to int warnings

    } else if (data_type == VTK_NODE_DATA && std::string(vname) == "_OWNER") {

      for (UInt i = 0; i < data.size(); ++i) nodeOwner[i] = (int)(data[i]+0.0001);

    } else {

    }

  } // while processing data

}

} // namespace
