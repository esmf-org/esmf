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
#include <Mesh/include/ESMCI_RefineTopo.h>

#include <map>

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

namespace ESMCI {

static UInt bar_homorefine_cnodes[] = {
0, 2, // child 0
2, 1  // child 1
};

static UInt hex_homorefine_cnodes[] = {
0, 8, 21, 11, 12, 25, 20, 23,
8, 1, 9, 21, 25, 13, 24, 20,
21, 9, 2, 10, 20, 24, 14, 26,
11, 21, 10, 3, 23, 20, 26, 15,
12, 25, 20, 23, 4, 16, 22, 19,
25, 13, 24, 20, 16, 5, 17, 22,
20, 24, 14, 26, 22, 17, 6, 18,
23, 20, 26, 15, 19, 22, 18, 7
};

static UInt quad_homorefine_cnodes[] = {
0, 4, 8, 7,
4, 1, 5, 8,
8, 5, 2, 6,
7, 8, 6, 3
};

static UInt tetra_homorefine_cnodes[] = {
0, 4, 6, 7,
4, 1, 5, 8,
6, 5, 2, 9,
7, 8, 9, 3,
8, 7, 6, 4,
6, 9, 8, 5,
9, 8, 7, 6,
5, 6, 4, 8
};

static UInt tri_homorefine_cnodes[] = {
0, 3, 5,
3, 1, 4,
5, 4, 2,
4, 5, 3
};

const RefineTopo *GetHomoRefineTopo(const std::string &tname) {
  const MeshObjTopo *topo = GetTopo(tname);
  ThrowRequire(topo);
  return GetHomoRefineTopo(topo);
}

static const HomoRefineTopo *ManufactureHomoRefineTopo(const MeshObjTopo *parent_topo) {

  const std::string &name = parent_topo->name;

  HomoRefineTopo *topo = NULL;

  if (name == "HEX8" || name == "HEX") {
    return new HomoRefineTopo(parent_topo,
                8, // nchild
                GetTopo(name), // ctopo
                hex_homorefine_cnodes, // child nodes
                GetHomoRefineTopo("QUAD_3D"), // face rtopo
                GetHomoRefineTopo("BAR2_3D")  // edge rtopo
                );
  }
  else if (name == "TETRA4" || name == "TETRA") {
    return new HomoRefineTopo(parent_topo,
                8, // nchild
                GetTopo(name), // ctopo
                tetra_homorefine_cnodes, // child nodes
                GetHomoRefineTopo("TRI3_3D"), // face rtopo
                GetHomoRefineTopo("BAR2_3D") // edge rtopo
                );
  }
  else if (name == "TRI3") {
    return new HomoRefineTopo(parent_topo,
                4, // nchild
                GetTopo(name), // ctopo
                tri_homorefine_cnodes, // child nodes
                NULL, // face rtopo
                GetHomoRefineTopo("BAR2_2D") // edge rtopo
                );
  }
  else if (name == "TRI3_3D") {
    return new HomoRefineTopo(parent_topo,
                4, // nchild
                GetTopo(name), // ctopo
                tri_homorefine_cnodes, // child nodes
                NULL, // face rtopo
                GetHomoRefineTopo("BAR2_3D") // edge rtopo
                );
  }
  else if (name == "QUAD4" || name == "QUAD") {
    return new HomoRefineTopo(parent_topo,
                4, // nchild
                GetTopo(name), // ctopo
                quad_homorefine_cnodes, // child nodes
                NULL, // face rtopo
                GetHomoRefineTopo("BAR2_2D") // edge rtopo
                );
  }
  else if (name == "QUAD4_3D" || name == "QUAD_3D") {
    return new HomoRefineTopo(parent_topo,
                4, // nchild
                GetTopo(name), // ctopo
                quad_homorefine_cnodes, // child nodes
                NULL, // face rtopo
                GetHomoRefineTopo("BAR2_3D") // edge rtopo
                );
  }
  else if (name == "SHELL" || name == "SHELL4") {
    return new HomoRefineTopo(parent_topo,
                4, // nchild
                GetTopo(name), // ctopo
                quad_homorefine_cnodes, // child nodes
                NULL, // face rtopo
                GetHomoRefineTopo("BAR2_3D") // edge rtopo
                );
  }
  else if (name == "SHELL3") {
    return new HomoRefineTopo(parent_topo,
                4, // nchild
                GetTopo(name), // ctopo
                tri_homorefine_cnodes, // child nodes
                NULL, // face rtopo
                GetHomoRefineTopo("BAR2_3D") // edge rtopo
                );
  }
  else if (name == "BAR2") {
    return new HomoRefineTopo(parent_topo,
                2, // nchild
                GetTopo(name), // ctopo
                bar_homorefine_cnodes, // child nodes
                NULL, // face rtopo
                NULL // edge topo
                );
  }
  else if (name == "BAR2_2D") {
    return new HomoRefineTopo(parent_topo,
                2, // nchild
                GetTopo(name), // ctopo
                bar_homorefine_cnodes, // child nodes
                NULL, // face rtopo
                NULL // edge topo
                );
  }
  else if (name == "BAR2_3D") {
    return new HomoRefineTopo(parent_topo,
                2, // nchild
                GetTopo(name), // ctopo
                bar_homorefine_cnodes, // child nodes
                NULL, // face rtopo
                NULL // edge topo
                );
  }
  return topo;
}


const RefineTopo *GetHomoRefineTopo(const MeshObjTopo *topo) {
  static std::map<std::string,const RefineTopo*> static_rtopo_map;
  const RefineTopo *rtopo;
  std::map<std::string, const RefineTopo*>::iterator mit;
  
  mit = static_rtopo_map.find(topo->name);

  if (mit == static_rtopo_map.end()) {
    rtopo = ManufactureHomoRefineTopo(topo);
    static_rtopo_map[topo->name] = rtopo;
  } else rtopo = mit->second;

  return rtopo;
}


} // namespace

