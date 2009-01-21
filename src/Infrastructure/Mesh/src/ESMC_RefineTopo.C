// $Id: ESMC_RefineTopo.C,v 1.2.2.2 2009/01/21 21:25:23 cdeluca Exp $
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
#include <ESMC_RefineTopo.h>

#include <map>

namespace ESMCI {
namespace MESH {

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

static const HomoRefineTopo *ManufactureHomoRefineTopo(const std::string &name) {
  HomoRefineTopo *topo = NULL;
  if (name == "HEX8" || name == "HEX") {
    return new HomoRefineTopo(8, // nchild
                GetTopo(name), // ctopo
                hex_homorefine_cnodes, // child nodes
                GetHomoRefineTopo("QUAD_3D"), // face rtopo
                GetHomoRefineTopo("BAR2_3D")  // edge rtopo
                );
  }
  else if (name == "TETRA4" || name == "TETRA") {
    return new HomoRefineTopo(8, // nchild
                GetTopo(name), // ctopo
                tetra_homorefine_cnodes, // child nodes
                GetHomoRefineTopo("TRI3_3D"), // face rtopo
                GetHomoRefineTopo("BAR2_3D") // edge rtopo
                );
  }
  else if (name == "TRI3") {
    return new HomoRefineTopo(4, // nchild
                GetTopo(name), // ctopo
                tri_homorefine_cnodes, // child nodes
                NULL, // face rtopo
                GetHomoRefineTopo("BAR2_2D") // edge rtopo
                );
  }
  else if (name == "TRI3_3D") {
    return new HomoRefineTopo(4, // nchild
                GetTopo(name), // ctopo
                tri_homorefine_cnodes, // child nodes
                NULL, // face rtopo
                GetHomoRefineTopo("BAR2_3D") // edge rtopo
                );
  }
  else if (name == "QUAD4" || name == "QUAD") {
    return new HomoRefineTopo(4, // nchild
                GetTopo(name), // ctopo
                quad_homorefine_cnodes, // child nodes
                NULL, // face rtopo
                GetHomoRefineTopo("BAR2_2D") // edge rtopo
                );
  }
  else if (name == "QUAD4_3D" || name == "QUAD_3D") {
    return new HomoRefineTopo(4, // nchild
                GetTopo(name), // ctopo
                quad_homorefine_cnodes, // child nodes
                NULL, // face rtopo
                GetHomoRefineTopo("BAR2_3D") // edge rtopo
                );
  }
  else if (name == "SHELL" || name == "SHELL4") {
    return new HomoRefineTopo(4, // nchild
                GetTopo(name), // ctopo
                quad_homorefine_cnodes, // child nodes
                NULL, // face rtopo
                GetHomoRefineTopo("BAR2_3D") // edge rtopo
                );
  }
  else if (name == "SHELL3") {
    return new HomoRefineTopo(4, // nchild
                GetTopo(name), // ctopo
                tri_homorefine_cnodes, // child nodes
                NULL, // face rtopo
                GetHomoRefineTopo("BAR2_3D") // edge rtopo
                );
  }
  else if (name == "BAR2") {
    return new HomoRefineTopo(2, // nchild
                GetTopo(name), // ctopo
                bar_homorefine_cnodes, // child nodes
                NULL, // face rtopo
                NULL // edge topo
                );
  }
  else if (name == "BAR2_2D") {
    return new HomoRefineTopo(2, // nchild
                GetTopo(name), // ctopo
                bar_homorefine_cnodes, // child nodes
                NULL, // face rtopo
                NULL // edge topo
                );
  }
  else if (name == "BAR2_3D") {
    return new HomoRefineTopo(2, // nchild
                GetTopo(name), // ctopo
                bar_homorefine_cnodes, // child nodes
                NULL, // face rtopo
                NULL // edge topo
                );
  }
  return topo;
}


const RefineTopo *GetHomoRefineTopo(const std::string &tname) {
  static std::map<std::string,const RefineTopo*> static_rtopo_map;
  const RefineTopo *rtopo;
  std::map<std::string, const RefineTopo*>::iterator mit;
  
  mit = static_rtopo_map.find(tname);

  if (mit == static_rtopo_map.end()) {
    rtopo = ManufactureHomoRefineTopo(tname);
    static_rtopo_map[tname] = rtopo;
  } else rtopo = mit->second;

  return rtopo;
}


} // namespace
} // namespace

