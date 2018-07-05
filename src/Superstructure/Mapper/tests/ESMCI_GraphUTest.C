#include <iostream>
#include <cstring>
#include <vector>
#include "ESMCI_Poly.h"
#include "ESMCI_PolyDer.h"
#include "ESMCI_Mat.h"
#include "ESMCI_Graph.h"
#include "ESMCI_GraphUtils.h"
#include "ESMC_Test.h"

int main(int argc, char *argv[])
{
  int rc = 0, result = 0;
  const int ESMF_MAX_STRLEN = 128;
  char name[ESMF_MAX_STRLEN];
  char failMsg[ESMF_MAX_STRLEN];
  double tol = 0.00000001;

  ESMC_TestStart(__FILE__, __LINE__, 0);

  rc = ESMF_SUCCESS;
  try{
    ESMCI::MapperUtil::DGraph<int> g1;
  }
  catch(...){
    rc = ESMF_FAILURE;
  }

  strncpy(name, "Graph create test", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Graph create test failed", ESMF_MAX_STRLEN);
  ESMC_Test((rc == ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);

  rc = ESMF_SUCCESS;
  try{
    ESMCI::MapperUtil::DGraph<int> g2;
    std::vector<ESMCI::MapperUtil::DGraph<int>::vertex_key> g2_vids;
    g2_vids.push_back(g2.add_node(0));
    g2_vids.push_back(g2.add_node(1));
    g2_vids.push_back(g2.add_node(2));

    g2.add_edge(g2_vids[0], g2_vids[1]);
    g2.add_edge(g2_vids[0], g2_vids[2]);

    ESMCI::MapperUtil::DGraph<int>::ColorMap cmap = g2.create_color_map();
    ESMCI::MapperUtil::PrintBFSVisitor<int> vis(g2, cmap);
    std::cout << "Nodes : ";
    ESMCI::MapperUtil::DGraph_BFS(g2, vis, g2_vids[0]);
    std::cout << "\n";
  }
  catch(...){
    rc = ESMF_FAILURE;
  }

  strncpy(name, "Simple graph create test", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Simple graph create test failed", ESMF_MAX_STRLEN);
  ESMC_Test((rc == ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);


  ESMC_TestEnd(__FILE__, __LINE__, 0);
}
