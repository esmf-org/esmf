#include <iostream>
#include <cstring>
#include <vector>
#include "ESMCI_Poly.h"
#include "ESMCI_PolyDer.h"
#include "ESMCI_Mat.h"
#include "ESMCI_Graph.h"
#include "ESMCI_GraphUtils.h"
#include "ESMC_Test.h"

class Element{
  public:
    Element(const std::string &name, int node_id):name_(name), node_id_(node_id){}
    std::string name_;
    int node_id_;
};
std::ostream &operator<<(std::ostream &ostr, const Element &elem)
{
  ostr << "(" << elem.name_.c_str() << "," << elem.node_id_ << ")";
  return ostr;
}

int main(int argc, char *argv[])
{
  int rc = 0, result = 0;
  const int ESMF_MAX_STRLEN = 128;
  char name[ESMF_MAX_STRLEN];
  char failMsg[ESMF_MAX_STRLEN];
  double tol = 0.00000001;
  bool bfs_has_dups = false, bfs_found_all_nodes = false;
  bool dfs_has_dups = false, dfs_found_all_nodes = false;

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
    ESMCI::MapperUtil::DGraph<int> gnull;
    std::vector<ESMCI::MapperUtil::DGraph<int>::vertex_key> gnull_vids;
    gnull_vids.push_back(gnull.add_node(0));

    ESMCI::MapperUtil::DGraph<int>::ColorMap cmap = gnull.create_color_map();
    //ESMCI::MapperUtil::PrintDGraphVisitor<int> vis(gnull, cmap);
    //std::cout << "Nodes : ";
    //ESMCI::MapperUtil::DGraph_BFS(gnull, vis, gnull_vids[0]);
    //std::cout << "\n";
  }
  catch(...){
    rc = ESMF_FAILURE;
  }

  strncpy(name, "Null graph create test", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Null graph create test failed", ESMF_MAX_STRLEN);
  ESMC_Test((rc == ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);

  rc = ESMF_SUCCESS;
  try{
    ESMCI::MapperUtil::DGraph<int> gsingle;
    std::vector<ESMCI::MapperUtil::DGraph<int>::vertex_key> gsingle_vids;
    gsingle_vids.push_back(gsingle.add_node(0));

    gsingle.add_edge(gsingle_vids[0], gsingle_vids[0]);
    ESMCI::MapperUtil::DGraph<int>::ColorMap cmap = gsingle.create_color_map();
    //ESMCI::MapperUtil::PrintDGraphVisitor<int> vis(gsingle, cmap);
    //std::cout << "Nodes : ";
    //ESMCI::MapperUtil::DGraph_BFS(gsingle, vis, gsingle_vids[0]);
    //std::cout << "\n";
  }
  catch(...){
    rc = ESMF_FAILURE;
  }

  strncpy(name, "Single node graph create test", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Single node graph create test failed", ESMF_MAX_STRLEN);
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

    g2.print_to_file("./SimpleGraph.dot");

    ESMCI::MapperUtil::DGraph<int>::ColorMap cmap = g2.create_color_map();
    //ESMCI::MapperUtil::PrintDGraphVisitor<int> vis(g2, cmap);
    //std::cout << "Nodes : ";
    //ESMCI::MapperUtil::DGraph_BFS(g2, vis, g2_vids[0]);
    //std::cout << "\n";
  }
  catch(...){
    rc = ESMF_FAILURE;
  }

  strncpy(name, "Simple graph create test", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Simple graph create test failed", ESMF_MAX_STRLEN);
  ESMC_Test((rc == ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);

  rc = ESMF_SUCCESS;
  try{
    ESMCI::MapperUtil::DGraph<int> gcycle;
    std::vector<ESMCI::MapperUtil::DGraph<int>::vertex_key> gcycle_vids;
    gcycle_vids.push_back(gcycle.add_node(0));
    gcycle_vids.push_back(gcycle.add_node(1));
    gcycle_vids.push_back(gcycle.add_node(2));
    gcycle_vids.push_back(gcycle.add_node(3));
    gcycle_vids.push_back(gcycle.add_node(4));

    gcycle.add_edge(gcycle_vids[0], gcycle_vids[1]);
    gcycle.add_edge(gcycle_vids[1], gcycle_vids[2]);
    gcycle.add_edge(gcycle_vids[2], gcycle_vids[3]);
    gcycle.add_edge(gcycle_vids[3], gcycle_vids[4]);
    gcycle.add_edge(gcycle_vids[4], gcycle_vids[0]);

    ESMCI::MapperUtil::DGraph<int>::ColorMap cmap = gcycle.create_color_map();
    //ESMCI::MapperUtil::PrintDGraphVisitor<int> vis(gcycle, cmap);
    //std::cout << "Nodes : ";
    //ESMCI::MapperUtil::DGraph_BFS(gcycle, vis, gcycle_vids[0]);
    //std::cout << "\n";
  }
  catch(...){
    rc = ESMF_FAILURE;
  }

  strncpy(name, "Cyclic graph create test", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Cyclic graph create test failed", ESMF_MAX_STRLEN);
  ESMC_Test((rc == ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);

  rc = ESMF_SUCCESS;
  try{
    ESMCI::MapperUtil::DGraph<int> tree;
    std::vector<ESMCI::MapperUtil::DGraph<int>::vertex_key> tree_vids;
    tree_vids.push_back(tree.add_node(0));
    tree_vids.push_back(tree.add_node(1));
    tree_vids.push_back(tree.add_node(2));
    tree_vids.push_back(tree.add_node(3));
    tree_vids.push_back(tree.add_node(4));
    tree_vids.push_back(tree.add_node(5));
    tree_vids.push_back(tree.add_node(6));

    tree.add_edge(tree_vids[1], tree_vids[0]);
    tree.add_edge(tree_vids[2], tree_vids[0]);
    tree.add_edge(tree_vids[3], tree_vids[1]);
    tree.add_edge(tree_vids[4], tree_vids[1]);
    tree.add_edge(tree_vids[5], tree_vids[2]);
    tree.add_edge(tree_vids[6], tree_vids[2]);

    ESMCI::MapperUtil::DGraph<int>::ColorMap cmap = tree.create_color_map();
    //ESMCI::MapperUtil::PrintDGraphVisitor<int> vis(tree, cmap);
    //std::cout << "Nodes : ";
    //ESMCI::MapperUtil::DGraph_BFS(tree, vis, tree_vids[0]);
    //std::cout << "\n";
    tree.print_to_file("./BinaryTree.dot");
    tree.inverse().print_to_file("./BinaryTreeInverse.dot");
  }
  catch(...){
    rc = ESMF_FAILURE;
  }

  strncpy(name, "Tree create test", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Tree create test failed", ESMF_MAX_STRLEN);
  ESMC_Test((rc == ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);

  rc = ESMF_SUCCESS;
  try{
    ESMCI::MapperUtil::DGraph<int> tree;
    std::vector<ESMCI::MapperUtil::DGraph<int>::vertex_key> tree_vids;
    tree_vids.push_back(tree.add_node(11));
    tree_vids.push_back(tree.add_node(1));
    tree_vids.push_back(tree.add_node(2));
    tree_vids.push_back(tree.add_node(3));
    tree_vids.push_back(tree.add_node(4));
    tree_vids.push_back(tree.add_node(5));
    tree_vids.push_back(tree.add_node(6));

    tree.add_edge(tree_vids[3], tree_vids[1]);
    tree.add_edge(tree_vids[4], tree_vids[1]);
    tree.add_edge(tree_vids[5], tree_vids[2]);
    tree.add_edge(tree_vids[6], tree_vids[2]);

    tree_vids.push_back(tree.add_root(0));
    ESMCI::MapperUtil::DGraph<int>::ColorMap cmap = tree.create_color_map();

    //ESMCI::MapperUtil::PrintDGraphVisitor<int> vis(tree, cmap);
    //std::cout << "Nodes : ";
    //ESMCI::MapperUtil::DGraph_BFS(tree, vis, tree_vids[0]);
    //std::cout << "\n";
    tree.print_to_file("./BinaryTreeForest.dot");
    tree.inverse().print_to_file("./BinaryTreeForestInverse.dot");
  }
  catch(...){
    rc = ESMF_FAILURE;
  }

  strncpy(name, "Forest with root create test", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Forest with root create test failed", ESMF_MAX_STRLEN);
  ESMC_Test((rc == ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);

  rc = ESMF_SUCCESS;
  bfs_has_dups = false;
  bfs_found_all_nodes = false;
  dfs_has_dups = false;
  dfs_found_all_nodes = false;
  try{
    ESMCI::MapperUtil::DGraph<int> gcomplete;
    std::vector<ESMCI::MapperUtil::DGraph<int>::vertex_key> gcomplete_vids;
    const int NNODES = 6;
    for(int i=0; i<NNODES; i++){
      gcomplete_vids.push_back(gcomplete.add_node(i));
    }
    for(int i=0; i<NNODES; i++){
      for(int j=i+1; j<NNODES; j++){
        gcomplete.add_edge(gcomplete_vids[i], gcomplete_vids[j]);
      }
    }

    gcomplete.print_to_file("./CompleteGraph.dot");

    ESMCI::MapperUtil::DGraph<int>::ColorMap cmap = gcomplete.create_color_map();
    //ESMCI::MapperUtil::PrintDGraphVisitor<int> vis(gcomplete, cmap);
    //std::cout << "Nodes : ";
    //ESMCI::MapperUtil::DGraph_BFS(gcomplete, vis, gcomplete_vids[0]);
    //std::cout << "\n";

    cmap.reset();
    ESMCI::MapperUtil::NodeValidatorVisitor<int> vvis_bfs(gcomplete, cmap);
    ESMCI::MapperUtil::DGraph_BFS(gcomplete, vvis_bfs, gcomplete_vids[0]);
    bfs_has_dups = vvis_bfs.has_dup_visits();
    if(vvis_bfs.nnodes_visited() == NNODES){
      std::cout << "BFS (Complete graph) Visited " << vvis_bfs.nnodes_visited() << " nodes\n";
      bfs_found_all_nodes = true;
    }
    cmap.reset();
    ESMCI::MapperUtil::NodeValidatorVisitor<int> vvis_dfs(gcomplete, cmap);
    ESMCI::MapperUtil::DGraph_DFS(gcomplete, vvis_dfs, gcomplete_vids[0]);
    dfs_has_dups = vvis_dfs.has_dup_visits();
    if(vvis_dfs.nnodes_visited() == NNODES){
      std::cout << "DFS (Complete graph) Visited " << vvis_dfs.nnodes_visited() << " nodes\n";
      dfs_found_all_nodes = true;
    }
  }
  catch(...){
    rc = ESMF_FAILURE;
  }

  strncpy(name, "Complete graph create test", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Complete graph create test failed", ESMF_MAX_STRLEN);
  ESMC_Test((rc == ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);

  strncpy(name, "Complete graph BFS dup visit test", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Complete graph BFS dup visit failed", ESMF_MAX_STRLEN);
  ESMC_Test((!bfs_has_dups), name, failMsg, &result, __FILE__, __LINE__, 0);

  strncpy(name, "Complete graph BFS visit test", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Complete graph BFS visit failed", ESMF_MAX_STRLEN);
  ESMC_Test((bfs_found_all_nodes), name, failMsg, &result, __FILE__, __LINE__, 0);

  strncpy(name, "Complete graph DFS dup visit test", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Complete graph DFS dup visit failed", ESMF_MAX_STRLEN);
  ESMC_Test((!dfs_has_dups), name, failMsg, &result, __FILE__, __LINE__, 0);

  strncpy(name, "Complete graph DFS visit test", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Complete graph DFS visit failed", ESMF_MAX_STRLEN);
  ESMC_Test((dfs_found_all_nodes), name, failMsg, &result, __FILE__, __LINE__, 0);

  rc = ESMF_SUCCESS;
  bfs_has_dups = false;
  bfs_found_all_nodes = false;
  dfs_has_dups = false;
  dfs_found_all_nodes = false;
  try{
    ESMCI::MapperUtil::DGraph<Element> gbenzene;
    std::vector<ESMCI::MapperUtil::DGraph<Element>::vertex_key> cvids;
    std::vector<ESMCI::MapperUtil::DGraph<Element>::vertex_key> hvids;
    const int NELEM_PAIRS = 6;
    const int NELEMS = NELEM_PAIRS * 2;
    for(int i=0; i<NELEM_PAIRS; i++){
      cvids.push_back(gbenzene.add_node(Element("C", i)));
      hvids.push_back(gbenzene.add_node(Element("H", i)));
    }
    for(int i=0; i<NELEM_PAIRS; i++){
      gbenzene.add_edge(cvids[i], cvids[(i+1)%NELEM_PAIRS]);
      gbenzene.add_edge(cvids[i], hvids[i]);
    }
    for(int i=0; i<NELEM_PAIRS; i+=2){
      gbenzene.add_edge(cvids[(i+1)%NELEM_PAIRS], cvids[i]);
    }

    gbenzene.print_to_file("./BenzeneGraph.dot");
    gbenzene.inverse().print_to_file("./BenzeneGraphInverse.dot");
    ESMCI::MapperUtil::DGraph<Element>::ColorMap cmap = gbenzene.create_color_map();
    ESMCI::MapperUtil::PrintDGraphVisitor<Element> vis(gbenzene, cmap);
    std::cout << "Nodes (BFS): ";
    ESMCI::MapperUtil::DGraph_BFS(gbenzene, vis, cvids[0]);
    std::cout << "\n";
    cmap.reset();
    std::cout << "Nodes (DFS): ";
    ESMCI::MapperUtil::DGraph_DFS(gbenzene, vis, cvids[0]);
    std::cout << "\n";

    cmap.reset();
    ESMCI::MapperUtil::NodeValidatorVisitor<Element> vvis_bfs(gbenzene, cmap);
    ESMCI::MapperUtil::DGraph_BFS(gbenzene, vvis_bfs, cvids[0]);
    bfs_has_dups = vvis_bfs.has_dup_visits();
    if(vvis_bfs.nnodes_visited() == NELEMS){
      std::cout << "BFS (Benzene graph) Visited " << vvis_bfs.nnodes_visited() << " nodes\n";
      bfs_found_all_nodes = true;
    }
    cmap.reset();
    ESMCI::MapperUtil::NodeValidatorVisitor<Element> vvis_dfs(gbenzene, cmap);
    ESMCI::MapperUtil::DGraph_DFS(gbenzene, vvis_dfs, cvids[0]);
    dfs_has_dups = vvis_dfs.has_dup_visits();
    if(vvis_dfs.nnodes_visited() == NELEMS){
      std::cout << "DFS (Benzene graph) Visited " << vvis_dfs.nnodes_visited() << " nodes\n";
      dfs_found_all_nodes = true;
    }
  }
  catch(...){
    rc = ESMF_FAILURE;
  }

  strncpy(name, "Benzene graph create test", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Benzene graph create test failed", ESMF_MAX_STRLEN);
  ESMC_Test((rc == ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);

  strncpy(name, "Benzene graph BFS dup visit test", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Benzene graph BFS dup visit failed", ESMF_MAX_STRLEN);
  ESMC_Test((!bfs_has_dups), name, failMsg, &result, __FILE__, __LINE__, 0);

  strncpy(name, "Benzene graph BFS visit test", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Benzene graph BFS visit failed", ESMF_MAX_STRLEN);
  ESMC_Test((bfs_found_all_nodes), name, failMsg, &result, __FILE__, __LINE__, 0);

  strncpy(name, "Benzene graph DFS dup visit test", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Benzene graph DFS dup visit failed", ESMF_MAX_STRLEN);
  ESMC_Test((!dfs_has_dups), name, failMsg, &result, __FILE__, __LINE__, 0);

  strncpy(name, "Benzene graph DFS visit test", ESMF_MAX_STRLEN);
  strncpy(failMsg, "Benzene graph DFS visit failed", ESMF_MAX_STRLEN);
  ESMC_Test((dfs_found_all_nodes), name, failMsg, &result, __FILE__, __LINE__, 0);

  ESMC_TestEnd(__FILE__, __LINE__, 0);
}
