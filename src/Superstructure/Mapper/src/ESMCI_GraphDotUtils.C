#include "ESMCI_GraphDotUtils.h"

namespace ESMCI{
  namespace MapperUtil{

    /* Dependency graph printer that writes the graph out in the DOT
     * graph format, https://www.graphviz.org/doc/info/lang.html
     */
    DGraphPrinter::DGraphPrinter(const std::string &ofile_name):ofile_(ofile_name)
    {
      print_header();
    }

    void DGraphPrinter::print_edge(const std::string &v1, const std::string &v2)
    {
      const std::string indent("\t");
      const std::string strdec("\"");
      const std::string space(" ");
      const std::string edge_conn("->");
      const std::string newline("\n");
      std::string edge_str = indent
                              + strdec + v1 + strdec
                              + space + edge_conn + space
                              + strdec + v2 + strdec
                              + newline;
      ofile_.write(edge_str.c_str(), strlen(edge_str.c_str()));
    }

    void DGraphPrinter::print_header(void )
    {
      const char *header = "digraph G {\n";
      ofile_.write(header, strlen(header));
    }

    void DGraphPrinter::print_footer(void )
    {
      const char *footer = "}\n";
      ofile_.write(footer, strlen(footer));
    }

    DGraphPrinter::~DGraphPrinter()
    {
      try{
        if(ofile_.is_open()){
          print_footer();
          ofile_.close();
        }
      }
      catch(...){
        std::cerr << "Error closing output file\n";
      }
    } 

  } // namespace MapperUtil
} //namespace ESMCI
