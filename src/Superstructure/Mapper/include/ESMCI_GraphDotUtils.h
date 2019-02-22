#ifndef ESMCI_GraphDotUtils_H
#define ESMCI_GraphDotUtils_H

#include <iostream>
#include <fstream>
#include <string>
#include <cstring>

namespace ESMCI{
  namespace MapperUtil{

    /* The DGraph printer base class */
    class DGraphPrinter{
      public:
        DGraphPrinter(const std::string &ofile_name);
        void print_edge(const std::string &v1, const std::string &v2);
        ~DGraphPrinter();
      private:
        void print_header(void );
        void print_footer(void );
        std::ofstream ofile_;
    };
  } // namespace MapperUtil
} //namespace ESMCI

#endif // ESMCI_GraphDotUtils_H
