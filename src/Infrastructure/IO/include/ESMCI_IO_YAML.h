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
// ESMF IO YAML C++ definition include file
//
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

#ifndef ESMCI_IO_YAML_H
#define ESMCI_IO_YAML_H

//-------------------------------------------------------------------------

 // put any constants or macros which apply to the whole component in this file.
 // anything public or esmf-wide should be up higher at the top level
 // include files.
#include "ESMCI_Macros.h"

//-------------------------------------------------------------------------
//BOP
// !CLASS: ESMCI::IO_YAML - Handles low-level YAML IO for ESMF internals and
//  ESMF user API.  Currently relies on the external yaml-cpp library to 
//  process YAML 1.2 content (see: https://github.com/jbeder/yaml-cpp).
//
// !DESCRIPTION:
//  TODO
//-------------------------------------------------------------------------
//
// !USES:
#include <string>

#ifdef ESMF_YAMLCPP
#include "yaml-cpp/yaml.h"
#endif

namespace ESMCI {

  class IO_YAML;


// class definition type

  class IO_YAML {
// !PUBLIC TYPES:
    public:
      struct ParseFormat {
        enum value { Unset, NUOPCFieldDictionary };
      };
      struct OutputType {
        enum value { Unset, Native, NUOPCFieldDictionary };
      };

// !PRIVATE TYPES:
    private:
#ifdef ESMF_YAMLCPP
      YAML::Node doc;
#endif
      struct {
        ParseFormat::value format;
#ifdef ESMF_YAMLCPP
        YAML::Node data;
#endif
      } parser;

      struct {
        OutputType::value type;
        int capacity;
        std::string buffer;
      } producer;

// !PUBLIC MEMBER FUNCTIONS:
    public:
      // explicit create/destroy:
      static IO_YAML* create(int* rc);
      static int destroy(IO_YAML** ioyaml);
      // input methods:
      // - read
      int read(const std::string& filename);
      // - ingest content
      int ingest(const std::string& content);

      // output methods:
      // - write
      //   - to standard output
      int write(void) const;
      //   - to file
      int write(const std::string& filename) const;
      //   - to output stream
      int write(std::ostream& ostream) const;
      // - output
      int output(const IO_YAML::OutputType::value& type, 
                 const char* filename, 
                 char* content,
                 int* contentsize);

      // process methods:
      // - parse 
      int parse(const IO_YAML::ParseFormat::value& format);
                
// !PRIVATE MEMBER FUNCTIONS:
    private:
      // helper to process methods:
      // - parse 
      int nuopc_fd_parser(void);
  };

  // auxiliary functions to allocate/deallocate IO_YAML objects from heap
  IO_YAML* ESMCI_IO_YAMLCreate(int*);
  void ESMCI_IO_YAMLDestroy(IO_YAML**, int*);

} // end of namespace ESMCI

#endif // ESMC_IO_YAML_H
