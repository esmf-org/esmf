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
// !CLASS: ESMCI::IO_YAML - Add support for YAML language
//
// !DESCRIPTION:
//  This class enables ESMF to read, write, and parse text in
//  "YAML Ain't Markup Language" (YAML) version 1.2 via the external
//  yaml-cpp library (see: https://github.com/jbeder/yaml-cpp).
//-------------------------------------------------------------------------
//
// !USES:
#include <string>

#ifdef ESMF_YAMLCPP
#include "yaml-cpp/yaml.h"
#endif

namespace ESMCI {

 // classes and structs

  class IO_YAML;

// class definition type

  class IO_YAML {
// !PUBLIC TYPES:
    public:
      struct ParseFormat {
        enum value { Unset = 0, NUOPCFieldDictionary };
      };
      struct ContentType {
        enum value { Unset = 0, Native, NUOPCFreeFormat };
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
        ContentType::value type;
        int capacity;
        std::string buffer;
      } producer;

// !PUBLIC MEMBER FUNCTIONS:
    public:
      // native constructor/destructor
      IO_YAML(void);
      ~IO_YAML() {};
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

      // content methods
      // - create content from parsed YAML
      int cinit(const IO_YAML::ContentType::value& type);
      // - write to standard output
      int cwrite(void) const;
      // - write to output stream
      int cwrite(std::ostream& ostream) const;
      // - write to file
      int cwrite(const std::string& filename) const;
      // - return content line count
      int clinec(void) const { return producer.capacity; };
      // - return content size
      int csize(void) const { return producer.buffer.size(); };
      // - return content buffer
      const char* cget(void) const { return producer.buffer.data(); };

      // process methods:
      // - parse 
      int parse(const IO_YAML::ParseFormat::value& format);
                
// !PRIVATE MEMBER FUNCTIONS:
    private:
      // helper to process methods:
      // - parse 
      int nuopc_fd_parser(void);
//
//EOP
//-------------------------------------------------------------------------

  }; // end class IO_YAML

} // end of namespace ESMCI

#endif // ESMC_IO_YAML_H
