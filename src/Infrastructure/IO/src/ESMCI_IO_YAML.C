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
// ESMC IO_YAML method code (body) file
//
//-------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt ESMC\_IO\_YAML} methods declared
// in the companion file {\tt ESMCI\_IO\_YAML.h}
//
//-------------------------------------------------------------------------
#define ESMC_FILENAME "ESMCI_IO_YAML.C"

// higher level, 3rd party or system includes here
#include <iostream>
#include <string>

// associated class definition files
#include "ESMCI_IO_YAML.h"

// ESMF headers
#include "ESMCI_LogErr.h"
#include "ESMCI_Util.h"
#include "ESMCI_VM.h"


//-------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-------------------------------------------------------------------------

namespace ESMCI {

//
//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
//
// This section includes all the IO_YAML routines
//
//

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  IO_YAML::create - explicit creator
//
// !INTERFACE:
  IO_YAML* IO_YAML::create(
//
// !RETURN VALUE:
//    Pointer to new IO_YAML class object
//
// !ARGUMENTS:
    int* rc) {       // out

// !DESCRIPTION:
//      Create a new {\tt ESMC\_IO\_YAML} object
//
//EOP
// !REQUIREMENTS:

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_IO_YAML::create()"

    IO_YAML* ioyaml;

    try {
      ioyaml = new IO_YAML;
    } catch(...) {
      ioyaml = ESMC_NULL_POINTER;
      ESMC_LogDefault.AllocError(ESMC_CONTEXT, rc);
      return ESMC_NULL_POINTER;
    }

    if (rc != NULL)
      *rc = ESMF_SUCCESS;

    return ioyaml;
  };

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  IO_YAML::destroy - delete IO_YAML object
//
// !INTERFACE:
  int IO_YAML::destroy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
    IO_YAML** ioyaml) {       // in

// !DESCRIPTION:
//      Delete an {\tt ESMC\_IO\_YAML} object and set pointer to NULL
//
//EOP
// !REQUIREMENTS:

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_IO_YAML::destroy()"

    int rc = ESMF_SUCCESS;

    // return with errors for NULL pointer
    if (ioyaml == ESMC_NULL_POINTER || *ioyaml == ESMC_NULL_POINTER){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
        "- Not a valid pointer to IO_YAML object", ESMC_CONTEXT, &rc);
      return rc;
    }

    try {
      delete (*ioyaml);
      *ioyaml = ESMC_NULL_POINTER;
    } catch(...) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, 
        "- Caught exception", ESMC_CONTEXT, &rc);
    }
    return rc;

  };

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  IO_YAML::read - read in a YAML 1.2 file
//
// !INTERFACE:
  int IO_YAML::read(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
    const std::string& filename) {       // in

// !DESCRIPTION:
//      Populate an {\tt ESMC\_IO\_YAML} object by reading a YAML file
//
//EOP
// !REQUIREMENTS:

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_IO_YAML::read()"

    int rc = ESMF_SUCCESS;

    if (filename.empty()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
        "Filename argument cannot be NULL", ESMC_CONTEXT, &rc);
      return rc;
    }

/*
    int localPet, petCount;
    ESMCI::VM *globalVM;

    // only read on pet 0
    globalVM = ESMCI::VM::getGlobal(&rc);
    if (ESMC_LogDefault.MsgFoundError (rc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, NULL))
      return rc;

    localPet = globalVM->getLocalPet();
    petCount = globalVM->getPetCount();

#ifdef ESMF_YAMLCPP

    if (localPet == 0) {
      try {
        this->doc = YAML::LoadFile(filename);
      } catch(...) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
          "- Caught exception", ESMC_CONTEXT, &rc);
      }
    }
    if (petCount > 1) {
      std::string buffer;
      if (localPet == 0) {
        std::ostringstream os;
        os << this->doc;
        buffer = os.str();
        

      bu
      

localrc = vmp->broadcast(bcstData, len, rootPet);
      
     
    }
      
      
#else
    // yaml-cpp library not present
    rc = ESMF_RC_LIB_NOT_PRESENT;

#endif
*/

    // read from all PETs for now
#ifdef ESMF_YAMLCPP

    try {
      this->doc = YAML::LoadFile(filename);
    } catch(...) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
        "- Caught exception", ESMC_CONTEXT, &rc);
    }

#else

    // yaml-cpp library not present
    rc = ESMF_RC_LIB_NOT_PRESENT;

#endif

    return rc;

  }; // IO_YAML::read

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  IO_YAML::ingest - read YAML content from string
//
// !INTERFACE:
  int IO_YAML::ingest(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
    const std::string& content) {       // in

// !DESCRIPTION:
//      Populate an {\tt ESMC\_IO\_YAML} object by reading from YAML string
//
//EOP
// !REQUIREMENTS:

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_IO_YAML::ingest()"

    int rc = ESMF_SUCCESS;

    try {
#ifdef ESMF_YAMLCPP
      this->doc = YAML::Load(content);
#else
      // yaml-cpp library not present
      rc = ESMF_RC_LIB_NOT_PRESENT;
#endif
    } catch(...) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
        "- Caught exception", ESMC_CONTEXT, &rc);
    }

    return rc;

  }; // IO_YAML::ingest

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  IO_YAML::write - write {\tt ESMC\_IO\_YAML} content to standard output
//
// !INTERFACE:
  int IO_YAML::write(void
//
// !RETURN VALUE:
//    int return code
//
    ) const {
// !DESCRIPTION:
//      Write YAML content of {\tt ESMC\_IO\_YAML} object to standard output
//
//EOP
// !REQUIREMENTS:

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_IO_YAML::write()"

    int rc = ESMF_SUCCESS;

    try {
#ifdef ESMF_YAMLCPP
      std::cout << this->doc << std::endl;
#else
      // yaml-cpp library not present
      rc = ESMF_RC_LIB_NOT_PRESENT;
#endif
    } catch(...) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
        "- Caught exception", ESMC_CONTEXT, &rc);
    }

    return rc;

  }; // IO_YAML::write to stdout

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  IO_YAML::write - write {\tt ESMC\_IO\_YAML} content to file
//
// !INTERFACE:
  int IO_YAML::write(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
    const std::string& filename) const {       // in

// !DESCRIPTION:
//      Write YAML content of {\tt ESMC\_IO\_YAML} object to file
//
//EOP
// !REQUIREMENTS:

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_IO_YAML::write()"

    int rc = ESMF_SUCCESS;

    if (filename.empty()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
        "Filename argument cannot be NULL", ESMC_CONTEXT, &rc);
      return rc;
    }

    try {
#ifdef ESMF_YAMLCPP
      std::ofstream fout(filename);
      fout << this->doc << std::endl;
      fout.close();
#else
      // yaml-cpp library not present
      rc = ESMF_RC_LIB_NOT_PRESENT;
#endif
    } catch(...) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
        "- Caught exception", ESMC_CONTEXT, &rc);
    }

    return rc;

  }; // IO_YAML::write to file

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  IO_YAML::write - write {\tt ESMC\_IO\_YAML} content to output stream
//
// !INTERFACE:
  int IO_YAML::write(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
    std::ostream& ostream) const {       // in

// !DESCRIPTION:
//      Write YAML content of {\tt ESMC\_IO\_YAML} object to output stream
//
//EOP
// !REQUIREMENTS:

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_IO_YAML::write()"

    int rc = ESMF_SUCCESS;

    try {
#ifdef ESMF_YAMLCPP
      ostream << this->doc << std::endl;
#else
      // yaml-cpp library not present
      rc = ESMF_RC_LIB_NOT_PRESENT;
#endif
    } catch(...) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
        "- Caught exception", ESMC_CONTEXT, &rc);
    }

    return rc;

  }; // IO_YAML::write to output stream


//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  IO_YAML::output - output parsed content of {\tt ESMC\_IO\_YAML} object
//
// !INTERFACE:
  int IO_YAML::output(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
    const IO_YAML::OutputType::value& outType, // in
    const char* filename,                      // in
    char* content,                             // out
    int*  contentsize) {                       // out

// !DESCRIPTION:
//      Output parsed YAML content of {\tt ESMC\_IO\_YAML} object in the 
//      format specified by outType. If outType is IO_YAML::OutputType::Unset,
//      no format conversion is performed and this function returns content
//      cached during a previous call.
//
//EOP
// !REQUIREMENTS:

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_IO_YAML::output()"

    int rc = ESMF_SUCCESS;

    if (outType == IO_YAML::OutputType::Unset) {

      // return cached content if available
      if (this->producer.buffer.empty()) 
        return rc;

    } else if (outType == IO_YAML::OutputType::Native) {

      try {
        std::ostringstream os;
#ifdef ESMF_YAMLCPP
        os << this->doc << std::endl;
#endif
        this->producer.type     = outType;
        this->producer.buffer   = os.str();
        this->producer.capacity = this->producer.buffer.size();
      } catch(...) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
          "- Caught exception", ESMC_CONTEXT, &rc);
        return rc;
      }
    
    } else if (outType == IO_YAML::OutputType::NUOPCFieldDictionary) {

      if (this->parser.format == IO_YAML::ParseFormat::NUOPCFieldDictionary) {
#ifdef ESMF_YAMLCPP
        YAML::Node names = this->parser.data["standardName"];
        YAML::Node units = this->parser.data["canonicalUnits"];
        std::ostringstream os;
        for (YAML::const_iterator it=names.begin(); it!=names.end(); ++it) {
          std::string stdname = it->first.as<std::string>();
          if (!units[stdname]) {
              ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_VALID,
                "canonical_units is required for field " + stdname, ESMC_CONTEXT, &rc);
              return rc;
          }
          os << "standardName:          " << stdname << std::endl;
          os << "canonicalUnits:        " << units[stdname] << std::endl;
          for (YAML::const_iterator q=names[stdname].begin();q!=names[stdname].end();q++) {
            if (stdname.compare(q->as<std::string>()) != 0)
              os << "synonym:               " << q->as<std::string>() << std::endl;
          }
          os << "----------------------------------------------------------------" << std::endl;
          this->producer.type     = outType;
          this->producer.buffer   = os.str();
          this->producer.capacity = std::count(this->producer.buffer.begin(),
                                               this->producer.buffer.end(),'\n');
        }
#endif
      } else {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
          "No suitable content for this output type", ESMC_CONTEXT, &rc);
        return rc;
      }
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_OUTOFRANGE,
        "Output format not yet implemented", ESMC_CONTEXT, &rc);
      return rc;
    }

    // retrieve previously cached or just created content
    // write content to file
    if (filename) {
      std::ofstream fout(filename);
      fout << this->producer.buffer;
      fout.close();
    } 

    // copy content to input char buffer
    if (content) {
      if (this->producer.buffer.empty()) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_BAD,
          "No stored content", ESMC_CONTEXT, &rc);
        return rc;
      }
      if (strlen(content) < this->producer.buffer.size()) {
        char logMsg[ESMF_MAXSTR];
        sprintf(logMsg, "content size (%lu) must be at least %lu",
          strlen(content), this->producer.buffer.size());
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE, logMsg, 
          ESMC_CONTEXT, &rc);
        return rc;
      } else {
          int lenbuf = this->producer.buffer.size();
          (void) this->producer.buffer.copy(content,lenbuf);
          content[lenbuf]='\0';
      }
    }

    // return content size
    if (contentsize) {
      *contentsize = this->producer.capacity;
    }

    return rc;

  }; // IO_YAML::output

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  IO_YAML::parse - parse content of {\tt ESMC\_IO\_YAML} object
//
// !INTERFACE:
  int IO_YAML::parse(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
    const IO_YAML::ParseFormat::value& format) { // in

// !DESCRIPTION:
//      Parse content of {\tt ESMC\_IO\_YAML} object according to input format.
//      The resulting data is stored internally and can be accessed by the
//      {\tt ESMCI::IO\_YAML::output()} function.
//
//EOP
// !REQUIREMENTS:

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_IO_YAML::parse()"

    int rc = ESMF_SUCCESS;

    if (format == IO_YAML::ParseFormat::NUOPCFieldDictionary) {
        rc = this->nuopc_fd_parser();
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_OUTOFRANGE,
        "Parse format not yet implemented", ESMC_CONTEXT, &rc);
      return rc;
    }

    return rc;
  }; // IO_YAML::parse

//-------------------------------------------------------------------------
// 
//  private functions
//
//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
//BOPI
// !IROUTINE:  IO_YAML::nuopc_fd_parser - parser for NUOPC Field Dictionary
//
// !INTERFACE:
  int IO_YAML::nuopc_fd_parser(void
//
// !RETURN VALUE:
//    int return code
//
    ) {
// !DESCRIPTION:
//      Interpret content of {\tt ESMC\_IO\_YAML} object according to the
//      NUOPC Field Dictionary format and store the corresponding data
//      internally. The resulting data can be accessed via the
//      {\tt ESMCI::IO\_YAML::output()} function.
//
//EOPI
// !REQUIREMENTS:

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_IO_YAML::parse()"

#ifdef ESMF_YAMLCPP
    int rc = ESMF_SUCCESS;

    YAML::Node dict;
    YAML::Node node;
    YAML::Node names;
    YAML::Node units;

    // lookup starting point
    if (this->doc["field_dictionary"]) {
      dict = this->doc["field_dictionary"];
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_FOUND,
        "NUOPC Field Dictionary not found", ESMC_CONTEXT, &rc);
      return rc;
    }

    // sanity check
    if (!dict.IsMap()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_VALID,
        "field_dictionary must be a YAML map", ESMC_CONTEXT, &rc);
      return rc;
    }

    // lookup dictionary entries
    if (dict["entries"]) {
      node = dict["entries"];
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_FOUND,
        "no entries found", ESMC_CONTEXT, &rc);
      return rc;
    }

    // sanity check
    if (!node.IsSequence()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_VALID,
        "field_dictionary must be a YAML sequence", ESMC_CONTEXT, &rc);
      return rc;
    }

    // iterate over list of maps
    for (YAML::const_iterator it=node.begin(); it!=node.end(); it++) {

      // verify that item is a map
      if (!it->IsMap()) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_VALID,
          "entry items must be formatted as maps (key: value)",
          ESMC_CONTEXT, &rc);
        return rc;
      }

      // check mandatory keywords
      std::string stdname;
      // - standard_name
      if ((*it)["standard_name"]) {
        // node holding variables as p should have local scope to avoid corruption of main doc
        YAML::Node p = (*it)["standard_name"];
        if (p.IsScalar()) {
          // add standard name to the list of synonyms
          stdname = p.as<std::string>();
          names[stdname].push_back(stdname);
        } else {
          ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_VALID,
            "standard_name values must be YAML scalars", ESMC_CONTEXT, &rc);
          return rc;
        }
      } else {
          ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_VALID,
            "entry items must include keyword 'standard_name'", ESMC_CONTEXT, &rc);
          return rc;
      }

      if ((*it)["alias"]) {
        YAML::Node p = (*it)["alias"];
        // aliases can be scalar or lists
        if (p.IsScalar()) {
          names[stdname].push_back(p.as<std::string>());
        } else if (p.IsSequence()) {
          for (YAML::const_iterator q=p.begin(); q!=p.end(); q++)
            names[stdname].push_back(q->as<std::string>());
        } else {
          ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_VALID,
            "alias values must be YAML scalar or sequence", ESMC_CONTEXT, &rc);
          return rc;
        }
      }

      // check if units
      if ((*it)["canonical_units"]) {
        YAML::Node p = (*it)["canonical_units"];
        // canonical units must also be a scalar
        if (!p.IsScalar()) {
          ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_VALID,
            "canonical_units values must be YAML scalars", ESMC_CONTEXT, &rc);
          return rc;
        }
        // store units for standard name
        units[stdname] = p.as<std::string>();
      }
    }

    // additional sanity check -- check if all mandatory attributes are present
    // loop over existing standard names
    for (YAML::const_iterator it=names.begin();it!=names.end(); ++it) {
      // loop over synonyms -- FreeFormat requires one field per synonym
      std::string stdname = it->first.as<std::string>();
      if (!units[stdname]) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_VALID,
          "canonical_units is required for field: " + stdname, ESMC_CONTEXT, &rc);
        return rc;
      }
    }

    // save parsed data internally
    this->parser.data["standardName"]   = names;
    this->parser.data["canonicalUnits"] = units;
    this->parser.format = IO_YAML::ParseFormat::NUOPCFieldDictionary;

    return rc;

#else
    int rc = ESMC_RC_LIB_NOT_PRESENT
#endif

    return rc;

  }; // IO_YAML::nuopc_fd_parser

}  // end namespace ESMCI
