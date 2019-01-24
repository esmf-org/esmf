// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMCI_IO_YAML.C"
//==============================================================================
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
// higher level, 3rd party or system includes here
#include <iostream>
#include <iomanip>
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

//-------------------------------------------------------------------------
//
// This section includes all the IO_YAML routines
//
//-------------------------------------------------------------------------

//-------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::IO_YAML() native constructor"
//BOPI
// !IROUTINE:  IO_YAML - native C++ constructor
//
// !INTERFACE:
      IO_YAML::IO_YAML(void)
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Initialize internal storage.
//
//EOPI
//-------------------------------------------------------------------------

 {
   // parser storage
   parser.format     = IO_YAML::ParseFormat::Unset;

   // content producer storage
   producer.type     = IO_YAML::ContentType::Unset;
   producer.buffer   = "";
   producer.capacity = 0;

 } // IO_YAML

//-------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_IO_YAML::create()"
//BOPI
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
//EOPI
//-------------------------------------------------------------------------
    // initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;

    IO_YAML* ioyaml;

    try {
      ioyaml = new IO_YAML;
    } catch(...) {
      ioyaml = ESMC_NULL_POINTER;
      ESMC_LogDefault.AllocError(ESMC_CONTEXT, rc);
      return ESMC_NULL_POINTER;
    }

    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;

    return ioyaml;
  };

//-------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_IO_YAML::destroy()"
//BOPI
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
//EOPI
//-------------------------------------------------------------------------
    // initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;

    // return with errors for NULL pointer
    if (ioyaml == ESMC_NULL_POINTER || *ioyaml == ESMC_NULL_POINTER){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
        "- Not a valid pointer to IO_YAML object", ESMC_CONTEXT, &rc);
      return rc;
    }

    try {
      delete (*ioyaml);
      *ioyaml = ESMC_NULL_POINTER;
      rc = ESMF_SUCCESS;
    } catch(...) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, 
        "- Caught exception", ESMC_CONTEXT, &rc);
    }
    return rc;

  };

//-------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_IO_YAML::read()"
//BOPI
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
//      Populate an {\tt ESMC\_IO\_YAML} object by reading a YAML file.
//      This method reads on PET 0 and broadcast the content to all
//      other PETs
//
//EOPI
//-------------------------------------------------------------------------
    // initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;

    if (filename.empty()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
        "Filename argument cannot be NULL", ESMC_CONTEXT, &rc);
      return rc;
    }

    // only read on PET 0 of current VM
    ESMCI::VM *vm = ESMCI::VM::getCurrent(&rc);
    if (ESMC_LogDefault.MsgFoundError (rc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, NULL))
      return rc;

    int localPet = vm->getLocalPet();
    int petCount = vm->getPetCount();

    size_t lbuf = 0;
    std::string yaml;

    if (localPet == 0) {
      try {
#ifdef ESMF_YAMLCPP
        // load YAML from file
        this->doc = YAML::LoadFile(filename);

        // copy content to string
        std::ostringstream os;
        os << this->doc;
        yaml = os.str();
        lbuf = yaml.size();

        rc = ESMF_SUCCESS;
#else
        // yaml-cpp library not present
        rc = ESMF_RC_LIB_NOT_PRESENT;
#endif
      } catch(...) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
          "Caught exception reading content from file", ESMC_CONTEXT, &rc);
      }
    }

    // broadcast return code
    vm->broadcast(&rc, sizeof(int), 0);
    if (ESMC_LogDefault.MsgFoundError (rc, "root PET exited with error",
      ESMC_CONTEXT, NULL))
      return rc;

    // broadcast buffer size
    rc = vm->broadcast(&lbuf, sizeof(lbuf), 0);
    if (ESMC_LogDefault.MsgFoundError (rc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, NULL))
      return rc;

    // allocate buffer on all PETs and populate it on PET 0
    std::string buf(lbuf,'\0');
    if (localPet == 0)
      buf = yaml;

    // broadcast buffer
    rc = vm->broadcast(&buf[0], lbuf, 0);
    if (ESMC_LogDefault.MsgFoundError (rc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, NULL))
      return rc;

    // now can ingest YAML content on all PETs
    rc = this->ingest(buf);
    if (ESMC_LogDefault.MsgFoundError (rc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, NULL))
      return rc;

    // return successfully
    rc = ESMF_SUCCESS;
    return rc;

  }; // IO_YAML::read

//-------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_IO_YAML::ingest()"
//BOPI
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
//EOPI
//-------------------------------------------------------------------------
    // initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;

    // internal storage is now stale, since fresh content is being ingested
    // clear stale internal storage
    parser.format     = IO_YAML::ParseFormat::Unset;
    producer.type     = IO_YAML::ContentType::Unset;
    producer.buffer   = "";
    producer.capacity = 0;

    try {
#ifdef ESMF_YAMLCPP
      this->doc = YAML::Load(content);
      rc = ESMF_SUCCESS;
#else
      // yaml-cpp library not present
      rc = ESMF_RC_LIB_NOT_PRESENT;
#endif
    } catch(...) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
        "Caught exception ingesting content", ESMC_CONTEXT, &rc);
    }

    return rc;

  }; // IO_YAML::ingest

//-------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_IO_YAML::write()"
//BOPI
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
//EOPI
//-------------------------------------------------------------------------
    // initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;

    try {
#ifdef ESMF_YAMLCPP
      std::cout << this->doc << std::endl;
      rc = ESMF_SUCCESS;
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
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_IO_YAML::write()"
//BOPI
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
//      Write YAML content of {\tt ESMC\_IO\_YAML} object to file.
//      This method only writes to file on PET 0.
//
//EOPI
//-------------------------------------------------------------------------
    // initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;

    if (filename.empty()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
        "Filename argument cannot be NULL", ESMC_CONTEXT, &rc);
      return rc;
    }

    // only read on PET 0 of current VM
    ESMCI::VM *vm = ESMCI::VM::getCurrent(&rc);
    if (ESMC_LogDefault.MsgFoundError (rc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, NULL))
      return rc;

    int localPet = vm->getLocalPet();

    // only write file on PET 0
    if (localPet == 0) {
      try {
#ifdef ESMF_YAMLCPP
        std::ofstream fout(filename);
        fout << this->doc << std::endl;
        fout.close();
        rc = ESMF_SUCCESS;
#else
        // yaml-cpp library not present
        rc = ESMF_RC_LIB_NOT_PRESENT;
#endif
      } catch(...) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
          "- Caught exception", ESMC_CONTEXT, &rc);
      }
    }

    // broadcast return code
    vm->broadcast(&rc, sizeof(int), 0);
    if (ESMC_LogDefault.MsgFoundError (rc, "root PET exited with error",
      ESMC_CONTEXT, NULL))
      return rc;

    return rc;

  }; // IO_YAML::write to file

//-------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_IO_YAML::write()"
//BOPI
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
//EOPI
//-------------------------------------------------------------------------
    // initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;

    try {
#ifdef ESMF_YAMLCPP
      ostream << this->doc << std::endl;
      rc = ESMF_SUCCESS;
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
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_IO_YAML::cinit()"
//BOPI
// !IROUTINE:  IO_YAML::cinit - create content from parsed YAML in {\tt ESMC\_IO\_YAML} object
//
// !INTERFACE:
  int IO_YAML::cinit(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
    const IO_YAML::ContentType::value& type) { // in

// !DESCRIPTION:
//      Create content from parsed YAML in {\tt ESMC\_IO\_YAML} object in the
//      format specified by type. If type is IO_YAML::ContentType::Unset,
//      no format conversion is performed and this function returns content
//      cached during a previous call.
//
//EOPI
//-------------------------------------------------------------------------
    // initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;

    std::ostringstream os;

    // reset internal storage at each call
    this->producer.type     = IO_YAML::ContentType::Unset;
    this->producer.buffer   = "";
    this->producer.capacity = 0;

    if (type == IO_YAML::ContentType::Unset) {

      // do nothing
      rc = ESMF_SUCCESS;
      return rc;

    } else if (type == IO_YAML::ContentType::Native) {

      try {
#ifdef ESMF_YAMLCPP
        os << this->doc << std::endl;
        rc = ESMF_SUCCESS;
#else
        rc = ESMF_RC_LIB_NOT_PRESENT;
#endif
      } catch(...) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
          "- Caught exception", ESMC_CONTEXT, &rc);
        return rc;
      }
    
    } else if (type == IO_YAML::ContentType::NUOPCFreeFormat) {

      if (this->parser.format == IO_YAML::ParseFormat::NUOPCFieldDictionary) {
#ifdef ESMF_YAMLCPP
        YAML::Node names = this->parser.data["standardName"];
        YAML::Node units = this->parser.data["canonicalUnits"];
        for (YAML::const_iterator it=names.begin(); it!=names.end(); ++it) {
          std::string stdname = it->first.as<std::string>();
          if (!units[stdname]) {
              ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_VALID,
                "canonical_units is required for field " + stdname, ESMC_CONTEXT, &rc);
              return rc;
          }
          os << "standardName: " << std::setw(50) << stdname << std::endl;
          os << "canonicalUnits: " << std::setw(48) << units[stdname].as<std::string>() << std::endl;
          for (YAML::const_iterator q=names[stdname].begin();q!=names[stdname].end();q++) {
            if (stdname.compare(q->as<std::string>()) != 0)
              os << "synonym: " << std::setw(55) << q->as<std::string>() << std::endl;
          }
          os << "----------------------------------------------------------------" << std::endl;
        }
#endif
        rc = ESMF_SUCCESS;
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

    this->producer.type     = type;
    this->producer.buffer   = os.str();
    this->producer.capacity = std::count(this->producer.buffer.begin(),
                                         this->producer.buffer.end(),'\n');
    // line count is set to at least 1 if buffer not empty
    this->producer.capacity =
      this->producer.capacity ? this->producer.capacity :
        ( this->producer.buffer.empty() ? 0 : 1 );

    return rc;

  }; // IO_YAML::cinit


//-------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_IO_YAML::cwrite()"
//BOPI
// !IROUTINE:  IO_YAML::cwrite - output parsed content of {\tt ESMC\_IO\_YAML} object
//
// !INTERFACE:
  int IO_YAML::cwrite(void
//
// !RETURN VALUE:
//    int return code
    ) const {

// !DESCRIPTION:
//      Write cached output from parsed YAML content of {\tt ESMC\_IO\_YAML}
//      object to standard output.
//
//EOPI
//-------------------------------------------------------------------------
    // initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;

    std::cout << this->producer.buffer;

    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }; // cwrite

//-------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_IO_YAML::cwrite()"
//BOPI
// !IROUTINE:  IO_YAML::cwrite - output parsed content of {\tt ESMC\_IO\_YAML} object
//
// !INTERFACE:
  int IO_YAML::cwrite(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
    const std::string& filename) const { // in

// !DESCRIPTION:
//      Write cached content from parsed YAML content of {\tt ESMC\_IO\_YAML}
//      object to file. If no content exists (e.g. cinit() was not called)
//      filename is NULL, return error.
//
//EOPI
//-------------------------------------------------------------------------
    // initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;

    // bail out if no filename provided
    if (filename.empty()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_BAD,
        "No filename provided", ESMC_CONTEXT, &rc);
      return rc;
    }

    // check if content is available
    if (this->producer.buffer.empty()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_BAD,
        "Content not initialized -- call cinit() first?", ESMC_CONTEXT, &rc);
      return rc;
    }

    // only read on PET 0 of current VM
    ESMCI::VM *vm = ESMCI::VM::getCurrent(&rc);
    if (ESMC_LogDefault.MsgFoundError (rc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, NULL))
      return rc;

    int localPet = vm->getLocalPet();

    // only write file on PET 0
    if (localPet == 0) {
      std::ofstream fout(filename.c_str());
      fout << this->producer.buffer;
      fout.close();
    }

    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }; // cwrite

//-------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_IO_YAML::cwrite()"
//BOPI
// !IROUTINE:  IO_YAML::cwrite - output parsed content of {\tt ESMC\_IO\_YAML} object
//
// !INTERFACE:
  int IO_YAML::cwrite(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
    std::ostream& ostream) const { // in

// !DESCRIPTION:
//      Write cached content from parsed YAML content of {\tt ESMC\_IO\_YAML}
//      object to file. If no content exists (e.g. cinit() was not called)
//      filename is NULL, return error.
//
//EOPI
//-------------------------------------------------------------------------
    // initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;

    ostream << this->producer.buffer;

    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }; // output_write

//-------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_IO_YAML::parse()"
//BOPI
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
//EOPI
//-------------------------------------------------------------------------
    // initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;

    if (format == IO_YAML::ParseFormat::NUOPCFieldDictionary) {
        rc = this->nuopc_fd_parser();
        if (ESMC_LogDefault.MsgFoundError (rc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, NULL))
          return rc;
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_OUTOFRANGE,
        "Parse format not yet implemented", ESMC_CONTEXT, &rc);
    }

    return rc;
  }; // IO_YAML::parse

//-------------------------------------------------------------------------
// 
//  private functions
//
//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_IO_YAML::nuopc_fd_parser()"
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
//-------------------------------------------------------------------------

#ifdef ESMF_YAMLCPP
    // initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;

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

    // return successfully
    rc = ESMF_SUCCESS;
#else
    int rc = ESMF_RC_LIB_NOT_PRESENT;
#endif

    return rc;

  }; // IO_YAML::nuopc_fd_parser

}  // end namespace ESMCI
