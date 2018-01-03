// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

#define ESMC_FILENAME "./src/Infrastructure/Attribute/src/ESMCI_Attribute.C"

// Attribute method implementation (body) file

// single blank line to make protex happy.
//BOPI

//EOPI
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt Attribute} methods declared
// in the companion file ESMCI_Attribute.h
//
//-----------------------------------------------------------------------------
// associated class definition file and others
#include "ESMCI_Attribute.h"

#include "ESMCI_Macros.h"
#include "ESMCI_IO_XML.h"
#include "ESMCI_Base.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_Time.h"
#include "ESMCI_Grid.h"

#include <sstream>
#include <cstring>
#include <fstream>
#include <cstdlib>
#include <vector>
#include <algorithm>

using std::string;
using std::vector;
using std::ostringstream;
using std::ofstream;
using std::transform;

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id$";
//-----------------------------------------------------------------------------


namespace ESMCI {

// initialize static pointer to Attribute on which AttributeWrite*()
// method was called; used for multiple nested recursions of the tree
// (e.g. for CIM output)
Attribute *Attribute::writeRoot = ESMC_NULL_POINTER;

// initialize static Attribute instance counter
// TODO: inherit from ESMC_Base class
//      -- but circular dependency exists
//         with 'root' in ESMC_Base
int Attribute::count = 0;

// initialize static convention and purpose strings
const char Attribute::CF_CONV[]      = "CF";
const char Attribute::ESG_CONV[]     = "ESG";
const char Attribute::ESMF_CONV[]    = "ESMF";
const char Attribute::CIM_1_5_CONV[]  = "CIM 1.5";
const char Attribute::CIM_1_5_1_CONV[]  = "CIM 1.5.1";
const char Attribute::CIM_1_7_1_CONV[]  = "CIM 1.7.1";

const char Attribute::GENERAL_PURP[]    = "General";
const char Attribute::EXTENDED_PURP[]   = "Extended";
const char Attribute::INPUTS_PURP[]     = "Inputs";
const char Attribute::MODEL_COMP_PURP[] = "ModelComp";
const char Attribute::PLATFORM_PURP[]   = "Platform";
const char Attribute::RESP_PARTY_PURP[] = "RespParty";
const char Attribute::CITATION_PURP[]   = "Citation";
const char Attribute::SCI_PROP_PURP[]   = "SciProp";
const char Attribute::COMP_PROP_PURP[]  = "CompProp";
const char Attribute::GRIDS_PURP[]   = "grids";


//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the Attribute routines
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// PRIVATE:
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "Attribute"
//BOPI
// !IROUTINE:  Attribute - empty private copy constructor
//
// !INTERFACE:
      Attribute::Attribute(
//
// !ARGUMENTS:
      const Attribute&) {
// 
// !RETURN VALUE:
//    {\tt Attribute} object.
// 
// !DESCRIPTION:
//    Empty private copy constructor.
//
//EOPI

}  // end Attribute

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// PUBLIC:
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttPackAddAttribute"
//BOPI
// !IROUTINE:  AttPackAddAttribute() - add an {\tt Attribute} to an attpack
//
// !INTERFACE:
      int Attribute::AttPackAddAttribute(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,                    // in - Attribute name
      const string &convention,              // in - Attribute convention
      const string &purpose,                 // in - Attribute purpose
      const string &object) {                // in - Attribute object type 
// 
// !DESCRIPTION:
//     Add an {\tt Attribute} with a specified name but no value.
//
//EOPI

  int localrc;
  Attribute *attr, *attpack;

  attr = NULL; attpack = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Search for the attpack, make it if not found
  string attPackInstanceName;
  attpack = AttPackGet(convention, purpose, object, attPackInstanceName,
                   ESMC_ATTNEST_ON);
  if(!attpack) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_NOT_CREATED, 
        "Cannot find the specified Attribute package\n", ESMC_CONTEXT, &localrc);
      return localrc;
  }
  
  // make an Attribute in the new attpack
  attr = new Attribute(name, convention, purpose, object);  
  if (!attr) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_NOT_CREATED,
      "failed initialized an attpack Attribute", ESMC_CONTEXT, &localrc);
    return localrc;
  }
  
  // add the new Attribute to the new attpack
  localrc = attpack->AttributeSet(attr);
  if (localrc != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ATTR_NOTSET,
      "failed adding an attpack Attribute", ESMC_CONTEXT, &localrc);
    return localrc;
  }
  
  return ESMF_SUCCESS;

}  // end AttPackAddAttribute()
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttPackAddAttribute"
//BOPI
// !IROUTINE:  AttPackAddAttribute() - add an {\tt Attribute} to this attpack
//
// !INTERFACE:
      int Attribute::AttPackAddAttribute(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name) {                    // in - Attribute name
// 
// !DESCRIPTION:
//     Add an {\tt Attribute} with a specified name but no value.
//
//EOPI

  int localrc;
  Attribute *attr;

  attr = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  if (!attrPackHead) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
      "cannot add attpack attribute to non-attpack", ESMC_CONTEXT, &localrc);
    return localrc;
  }

  // make an Attribute in the new attpack
  attr = new Attribute(name, attrConvention, attrPurpose, attrObject);  
  if (!attr) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_NOT_CREATED,
      "failed initialized an attpack Attribute", ESMC_CONTEXT, &localrc);
    return localrc;
  }
  
  // add the new Attribute to this attPack
  localrc = AttributeSet(attr);
  if (localrc != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ATTR_NOTSET,
      "failed adding an attpack Attribute", ESMC_CONTEXT, &localrc);
    return localrc;
  }

  return ESMF_SUCCESS;

}  // end AttPackAddAttribute()
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttPackCreateCustom"
//BOPI
// !IROUTINE:  AttPackCreateCustom() - create an attpack
//
// !INTERFACE:
      Attribute *Attribute::AttPackCreateCustom(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &convention,              // in - Attribute convention
      const string &purpose,                 // in - Attribute purpose
      const string &object) {                // in - Attribute object type
//
// !DESCRIPTION:
//     Setup the name, convention and purpose of an attpack.
//
//EOPI

  int localrc;
  Attribute *attpack;
  attpack = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // name the attribute package using convention, purpose, and object
  attpack = new Attribute(convention, purpose, object);
  if (!attpack) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_NOT_CREATED,
      "failed initializing an attpack", ESMC_CONTEXT, &localrc);
    return NULL;
  }

  localrc = AttPackSet(attpack);
  if (localrc != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ATTR_NOTSET,
      "failed adding an attpack to an Attribute", ESMC_CONTEXT, &localrc);
    return NULL;
  }

  return attpack;

}  // end AttPackCreateCustom()
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttPackCreateStandard"
//BOPI
// !IROUTINE:  AttPackCreateStandard() - create an attpack
//
// !INTERFACE:
      int Attribute::AttPackCreateStandard(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &convention,              // in - Attribute convention
      const string &purpose,                 // in - Attribute purpose
      const string &object) {                // in - Attribute object type
// 
// !DESCRIPTION:
//     Setup the name, convention and purpose of an attpack.
//
//EOPI

  int localrc;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  Attribute *attr;
  Attribute *attpack;

  // Grid standard Attribute package
  if (object.compare("grid")==0) {
    if ((convention.compare(CIM_1_5_1_CONV)==0 || 
         convention.compare(CIM_1_7_1_CONV)==0) && 
         purpose.compare(GRIDS_PURP)==0) {

      // create an Attribute package for grids which uses internal Grid info
      attpack = AttPackCreateCustom(convention, GRIDS_PURP, object);
      if (attpack == NULL) {
          ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                        ESMC_CONTEXT, &localrc);
          return localrc;
      }

      // add Attributes to the grids Attribute package
      // and set the Attributes in this Attpack to have links to internal info
      //RLO: this removed when AttPackCreateCustom changed to return attpack
      /*string attPackInstanceName;
      attpack = AttPackGet(convention, GRIDS_PURP, object, attPackInstanceName,
                       ESMC_ATTNEST_ON);*/
  
      string name, value;
      vector<string> vv;

      name = "shortName";
      value = "ESMF:name";
      vv.clear();
      vv.push_back(value);
      localrc = attpack->AttributeSet(name, vv.size(), &vv);
      name = "longName";
      value = "CF 1.6 formatted ESMF Grid definition";
      vv.clear();
      vv.push_back(value);
      localrc = attpack->AttributeSet(name, vv.size(), &vv);
      name = "gridTile";
      value = "ESMF:tileCount";
      vv.clear();
      vv.push_back(value);
      localrc = attpack->AttributeSet(name, vv.size(), &vv);
      name = "id";
      value = "ESMF:name";
      vv.clear();
      vv.push_back(value);
      localrc = attpack->AttributeSet(name, vv.size(), &vv);
      name = "discretizationType";
      value = "logically_rectangular";
      vv.clear();
      vv.push_back(value);
      localrc = attpack->AttributeSet(name, vv.size(), &vv);
      name = "geometryType";
      value = "sphere";
      vv.clear();
      vv.push_back(value);
      localrc = attpack->AttributeSet(name, vv.size(), &vv);
      name = "numDims";
      value = "ESMF:dimCount";
      vv.clear();
      vv.push_back(value);
      localrc = attpack->AttributeSet(name, vv.size(), &vv);
      name = "xcoords";
      value = "ESMF:farrayPtr";
      vv.clear();
      vv.push_back(value);
      value = "Input:coordDim=1";
      vv.push_back(value);
      localrc = attpack->AttributeSet(name, vv.size(), &vv);
      name = "ycoords";
      value = "ESMF:farrayPtr";
      vv.clear();
      vv.push_back(value);
      value = "Input:coordDim=2";
      vv.push_back(value);
      localrc = attpack->AttributeSet(name, vv.size(), &vv);
      name = "isLeaf";
      value = "true";
      vv.clear();
      vv.push_back(value);
      localrc = attpack->AttributeSet(name, vv.size(), &vv);
      name = "gridType";
      value = "regular_lat_lon";
      vv.clear();
      vv.push_back(value);
      localrc = attpack->AttributeSet(name, vv.size(), &vv);
    }
    if (convention.compare(ESMF_CONV)==0 && purpose.compare(GENERAL_PURP)==0) {
      attpack = AttPackCreateCustom(ESMF_CONV, GENERAL_PURP, object);
      if (attpack == NULL) {
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, &localrc);
        return localrc;
      }
      localrc = attpack->AttPackAddAttribute("RegDecompX");
      localrc = attpack->AttPackAddAttribute("RegDecompY");
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            ESMC_CONTEXT, &localrc)) return localrc;
    }

  } else if (object.compare("field")==0 || object.compare("array")==0) {
  // Field standard Attribute package
    if (((convention.compare(CF_CONV)==0  ||
          convention.compare(ESG_CONV)==0 ||
          convention.compare(ESMF_CONV)==0) && purpose.compare(GENERAL_PURP)==0) ||
         (convention.compare(CF_CONV)==0 && purpose.compare(EXTENDED_PURP)==0) ||
         ((convention.compare(CIM_1_5_CONV)==0 ||
          convention.compare(CIM_1_5_1_CONV)==0 ||
          convention.compare(CIM_1_7_1_CONV)==0) && purpose.compare(INPUTS_PURP)==0)) {
      attpack = AttPackCreateCustom(CF_CONV, GENERAL_PURP, object);
      if (attpack == NULL) {
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, &localrc);
        return localrc;
      }
      localrc = attpack->AttPackAddAttribute("LongName");
      localrc = attpack->AttPackAddAttribute("ShortName");
      localrc = attpack->AttPackAddAttribute("Units");
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            ESMC_CONTEXT, &localrc)) return localrc;
    }
    if (((convention.compare(ESG_CONV)==0 ||
          convention.compare(ESMF_CONV)==0) && purpose.compare(GENERAL_PURP)==0) ||
         (convention.compare(CF_CONV)==0    && purpose.compare(EXTENDED_PURP)==0) ||
         ((convention.compare(CIM_1_5_CONV)==0 ||
          convention.compare(CIM_1_5_1_CONV)==0 ||
          convention.compare(CIM_1_7_1_CONV)==0) && purpose.compare(INPUTS_PURP)==0)) {
      localrc = AttPackNest(CF_CONV, EXTENDED_PURP, object, CF_CONV, GENERAL_PURP);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            ESMC_CONTEXT, &localrc)) return localrc;
      localrc = AttPackAddAttribute("StandardName", CF_CONV, EXTENDED_PURP, object);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            ESMC_CONTEXT, &localrc)) return localrc;
    }
    if (((convention.compare(ESG_CONV)==0 ||
          convention.compare(ESMF_CONV)==0) && purpose.compare(GENERAL_PURP)==0) ||
         ((convention.compare(CIM_1_5_CONV)==0 ||
          convention.compare(CIM_1_5_1_CONV)==0 ||
          convention.compare(CIM_1_7_1_CONV)==0) && purpose.compare(INPUTS_PURP)==0)) {
      localrc = AttPackNest(ESG_CONV, GENERAL_PURP, object, CF_CONV, EXTENDED_PURP);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            ESMC_CONTEXT, &localrc)) return localrc;
      localrc = AttPackAddAttribute("Intent", ESG_CONV, GENERAL_PURP, object);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            ESMC_CONTEXT, &localrc)) return localrc;
    }
    if ((convention.compare(ESMF_CONV)==0 && purpose.compare(GENERAL_PURP)==0) ||
         ((convention.compare(CIM_1_5_CONV)==0 ||
          convention.compare(CIM_1_5_1_CONV)==0 ||
          convention.compare(CIM_1_7_1_CONV)==0) && purpose.compare(INPUTS_PURP)==0)) {
      localrc = AttPackNest(ESMF_CONV, GENERAL_PURP, object, ESG_CONV, GENERAL_PURP);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            ESMC_CONTEXT, &localrc)) return localrc;
    }
    // CIM inherits (ESMF, General)
    if ((convention.compare(CIM_1_5_CONV)==0 ||
          convention.compare(CIM_1_5_1_CONV)==0 ||
          convention.compare(CIM_1_7_1_CONV)==0) && purpose.compare(INPUTS_PURP)==0) {
      localrc = AttPackNest(convention, INPUTS_PURP, object,
                            ESMF_CONV, GENERAL_PURP);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            ESMC_CONTEXT, &localrc)) return localrc;
      localrc = AttPackAddAttribute("CouplingPurpose", convention,
                            INPUTS_PURP, object);
      localrc = AttPackAddAttribute("CouplingSource", convention,
                            INPUTS_PURP, object);
      localrc = AttPackAddAttribute("CouplingTarget", convention,
                            INPUTS_PURP, object);
      localrc = AttPackAddAttribute("Description", convention,
                            INPUTS_PURP, object);
      localrc = AttPackAddAttribute("Frequency", convention,
                            INPUTS_PURP, object);
      localrc = AttPackAddAttribute("SpatialRegriddingMethod", convention,
                            INPUTS_PURP, object);
      localrc = AttPackAddAttribute("SpatialRegriddingDimension", convention,
                            INPUTS_PURP, object);
      localrc = AttPackAddAttribute("Technique", convention,
                            INPUTS_PURP, object);
      localrc = AttPackAddAttribute("TimeTransformationType", convention,
                            INPUTS_PURP, object);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            ESMC_CONTEXT, &localrc)) return localrc;
    }
    
  } else if (object.compare("state")==0) {
  // State standard Attribute package
    attpack = AttPackCreateCustom(ESMF_CONV, GENERAL_PURP, object);
    if (attpack == NULL) {
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, &localrc);
        return localrc;
    }
    localrc = attpack->AttPackAddAttribute("Intent");
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, &localrc)) return localrc;

  } else if (object.compare("comp")==0) {
  // Component standard Attribute packages
    if ((convention.compare(ESG_CONV)==0 ||
         convention.compare(ESMF_CONV)==0) && purpose.compare(GENERAL_PURP)==0) {
      attpack = AttPackCreateCustom(ESG_CONV, GENERAL_PURP, object);
      if (attpack == NULL) {
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                      ESMC_CONTEXT, &localrc);
        return localrc;
      }
      localrc = attpack->AttPackAddAttribute("Agency");
      localrc = attpack->AttPackAddAttribute("Author");
      localrc = attpack->AttPackAddAttribute("CodingLanguage");
      localrc = attpack->AttPackAddAttribute("ComponentLongName");
      localrc = attpack->AttPackAddAttribute("ComponentShortName");
      localrc = attpack->AttPackAddAttribute("Discipline");
      localrc = attpack->AttPackAddAttribute("Institution");
      localrc = attpack->AttPackAddAttribute("ModelComponentFramework");
      localrc = attpack->AttPackAddAttribute("PhysicalDomain");
      localrc = attpack->AttPackAddAttribute("Version");
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            ESMC_CONTEXT, &localrc)) return localrc;
    }
    if (convention.compare(ESMF_CONV)==0 && purpose.compare(GENERAL_PURP)==0) {
      localrc = AttPackNest(ESMF_CONV, GENERAL_PURP, object, ESG_CONV, GENERAL_PURP);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            ESMC_CONTEXT, &localrc)) return localrc;
    }
    if ((convention.compare(CIM_1_5_CONV)==0 ||
         convention.compare(CIM_1_5_1_CONV)==0 ||
         convention.compare(CIM_1_7_1_CONV)==0) &&
        purpose.compare(MODEL_COMP_PURP)==0) {

      // TODO: uncomment and expand when we have better definition from CIM
      //localrc = AttPackCreateCustom(convention,
      //                              "Scientific Property Description", object);
      //if (attpack == NULL) {
      //  ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      //                                ESMC_CONTEXT, &localrc);
      //  return localrc;
      //}

        attpack = AttPackCreateCustom("ISO 19115", RESP_PARTY_PURP, object);
        if (attpack == NULL) {
          ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                        ESMC_CONTEXT, &localrc);
          return localrc;
        }

        // nest the newly created package inside of this package
        localrc = AttPackNest("ISO 19115", CITATION_PURP, object, "ISO 19115", RESP_PARTY_PURP);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                          ESMC_CONTEXT, &localrc)) return localrc;

        // nest the newly created package inside of this package
        localrc = AttPackNest(convention, PLATFORM_PURP, object, "ISO 19115", CITATION_PURP);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                          ESMC_CONTEXT, &localrc)) return localrc;

        localrc = AttPackNest(convention, purpose, object, convention, PLATFORM_PURP);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                          ESMC_CONTEXT, &localrc)) return localrc;


        //
      // Model Component attributes
      //  1 <modelComponent> in separate CIM document node, also
      //    1 within each <childComponent>
      //
      localrc = AttPackAddAttribute("Description", convention,
                            MODEL_COMP_PURP, object);
      localrc = AttPackAddAttribute("LongName", convention,
                            MODEL_COMP_PURP, object);
      localrc = AttPackAddAttribute("MetadataVersion", convention,
                            MODEL_COMP_PURP, object);
      localrc = AttPackAddAttribute("ModelType", convention,
                            MODEL_COMP_PURP, object);
      localrc = AttPackAddAttribute("ReleaseDate", convention,
                            MODEL_COMP_PURP, object);
      localrc = AttPackAddAttribute("ShortName", convention,
                            MODEL_COMP_PURP, object);
      localrc = AttPackAddAttribute("URL", convention,
                            MODEL_COMP_PURP, object);
      localrc = AttPackAddAttribute("Version", convention,
                            MODEL_COMP_PURP, object);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            ESMC_CONTEXT, &localrc)) return localrc;

      //
      // Simulation Run attributes
      //  1 <simulationRun> in separate CIM document node
      //
      localrc = AttPackAddAttribute("SimulationDuration", convention,
                            MODEL_COMP_PURP, object);
      localrc = AttPackAddAttribute("SimulationEndDate", convention,
                            MODEL_COMP_PURP, object);
      localrc = AttPackAddAttribute("SimulationEnsembleID", convention,
                            MODEL_COMP_PURP, object);
      localrc = AttPackAddAttribute("SimulationLongName", convention,
                            MODEL_COMP_PURP, object);
      localrc = AttPackAddAttribute("SimulationNumberOfProcessingElements",
                                                          convention,
                            MODEL_COMP_PURP, object);
      localrc = AttPackAddAttribute("SimulationProjectName", convention,
                            MODEL_COMP_PURP, object);
      localrc = AttPackAddAttribute("SimulationRationale", convention,
                            MODEL_COMP_PURP, object);
      localrc = AttPackAddAttribute("SimulationShortName", convention,
                            MODEL_COMP_PURP, object);
      localrc = AttPackAddAttribute("SimulationStartDate", convention,
                            MODEL_COMP_PURP, object);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            ESMC_CONTEXT, &localrc)) return localrc;

      //
      // Document Relationship attributes
      //  1 <documentGenealogy> at end of <modelComponent>
      //
      localrc = AttPackAddAttribute("PreviousVersion", convention,
                            MODEL_COMP_PURP, object);
      localrc = AttPackAddAttribute("PreviousVersionDescription", convention,
                            MODEL_COMP_PURP, object);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            ESMC_CONTEXT, &localrc)) return localrc;

      //
      // Scientific Property attributes
      //  n <componentProperty>s in 1 <componentProperties> in
      //    <modelComponent>
      //
      // TODO: uncomment and expand when we have better definition from CIM
      //localrc = AttPackAddAttribute("ScientificPropertyLongName", convention,
      //                      "Scientific Property Description", object);
      //localrc = AttPackAddAttribute("ScientificPropertyShortName", convention,
      //                      "Scientific Property Description", object);
      //localrc = AttPackAddAttribute("ScientificPropertyValue", convention,
      //                      "Scientific Property Description", object);
      //if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      //      ESMC_CONTEXT, &localrc)) return localrc;

      //
      // Platform attributes
      //  1 <platform> in separate CIM document node
      //    also 1 within <deployment> within <simulationRun> CIM document node
      //
      localrc = AttPackAddAttribute("CompilerName", convention,
                            PLATFORM_PURP, object);
      localrc = AttPackAddAttribute("CompilerVersion", convention,
                            PLATFORM_PURP, object);
      localrc = AttPackAddAttribute("MachineCoresPerProcessor", convention,
                            PLATFORM_PURP, object);
      localrc = AttPackAddAttribute("MachineDescription", convention,
                            PLATFORM_PURP, object);
      localrc = AttPackAddAttribute("MachineInterconnectType", convention,
                            PLATFORM_PURP, object);
      localrc = AttPackAddAttribute("MachineMaximumProcessors", convention,
                            PLATFORM_PURP, object);
      localrc = AttPackAddAttribute("MachineName", convention,
                            PLATFORM_PURP, object);
      localrc = AttPackAddAttribute("MachineOperatingSystem", convention,
                            PLATFORM_PURP, object);
      localrc = AttPackAddAttribute("MachineProcessorType", convention,
                            PLATFORM_PURP, object);
      localrc = AttPackAddAttribute("MachineSystem", convention,
                            PLATFORM_PURP, object);
      localrc = AttPackAddAttribute("MachineVendor", convention,
                            PLATFORM_PURP, object);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            ESMC_CONTEXT, &localrc)) return localrc;
      //
      // Citation attributes
      //  n <citation>s in <modelComponent>
      //
      localrc = AttPackAddAttribute("Date", "ISO 19115",
                            CITATION_PURP, object);

      localrc = AttPackAddAttribute("DOI", "ISO 19115",
                            CITATION_PURP, object);

      localrc = AttPackAddAttribute("LongTitle", "ISO 19115",
                            CITATION_PURP, object);

      localrc = AttPackAddAttribute("PresentationForm", "ISO 19115",
                            CITATION_PURP, object);

      localrc = AttPackAddAttribute("ShortTitle", "ISO 19115",
                            CITATION_PURP, object);

      localrc = AttPackAddAttribute("URL", "ISO 19115",
                            CITATION_PURP, object);

      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            ESMC_CONTEXT, &localrc)) return localrc;

      //
      // Responsible Party attributes
      //  n <responsibleParty>s in <modelComponent>, others
      //
      string attPackInstanceName;
      localrc = AttPackAddAttribute("Abbreviation", "ISO 19115",
                                      RESP_PARTY_PURP, object);

      localrc = AttPackAddAttribute("EmailAddress", "ISO 19115",
                                      RESP_PARTY_PURP, object);

      localrc = AttPackAddAttribute("Name", "ISO 19115",
                                      RESP_PARTY_PURP, object);

      localrc = AttPackAddAttribute("NameType", "ISO 19115",
                                      RESP_PARTY_PURP, object);

      localrc = AttPackAddAttribute("PhysicalAddress", "ISO 19115",
                                      RESP_PARTY_PURP, object);

      localrc = AttPackAddAttribute("ResponsiblePartyRole", "ISO 19115",
                                      RESP_PARTY_PURP, object);

      localrc = AttPackAddAttribute("URL", "ISO 19115",
                                      RESP_PARTY_PURP, object);

      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            ESMC_CONTEXT, &localrc)) return localrc;
    }
  }

  return ESMF_SUCCESS;

}  // end AttPackCreateStandard()
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttPackCreateStandard"
//BOPI
// !IROUTINE:  AttPackCreateStandard(instance list) - nest a instance list of attpacks (full multi-child tree)
//
// !INTERFACE:
      int Attribute::AttPackCreateStandard(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &convention,              // in - Attribute convention
      const string &purpose,                 // in - Attribute purpose
      const string &object,                  // in - Attribute object type
      const vector<string> &nestConvention,  // in - Attribute nestConventions
      const vector<string> &nestPurpose,     // in - Attribute nestPurposes
      const vector<int> &nestAttPackInstanceCountList, // in - # of attPack instances of each (conv,purp) type
      int   nestCount,                       // in - # of attpacks (child nodes)
      vector<string> &nestAttPackInstanceNameList, // inout - Attribute package instance names
      int   &nestAttPackInstanceNameCount) { // inout - # of attPack instance names
// 
// !DESCRIPTION:
//     Top-down approach:  Create a new standard parent attpack of type
//     (conv,purp), and create within it nested standard children attpacks of 
//     type (nestConv,nestPurp).  For each (nestConv,nestPurp) child type, 
//     create multiple nested attpacks as specified in 
//     nestAttPackInstanceCountList, and return the unique instance names in
//     nestAttPackInstanceNameList.
//
//EOPI

  int localrc;
  unsigned int i,j;
  Attribute *stdParent, *stdChild;

  if (!(convention.compare(CIM_1_5_CONV)==0 ||
      convention.compare(CIM_1_5_1_CONV)==0 ||
      convention.compare(CIM_1_7_1_CONV)==0) ||
      purpose.compare(MODEL_COMP_PURP)!=0) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "non-standard attpack type", ESMC_CONTEXT, &localrc);
        return localrc;
  }

  // create parent standard attpack ...
  stdParent = new Attribute(convention, purpose, object);
  if(!stdParent) {
    // TODO:  more detailed error message including conv,purp,object 
    ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_NOT_CREATED,
      "could not make the attpack", ESMC_CONTEXT, &localrc);
    return localrc;
  }
  // ... and attach to *this* attribute node
  localrc = AttPackSet(stdParent);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, &localrc)) return localrc;

  // populate parent attpack with standard attributes

  //
  // Model Component attributes
  //  1 <modelComponent> in separate CIM document node, also
  //    1 within each <childComponent>
  //
  localrc = stdParent->AttPackAddAttribute("Description");
  localrc = stdParent->AttPackAddAttribute("LongName");
  localrc = stdParent->AttPackAddAttribute("MetadataVersion");
  localrc = stdParent->AttPackAddAttribute("ModelType");
  localrc = stdParent->AttPackAddAttribute("ReleaseDate");
  localrc = stdParent->AttPackAddAttribute("ShortName");
  localrc = stdParent->AttPackAddAttribute("URL");
  localrc = stdParent->AttPackAddAttribute("Version");
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, &localrc)) return localrc;

  //
  // Simulation Run attributes
  //  1 <simulationRun> in separate CIM document node
  //
  localrc = stdParent->AttPackAddAttribute("SimulationDuration");
  localrc = stdParent->AttPackAddAttribute("SimulationEndDate");
  localrc = stdParent->AttPackAddAttribute("SimulationEnsembleID");
  localrc = stdParent->AttPackAddAttribute("SimulationLongName");
  localrc = stdParent->AttPackAddAttribute("SimulationNumberOfProcessingElements");
  localrc = stdParent->AttPackAddAttribute("SimulationProjectName");
  localrc = stdParent->AttPackAddAttribute("SimulationRationale");
  localrc = stdParent->AttPackAddAttribute("SimulationShortName");
  localrc = stdParent->AttPackAddAttribute("SimulationStartDate");
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        &localrc)) return localrc;

  //
  // Document Relationship attributes
  //  1 <documentGenealogy> at end of <modelComponent>
  //
  localrc = stdParent->AttPackAddAttribute("PreviousVersion");
  localrc = stdParent->AttPackAddAttribute("PreviousVersionDescription");
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        &localrc)) return localrc;

  // TODO:  Scientific Property attributes
  //  n <componentProperty>s in 1 <componentProperties> in
  //    <modelComponent>
  //

  // create child standard attpacks, attach to parent attpack

  // create one Platform child attpack
  stdChild = new Attribute(convention, PLATFORM_PURP, object);
  if(!stdChild) {
    // TODO:  more detailed error message including conv,purp,object 
    ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_NOT_CREATED,
      "could not make the attpack", ESMC_CONTEXT, &localrc);
    return localrc;
  }
  localrc = stdParent->AttPackSet(stdChild);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        &localrc)) return localrc;

  //
  // Platform attributes
  //  1 <platform> in separate CIM document node
  //    also 1 within <deployment> within <simulationRun> CIM document node
  //
  localrc = stdChild->AttPackAddAttribute("CompilerName");
  localrc = stdChild->AttPackAddAttribute("CompilerVersion");
  localrc = stdChild->AttPackAddAttribute("MachineCoresPerProcessor");
  localrc = stdChild->AttPackAddAttribute("MachineDescription");
  localrc = stdChild->AttPackAddAttribute("MachineInterconnectType");
  localrc = stdChild->AttPackAddAttribute("MachineMaximumProcessors");
  localrc = stdChild->AttPackAddAttribute("MachineName");
  localrc = stdChild->AttPackAddAttribute("MachineOperatingSystem");
  localrc = stdChild->AttPackAddAttribute("MachineProcessorType");
  localrc = stdChild->AttPackAddAttribute("MachineSystem");
  localrc = stdChild->AttPackAddAttribute("MachineVendor");
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        &localrc)) return localrc;

  // for each standard nested attpack type ...
  nestAttPackInstanceNameCount = 0;
  for(i=0; i<nestCount; i++) {
    if (nestConvention[i].compare("ISO 19115") != 0 ||
        (nestPurpose[i].compare(CITATION_PURP) != 0 &&
         nestPurpose[i].compare(RESP_PARTY_PURP) != 0)) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "attpack not a standard child of parent", ESMC_CONTEXT, &localrc);
      return localrc;
    }
    // ... create multiple standard child attpacks
    for(j=0; j<nestAttPackInstanceCountList[i]; j++) {
      stdChild = new Attribute(nestConvention[i], nestPurpose[i], object);
      if(!stdChild) {
        // TODO:  more detailed error message including conv,purp,object 
        ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_NOT_CREATED,
          "could not make the attpack", ESMC_CONTEXT, &localrc);
        return localrc;
      }
      localrc = stdParent->AttPackSet(stdChild);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
            &localrc)) return localrc;

      // return the child's unique name (out)
      // TODO:  use names specified by user (in)
      // TODO:  enhancement to allow sparsely populated NameList 
      //          provided by user? (mix of default names and user-specified)
      nestAttPackInstanceNameList.push_back(stdChild->attrName); 
      nestAttPackInstanceNameCount++;

      //
      // Citation attributes
      //  n <citation>s in <modelComponent>
      //
      if (nestConvention[i].compare("ISO 19115") == 0 &&
          nestPurpose[i].compare(CITATION_PURP) == 0) {
        localrc = stdChild->AttPackAddAttribute("Date");
        localrc = stdChild->AttPackAddAttribute("DOI");
        localrc = stdChild->AttPackAddAttribute("LongTitle");
        localrc = stdChild->AttPackAddAttribute("PresentationForm");
        localrc = stdChild->AttPackAddAttribute("ShortTitle");
        localrc = stdChild->AttPackAddAttribute("URL");
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
            &localrc)) return localrc;
      }

      //
      // Responsible Party attributes
      //  n <responsibleParty>s in <modelComponent>
      //
      if (nestConvention[i].compare("ISO 19115") == 0 &&
          nestPurpose[i].compare(RESP_PARTY_PURP) == 0) {
        localrc = stdChild->AttPackAddAttribute("Abbreviation");
        localrc = stdChild->AttPackAddAttribute("EmailAddress");
        localrc = stdChild->AttPackAddAttribute("Name");
        localrc = stdChild->AttPackAddAttribute("NameType");
        localrc = stdChild->AttPackAddAttribute("PhysicalAddress");
        localrc = stdChild->AttPackAddAttribute("ResponsiblePartyRole");
        localrc = stdChild->AttPackAddAttribute("URL");
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
            &localrc)) return localrc;
      }
    }
  }

  return ESMF_SUCCESS;

}  // end AttPackCreateStandard(instance list)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttPackNest"
//BOPI
// !IROUTINE:  AttPackNest(single) - nest a single attpack (single-child tree)
//
// !INTERFACE:
      int Attribute::AttPackNest(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &convention,              // in - Attribute convention
      const string &purpose,                 // in - Attribute purpose
      const string &object,                  // in - Attribute object type
      const string &nestConvention,          // in - Attribute nestConvention
      const string &nestPurpose) {           // in - Attribute nestPurpose
// 
// !DESCRIPTION:
//     Setup the name, convention and purpose of a single nested attpack.
//
//EOPI

  int localrc;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // create vectors of one item ...
  vector<string> cnconv, cnpurp;
  cnconv.reserve(1);
  cnpurp.reserve(1);
  cnconv.push_back(nestConvention);
  cnpurp.push_back(nestPurpose);

  // ... and then use the multi-item vector interface
  localrc = AttPackNest(convention, purpose, object, 1, cnconv, cnpurp);
  ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
    ESMC_CONTEXT, &localrc);

  return localrc;

}  // end AttPackNest(single)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttPackNest"
//BOPI
// !IROUTINE:  AttPackNest(list) - nest a list of attpacks (full multi-child tree)
//
// !INTERFACE:
      int Attribute::AttPackNest(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &convention,              // in - Attribute convention
      const string &purpose,                 // in - Attribute purpose
      const string &object,                  // in - Attribute object type
      int   nestCount,                       // in - # of attpacks (child nodes)
      const vector<string> &nestConvention,  // in - Attribute nestConventions
      const vector<string> &nestPurpose) {   // in - Attribute nestPurposes
// 
// !DESCRIPTION:
//     Bottom up approach:  Given a set of already created attpacks, nest them
//     within a new parent attpack.  Assumes one attpack instance for each
//     (nestConv, nestPurp) attpack type.
//     TODO:  Create a different overloaded method that takes a list of 
//            attpack instance names instead of (nestConv,nestPurp) pairs.
//            This will handle the bottom-up case of already-created multiple
//            instances of (nestConv, nestPurp) attpack types.
//
//EOPI

  int localrc;
  unsigned int i;
  Attribute *attpack, **nestpack, *localParent;
  bool done = false;

  attpack = NULL; nestpack = NULL; localParent = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // create array of temp attpack pointers
  try {
    nestpack = new ESMCI_AttributePtr[nestCount];
  } catch (...) {
      ESMC_LogDefault.AllocError(ESMC_CONTEXT, ESMC_NULL_POINTER);
      return(ESMC_RC_MEM_ALLOCATE);
  }

  // Search for the packs to nest this new one around
  for(i=0; i<nestCount; i++) {
    string attPackInstanceName;
    nestpack[i] = AttPackGet(nestConvention[i], nestPurpose[i], object,
                             attPackInstanceName, ESMC_ATTNEST_ON);
    if(!nestpack[i]) {
      // TODO:  more detailed error message including conv,purp,object
      ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_FOUND,
        "could not find the attpack", ESMC_CONTEXT, &localrc);
      delete [] nestpack;
      return localrc;
    }
    if (i == 0) {
      localParent = nestpack[0]->parent;
    } else {
      if (nestpack[i]->parent != localParent) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "parents of nested attpacks not one and the same",
           ESMC_CONTEXT, &localrc);
        delete [] nestpack;
        return localrc;
      }
    }
  }
    
  // Make the attpack
  attpack = new Attribute(convention, purpose, object);
  if(!attpack) {
    // TODO:  more detailed error message including conv,purp,object 
    ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_NOT_CREATED,
      "could not make the attpack", ESMC_CONTEXT, &localrc);
    delete [] nestpack;
    return localrc;
  }
  
  // Put the attpack onto nestPack's parent
  localrc = localParent->AttPackSet(attpack);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, &localrc)) {delete [] nestpack; return localrc;}

  // Now remove nestpacks from their parent
  int removed=0;
  for (i=0; i<localParent->packList.size(); i++) {
    for (int j=0; j<nestCount; j++) {
      if (nestpack[j] == localParent->packList.at(i)) { 
        localParent->packList.erase(localParent->packList.begin() + i);
        //localParent->structChange = ESMF_TRUE;
        removed++;
        if (removed == nestCount) {
          done = true;
          break;
        }
      }
    }
    if (done) break;
  }
  
  // Put nestpacks onto attpack as child nodes
  for (i=0; i<nestCount; i++) {
    localrc = attpack->AttPackSet(nestpack[i]);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, &localrc)) {delete [] nestpack; return localrc;}
  }
  
  if (!done) {
    // TODO:  more detailed error message including conv,purp,object 
    ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_DELETED,
      "AttPackNest() failed removing the nested Attribute packages",
       ESMC_CONTEXT, &localrc);
    delete [] nestpack;
    return localrc;
  }
 
  // delete array of temp attpack pointers
  delete [] nestpack;

  return ESMF_SUCCESS;

}  // end AttPackNest(list)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttPackGet"
//BOPI
// !IROUTINE:  AttPackGet - get an attpack on an {\tt Attribute}
//
// !INTERFACE:
      Attribute *Attribute::AttPackGet(
// 
// !RETURN VALUE:
//    {\tt Attribute} pointer to requested object or NULL on early exit.
// 
// !ARGUMENTS:
      const string &convention,          // in - Attribute convention to retrieve
      const string &purpose,             // in - Attribute purpose to retrieve
      const string &object,              // in - Attribute object type to retrieve
      const string &attPackInstanceName, // in - attPack name
    ESMC_AttNest_Flag anflag) const {

// !DESCRIPTION:
//    Get an attpack on an {\tt Attribute} given it's convention, 
//    purpose, object type, and optional attPackInstanceName
//
//EOPI

  int i;
  Attribute *ap=NULL;

  // look for the attpack on this Attribute, at this level, and return the
  // first one if any matches there, or the desired attPackInstanceName one
  //for (i=0; i<packList.size(); i++) {
  //  ap = packList.at(i);
  for (i=packList.size(); i > 0; i--) {
    ap = packList.at(i-1);
    if (convention.compare(ap->attrConvention) == 0 &&
        purpose.compare(ap->attrPurpose) == 0 &&
        object.compare(ap->attrObject) == 0 &&
        (attPackInstanceName.empty() ||
         (!attPackInstanceName.empty() && 
          attPackInstanceName.compare(ap->attrName) == 0))) {
          return (ap);
    }
  }

  // if not found at this level, and anflag is ON,
  // recurse through the nested Attribute packages, one level at a time,
  // right-to-left, to find right-most package
  //RLO: not sure why this is right to left (post order), but for some reason
  //     the newest block of nesting related code does everything post instead
  //     of pre-order.  this can conflict with old search code is some cases
  if (anflag == ESMC_ATTNEST_ON) {
    for (i=packList.size(); i > 0; i--) {
      ap = packList.at(i-1)->AttPackGet(convention, purpose, object,
                                        attPackInstanceName, anflag);
      if (ap) return ap;
    }
  }
  
  return ap;

}  // end AttPackGet
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttPackGet"
//BOPI
// !IROUTINE:  AttPackGet - get an attpack
//
// !INTERFACE:
      Attribute *Attribute::AttPackGet(
// 
// !RETURN VALUE:
//    {\tt Attribute} pointer to requested object or NULL on early exit.
// 
// !ARGUMENTS:
      const string &convention,      // in - attpack convention,
      const string &purpose,         //      purpose, and
      const string &object,          //      object to match
      const string &name,            // in - Attribute name to match)
      const string &value) const {   // in - Attribute value to match
// 
// !DESCRIPTION:
//     Get an attpack of given convention, purpose and object type, and which
//     contains an attribute of given name and value.  Recursively search
//     through the ESMF object/attribute tree from the point of invocation.
//
//EOPI

  Attribute *attpack = NULL;
  vector<string> vv;

  // first check if given attpack is set on *this* esmf object's attribute tree
  string attPackInstanceName;
  attpack = AttPackGet(convention, purpose, object, attPackInstanceName,
                   ESMC_ATTNEST_ON);
  if (attpack) {
    if (attpack->AttributeGet(name)->isSet()) {
      attpack->AttributeGet(name)->get(&vv);
      if (vv.size() == 1) {
        if (value.compare(vv.at(0)) == 0) return attpack; // match !
      }
    }
  }

  // matching attribute not on *this* esmf object; look further down the
  // esmf object tree -- recurse
  for(unsigned int i=0; i<linkList.size(); i++) {
    attpack = linkList.at(i)->AttPackGet(convention, purpose, object,
                                         name, value);
    if (attpack != NULL) return attpack;
  }

  // no match
  return attpack;

}  // end AttPackGet
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttPackGet"
//BOPI
// !IROUTINE:  AttPackGet - get attpack instance names associated with the given
//                          convention, purpose, and object
//
// !INTERFACE:
      int Attribute::AttPackGet(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &convention,        // in - Attribute convention to retrieve
      const string &purpose,           // in - Attribute purpose to retrieve
      const string &object,            // in - Attribute object type to retrieve
      vector<string> &attPackInstanceNameList, // out - Attribute package instance names
      int &attPackInstanceNameCount,            // out - # of attPack instance names
    ESMC_AttNest_Flag anflag) const {        // in - attnestflag
// !DESCRIPTION:
//    Get the attpack instance names given its convention, 
//    purpose, and object type.  Looks for all the instance names to be 
//    at one level in the tree.
//
//EOPI

  Attribute *ap;

  // look for the attpacks on this Attribute, at this level
  attPackInstanceNameCount = 0;
  for (unsigned int i=0; i<packList.size(); i++) {
    ap = packList.at(i);
    if (convention.compare(ap->attrConvention) == 0 && 
        purpose.compare(ap->attrPurpose) == 0 &&
        object.compare(ap->attrObject) == 0) {
      attPackInstanceNameList.push_back(ap->attrName); 
      attPackInstanceNameCount++;
    }
  }
  if (attPackInstanceNameCount > 0) return ESMF_SUCCESS;

  // if not found at this level, recurse through the nested Attribute packages,
  // one level at a time, right-to-left, to find right-most package
  if (anflag == ESMC_ATTNEST_ON) {
    for (unsigned int i=packList.size(); i > 0; i--) {
      packList.at(i-1)->AttPackGet(convention, purpose, object,
                                   attPackInstanceNameList,
                                   attPackInstanceNameCount, anflag);
      if (attPackInstanceNameCount > 0) return ESMF_SUCCESS;
    }
  }

  // none found
  return ESMF_FAILURE;

}  // end AttPackGet
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttPackGetAttribute"
//BOPI
// !IROUTINE:  AttPackGetAttribute - get an {\tt Attribute} from an attpack
//
// !INTERFACE:
      Attribute *Attribute::AttPackGetAttribute(
// 
// !RETURN VALUE:
//    {\tt Attribute} pointer to requested object or NULL on early exit.
// 
// !ARGUMENTS:
      const string &name) const {         // in - Attribute name to retrieve)
// 
// !DESCRIPTION:
//     Get an {\tt Attribute} from an attpack given its name, convention, 
//     purpose, and object type.  This routine is assumed to be called on the 
//     Attribute package that holds the Attribute in question.
//
//EOPI

  Attribute *attr;
  unsigned int i;
  
  attr = NULL;
    
  // look for the Attribute on this attpack
  for (i=0; i<attrList.size(); i++) {
    if (name.compare(attrList.at(i)->attrName) == 0)
      return attrList.at(i);
  }

  return attr;

}  // end AttPackGetAttribute

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttPackGetAttribute"
//BOPI
// !IROUTINE:  AttPackGetAttribute - get an {\tt Attribute} from an attpack
//
// !INTERFACE:
      Attribute *Attribute::AttPackGetAttribute(
//
// !RETURN VALUE:
//    {\tt Attribute} pointer to requested object or NULL on early exit.
//
// !ARGUMENTS:
      const int &num) const {         // in - Attribute name to retrieve)
//
// !DESCRIPTION:
//     Get an {\tt Attribute} from an attpack given its index.
//     This routine is assumed to be called on the
//     Attribute package that holds the Attribute in question.
//
//EOPI

  int localrc;

  if (num >= attrList.size()) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_VAL_OUTOFRANGE,
            "index number is too large for Attribute list\n",
        ESMC_CONTEXT, &localrc);
      return NULL;
  }

  return attrList.at(num);

}  // end AttPackGetAttribute

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttPackGetAttribute"
//BOPI
// !IROUTINE:  AttPackGetAttribute - get an {\tt Attribute} from an attpack
//
// !INTERFACE:
      Attribute *Attribute::AttPackGetAttribute(
//
// !RETURN VALUE:
//    {\tt Attribute} pointer to requested object or NULL on early exit.
//
// !ARGUMENTS:
      const string &name,         // in - Attribute name to retrieve)
    ESMC_AttNest_Flag anflag    // in - attnestflag
    ) const {
//
// !DESCRIPTION:
//     Get an {\tt Attribute} from an attpack given its name, convention,
//     purpose, and object type.  This routine is assumed to be called on the
//     Attribute package that holds the Attribute in question.
//
//EOPI

  Attribute *attr;
  unsigned int i;

  // look for the Attribute on this attpack
  for (i=0; i<attrList.size(); i++)
    if (name.compare(attrList.at(i)->attrName) == 0)
      return attrList.at(i);

  // recurse through the nested Attribute packages
  if (anflag == ESMC_ATTNEST_ON) {
    for (i=packList.size(); i > 0; i--) {
      attr = packList.at(i-1)->AttPackGetAttribute(name, anflag);
      // return first that is found (highest in nested tree)
          if (attr) return attr;
    }
  }


  // if nothing is found return NULL initialized attr
  return NULL;

}  // end AttPackGetAttribute

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttPackGetAttribute"
//BOPI
// !IROUTINE:  AttPackGetAttribute - get an {\tt Attribute} from an attpack
//
// !INTERFACE:
      Attribute *Attribute::AttPackGetAttribute(
//
// !RETURN VALUE:
//    {\tt Attribute} pointer to requested object or NULL on early exit.
//
// !ARGUMENTS:
      const int &num,              // in - Attribute index
    ESMC_AttNest_Flag anflag    // in - attnestflag
    ) const {
//
// !DESCRIPTION:
//     Get an {\tt Attribute} from an attpack given its index.
//     This routine is assumed to be called on the
//     Attribute package that holds the Attribute in question.
//
//EOPI

  unsigned int i;
  int localrc;
  Attribute *attr;

  // if num is valid on this attribute return immediately
  if (num < attrList.size())
    return attrList.at(num);
  // recurse packages until we get a valid index
  else {
    if (anflag == ESMC_ATTNEST_ON) {
      for (i=packList.size(); i > 0; i--) {
        attr = packList.at(i-1)->AttPackGetAttribute(num-attrList.size(),
                                               anflag);
          // return first that is found (highest in nested tree)
          if (attr) return attr;
      }
    }
    else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_VAL_OUTOFRANGE,
                  "index number is too large for Attribute list\n",
              ESMC_CONTEXT, &localrc);
      return NULL;
    }
  }

  // if nothing is found return NULL initialized attr
  return NULL;

}  // end AttPackGetAttribute

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttPackIsPresent"
//BOPI
// !IROUTINE:  AttPackIsPresent - query an {\tt Attpack} about existence
//
// !INTERFACE:
      int Attribute::AttPackIsPresent(
// 
// !RETURN VALUE:
//    Value of the present flag.
// 
// !ARGUMENTS:
      const Attribute *attpack,          // in - Attribute package
      ESMC_Logical *present) {     // in/out - the present flag
// 
// !DESCRIPTION:
//     Query an Attribute package for an {\tt Attribute} given its name.
//
//EOPI

  // get the attpack
  if (!attpack || !attrPackHead) {
    *present = ESMF_FALSE;
    return ESMF_SUCCESS;
  }
  else *present = ESMF_TRUE;

  // return
  return ESMF_SUCCESS;

}  // end AttPackIsPresent

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttPackIsPresent"
//BOPI
// !IROUTINE:  AttPackIsPresent - query an {\tt Attribute} for an attpack
//
// !INTERFACE:
      int Attribute::AttPackIsPresent(
//
// !RETURN VALUE:
//    Value of the present flag.
//
// !ARGUMENTS:
      const string &name,                // in - Attribute name
      const Attribute *attpack,          // in - Attribute package
      ESMC_AttNest_Flag anflag,          // in - attgetcount flag
      ESMC_Logical *present) const {     // in/out - the present flag
//
// !DESCRIPTION:
//     Query an Attribute package for an {\tt Attribute} given its name.
//
//EOPI

  unsigned int i;
  Attribute *attr;

  attr = NULL;

  // get the attpack
  if (!attpack) {
    *present = ESMF_FALSE;
    return ESMF_SUCCESS;
  }

  // get the attr on the attpack
  attr = attpack->AttPackGetAttribute(name, anflag);
  if (!attr) *present = ESMF_FALSE;
  else *present = ESMF_TRUE;

  // return
  return ESMF_SUCCESS;

}  // end AttPackIsPresent

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttPackIsPresent"
//BOPI
// !IROUTINE:  AttPackIsPresent - query an {\tt Attribute} for an attpack
//
// !INTERFACE:
      int Attribute::AttPackIsPresent(
//
// !RETURN VALUE:
//    Value of the present flag.
//
// !ARGUMENTS:
      const int &num,                    // in - Attribute name
      const Attribute *attpack,          // in - Attribute package
      ESMC_AttNest_Flag anflag,          // in - attgetcount flag
      ESMC_Logical *present) const {     // in/out - the present flag
//
// !DESCRIPTION:
//     Query an Attribute package for an {\tt Attribute} given its name.
//
//EOPI

  unsigned int i;
  Attribute *attr;

  attr = NULL;

  // get the attpack
  if (!attpack) {
    *present = ESMF_FALSE;
    return ESMF_SUCCESS;
  }

  // get the attr on the attpack
  attr = attpack->AttPackGetAttribute(num, anflag);
  if (!attr) *present = ESMF_FALSE;
  else *present = ESMF_TRUE;
  
  // return
  return ESMF_SUCCESS;

}  // end AttPackIsPresent

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttPackIsSet"
//BOPI
// !IROUTINE:  AttPackIsSet - check if any {\tt Attribute} is set in an attpack
//
// !INTERFACE:
      bool Attribute::AttPackIsSet(
// 
// !RETURN VALUE:
//    true if attribute set, false otherwise.
// 
// !ARGUMENTS:
      const string &convention,          // in - Attribute convention
      const string &purpose,             // in - Attribute purpose
      const string &object,              // in - Attribute object type
      const bool   &inObjectTree,        // in - search ESMF object tree?
      const bool   &inThisCompTreeOnly,  // in - only search within this component tree?
      const bool   &inNestedAttPacks) const {// in -search within nested attpacks?
// 
// !DESCRIPTION:
//     Search for set Attribute packages of the given type, within the
//     attribute hierarchy of the given ESMF object, and optionally within the
//     attribute hierarchies of other ESMF objects, within the tree of this ESMF
//     object, to see if any {\tt Attribute} of the attpack has been set.
//     Returns true as soon as a set attribute is found.
//
//EOPI

  Attribute *attpack = NULL;
  string attPackInstanceName;

  // first check if given attpack is set on *this* esmf object's attribute tree
  attpack = AttPackGet(convention, purpose, object, attPackInstanceName,
                   ESMC_ATTNEST_ON);
  if (attpack != NULL) {
    if (attpack->AttPackIsSet(inNestedAttPacks)) return true;
  }

  // package not set on *this* esmf object; if we don't want to look
  // any further down the esmf object tree, we're done
  if (!inObjectTree) return false;

  // otherwise, check all attpacks on objects in this esmf object tree
  for(unsigned int i=0; i<linkList.size(); i++) {
    if (inThisCompTreeOnly) {
      // only consider esmf objects within this component
      if (strcmp(linkList.at(i)->attrBase->ESMC_BaseGetClassName(),
          "Component")==0)
        continue; // skip if any other linked (child) component object
    }

    // recurse until we reach objects of the specified type
    if (linkList.at(i)->AttPackIsSet(convention, purpose, object, 
                                     inObjectTree, inThisCompTreeOnly, 
                                     inNestedAttPacks))
        return true; else continue;
  }

  // if we get here, no set attributes found
  return false;
}  // end AttPackIsSet

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttPackIsSet"
//BOPI
// !IROUTINE:  AttPackIsSet - check if any {\tt Attribute} is set in an attpack
//
// !INTERFACE:
      bool Attribute::AttPackIsSet(
// 
// !RETURN VALUE:
//    true if attribute set, false otherwise.
// 
// !ARGUMENTS:
      const bool &inNestedAttPacks) const {  // search within nested attpacks?
// 
// !DESCRIPTION:
//     Query Attribute package to see if any {\tt Attribute} has been set,
//     within the given package or optionally any of its nested packages.
//     Returns true as soon as a set attribute is found.
//
//     TODO: Ability to specify which level in the nest (and optionally below
//           or not) to search for set attributes (via pathing mechanism?) 
//           E.g., only interested in attributes set within CF/Extended or
//           CF/General within a CIM/Inputs package, as used in
//           AttributeWriteCIMmodelComp().
//EOPI

  Attribute *ap;

  // check attributes defined on this attpack
  for(unsigned int i=0; i<attrList.size(); i++) {
    string name = attrList.at(i)->attrName;
    if (AttPackGetAttribute(name)->isSet()) return true;
  }
  // otherwise check for any set attributes on nested attpacks, if requested
  if (inNestedAttPacks) {
    for(unsigned int i=0; i<packList.size(); i++) { 
      if (packList.at(i)->AttPackIsSet(inNestedAttPacks)) return true; 
    }
  }

  // if we get here, no set attributes found
  return false;

}  // end AttPackIsSet
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttPackRemove"
//BOPI
// !IROUTINE:  AttPackRemove - Remove an {\tt Attribute} package
//
// !INTERFACE:
      int Attribute::AttPackRemove(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
        ESMCI::Attribute *attpack) {   // in - attPack
// 
// !DESCRIPTION:
//     Remove an {\tt Attribute} package

//EOPI

  int localrc;
  unsigned int i;
  Attribute *attrparent;
  bool done = false;
  
  attrparent = NULL;

  // Initialize local return code
  localrc = ESMC_RC_NOT_IMPL;
    
  // get the attpack
  if(!attpack) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_FOUND, 
      "Cannot find the Attribute package", ESMC_CONTEXT, &localrc);
    return localrc;
  }

  // save the parent, remove attpack from it's parent, then delete attpack
  attrparent = attpack->parent;
  for (i=0; i<attrparent->packList.size(); i++) {
    if (attrparent->packList.at(i) == attpack) {
      delete (attrparent->packList.at(i));
      attrparent->packList.erase(attrparent->packList.begin() + i);
      attrparent->deleteChange = ESMF_TRUE;
      done = true;
      break;
    }
  }
  
  if (!done) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_DELETED,
      "AttPackRemove could not locate the Attribute package", ESMC_CONTEXT, &localrc);
    return localrc;
  }

  return ESMF_SUCCESS;

}  // end AttPackRemove
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttPackRemoveAttribute"
//BOPI
// !IROUTINE:  AttPackRemoveAttribute - Remove an {\tt Attribute} from
//                                            an {\tt Attribute} package
//
// !INTERFACE:
      int Attribute::AttPackRemoveAttribute(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,         // in - name
      ESMCI::Attribute *attpack,  // in - attPack name
    ESMC_AttNest_Flag anflag) { // in - attnestflag
// 
// !DESCRIPTION:
//     Remove an {\tt Attribute} from an {\tt Attribute} package

//EOPI

  int localrc;
  unsigned int i;
  Attribute *attr, *attrparent;
  bool done = false;

  attr = NULL; attrparent = NULL;

  // Initialize local return code
  localrc = ESMC_RC_NOT_IMPL;
  
  // get the attpack
  if(!attpack) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_FOUND, 
      "Cannot find the specified Attribute package", ESMC_CONTEXT, &localrc);
    return localrc;
  }
  
  attr = attpack->AttPackGetAttribute(name, anflag);
  if(!attr) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_FOUND, 
      "Cannot find the Attribute in this Attribute Package", ESMC_CONTEXT, &localrc);
    return localrc;
  }
    
  attrparent = attr->parent;
  localrc = attrparent->AttributeRemove(name);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, &localrc)) return localrc;
  
  return ESMF_SUCCESS;

}  // end AttPackRemoveAttribute
//-----------------------------------------------------------------------------
/*#undef  ESMC_METHOD
#define ESMC_METHOD "AttPackSet"
//BOPI
// !IROUTINE:  AttPackSet() - set an {\tt Attribute} in an attpack
//
// !INTERFACE:
      int Attribute::AttPackSet(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,             // in - Attribute name
      const ESMC_TypeKind_Flag &tk,       // in - typekind
      int count,              // in - item count
      void *value,            // in - Attribute value
      const string &convention,       // in - attpack convention
      const string &purpose,          // in - attpack purpose
      const string &object,           // in - attpack object type
      const string &attPackInstanceName) {   // in - attPack name
                                       // specifying which one of multiple packs
// 
// !DESCRIPTION:
//     Set the value for an {\tt Attribute} belonging to an attpack with  
//     convention, purpose, and object type.
//
//EOPI

  int localrc;
  Attribute *attr;
  Attribute *attpack;
  char msgbuf[4*ESMF_MAXSTR];

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Find the attpack
  attpack = AttPackGet(convention, purpose, object, attPackInstanceName);
  if(!attpack) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_FOUND, 
      "Cannot find the specified Attribute package", ESMC_CONTEXT, &localrc);
    return localrc;
  }
  
  // Find the attribute
  attr = attpack->AttPackGetAttribute(name);
  if (!attr) {
    sprintf(msgbuf, 
      "This Attribute package does not have an Attribute named %s\n",
       name.c_str());
    ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_FOUND, msgbuf, ESMC_CONTEXT, &localrc);
    return localrc;
  }

  // Set the Attribute
  localrc = attr->AttrModifyValue(tk, count, value);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, &localrc)) return localrc;
  
  // return
  if (localrc != ESMF_SUCCESS) return ESMF_FAILURE;
  return ESMF_SUCCESS;
  
}  // end AttPackSet()
*///-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttPackSet"
//BOPI
// !IROUTINE:  AttPackSet - set {\tt Attribute} on an ESMF type
//
// !INTERFACE:
      int Attribute::AttPackSet(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      Attribute *attr) {   // in - Attribute name, type, value
// 
// !DESCRIPTION:
//     Associate a (name,value) pair with any type in the system.
//     This version of set is used when the caller has already allocated
//     an {\tt Attribute} object and filled it, and the {\tt Attribute} 
//     is simply added to the list belonging to this object.
//
//EOPI

  int localrc;
  unsigned int i;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // simple sanity checks
  if (!attr) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_BAD, 
      "bad Attribute object", ESMC_CONTEXT, &localrc);
    return localrc;
  }

// ERS: commented out to allow duplicate attpacks
#if 0

  // first, see if you are replacing an existing Attribute
  for (i=0; i<packList.size(); i++) {
    if ((attr->attrName).compare(packList.at(i)->attrName)==0 &&
        (attr->attrConvention).compare(packList.at(i)->attrConvention)==0 &&
        (attr->attrPurpose).compare(packList.at(i)->attrPurpose)==0 &&
        (attr->attrObject).compare(packList.at(i)->attrObject)==0) {

      // if you get here, you found a match.  replace previous copy.

      // delete old Attribute, including possibly freeing a list
      delete packList.at(i);

      // replace the original Attribute with attr
      packList.at(i) = attr;
      // attr may be of a different value than the original Attribute
      attr->valueChange = ESMF_TRUE;
      attr->attrBase = this->attrBase; 
      attr->parent = this;
      return ESMF_SUCCESS;
    }
  }   
#endif

  // point attr to its new Base
  attr->attrBase = this->attrBase;
  attr->parent = this;
  
  // add the Attribute
  packList.push_back(attr);
  //structChange = ESMF_TRUE;

/*
if (attrRoot == ESMF_TRUE) {
  printf("BIG PROBLEM - setting attrPack on a root Attribute");
  ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_BAD, 
        "BIG PROBLEM - setting attrPack on a root Attribute", ESMC_CONTEXT, &localrc);
  return localrc;
}
  // RLO: removed this because attrPack should only be set on attpack attributes, 
  //      *this is the attribute holding the attpack, which could be root.
  attrPack = ESMF_TRUE;
*/
  
  return ESMF_SUCCESS;

}  // end AttPackSet
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeCopy"
//BOPI
// !IROUTINE:  AttributeCopy - copy all {\tt Attribute} data 
//
// !INTERFACE:
      int Attribute::AttributeCopy(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
        const Attribute &source) {   // in - source
// 
// !DESCRIPTION:
//     All of the {\tt Attribute} data associated with the source object is 
//     copied.
//
//EOPI

  int localrc;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // TODO: should check for self copy!!!

  // first clear destinations value arguments
  vip.clear();
  vlp.clear();
  vfp.clear();
  vdp.clear();
  vbp.clear();
  vcpp.clear();
  
  // now reset all Attribute info
  attrName = source.attrName;
  tk = source.tk;
  items = source.items;
  attrRoot = source.attrRoot;

  attrConvention = source.attrConvention;
  attrPurpose = source.attrPurpose;
  attrObject = source.attrObject;
  attrPack = source.attrPack;
  attrPackHead = source.attrPackHead;
  attrNested = source.attrNested;

  valueChange = ESMF_TRUE;

  if (source.tk == ESMC_TYPEKIND_I4) {
      vip.reserve(source.items);
      vip = source.vip;
  } else if (source.tk == ESMC_TYPEKIND_I8) {
      vlp.reserve(source.items);
      vlp = source.vlp;
  } else if (source.tk == ESMC_TYPEKIND_R4) {
      vfp.reserve(source.items);
      vfp = source.vfp;
  } else if (source.tk == ESMC_TYPEKIND_R8) {
      vdp.reserve(source.items);
      vdp = source.vdp;
  } else if (source.tk == ESMC_TYPEKIND_LOGICAL){
      vbp.reserve(source.items);
      vbp = source.vbp;
  } else if (source.tk == ESMC_TYPEKIND_CHARACTER) {
      vcpp.reserve(source.items);
      vcpp = source.vcpp;
  }

  return ESMF_SUCCESS;

}  // end AttributeCopy
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeCopyHybrid"
//BOPI
// !IROUTINE:  AttributeCopyHybrid - copy {\tt Attributes} between ESMF objects
//
// !INTERFACE:
      int Attribute::AttributeCopyHybrid(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
        const Attribute &source) {   // in - source
//
// !DESCRIPTION:
//   Copy all {\tt Attribute} data, copy by value all {\tt Attributes} in this
//   base level, and copy by reference all {\tt Attributes} in lower base levels.
//
//EOPI
  int localrc;
  unsigned int i;
  Attribute *attr;
  
  attr = NULL;
  
  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // copy all Attributes and Attribute packages by value
  localrc = AttributeCopyIgnore(source);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        &localrc)) return localrc;
/*
  // call local copy on this Attribute
  localrc = AttributeCopy(source);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        &localrc)) return localrc;

  // copy Attributes and Attribute packages by value
  for (i=0; i<source.attrList.size(); i++) {
    attr = NULL;
    attr = new Attribute(ESMF_FALSE);
    if (!attr) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_NOT_CREATED,
        "Failed allocating an Attribute", ESMC_CONTEXT, &localrc);
      return localrc;
    }
    // set new attr to point to its intented destination and recurse
    (attr->attrBase) = (this->attrBase); 
    (attr->parent) = this;
    localrc = attr->AttributeCopyValue(*(source.attrList.at(i)));
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
          &localrc)) return localrc;
    localrc = AttributeSet(attr);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
          &localrc)) return localrc;
  }
  // copy Attribute packages by value
  for (i=0; i<source.packList.size(); i++) {
    attr = NULL;
    attr = new Attribute(ESMF_FALSE);
    if (!attr) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_NOT_CREATED,
        "Failed allocating an Attribute", ESMC_CONTEXT, &localrc);
      return localrc;
    }
    // set new attr to point to its intented destination and recurse
    (attr->attrBase) = (this->attrBase);
    (attr->parent) = this;
    localrc = attr->AttributeCopyValue(*(source.packList.at(i)));
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
          &localrc)) return localrc;
    localrc = AttPackSet(attr);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
          &localrc)) return localrc;
  }
*/
  // link from this Attribute to all links of the source Attribute
  for (i=0; i<source.linkList.size(); i++) {
    attr = source.linkList.at(i);
    ESMC_Logical temp_linkChange = ESMF_TRUE;
    localrc = AttributeLink(attr, &temp_linkChange);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
          &localrc)) return localrc;
  }

  return ESMF_SUCCESS;

 } // end AttributeCopyHybrid
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeCopyIgnore"
//BOPI
// !IROUTINE:  AttributeCopyIgnore - copy {\tt Attributes} between ESMF objects
//
// !INTERFACE:
      int Attribute::AttributeCopyIgnore(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
        const Attribute &source) {   // in - source
//
// !DESCRIPTION:
//   Copy all {\tt Attribute} data and copy by value all {\tt Attributes} in
//   this base level.
//
//EOPI
  int localrc;
  unsigned int i;
  Attribute *attr, *attpack;

  attr = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // call local copy on source
  localrc = AttributeCopy(source);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        &localrc)) return localrc;

  // copy base level Attributes by value
  for (i=0; i<source.attrList.size(); i++) {
    // look to see if source.attrList.at(i) already exists on *this
    attr = AttributeGet(source.attrList.at(i)->attrName);
    if (!attr) {
      attr = new Attribute(ESMF_FALSE);

      // set the parent
      attr->parent = parent;
      attr->setBase(attrBase);

      // recurse to set the attribute values
      localrc = attr->AttributeCopyIgnore(*(source.attrList.at(i)));
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        &localrc)) return localrc;

      // add new attr to *this
      localrc = AttributeSet(attr);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
          &localrc)) return localrc;
    } else {
      // still have to reset the parent and base
      attr->parent = parent;
      attr->setBase(attrBase);
    }
  }

  // copy base level Attribute packages by value
  for (i=0; i<source.packList.size(); i++) {
    attpack = source.packList.at(i);
    string empty("");
    attr = AttPackGet(attpack->attrConvention, attpack->attrPurpose,
                      attpack->attrObject, empty, ESMC_ATTNEST_ON);
    if (!attr) {
      attr = new Attribute(ESMF_FALSE);

      // set the parent and base
      attr->parent = parent;
      attr->setBase(attrBase);

      // recurse through nested attribute packages
      localrc = attr->AttributeCopyIgnore(*(source.packList.at(i)));
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        &localrc)) return localrc;

      // add new attr to *this
      localrc = AttPackSet(attr);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
          &localrc)) return localrc;
    } else {
      // still have to reset the parent and base
      attr->parent = parent;
      attr->setBase(attrBase);
    }
  }

  return ESMF_SUCCESS;

 } // end AttributeCopyIgnore
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeCopyReplace"
//BOPI
// !IROUTINE:  AttributeCopyReplace - copy {\tt Attributes} between ESMF objects
//
// !INTERFACE:
      int Attribute::AttributeCopyReplace(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
        const Attribute &source) {   // in - source
//
// !DESCRIPTION:
//   Copy all {\tt Attribute} data and copy by value all {\tt Attributes} in 
//   this base level, replacing those that already exist.
//
//EOPI
  int localrc;
  unsigned int i;
  Attribute *attr, *attpack;
  
  attr = NULL;
  bool newattr = false;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // call local copy on source
  localrc = AttributeCopy(source);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        &localrc)) return localrc;

  // copy base level Attributes by value
  for (i=0; i<source.attrList.size(); i++) {
    newattr = false;
    // look to see if source.attrList.at(i) already exists on *this
    attr = AttributeGet(source.attrList.at(i)->attrName);
    if (!attr) {
      attr = new Attribute(ESMF_FALSE);
      newattr = true;
    }

    // HERE YOU COULD PUT IN if(!attr) TO IGNORE PRE-EXISTING VALUES
    // recurse to set the attribute values
    localrc = attr->AttributeCopyReplace(*(source.attrList.at(i)));
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        &localrc)) return localrc;

    if (newattr) {
      // add new attr to *this
      localrc = AttributeSet(attr);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
          &localrc)) return localrc;
    }
  }

  // copy base level Attribute packages by value
  for (i=0; i<source.packList.size(); i++) {
    newattr = false;
    attpack = source.packList.at(i);
    string empty("");
    attr = AttPackGet(attpack->attrConvention, attpack->attrPurpose, 
                      attpack->attrObject, empty, ESMC_ATTNEST_ON);
    if (!attr) {
      attr = new Attribute(ESMF_FALSE);
      newattr = true;
    }

    // recurse through nested attribute packages
    localrc = attr->AttributeCopyReplace(*(source.packList.at(i)));
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        &localrc)) return localrc;

    if (newattr) {
      // add new attr to *this
      localrc = AttPackSet(attr);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
          &localrc)) return localrc;
    }
  }

  return ESMF_SUCCESS;

 } // end AttributeCopyReplace
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeMove"
//BOPI
// !IROUTINE:  AttributeMove - Move {\tt Attributes} between ESMF objects
//
// !INTERFACE:
      int Attribute::AttributeMove(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
        Attribute *source) {   // in - source
//
// !DESCRIPTION:
//   Copy all {\tt Attribute} data and copy by reference all {\tt Attributes} in 
//   this base level and remove everything copied from source.  
//
//   Note:  this routine is written with the assumption of being called from a Base
//
//EOPI
  int localrc;
  unsigned int i;
  Attribute *attr;
  
  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // call local copy on source
  localrc = AttributeCopy(*source);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        &localrc)) return localrc;

  // copy base level Attributes by value
  for (i=0; i<source->attrList.size(); i++) {
    // add each attr to destination
    localrc = AttributeSet(source->attrList.at(0));
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
          &localrc)) return localrc;
    // now remove this Attribute from source, this is a swap
    source->attrList.erase(source->attrList.begin());
  }
  // copy base level Attribute packages by value
  for (i=0; i<source->packList.size(); i++) {
    // add each attr to destination
    localrc = AttPackSet(source->packList.at(0));
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
          &localrc)) return localrc;
    // now remove this Attribute from source, this is a swap
    source->packList.erase(source->packList.begin());
  }
/*
  // copy the Attribute links by value
  for (i=0; i<source->linkList.size(); i++) {
    // set the links
    localrc = AttributeLink(source->linkList.at(0));
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
          &localrc)) return localrc;
    // now remove the link from source, this is a swap
    localrc = source->AttributeLinkRemove(source->linkList.at(0));
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
          &localrc)) return localrc;
  }
*/

  return ESMF_SUCCESS;

 } // end AttributeMove
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeCountTree"
//BOPI
// !IROUTINE:  AttributeCountTree - count objects in {\tt Attribute} hierarchy 
//
// !INTERFACE:
      int Attribute::AttributeCountTree(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &convention,                 // in - convention
      const string &purpose,                    // in - purpose
      const string &object,                     // in - object type to look for
      int &objcount,             // inout - object count
      int &numattrs ) const{     // inout - count of attrs on objects in tree
// 
// !DESCRIPTION:
//     Count the number of objects in the {\tt Attribute} hierarchy 

//EOPI

  int localrc;
  unsigned int i;
  Attribute *attpack;

  attpack = NULL;

  // Initialize local return code
  localrc = ESMC_RC_NOT_IMPL;
 
  string attPackInstanceName;
  attpack = AttPackGet(convention, purpose, object, attPackInstanceName,
                   ESMC_ATTNEST_ON);
  if (attpack) {
    numattrs = 0;
    objcount++;
    localrc = attpack->AttributeCountTreeAttpack(objcount, numattrs);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
          &localrc)) return localrc;
  }

  // Recurse the hierarchy
  for (i=0; i<linkList.size(); i++) {
    localrc = linkList.at(i)->AttributeCountTree(convention, purpose, object, 
      objcount, numattrs);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
          &localrc)) return localrc;
    }

  return ESMF_SUCCESS;

}  // end AttributeCountTree
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeCountTreeAttpack"
//BOPI
// !IROUTINE:  AttributeCountTreeAttpack - count objects in {\tt Attribute} hierarchy 
//
// !INTERFACE:
      int Attribute::AttributeCountTreeAttpack(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      int &objcount,             // inout - count of objects to write
      int &numattrs ) const{     // inout - count of attrs on objects in tree
// 
// !DESCRIPTION:
//     Count the number of objects in the {\tt Attribute} hierarchy 

//EOPI

  int localrc;
  unsigned int i;

  // Initialize local return code
  localrc = ESMC_RC_NOT_IMPL;

  numattrs += attrList.size(); 

  // Recurse the hierarchy
  for (i=0; i<packList.size(); i++) {
    localrc = packList.at(i)->AttributeCountTreeAttpack(objcount,numattrs);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
          &localrc)) return localrc;
    }

  return ESMF_SUCCESS;

}  // end AttributeCountTreeAttpack
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeCountTreeLens"
//BOPI
// !IROUTINE:  AttributeCountTreeLens - get lengths of {\tt Attribute} values
//
// !INTERFACE:
      int Attribute::AttributeCountTreeLens(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &convention,           // in - convention
      const string &purpose,              // in - purpose
      const string &object,               // in - object type to look for
      int *attrLens,                      // inout - lengths of column names
      vector<string> &attrNames ) const{  // inout - names of columns
// 
// !DESCRIPTION:
//     Count the number of objects in the Attribute hierarchy

//EOPI

  int localrc;
  unsigned int i;
  int index;
  Attribute *attpack;
  
  attpack = NULL;

  // Initialize local return code
  localrc = ESMC_RC_NOT_IMPL;
  
  string attPackInstanceName;
  attpack = AttPackGet(convention, purpose, object, attPackInstanceName,
                   ESMC_ATTNEST_ON);
  if (attpack) {
    index = 0;
    localrc = attpack->AttributeCountTreeLensAttpack(index, attrLens, attrNames);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
          &localrc)) return localrc;
  }

  // Recurse the hierarchy
  for (i=0; i<linkList.size(); ++i) {
    localrc = linkList.at(i)->AttributeCountTreeLens(convention, purpose, object, attrLens, attrNames);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
          &localrc)) return localrc;
    }

  return ESMF_SUCCESS;

}  // end AttributeCountTreeLens
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeCountTreeLensAttpack"
//BOPI
// !IROUTINE:  AttributeCountTreeLensAttpack - get lengths and names of columns
//
// !INTERFACE:
      int Attribute::AttributeCountTreeLensAttpack(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      int &index,               // in - index counter
      int *attrLens,                      // inout - lengths of column names
      vector<string> &attrNames ) const{  // inout - names of columns
// 
// !DESCRIPTION:
//     Get the lengths and names of the columns in the table

//EOPI

  int localrc;
  unsigned int i;
  Attribute *attr;
  
  // Initialize local return code
  localrc = ESMC_RC_NOT_IMPL;
  
  for (i=0; i<attrList.size(); ++i) {
    if (attrLens[index] == 0)
      attrNames.push_back(attrList.at(i)->attrName);
    else if (attrLens[index] > 0) {
      // this should fail
      if (attrNames[index].compare(attrList.at(i)->attrName)) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_WRONG, 
          "Attribute package name out of order", ESMC_CONTEXT, &localrc);
        return localrc;
      }
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, 
        "Length < 0 = no good", ESMC_CONTEXT, &localrc);
      return localrc;
    }
    // add length
    if (attrList.at(i)->items > 1) {
      ESMC_LogDefault.Write("Write items >1 not yet implemented", 
        ESMC_LOGMSG_INFO, ESMC_CONTEXT);
      attrLens[index] = 0;
    } else if (attrList.at(i)->items == 1) {
        if (attrList.at(i)->tk == ESMC_TYPEKIND_LOGICAL)
          attrLens[index] = 8;
        else if (attrList.at(i)->tk == ESMC_TYPEKIND_CHARACTER) {
          if ((attrList.at(i)->vcpp[0].size()+3) > attrLens[index])
            attrLens[index] = (attrList.at(i)->vcpp[0].size()+3);
        } else {
            attr = attrList.at(i);
            if (attr->tk == ESMC_TYPEKIND_I4) {
              /*if (attr->vi < -1) attrLens[index] = 3 + ceil(log10(-1*attr->vi));
              else if (attr->vi >= -1 && attr->vi <=1) attrLens[index] = 4;
              else attrLens[index] = 3 + ceil(log10(attr->vi));
              */ attrLens[index] = 10;
            } else if (attr->tk == ESMC_TYPEKIND_I8) {
              /*if (attr->vl < -1) attrLens[index] = 3 + ceil(log10(-1*attr->vl));
              else if (attr->vl >= -1 && attr->vl <=1) attrLens[index] = 4;
              else attrLens[index] = 3 + ceil(log10(attr->vl));
              */ attrLens[index] = 10;
            } else if (attr->tk == ESMC_TYPEKIND_R4) {
              /*if (attr->vf < -1) attrLens[index] = 3 + ceil(log10(-1*attr->vf));
              else if (attr->vf >= -1 && attr->vf <=1) attrLens[index] = 4;
              else attrLens[index] = 3 + ceil(log10(attr->vf));
              */ attrLens[index] = 10;
            } else if (attr->tk == ESMC_TYPEKIND_R8) {
              /*if (attr->vd < -1) attrLens[index] = 3 + ceil(log10(-1*attr->vd));
              else if (attr->vd >= -1 && attr->vd <=1) attrLens[index] = 4;
              else attrLens[index] = 3 + ceil(log10(attr->vd));
              */ attrLens[index] = 10;
            } else {
              ESMC_LogDefault.Write(
               "Couldn't find data type, using generic string length", 
               ESMC_LOGMSG_INFO, ESMC_CONTEXT);
              attrLens[index] = 10;
            }
        }
    }
    ++index;
  }
  
  // Recurse the hierarchy
  for (i=0; i<packList.size(); i++) {
    localrc = packList.at(i)->AttributeCountTreeLensAttpack(index, attrLens, attrNames);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
          &localrc)) return localrc;
    }

  return ESMF_SUCCESS;

}  // end AttributeCountTreeLensAttpack
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "Get"
//BOPI
// !IROUTINE:  get(int *)
//
// !INTERFACE:
      int Attribute::get(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      int *count,                      // out - number of values in list
      vector<ESMC_I4> *value) const {  // out - Attribute value
// 
// !DESCRIPTION:
//    Get the {\tt ESMC\_I4} valueList of an {\tt Attribute}.
//
//EOPI

  int localrc;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // simple sanity checks
  if (tk != ESMC_TYPEKIND_I4) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ATTR_WRONGTYPE,
      "Attribute not typekind I4", ESMC_CONTEXT, &localrc);
    return localrc;
  }

  if (count)
    *count = items;

  if (value)
    *value = vip;

  return ESMF_SUCCESS;

}  // end get(int *)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "get"
//BOPI
// !IROUTINE:  get(ESMC_I8 *)
//
// !INTERFACE:
      int Attribute::get(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      int *count,                      // out - number of values in list
      vector<ESMC_I8> *value) const {  // out - Attribute value
// 
// !DESCRIPTION:
//    Get the {\tt ESMC\_I8} valueList of an {\tt Attribute}.
//
//EOPI

  int localrc;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // simple sanity checks
  if (tk != ESMC_TYPEKIND_I8) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ATTR_WRONGTYPE,
      "Attribute not typekind I8", ESMC_CONTEXT, &localrc);
    return localrc;
  }

  if (count)
    *count = items;

  if (value)
    *value = vlp;

  return ESMF_SUCCESS;

}  // end get(ESMC_I8 *)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "get"
//BOPI
// !IROUTINE:  get(ESMC_R4 *)
//
// !INTERFACE:
      int Attribute::get(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      int *count,                      // out - number of values in list
      vector<ESMC_R4> *value) const {  // out - Attribute value
// 
// !DESCRIPTION:
//    Get the {\tt ESMC\_R4} valueList of an {\tt Attribute}.
//
//EOPI

  int localrc;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // simple sanity checks
  if (tk != ESMC_TYPEKIND_R4) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ATTR_WRONGTYPE,
      "Attribute not typekind R4", ESMC_CONTEXT, &localrc);
    return localrc;
  }

  if (count)
    *count = items;

  if (value)
    *value = vfp;

  return ESMF_SUCCESS;

}  // end get(ESMC_R4 *)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "get"
//BOPI
// !IROUTINE:  get(ESMC_R8 *)
//
// !INTERFACE:
      int Attribute::get(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:

      int *count,                      // out - number of values in list
      vector<ESMC_R8> *value) const {  // out - Attribute value
// 
// !DESCRIPTION:
//    Get the {\tt ESMC\_R8} valueList of an {\tt Attribute}.
//
//EOPI

  int localrc;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL; 
  
    // simple sanity checks
    if (tk != ESMC_TYPEKIND_R8) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ATTR_WRONGTYPE, 
        "Attribute not typekind R8", ESMC_CONTEXT, &localrc);
      return localrc;
    }

    if (count) 
      *count = items;

    if (value) 
      *value = vdp;

  return ESMF_SUCCESS;

}  // end get(ESMC_R8 *)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "get"
//BOPI
// !IROUTINE:  get(ESMC_Logical *)
//
// !INTERFACE:
      int Attribute::get(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      int *count,                           // out - number of values in list
      vector<ESMC_Logical> *value) const {  // out - Attribute value
// 
// !DESCRIPTION:
//    Get the {\tt ESMC\_Logical} valueList of an {\tt Attribute}.
//
//EOPI

  int localrc;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

    // simple sanity checks
    if (tk != ESMC_TYPEKIND_LOGICAL) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ATTR_WRONGTYPE, 
        "Attribute not typekind LOGICAL", ESMC_CONTEXT, &localrc);
      return localrc;
    }

    if (count) 
      *count = items;

    if (value) 
      *value = vbp;

  return ESMF_SUCCESS;

}  // end get(ESMC_Logical *)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "get"
//BOPI
// !IROUTINE:  get(charlist)
//
// !INTERFACE:
      int Attribute::get(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      vector<string> *value) const {  // out - Attribute values
// 
// !DESCRIPTION:
//    Get the charlist value of an {\tt Attribute}.
//
//EOPI

  int localrc;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // simple sanity checks
  if (tk != ESMC_TYPEKIND_CHARACTER) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ATTR_WRONGTYPE,
      "Attribute not typekind CHARACTER", ESMC_CONTEXT, &localrc);
    return localrc;
  }

  *value = vcpp;
  
  return ESMF_SUCCESS;

}  // end get(charlist)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "get"
//BOPI
// !IROUTINE:  get - get lengths of strings in an {\tt Attribute}
//
// !INTERFACE:
      int Attribute::get(
//
// !RETURN VALUE:
//    {\tt Attribute} pointer or NULL on error exit.
//
// !ARGUMENTS:
      int *lens,          // out - Atttribute char* lengths to retrieve
      int count) const {  // in/out - number of Attribute lengths to retrieve
//
// !DESCRIPTION:
//    Get the lengths of the strings in an {\tt Attribute}.
//
//EOPI

  int size, localrc;
  unsigned int i;

  // check whether this Attribute has been set
  if (!isSet()) {
    // not set -> return in a sensible way
    for (i=0; i<count; i++)
      lens[i] = 0;
    return ESMF_SUCCESS;
  }

  // check that this is a char Attribute
  if (tk != ESMC_TYPEKIND_CHARACTER) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ATTR_WRONGTYPE,
      "Attribute is not typekind CHARACTER", ESMC_CONTEXT, &localrc);
    return localrc;
  }

  // check that the count is correct
  if (count < 0 || (count > items)) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ATTR_ITEMSOFF,
      "Count argument is incorrect", ESMC_CONTEXT, &localrc);
    return localrc;
  }

  // find the lengths of the strings on this Attribute
  if (!vcpp.empty()) {
  for (i=0; i<count; i++)
    lens[i] = (vcpp[i]).size();
  } //else if (!vcp.empty()) lens[0] = vcp.size();
  else lens[0] = 0;

  return ESMF_SUCCESS;

}  // end get
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "getCount"
//BOPI
// !IROUTINE:  getCount - get an the number of {\tt Attributes}
// 
// !INTERFACE:
      int Attribute::getCount(
// 
// !RETURN VALUE:
//    number of {\tt Attributes} in this attrList
// 
// !ARGUMENTS:
      ESMC_AttGetCountFlag gcflag,  // in - attgetcount flag
    int *count                     // out - the count to return
    ) const {
//
// !DESCRIPTION:
//      Returns number of {\tt Attributes} present
//
//EOPI

  int localrc;

  if (gcflag == ESMC_ATTGETCOUNT_ATTRIBUTE)
      *count = this->getCountAttr();
  else if (gcflag == ESMC_ATTGETCOUNT_ATTPACK)
      *count = this->getCountPack();
  else if (gcflag == ESMC_ATTGETCOUNT_ATTLINK)
      *count = this->getCountLink();
  else if (gcflag == ESMC_ATTGETCOUNT_TOTAL)
      *count = this->getCountTotal();
  else {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                     "invalid value for attcountflag", ESMC_CONTEXT, &localrc);
    return localrc;
  }

  if (*count < 0) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                      "failed getting attribute count", ESMC_CONTEXT, &localrc);
      return localrc;
  }

  return ESMF_SUCCESS;

} // end getCount
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "getCount"
//BOPI
// !IROUTINE:  getCount - get an the number of {\tt Attributes}
//
// !INTERFACE:
      int Attribute::getCount(
//
// !RETURN VALUE:
//    number of {\tt Attributes} in this attrList
//
// !ARGUMENTS:
      ESMC_AttGetCountFlag gcflag,   // in - attgetcount flag
      ESMC_AttNest_Flag anflag,      // in - attnestflag
    int *count                     // out - the count to return
    ) const {
//
// !DESCRIPTION:
//      Returns number of {\tt Attributes} present
//
//EOPI

  int localrc;
  int lcount;

  localrc = getCount(gcflag, &lcount);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
          &localrc)) return localrc;

  *count += lcount;

  /*
  printf("getCount, count = %d, lcount = %d\n", *count, lcount);
  printf("getCount attpack convention=%s, purpose=%s\n",
      getConvention().c_str(), getPurpose().c_str());
  */

  if (anflag == ESMC_ATTNEST_ON)
    for (unsigned int i=0; i<this->packList.size(); ++i)
      this->packList.at(i)->getCount(gcflag, anflag, count);

  return ESMF_SUCCESS;

} // end getCount
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeGet"
//BOPI
// !IROUTINE:  AttributeGet - get {\tt Attribute} from an ESMF type
//
// !INTERFACE:
      Attribute *Attribute::AttributeGet(
//
// !RETURN VALUE:
//    {\tt Attribute} pointer or NULL on error exit.
//
// !ARGUMENTS:
      const string &name) const {        // in - Attribute name to retrieve
//
// !DESCRIPTION:
//    Get the {\tt Attribute} name.
//
//EOPI

  unsigned int i;

  for (i=0; i<attrList.size(); i++) {
      if (name.compare(attrList.at(i)->attrName) == 0)
          return attrList.at(i);
  }

  // you get here if no matches found
  return NULL;

}  // end AttributeGet
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeGet"
//BOPI
// !IROUTINE:  AttributeGet - get an {\tt Attribute} by number
//
// !INTERFACE:
      Attribute *Attribute::AttributeGet(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      int number) const {             // in - Attribute number
//
// !DESCRIPTION:
//     Allows the caller to get {\tt Attributes} by number instead of by name.
//     This can be useful in iterating through all {\tt Attributes} in a loop.
//
//EOPI

  // simple sanity check
  if ((number < 0) || (number >= attrList.size())) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_VAL_OUTOFRANGE,
      "Invalid index for AttributeGet(index)", ESMC_CONTEXT, NULL);
    return NULL;
  }

  return attrList.at(number);

}  // end AttributeGet
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeIsPresent"
//BOPI
// !IROUTINE:  AttributeIsPresent - query for an {\tt Attribute}
//
// !INTERFACE:
      int Attribute::AttributeIsPresent(
// 
// !RETURN VALUE:
//    Value of the present flag.
// 
// !ARGUMENTS:
      const string &name,                             // in - Attribute name
      ESMC_Logical *present) const {         // in/out - the present flag
// 
// !DESCRIPTION:
//     Query for an {\tt Attribute} given its name
//
//EOPI

  Attribute *attr;
  
  attr = NULL;

  attr = AttributeGet(name);
  if (!attr)
    *present = ESMF_FALSE;
  else *present = ESMF_TRUE;
  
  // return
  return ESMF_SUCCESS;

}  // end AttributeIsPresent
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeIsPresent"
//BOPI
// !IROUTINE:  AttributeIsPresent - query for an {\tt Attribute}
//
// !INTERFACE:
      int Attribute::AttributeIsPresent(
//
// !RETURN VALUE:
//    Value of the present flag.
//
// !ARGUMENTS:
      const int &num,                 // in - Attribute index
      ESMC_Logical *present) const {  // in/out - the present flag
//
// !DESCRIPTION:
//     Query for an {\tt Attribute} given its index
//
//EOPI

  Attribute *attr;

  attr = NULL;

  attr = AttributeGet(num);
  if (!attr)
    *present = ESMF_FALSE;
  else *present = ESMF_TRUE;

  // return
  return ESMF_SUCCESS;

}  // end AttributeIsPresent
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "isSet"
//BOPI
// !IROUTINE:  isSet - query if an {\tt Attribute} has been set
//
// !INTERFACE:
      bool Attribute::isSet() const {

// 
// !RETURN VALUE:
//    Whether the value has been set
// 
// !ARGUMENTS:
// 
//    None
//
// !DESCRIPTION:
//     Query for whether an {\tt Attribute} has been set
//
//EOPI

  if (this == NULL) {
    ESMC_LogDefault.Write("isSet - this == NULL",
          ESMC_LOGMSG_WARN, ESMC_CONTEXT);
    return false;
  }

  if (items > 0 && tk != ESMF_NOKIND)
    return true;  // set

  //char msg[ESMF_MAXSTR];
  //sprintf(msg, "isSet - items <= 0 or tk == ESMF_NOKIND: %s", attrName.c_str());
  //ESMC_LogDefault.Write(msg,
  //   ESMC_LOGMSG_WARN, ESMC_CONTEXT);

  return false;     // not set

}  // end isSet
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeLink"
//BOPI
// !IROUTINE:  AttributeLink - Link an {\tt Attribute} hierarchy
//
// !INTERFACE:
      int Attribute::AttributeLink(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      Attribute *destination,         // in/out destination Attribute to be linked
      ESMC_Logical *linkChangeIn) {   // in - link Changes?
// !DESCRIPTION:
//     Link an {\tt Attribute} hierarchy.
//
//EOPI

  int localrc;
  unsigned int i;
  Attribute *attr;
  
  attr = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
    
/*  for (i=0; i<linkList.size(); i++) {
    if ((destination->attrBase->ESMC_BaseGetID() == 
      linkList.at(i)->attrBase->ESMC_BaseGetID()) &&
      ESMCI::VMIdCompare(destination->attrBase->ESMC_BaseGetVMId(),
      linkList.at(i)->attrBase->ESMC_BaseGetVMId())) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ATTR_LINK, 
        "AttributeLink tried to double set a link", ESMC_CONTEXT, &localrc);
      return localrc;
    }
  }
*/

  // actually set the link
  attr = destination;
  // attrBase and parent should already be set
  linkList.push_back(attr);  
  // set the linkChange as desired
  linkChange = *linkChangeIn;

  return ESMF_SUCCESS;

}  // end AttributeLink
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeLinkRemove"
//BOPI
// !IROUTINE:  AttributeLinkRemove - Remove a link in an {\tt Attribute} hierarchy
//
// !INTERFACE:
      int Attribute::AttributeLinkRemove(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      Attribute *destination,         // in/out destination Attribute to be linked
      ESMC_Logical *linkChangeIn) {   // in - link Changes?
// !DESCRIPTION:
//     Set a link in an {\tt Attribute} hierarchy.
//
//EOPI

  int localrc;
  unsigned int i;
  bool done=false;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  for (i=0; i<linkList.size(); i++) {
    if (destination->attrBase->ESMC_BaseGetID() ==
        linkList.at(i)->attrBase->ESMC_BaseGetID() &&
        ESMCI::VMIdCompare(destination->attrBase->ESMC_BaseGetVMId(),
        linkList.at(i)->attrBase->ESMC_BaseGetVMId())) {
        // don't delete the root, but erase the Attribute pointer
        linkList.erase(linkList.begin() + i);
        //structChange = ESMF_TRUE;
        linkChange = *linkChangeIn;
        done = true;
        break;
    }
  }
  
  // link wasn't found
  if (!done) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_FOUND, 
      "AttributeLink could not find the link to remove", ESMC_CONTEXT,
      &localrc);
    return localrc;
  }
  
  return ESMF_SUCCESS;
    
}  // end AttributeLinkRemove
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeRemove"
//BOPI
// !IROUTINE:  AttributeRemove - Remove the {\tt Attribute}
//
// !INTERFACE:
      int Attribute::AttributeRemove(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name) {                // in - name
// 
// !DESCRIPTION:
//     Remove the {\tt Attribute} 

//EOPI

  int localrc;
  unsigned int i;
  bool done=false;

  // Initialize local return code
  localrc = ESMC_RC_NOT_IMPL;
  
  for (i=0; i<attrList.size(); i++) {
    if (name.compare(attrList.at(i)->attrName) == 0) {
      // found a match, destroy it
      delete attrList.at(i);
      attrList.erase(attrList.begin() + i);
      deleteChange = ESMF_TRUE;
      done = true;
      break;
    }
  }
  
  if (!done) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_FOUND, 
      "Could not locate the Attribute to remove", ESMC_CONTEXT, &localrc);
    return localrc;
  }
  
  return ESMF_SUCCESS;

}  // end AttributeRemove
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeSet"
//BOPI
// !IROUTINE:  AttributeSet - set {\tt Attribute} on an ESMF type
//
// !INTERFACE:
      int Attribute::AttributeSet(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      Attribute *attr) {   // in - Attribute name, type, value
// 
// !DESCRIPTION:
//     Associate a (name,value) pair with any type in the system.
//     This version of set is used when the caller has already allocated
//     an {\tt Attribute} object and filled it, and the {\tt Attribute} 
//     is simply added to the list belonging to this object.
//
//EOPI

  int localrc;
  unsigned int i;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // simple sanity checks
  if (!attr) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_BAD, 
      "Bad Attribute object", ESMC_CONTEXT, &localrc);
    return localrc;
  }

  // first, see if you are replacing an existing Attribute
  for (i=0; i<attrList.size(); i++) {
    if ((attr->attrName).compare(attrList.at(i)->attrName)==0 &&
        (attr->attrConvention).compare(attrList.at(i)->attrConvention)==0 &&
        (attr->attrPurpose).compare(attrList.at(i)->attrPurpose)==0 &&
        (attr->attrObject).compare(attrList.at(i)->attrObject)==0) {

      // if you get here, you found a match.  replace previous copy.

      // delete old Attribute, including possibly freeing a list
      delete attrList.at(i);

      // replace the original Attribute with attr
      attrList.at(i) = attr;
      // attr may be of a different value than the original Attribute
      attr->valueChange = ESMF_TRUE;
      // point attr to its new Base
      attr->attrBase = this->attrBase; 
      attr->parent = this;
      return ESMF_SUCCESS;
    }
  }   

  // point attr to its new Base
  attr->attrBase = this->attrBase;
  attr->parent = this;
  
  // add the Attribute
  attrList.push_back(attr);
  //structChange = ESMF_TRUE;
  
  return ESMF_SUCCESS;

}  // end AttributeSet
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeSet"
//BOPI
// !IROUTINE:  AttributeSet(int *) - set {\tt Attribute} on an ESMF type
//
// !INTERFACE:
      int Attribute::AttributeSet(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,              // in - Attribute name
      int count,               // in - number of ints in list
      vector<ESMC_I4> *value) {        // in - Attribute values
// 
// !DESCRIPTION:
//    Set the {\tt ESMC\_I4} valueList of an {\tt Attribute}.
//
//EOPI

  int localrc;
  Attribute *attr;

  attr = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new Attribute(name, ESMC_TYPEKIND_I4, count, value);
  if (!attr) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_NOT_CREATED, 
      "Bad Attribute object", ESMC_CONTEXT, &localrc);
    return localrc;
  }
 
  localrc = AttributeSet(attr);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        &localrc)) return localrc;

  return ESMF_SUCCESS;

}  // end AttributeSet(int *)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeSet"
//BOPI
// !IROUTINE:  AttributeSet(ESMC_I8 *) - set {\tt Attribute} on an ESMF type
//
// !INTERFACE:
      int Attribute::AttributeSet(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,              // in - Attribute name
      int count,               // in - number of ints in list
      vector<ESMC_I8> *value) {        // in - Attribute values
// 
// !DESCRIPTION:
//    Set the {\tt ESMC\_I8} valueList of an {\tt Attribute}.
//
//EOPI

  int localrc;
  Attribute *attr;

  attr = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new Attribute(name, ESMC_TYPEKIND_I8, count, value);
  if (!attr) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_NOT_CREATED, 
      "Bad Attribute object", ESMC_CONTEXT, &localrc);
    return localrc;
  }
 
  localrc = AttributeSet(attr);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        &localrc)) return localrc;

  return ESMF_SUCCESS;

}  // end AttributeSet(ESMC_I8 *)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeSet"
//BOPI
// !IROUTINE:  AttributeSet(ESMC_R4 *) - set {\tt Attribute} on an ESMF type
//
// !INTERFACE:
      int Attribute::AttributeSet(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,              // in - Attribute name
      int count,               // in - number of ESMC_R4s in list
      vector<ESMC_R4> *value) {        // in - Attribute values
// 
// !DESCRIPTION:
//    Set the {\tt ESMC\_R4} valueList of an {\tt Attribute}.
//
//EOPI

  int localrc;
  Attribute *attr;

  attr = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new Attribute(name, ESMC_TYPEKIND_R4, count, value);
  if (!attr) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_NOT_CREATED, 
      "Bad Attribute object", ESMC_CONTEXT, &localrc);
    return localrc;
  }
 
  localrc = AttributeSet(attr);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        &localrc)) return localrc;

  return ESMF_SUCCESS;

}  // end AttributeSet(ESMC_R4 *)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeSet"
//BOPI
// !IROUTINE:  AttributeSet(ESMC_R8 *) - set {\tt Attribute} on an ESMF type
//
// !INTERFACE:
      int Attribute::AttributeSet(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,              // in - Attribute name
      int count,               // in - number of ESMC_R8s in list
      vector<ESMC_R8> *value) {        // in - Attribute values
// 
// !DESCRIPTION:
//    Set the {\tt ESMC\_R8} valueList of an {\tt Attribute}.
//
//EOPI

  int localrc;
  Attribute *attr;

  attr = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new Attribute(name, ESMC_TYPEKIND_R8, count, value);  
  if (!attr) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_NOT_CREATED, 
      "Bad Attribute object", ESMC_CONTEXT, &localrc);
    return localrc;
  }
 
  localrc = AttributeSet(attr);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        &localrc)) return localrc;

  return ESMF_SUCCESS;

}  // end AttributeSet(ESMC_R8 *)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeSet"
//BOPI
// !IROUTINE:  AttributeSet(ESMC_Logical *) - set {\tt Attribute} on an ESMF type
//
// !INTERFACE:
      int Attribute::AttributeSet(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,              // in - Attribute name
      int count,               // in - number of logicals in list
      vector<ESMC_Logical> *value) {   // in - Attribute values
// 
// !DESCRIPTION:
//    Set the {\tt ESMC\_Logical} valueList of an {\tt Attribute}.
//
//EOPI

  int localrc;
  Attribute *attr;

  attr = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new Attribute(name, ESMC_TYPEKIND_LOGICAL, count, value);
  if (!attr) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_NOT_CREATED, 
      "Bad Attribute object", ESMC_CONTEXT, &localrc);
    return localrc;
  }
 
  localrc = AttributeSet(attr);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        &localrc)) return localrc;

  return ESMF_SUCCESS;

}  // end AttributeSet(ESMC_Logical *)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeSet"
//BOPI
// !IROUTINE:  AttributeSet(charlist) - set {\tt Attribute} on an ESMF type
//
// !INTERFACE:
      int Attribute::AttributeSet(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,       // in - Attribute name
      int count,               // in - number of strings in vector
      vector<string> *value) {    // in - Attribute value
// 
// !DESCRIPTION:
//    Set the value of an {\tt Attribute}.
//
//EOPI

  int localrc;
  Attribute *attr;

  attr = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new Attribute(name, ESMC_TYPEKIND_CHARACTER, count, value);
  if (!attr) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_NOT_CREATED, 
      "Bad Attribute object", ESMC_CONTEXT, &localrc);
    return localrc;
  }
 
  localrc = AttributeSet(attr);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        &localrc)) return localrc;

  return ESMF_SUCCESS;

}  // end AttributeSet(charlist)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeSetObjsInTree"
//BOPI
// !IROUTINE:  AttributeSetObjsInTree - set all objects in {\tt Attribute} hierarchy 
//
// !INTERFACE:
      int Attribute::AttributeSetObjsInTree(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,                    // in - name
      const string &object,                  // in - object
      const ESMC_TypeKind_Flag &tk,              // in - typekind
      const int &count,                     // in - count
      void *value) {                 // in - value
// 
// !DESCRIPTION:
//     Set the objects in the {\tt Attribute} hierarchy 

//EOPI

  int localrc;
  unsigned int i;

  // Initialize local return code
  localrc = ESMC_RC_NOT_IMPL;
  
  for (i=0; i<attrList.size(); i++) {
  // If this is object matches, count it
  if (object.compare(attrList.at(i)->attrObject) == 0 && 
      name.compare(attrList.at(i)->attrName) == 0) {
    localrc = attrList.at(i)->AttrModifyValue(tk, count, value);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
          &localrc)) return localrc;
  }
  }
  
  // Recurse the hierarchy
  for (i=0; i<packList.size(); i++) {
    localrc = packList.at(i)->AttributeSetObjsInTree(object,name,tk,count,value);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
          &localrc)) return localrc;
  }  

  // Recurse the hierarchy
  for (i=0; i<linkList.size(); i++) {
    localrc = linkList.at(i)->AttributeSetObjsInTree(object,name,tk,count,value);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
          &localrc)) return localrc;
  }  

  return ESMF_SUCCESS;

}  // end AttributeSetObjsInTree

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_Print"
//BOPI
// !IROUTINE:  Attribute::ESMC_Print - Print the {\tt Attribute} contents
//
// !INTERFACE:
      int Attribute::ESMC_Print(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      bool tofile,            // stream to stdout or file
      const char *filename,   // filename
      bool append) const {    // append or start new
// 
// !DESCRIPTION:
//     Print the contents of an {\tt Attribute} object
//
//EOPI
  int strsize=4*ESMF_MAXSTR;
  char msgbuf[strsize];
  ofstream fp;

  if (tofile) {
    sprintf(msgbuf, filename);
    // open file for writing and append to previous contents
    if (append)
      fp.open(msgbuf, ofstream::out | ofstream::app);
    // open file for writing and throw away previous contents
    else
      fp.open(msgbuf, ofstream::out | ofstream::trunc);
  }

  print_to_file(tofile, fp, 0);

  if (tofile)
    fp.close();
  else
    fflush (stdout);

  return ESMF_SUCCESS;

}  // end ESMC_Print
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "print_to_file"
//BOPI
// !IROUTINE:  Attribute::print_to_file - Print the {\tt Attribute} contents
//
// !INTERFACE:
      int Attribute::print_to_file(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      bool tofile,                  // stream to stdout or file
      ofstream &fp,                 // file handle
      unsigned int level) const {   // indentation according to attpack
//
// !DESCRIPTION:
//     Print the contents of an {\tt Attribute} object
//
//EOPI
  unsigned int i;
  int strsize=4*ESMF_MAXSTR;
  char msgbuf[strsize];
  string indent = "";
  int localrc;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  for (i=0; i<level; ++i) indent += "  ";

  for (i=0; i<attrList.size(); i++) {
    sprintf(msgbuf, "%sAttr %d:\n", indent.c_str(), i);
    attprint(msgbuf, strsize, tofile, fp);

    sprintf(msgbuf, "%s    name: %s\n", indent.c_str(),
        attrList.at(i)->attrName.c_str());
    attprint(msgbuf, strsize, tofile, fp);

    if (attrList.at(i)->items <= 0) {
      sprintf(msgbuf, "%s    value: \n", indent.c_str());
      attprint(msgbuf, strsize, tofile, fp);
    }

    if (attrList.at(i)->items == 1) {
      sprintf(msgbuf, "%s    value: ", indent.c_str());
      attprint(msgbuf, strsize, tofile, fp);

      if (attrList.at(i)->tk == ESMC_TYPEKIND_I4)
        sprintf(msgbuf, "%d\n", attrList.at(i)->vip.at(0));
      else if (attrList.at(i)->tk == ESMC_TYPEKIND_I8)
        sprintf(msgbuf, "%lld\n", attrList.at(i)->vlp.at(0));
      else if (attrList.at(i)->tk == ESMC_TYPEKIND_R4)
        sprintf(msgbuf, "%f\n", attrList.at(i)->vfp.at(0));
      else if (attrList.at(i)->tk == ESMC_TYPEKIND_R8)
        sprintf(msgbuf, "%g\n", attrList.at(i)->vdp.at(0));
      else if (attrList.at(i)->tk == ESMC_TYPEKIND_LOGICAL)
        sprintf(msgbuf, "%s\n", ESMC_LogicalString(attrList.at(i)->vbp.at(0)));
      else if (attrList.at(i)->tk == ESMC_TYPEKIND_CHARACTER)
        sprintf(msgbuf, "%s\n", attrList.at(i)->vcpp.at(0).c_str());
      else{
        sprintf(msgbuf, "unknown value");
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ATTR_WRONGTYPE, msgbuf,
          ESMC_CONTEXT, &localrc);
        return localrc;
      }
      attprint(msgbuf, strsize, tofile, fp);
    }

    if (attrList.at(i)->items > 1) {
      sprintf(msgbuf, "%s    %d items, values:\n", indent.c_str(),
          attrList.at(i)->items);
      attprint(msgbuf, strsize, tofile, fp);
      for (unsigned int j=0; j<attrList.at(i)->items; j++) {
        if (attrList.at(i)->tk == ESMC_TYPEKIND_I4) {
          sprintf(msgbuf, "%s        item %d: %d\n", indent.c_str(), j,
              attrList.at(i)->vip[j]);
        } else if (attrList.at(i)->tk == ESMC_TYPEKIND_I8) {
          sprintf(msgbuf, "%s        item %d: %lld\n", indent.c_str(), j,
              attrList.at(i)->vlp[j]);
        } else if (attrList.at(i)->tk == ESMC_TYPEKIND_R4) {
          sprintf(msgbuf, "%s        item %d: %f\n", indent.c_str(), j,
              attrList.at(i)->vfp[j]);
        } else if (attrList.at(i)->tk == ESMC_TYPEKIND_R8) {
          sprintf(msgbuf, "%s        item %d: %g\n", indent.c_str(), j,
              attrList.at(i)->vdp[j]);
        } else if (attrList.at(i)->tk == ESMC_TYPEKIND_LOGICAL) {
          sprintf(msgbuf, "%s        item %d: %s\n", indent.c_str(), j,
              ESMC_LogicalString(attrList.at(i)->vbp[j]));
        } else if (attrList.at(i)->tk == ESMC_TYPEKIND_CHARACTER) {
          sprintf(msgbuf, "%s        item %d: %s\n", indent.c_str(), j,
              attrList.at(i)->vcpp[j].c_str());
        } else{
          sprintf(msgbuf, "%s        unknown value", indent.c_str());
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, ESMC_CONTEXT,
            &localrc);
          return localrc;
        }
      attprint(msgbuf, strsize, tofile, fp);
      }
    }
    // print convention
    sprintf(msgbuf, "%s    convention: %s\n", indent.c_str(),
        attrList.at(i)->attrConvention.c_str());
    attprint(msgbuf, strsize, tofile, fp);

    // print purpose
    sprintf(msgbuf, "%s    purpose: %s\n", indent.c_str(),
        attrList.at(i)->attrPurpose.c_str());
    attprint(msgbuf, strsize, tofile, fp);

    // print object
    sprintf(msgbuf, "%s    object: %s\n", indent.c_str(),
        attrList.at(i)->attrObject.c_str());
    attprint(msgbuf, strsize, tofile, fp);

    sprintf(msgbuf, "%s    attrCount: %d\n", indent.c_str(),
        attrList.at(i)->getCountTotal());
    attprint(msgbuf, strsize, tofile, fp);
  }

  for (i=0; i<packList.size(); i++) {
    sprintf(msgbuf, "\n%sPack %d: %s\n", indent.c_str(),
        i, packList.at(i)->attrName.c_str());
    attprint(msgbuf, strsize, tofile, fp);
    packList.at(i)->print_to_file(tofile, fp, ++level);
  }

/*  RLO: only to be enabled for special cases
  for (i=0; i<linkList.size(); i++) {
    sprintf(msgbuf, "\n%sLink to Object %d: %s\n", indent.c_str(),
        i, linkList.at(i)->attrBase->ESMC_Base::ESMC_BaseGetName());
    attprint(msgbuf, strsize, tofile, fp);
    linkList.at(i)->print_to_file(tofile, fp, ++level);
  }
*/

  return ESMF_SUCCESS;

}  // end print_to_file
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_Print"
//BOPI
// !IROUTINE:  Attribute::ESMC_Print - Print the {\tt Attribute} contents
//
// !INTERFACE:
      int Attribute::ESMC_Print(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      ) const {                    // could add options at some point
//
// !DESCRIPTION:
//     Print the contents of an {\tt Attribute} object
//
//EOPI
  int localrc;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  localrc = ESMC_Print(false, NULL, false);

   return ESMF_SUCCESS;
}  // end ESMC_Print
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// Modifiers, Constructors, Destructors, Serializers, Print:
//
//-----------------------------------------------------------------------------
//----------------------------------------------------------------------------- 
#undef  ESMC_METHOD
#define ESMC_METHOD "Attribute()"
//BOPI
// !IROUTINE:  Attribute - native C++ constructor for Attribute class
//
// !INTERFACE:
      Attribute::Attribute(
//
// !RETURN VALUE:
//    new {\tt Attribute} object
//
// !ARGUMENTS:
        const string &conv,                  // convention
        const string &purp,                  // purpose
        const string &obj) {                 // object
//
// !DESCRIPTION:
//   Initialize an {\tt Attribute} and set the name, convention, and purpose.
//
//EOPI

  char name[ESMF_MAXSTR];
  
  tk = ESMF_NOKIND;
  items = 0;
  attrRoot = ESMF_FALSE;
  attrUpdateDone = ESMF_FALSE; // hack for non-ordered containers

  attrConvention = conv;
  attrPurpose = purp;
  attrObject = obj;
  attrPack = ESMF_TRUE;
  attrPackHead = ESMF_TRUE;
  attrNested = ESMF_FALSE;
  
  linkChange = ESMF_FALSE;
  structChange = ESMF_TRUE;
  valueChange = ESMF_FALSE;
  deleteChange = ESMF_FALSE;

  attrBase = ESMC_NULL_POINTER;
  parent = ESMC_NULL_POINTER;
  
  attrList.reserve(0);
  packList.reserve(0);
  linkList.reserve(0);

  vip.reserve(0);
  vlp.reserve(0);
  vfp.reserve(0);
  vdp.reserve(0);
  vbp.reserve(0);

  id = ++count;  // TODO: inherit from ESMC_Base class?

  // create unique attPackInstanceName (within this address space)
  sprintf(name, "Attribute package - %s %s %s %d", 
    conv.c_str(), purp.c_str(), obj.c_str(), id);
  attrName = name;

} // end Attribute
//----------------------------------------------------------------------------- 
#undef  ESMC_METHOD
#define ESMC_METHOD "Attribute()"
//BOPI
// !IROUTINE:  Attribute - native C++ constructor for Attribute class
//
// !INTERFACE:
      Attribute::Attribute(
//
// !RETURN VALUE:
//    new {\tt Attribute} object
//
// !ARGUMENTS:
        const string &name,                  // Attribute name
        const string &conv,                  // convention
        const string &purp,                  // purpose
        const string &obj) {                 // object
//
// !DESCRIPTION:
//   Initialize an {\tt Attribute} and set the name, convention, and purpose.
//
//EOPI

  attrName = name;
  tk = ESMF_NOKIND;
  items = 0;
  attrRoot = ESMF_FALSE;
  attrUpdateDone = ESMF_FALSE; // hack for non-ordered containers

  attrConvention = conv;
  attrPurpose = purp;
  attrObject = obj;
  attrPack = ESMF_TRUE;
  attrPackHead = ESMF_FALSE;
  attrNested = ESMF_FALSE;

  linkChange = ESMF_FALSE;
  structChange = ESMF_TRUE;
  valueChange = ESMF_FALSE;
  deleteChange = ESMF_FALSE;

  attrBase = ESMC_NULL_POINTER;
  parent = ESMC_NULL_POINTER;
  
  attrList.reserve(0);
  packList.reserve(0);
  linkList.reserve(0);

  vip.reserve(0);
  vlp.reserve(0);
  vfp.reserve(0);
  vdp.reserve(0);
  vbp.reserve(0);

  id = ++count;  // TODO: inherit from ESMC_Base class?

} // end Attribute
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "Attribute()"
//BOPI
// !IROUTINE:  Attribute - native C++ constructor for Attribute class
//
// !INTERFACE:
      Attribute::Attribute(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//     Create an empty {\tt Attribute} structure.
//
//EOPI

  attrName = "";
  tk = ESMF_NOKIND;
  items = 0;
  attrRoot = ESMF_TRUE;
  attrUpdateDone = ESMF_FALSE; // hack for non-ordered containers

  attrConvention = "";
  attrPurpose = "";
  attrObject = "";
  attrPack = ESMF_FALSE;
  attrPackHead = ESMF_FALSE;
  attrNested = ESMF_FALSE;

  linkChange = ESMF_FALSE;
  structChange = ESMF_TRUE;
  valueChange = ESMF_FALSE;
  deleteChange = ESMF_FALSE;

  attrBase = ESMC_NULL_POINTER;
  parent = ESMC_NULL_POINTER;
  
  attrList.reserve(0);
  packList.reserve(0);
  linkList.reserve(0);

  vip.reserve(0);
  vlp.reserve(0);
  vfp.reserve(0);
  vdp.reserve(0);
  vbp.reserve(0);

  attrGUID = "";
  id = ++count;  // TODO: inherit from ESMC_Base class?
  
 } // end Attribute
//----------------------------------------------------------------------------- 
#undef  ESMC_METHOD
#define ESMC_METHOD "Attribute()"
//BOPI
// !IROUTINE:  Attribute - native C++ constructor for Attribute class
//
// !INTERFACE:
      Attribute::Attribute(
//
// !RETURN VALUE:
//    new {\tt Attribute} object
//
// !ARGUMENTS:
        const ESMC_Logical &attributeRoot) {                 // root value
//
// !DESCRIPTION:
//   Initialize an {\tt Attribute} and set the name, convention, and purpose.
//
//EOPI

  attrName ="";
  tk = ESMF_NOKIND;
  items = 0;
  attrRoot = attributeRoot;
  attrUpdateDone = ESMF_FALSE; // hack for non-ordered containers

  attrConvention = "";
  attrPurpose = "";
  attrObject = "";
  attrPack = ESMF_FALSE;
  attrPackHead = ESMF_FALSE;
  attrNested = ESMF_FALSE;

  linkChange = ESMF_FALSE;
  structChange = ESMF_TRUE;
  valueChange = ESMF_FALSE;
  deleteChange = ESMF_FALSE;

  attrBase = ESMC_NULL_POINTER;
  parent = ESMC_NULL_POINTER;
  
  attrList.reserve(0);
  packList.reserve(0);
  linkList.reserve(0);

  vip.reserve(0);
  vlp.reserve(0);
  vfp.reserve(0);
  vdp.reserve(0);
  vbp.reserve(0);

  attrGUID = "";
  id = ++count;  // TODO: inherit from ESMC_Base class?
  
} // end Attribute
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "Attribute()"
//BOPI
// !IROUTINE:  Attribute - native C++ constructor for Attribute class
//
// !INTERFACE:
      Attribute::Attribute(
//
// !RETURN VALUE:
//    {\tt Attribute} object
//
// !ARGUMENTS:
        const string &name,                // Attribute name
        const ESMC_TypeKind_Flag &typekind,    // typekind
        int numitems,              // single or list
        void *datap) {             // generic pointer to values
//
// !DESCRIPTION:
//   Initialize an {\tt Attribute}, and make a copy of the data - multi item case
//
//EOPI
  unsigned int i;

  attrName = name;
  tk = typekind;
  items = numitems;
  attrRoot = ESMF_FALSE;
  attrUpdateDone = ESMF_FALSE; // hack for non-ordered containers

  attrConvention = "";
  attrPurpose = "";
  attrObject = "";
  attrPack = ESMF_FALSE;
  attrPackHead = ESMF_FALSE;
  attrNested = ESMF_FALSE;

  linkChange = ESMF_FALSE;
  structChange = ESMF_TRUE;
  valueChange = ESMF_FALSE;
  deleteChange = ESMF_FALSE;

  attrBase = ESMC_NULL_POINTER;
  parent = ESMC_NULL_POINTER;
  
  attrList.reserve(0);
  packList.reserve(0);
  linkList.reserve(0);
  
  vip.reserve(0);
  vlp.reserve(0);
  vfp.reserve(0);
  vdp.reserve(0);
  vbp.reserve(0);
 
    // alloc space for a list and do the copy
        if (tk == ESMC_TYPEKIND_I4) {
            vip.reserve(items);      
            if (datap) 
              for (i=0; i<items; i++)
                vip.push_back((*(static_cast<vector<ESMC_I4>*> (datap)))[i]);  
        } else if (tk == ESMC_TYPEKIND_I8) {
            vlp.reserve(items);      
            if (datap) 
              for (i=0; i<items; i++)
                vlp.push_back((*(static_cast<vector<ESMC_I8>*> (datap)))[i]);  
        } else if (tk == ESMC_TYPEKIND_R4) {
            vfp.reserve(items);      
            if (datap) 
              for (i=0; i<items; i++)
                vfp.push_back((*(static_cast<vector<ESMC_R4>*> (datap)))[i]);  
        } else if (tk == ESMC_TYPEKIND_R8) {
            vdp.reserve(items);      
            if (datap) 
              for (i=0; i<items; i++)
                vdp.push_back((*(static_cast<vector<ESMC_R8>*> (datap)))[i]);  
        } else if (tk == ESMC_TYPEKIND_LOGICAL) {
            vbp.reserve(items);      
            if (datap) 
              for (i=0; i<items; i++)
                vbp.push_back((*(static_cast<vector<ESMC_Logical>*> (datap)))[i]);  
        } else if (tk == ESMC_TYPEKIND_CHARACTER) {
            vcpp.reserve(items);
            if (datap) {
              for (i=0; i<items; i++) 
                vcpp.push_back((*(static_cast<vector<string>*> (datap)))[i]);
            }
        }

  attrGUID = "";
  id = ++count;  // TODO: inherit from ESMC_Base class?

 } // end Attribute
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttrModifyValue()"
//BOPI
// !IROUTINE:  AttrModifyValue - native C++ modifier for Attribute class
//
// !INTERFACE:
      int Attribute::AttrModifyValue(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
        const ESMC_TypeKind_Flag &typekind,    // typekind
        int numitems,              // single or list
        void *datap) {             // generic pointer to values
//
// !DESCRIPTION:
//   Set a value on an existing {\tt Attribute} object.
//
//EOPI
  int localrc;
  unsigned int i;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

    if (typekind == ESMC_TYPEKIND_I4) {
        vip.clear();
        vip.reserve(numitems);      
        if (datap) 
          for (i=0; i<numitems; i++)
            vip.push_back((*(static_cast<vector<ESMC_I4>*> (datap)))[i]);  
    } else if (typekind == ESMC_TYPEKIND_I8) {
        vlp.clear();
        vlp.reserve(numitems);      
        if (datap) 
          for (i=0; i<numitems; i++)
            vlp.push_back((*(static_cast<vector<ESMC_I8>*> (datap)))[i]);  
    } else if (typekind == ESMC_TYPEKIND_R4) {
        vfp.clear();
        vfp.reserve(numitems);      
        if (datap) 
          for (i=0; i<numitems; i++)
            vfp.push_back((*(static_cast<vector<ESMC_R4>*> (datap)))[i]);  
    } else if (typekind == ESMC_TYPEKIND_R8) {
        vdp.clear();
        vdp.reserve(numitems);      
        if (datap) 
          for (i=0; i<numitems; i++)
            vdp.push_back((*(static_cast<vector<ESMC_R8>*> (datap)))[i]);  
    } else if (typekind == ESMC_TYPEKIND_LOGICAL) {
        vbp.clear();
        vbp.reserve(numitems);      
        if (datap) 
          for (i=0; i<numitems; i++)
            vbp.push_back((*(static_cast<vector<ESMC_Logical>*> (datap)))[i]);  
    } else if (typekind == ESMC_TYPEKIND_CHARACTER) {
        vcpp.clear();
        vcpp.reserve(numitems);
        if (datap) {
          for (i=0; i<numitems; i++) 
            vcpp.push_back((*(static_cast<vector<string>*> (datap)))[i]);
        }
    }
 
  // if a change was made, note the new values
  if (numitems >= 1) {
    items = numitems;
  }

  tk = typekind;
  valueChange = ESMF_TRUE;

  return ESMF_SUCCESS;

 } // end AttrModifyValue
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeOperator="
//BOPI
// !IROUTINE:  Attribute - generic operator=
//
// !INTERFACE:
      Attribute& Attribute::operator=(
//
// !ARGUMENTS:
      const Attribute& source) {
// 
// !RETURN VALUE:
//    {\tt Attribute} object.
// 
// !DESCRIPTION:
//    Generic operator= call AttributeCopyValue.
//
//EOPI

  int localrc;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  localrc = AttributeCopyIgnore(source);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        &localrc)) delete this;

  return (*this);

}  // end AttributeOperator=
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "clean()"
//BOPI
// !IROUTINE:  clean - remove all Attributes, packages and links
//
// !INTERFACE:
      void Attribute::clean() {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//    Delete an {\tt Attribute} hierarchy.
//
//EOPI

    attrBase = ESMC_NULL_POINTER;
    parent = ESMC_NULL_POINTER;

    if (tk == ESMC_TYPEKIND_I4) {vip.clear(); vector<ESMC_I4>().swap(vip); }
    else if (tk == ESMC_TYPEKIND_I8) {vlp.clear(); vector<ESMC_I8>().swap(vlp); }
    else if (tk == ESMC_TYPEKIND_R4) {vfp.clear(); vector<ESMC_R4>().swap(vfp); }
    else if (tk == ESMC_TYPEKIND_R8) {vdp.clear(); vector<ESMC_R8>().swap(vdp); }
    else if (tk == ESMC_TYPEKIND_LOGICAL) {vbp.clear(); vector<ESMC_Logical>().swap(vbp); }
    else if (tk == ESMC_TYPEKIND_CHARACTER) {vcpp.clear(); vector<string>().swap(vcpp); }

    while (!attrList.empty()) {
      delete attrList.back();
      attrList.pop_back();
    }
    vector<Attribute*>().swap(attrList);

    while (!packList.empty()) {
      delete packList.back();
      packList.pop_back();
    }
    vector<Attribute*>().swap(packList);

    for(std::vector<Attribute*>::iterator it = linkList.begin();
      it != linkList.end(); ++it) *it = NULL;
    vector<Attribute*>().swap(linkList);

 } // end clean
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "~Attribute()"
//BOPI
// !IROUTINE:  ~Attribute - native C++ destructor for Attribute class
//
// !INTERFACE:
      Attribute::~Attribute(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//    Delete an {\tt Attribute} hierarchy.
//
//EOPI

    attrBase = ESMC_NULL_POINTER;
    parent = ESMC_NULL_POINTER;

    if (tk == ESMC_TYPEKIND_I4) {vip.clear(); vector<ESMC_I4>().swap(vip); }
    else if (tk == ESMC_TYPEKIND_I8) {vlp.clear(); vector<ESMC_I8>().swap(vlp); }
    else if (tk == ESMC_TYPEKIND_R4) {vfp.clear(); vector<ESMC_R4>().swap(vfp); }
    else if (tk == ESMC_TYPEKIND_R8) {vdp.clear(); vector<ESMC_R8>().swap(vdp); }
    else if (tk == ESMC_TYPEKIND_LOGICAL) {vbp.clear(); vector<ESMC_Logical>().swap(vbp); }
    else if (tk == ESMC_TYPEKIND_CHARACTER) {vcpp.clear(); vector<string>().swap(vcpp); }

    while (!attrList.empty()) {
      delete attrList.back();
      attrList.pop_back();
    }
    vector<Attribute*>().swap(attrList);

    while (!packList.empty()) {
      delete packList.back();
      packList.pop_back();
    }
    vector<Attribute*>().swap(packList);

    for(std::vector<Attribute*>::iterator it = linkList.begin();
        it != linkList.end(); ++it) *it = NULL;
    vector<Attribute*>().swap(linkList);

 } // end ~Attribute
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_Deserialize"
//BOPI
// !IROUTINE:  ESMC_Deserialize - Turn a byte stream into an object
//
// !INTERFACE:
      int Attribute::ESMC_Deserialize(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      char *buffer,          // in - byte stream to read
      int *offset) {         // inout - original offset, updated to point 
                             //       to first free byte after current obj
//
// !DESCRIPTION:
//    Turn a stream of bytes into an {\tt Attribute} hierarchy.
//
//EOPI
    int nbytes, chars;
    int localrc;
    int attrCount, packCount, linkCount;
    unsigned int i;
    Attribute *attr;
    
    // Initialize local return code; assume routine not implemented
    localrc = ESMC_RC_NOT_IMPL;

    // Define serialization macros
#define DESERIALIZE_VAR(bufptr,loff,var,t) \
  var=(*(reinterpret_cast<t*> ((bufptr)+(loff))));    \
  loff += (sizeof(t));  

#define DESERIALIZE_VARC(bufptr,loff,var,var2,s) \
  string var2((bufptr)+(loff),s); \
  var = var2; \
  loff += s; \

    // get localoffset
    int r=*offset%8;
    if (r!=0) *offset += 8-r;  // alignment
    int loffset=*offset;
    
    DESERIALIZE_VAR(buffer,loffset,chars,string::size_type);
    DESERIALIZE_VARC(buffer,loffset,attrName,temp,chars);

    DESERIALIZE_VAR(buffer,loffset,tk,ESMC_TypeKind_Flag);
    DESERIALIZE_VAR(buffer,loffset,items,int);
    DESERIALIZE_VAR(buffer,loffset,attrRoot,ESMC_Logical);
    
    DESERIALIZE_VAR(buffer,loffset,chars,string::size_type);
    DESERIALIZE_VARC(buffer,loffset,attrConvention,temp2,chars);
    DESERIALIZE_VAR(buffer,loffset,chars,string::size_type);
    DESERIALIZE_VARC(buffer,loffset,attrPurpose,temp3,chars);
    DESERIALIZE_VAR(buffer,loffset,chars,string::size_type);
    DESERIALIZE_VARC(buffer,loffset,attrObject,temp4,chars);
      
    DESERIALIZE_VAR(buffer,loffset,attrPack,ESMC_Logical);
    DESERIALIZE_VAR(buffer,loffset,attrPackHead,ESMC_Logical);
    DESERIALIZE_VAR(buffer,loffset,attrNested,ESMC_Logical);
    
    DESERIALIZE_VAR(buffer,loffset,attrCount,int);
    DESERIALIZE_VAR(buffer,loffset,packCount,int);
///    DESERIALIZE_VAR(buffer,loffset,linkCount,int);
        
    attrList.reserve(attrCount);
    packList.reserve(packCount);
//    linkList.reserve(linkCount);

      if (tk == ESMC_TYPEKIND_I4) {
        vip.reserve(items);
        for (i=0; i<items; i++) {
          ESMC_I4 vipTemp;
          vipTemp = (*(reinterpret_cast<ESMC_I4*> (buffer+loffset)));
          vip.push_back(vipTemp);
          loffset += sizeof(ESMC_I4);
        }}
      else if (tk == ESMC_TYPEKIND_I8) {
        vlp.reserve(items);
        for (i=0; i<items; i++) {
          ESMC_I8 vlpTemp;
          vlpTemp = (*(reinterpret_cast<ESMC_I8*> (buffer+loffset)));
          vlp.push_back(vlpTemp);
          loffset += sizeof(ESMC_I8);
        }}
      else if (tk == ESMC_TYPEKIND_R4) {
        vfp.reserve(items);
        for (i=0; i<items; i++) {
          ESMC_R4 vfpTemp;
          vfpTemp = (*(reinterpret_cast<ESMC_R4*> (buffer+loffset)));
          vfp.push_back(vfpTemp);
          loffset += sizeof(ESMC_R4);
        }}
      else if (tk == ESMC_TYPEKIND_R8) {
        vdp.reserve(items);
        for (i=0; i<items; i++) {
          ESMC_R8 vdpTemp;
          vdpTemp = (*(reinterpret_cast<ESMC_R8*> (buffer+loffset)));
          vdp.push_back(vdpTemp);
          loffset += sizeof(ESMC_R8);
        }}
      else if (tk == ESMC_TYPEKIND_LOGICAL) {
        vbp.reserve(items);
        for (i=0; i<items; i++) {
          ESMC_Logical vbpTemp;
          vbpTemp = (*(reinterpret_cast<ESMC_Logical*> (buffer+loffset)));
          vbp.push_back(vbpTemp);
          loffset += sizeof(ESMC_Logical);
        }}
      else if (tk == ESMC_TYPEKIND_CHARACTER) {
          vcpp.reserve(items);
          for (i=0; i<items; i++) {
            DESERIALIZE_VAR(buffer,loffset,chars,string::size_type);
            string vcppTemp((buffer)+(loffset),chars);
            loffset += chars;
            vcpp.push_back(vcppTemp);
          }
        }

    // make sure loffset is aligned correctly
    nbytes=loffset%8;
    if (nbytes!=0) loffset += 8-nbytes;  

    // Deserialize the {\tt Attribute} hierarchy
    for (i=0; i<attrCount; i++) {
      attr = NULL;
      attr = new Attribute(ESMF_FALSE);
      if (!attr)
        return ESMF_FAILURE;
      attr->setBase(attrBase);
      attr->parent = this;
      localrc = attr->ESMC_Deserialize(buffer,&loffset);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
            &localrc)) return localrc;
      localrc = AttributeSet(attr);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
            &localrc)) return localrc;
    }
      
    for (i=0; i<packCount; i++) {
      attr = NULL;
      attr = new Attribute(ESMF_FALSE);
      if (!attr)
        return ESMF_FAILURE;
      attr->setBase(attrBase);
      attr->parent = this;
      localrc = attr->ESMC_Deserialize(buffer,&loffset);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
            &localrc)) return localrc;
      localrc = AttPackSet(attr);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
            &localrc)) return localrc;
    }
      
    // make sure loffset is aligned correctly
    nbytes=loffset%8;
    if (nbytes!=0) loffset += 8-nbytes;
       
    // output localoffset
    *offset=loffset;

    // Undefine serialization macros, so they don't cause troubles elsewhere
#undef DESERIALIZE_VAR
#undef DESERIALIZE_VARC

   return ESMF_SUCCESS;

 } // end ESMC_Deserialize
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_Serialize"
//BOPI
// !IROUTINE:  ESMC_Serialize - Turn the object information into a byte stream
//
// !INTERFACE:
      int Attribute::ESMC_Serialize(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      char *buffer,          // inout - byte stream to fill
      int *length,           // inout - buf length; realloc'd here if needed
      int *offset,           // inout - original offset, updated to point 
                             //  to first free byte after current obj info
      ESMC_InquireFlag inquireflag) const { // in - inquire flag
// 
// !DESCRIPTION:
//    Turn an {\tt Attribute} into a stream of bytes.
//
//EOPI
    bool cc;
    int localrc;

    // Initialize local return code; assume routine not implemented
    localrc = ESMC_RC_NOT_IMPL;

    int r=*offset%8;
    if (r!=0) *offset += 8-r;  // alignment
    int loffset=*offset;

    cc = false;
    localrc = ESMC_SerializeCC(buffer,length,loffset,cc,inquireflag);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
          &localrc)) return localrc;
    if (inquireflag != ESMF_INQUIREONLY) {
      cc = true;
      localrc = ESMC_SerializeCC(buffer,length,*offset,cc,inquireflag);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
            &localrc)) return localrc;
    } else
      *offset = loffset;

    // return successfully
    return ESMF_SUCCESS;

 } // end ESMC_Serialize
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_SerializeCC"
//BOPI
// !IROUTINE:  ESMC_SerializeCC - Turn the object information into a byte stream
//
// !INTERFACE:
    int Attribute::ESMC_SerializeCC(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
    char *buffer,          // inout - byte stream to fill
    int *length,           // inout - buf length; realloc'd here if needed
    int &offset,           // inout - original offset, updated throughout
    bool cc,               // in - to tell whether in count or copy mode
    ESMC_InquireFlag inquireflag) const { // in - inquire flag
//
// !DESCRIPTION:
//    Turn an {\tt Attribute} into a stream of bytes.
//
//EOPI
    int nbytes;
    int localrc;
    unsigned int i;
    int offset_in;

    // Define serialization macros
#define SERIALIZE_VAR(cc,bufptr,loff,var,t) \
  if (cc) *(reinterpret_cast<t*> ((bufptr)+(loff)))=var;    \
  loff += (sizeof(t));   

#define SERIALIZE_VARC(cc,bufptr,loff,var,s) \
  if (cc) strncpy((bufptr)+(loff),(var).c_str(),s);      \
  loff += s;

    // initialize offset
    offset_in = offset;

    // Initialize local return code; assume routine not implemented
    localrc = ESMC_RC_NOT_IMPL;

    SERIALIZE_VAR(cc,buffer,offset,(attrName.size()),string::size_type);
    SERIALIZE_VARC(cc,buffer,offset,attrName,(attrName.size()));

    SERIALIZE_VAR(cc,buffer,offset,tk,ESMC_TypeKind_Flag);

    SERIALIZE_VAR(cc,buffer,offset,items,int);
    SERIALIZE_VAR(cc,buffer,offset,attrRoot,ESMC_Logical);

    SERIALIZE_VAR(cc,buffer,offset,(attrConvention.size()),string::size_type);
    SERIALIZE_VARC(cc,buffer,offset,attrConvention,(attrConvention.size()));
    SERIALIZE_VAR(cc,buffer,offset,(attrPurpose.size()),string::size_type);
    SERIALIZE_VARC(cc,buffer,offset,attrPurpose,(attrPurpose.size()));
    SERIALIZE_VAR(cc,buffer,offset,(attrObject.size()),string::size_type);
    SERIALIZE_VARC(cc,buffer,offset,attrObject,(attrObject.size()));

    SERIALIZE_VAR(cc,buffer,offset,attrPack,ESMC_Logical);
    SERIALIZE_VAR(cc,buffer,offset,attrPackHead,ESMC_Logical);
    SERIALIZE_VAR(cc,buffer,offset,attrNested,ESMC_Logical);

    SERIALIZE_VAR(cc,buffer,offset,attrList.size(),int);
    SERIALIZE_VAR(cc,buffer,offset,packList.size(),int);
//    SERIALIZE_VAR(cc,buffer,offset,linkList.size(),int);

    if (tk == ESMC_TYPEKIND_I4) {
      for (i=0; i<items; i++) {
        SERIALIZE_VAR(cc,buffer,offset,vip[i],ESMC_I4);
      }}
    else if (tk == ESMC_TYPEKIND_I8) {
      for (i=0; i<items; i++) {
        SERIALIZE_VAR(cc,buffer,offset,vlp[i],ESMC_I8);
      }}
    else if (tk == ESMC_TYPEKIND_R4) {
      for (i=0; i<items; i++) {
        SERIALIZE_VAR(cc,buffer,offset,vfp[i],ESMC_R4);
      }}
    else if (tk == ESMC_TYPEKIND_R8) {
      for (i=0; i<items; i++) {
        SERIALIZE_VAR(cc,buffer,offset,vdp[i],ESMC_R8);
      }}
    else if (tk == ESMC_TYPEKIND_LOGICAL) {
      for (i=0; i<items; i++) {
        SERIALIZE_VAR(cc,buffer,offset,vbp[i],ESMC_Logical);
      }}
    else if (tk == ESMC_TYPEKIND_CHARACTER) {
      for (i=0; i<items; i++) {
        SERIALIZE_VAR(cc,buffer,offset,(vcpp[i].size()),string::size_type);
        SERIALIZE_VARC(cc,buffer,offset,vcpp[i],(vcpp[i].size()));
      }
    }

    // make sure offset is aligned correctly
    nbytes=offset%8;
    if (nbytes!=0) offset += 8-nbytes;
    
    // Serialize the Attribute hierarchy
    for (i=0; i<attrList.size(); i++)
        attrList.at(i)->ESMC_SerializeCC(buffer,length,offset,cc,inquireflag);
  
    for (i=0; i<packList.size(); i++)
        packList.at(i)->ESMC_SerializeCC(buffer,length,offset,cc,inquireflag);
  
/*      for (i=0; i<linkList.size(); i++)
          linkList.at(i)->ESMC_SerializeCC(buffer,length,offset,cc,inquireflag); */
  
    // make sure offset is aligned correctly
    nbytes=offset%8;
    if (nbytes!=0) offset += 8-nbytes;

    // check if buffer has enough free memory, expand?
    if (inquireflag != ESMF_INQUIREONLY) {
      if (*length < offset){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_MEM_ALLOCATE,
          "Buffer too short to add an Attribute hierarchy", ESMC_CONTEXT,
          &localrc);
        return localrc;
      }
    }
      
// Undefine serialization macros, so they don't cause troubles elsewhere
#undef SERIALIZE_VAR
#undef SERIALIZE_VARC

    // return successfully
    return ESMF_SUCCESS;

    } // end ESMC_SerializeCC
//-----------------------------------------------------------------------------

} // namespace ESMCI
