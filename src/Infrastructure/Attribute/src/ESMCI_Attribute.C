// $Id: ESMCI_Attribute.C,v 1.113 2011/06/16 05:56:35 eschwab Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2011, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

#define ESMF_FILENAME "ESMCI_Attribute.C"

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
//
 // associated class definition file and others
#include "ESMCI_Macros.h"
#include "ESMCI_Attribute.h"
#include "ESMCI_IO_XML.h"
#include "ESMCI_Base.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_Time.h"
#include "ESMF_LogMacros.inc"
//#include "ESMCI_VM.h"

#include <sstream>
#include <cstring>

using std::string;
using std::vector;
using std::ostringstream;
using std::transform;

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMCI_Attribute.C,v 1.113 2011/06/16 05:56:35 eschwab Exp $";
//-----------------------------------------------------------------------------

namespace ESMCI {

// initialize static Attribute instance counter
// TODO: inherit from ESMC_Base class
//      -- but circular dependency exists
//         with 'root' in ESMC_Base
int Attribute::count=0;

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
  attpack = AttPackGet(convention, purpose, object, attPackInstanceName);
  if(!attpack) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_NOT_CREATED, 
        "Cannot find the specified Attribute package\n", &localrc);
      return localrc;
  }
  
  // make an Attribute in the new attpack
  attr = new Attribute(name, convention, purpose, object);  
  if (!attr) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_NOT_CREATED,
      "failed initialized an attpack Attribute", &localrc);
    return localrc;
  }
  
  // add the new Attribute to the new attpack
  localrc = attpack->AttributeSet(attr);
  if (localrc != ESMF_SUCCESS) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_NOTSET,
      "failed adding an attpack Attribute", &localrc);
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
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
      "cannot add attpack attribute to non-attpack", &localrc);
    return localrc;
  }

  // make an Attribute in the new attpack
  attr = new Attribute(name, attrConvention, attrPurpose, attrObject);  
  if (!attr) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_NOT_CREATED,
      "failed initialized an attpack Attribute", &localrc);
    return localrc;
  }
  
  // add the new Attribute to this attPack
  localrc = AttributeSet(attr);
  if (localrc != ESMF_SUCCESS) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_NOTSET,
      "failed adding an attpack Attribute", &localrc);
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
      int Attribute::AttPackCreateCustom(
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
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_NOT_CREATED,
      "failed initializing an attpack", &localrc);
    return localrc;
  }

  localrc = AttPackSet(attpack);
  if (localrc != ESMF_SUCCESS) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_NOTSET,
      "failed adding an attpack to an Attribute", &localrc);
    return localrc;
  }
    
  return ESMF_SUCCESS;

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

  // Grid standard Attribute package
  if (object.compare("grid")==0) {
    if ((convention.compare("GridSpec")==0 ||
         convention.compare("ESMF")==0) && purpose.compare("General")==0) {
      localrc = AttPackCreateCustom("GridSpec", "General", object);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            &localrc)) return localrc;
      // TODO: CongruentTiles & GridType will be at the mosaic level,
      //       others at the tile level
      localrc = AttPackAddAttribute("CongruentTiles", "GridSpec", "General", object);
      localrc = AttPackAddAttribute("DimensionOrder", "GridSpec", "General", object);

      // TODO: Area & Coordinatepoles await further spec from Sylvia Murphy & Co.
      //localrc = AttPackAddAttribute("Area", "GridSpec", "General", object);
      //localrc = AttPackAddAttribute("CoordinatePoles", "GridSpec", "General", object);

      localrc = AttPackAddAttribute("DiscretizationType", "GridSpec", "General", object);
      localrc = AttPackAddAttribute("GeometryType", "GridSpec", "General", object);
      localrc = AttPackAddAttribute("GridType", "GridSpec", "General", object);
      localrc = AttPackAddAttribute("HorizontalResolution", "GridSpec", "General", object);
      localrc = AttPackAddAttribute("IsConformal", "GridSpec", "General", object);
      localrc = AttPackAddAttribute("IsRegular", "GridSpec", "General", object);
      localrc = AttPackAddAttribute("IsUniform", "GridSpec", "General", object);
      localrc = AttPackAddAttribute("NorthPoleLocation", "GridSpec", "General", object);
      localrc = AttPackAddAttribute("NumberOfCells", "GridSpec", "General", object);
      localrc = AttPackAddAttribute("NumberOfGridTiles", "GridSpec", "General", object);
      localrc = AttPackAddAttribute("NX", "GridSpec", "General", object);
      localrc = AttPackAddAttribute("NY", "GridSpec", "General", object);
    }
    if (convention.compare("ESMF")==0 && purpose.compare("General")==0) {
      localrc = AttPackNest("ESMF", "General", object, "GridSpec", "General");
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            &localrc)) return localrc;
      localrc = AttPackAddAttribute("RegDecompX", "ESMF", "General", object);
      localrc = AttPackAddAttribute("RegDecompY", "ESMF", "General", object);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            &localrc)) return localrc;
    }

  } else if (object.compare("field")==0 || object.compare("array")==0) {
  // Field standard Attribute package
    if (((convention.compare("CF")==0  ||
          convention.compare("ESG")==0 ||
          convention.compare("ESMF")==0) && purpose.compare("General")==0) ||
         (convention.compare("CF")==0 && purpose.compare("Extended")==0) ||
         (convention.compare("CIM 1.5")==0 && purpose.compare("Inputs Description")==0))
    {
      localrc = AttPackCreateCustom("CF", "General", object);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            &localrc)) return localrc;
      localrc = AttPackAddAttribute("LongName", "CF", "General",object);
      localrc = AttPackAddAttribute("ShortName", "CF", "General", object);
      localrc = AttPackAddAttribute("Units", "CF", "General", object);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            &localrc)) return localrc;
    }
    if (((convention.compare("ESG")==0 ||
          convention.compare("ESMF")==0) && purpose.compare("General")==0) ||
         (convention.compare("CF")==0    && purpose.compare("Extended")==0) ||
         (convention.compare("CIM 1.5")==0 && purpose.compare("Inputs Description")==0)) {
      localrc = AttPackNest("CF", "Extended", object, "CF", "General");
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            &localrc)) return localrc;
      localrc = AttPackAddAttribute("StandardName", "CF", "Extended", object);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            &localrc)) return localrc;
    }
    if (((convention.compare("ESG")==0 ||
          convention.compare("ESMF")==0) && purpose.compare("General")==0) ||
         (convention.compare("CIM 1.5")==0 && purpose.compare("Inputs Description")==0)) {
      localrc = AttPackNest("ESG", "General", object, "CF", "Extended");
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            &localrc)) return localrc;
      localrc = AttPackAddAttribute("Intent", "ESG", "General", object);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            &localrc)) return localrc;
    }
    if ((convention.compare("ESMF")==0 && purpose.compare("General")==0) ||
        (convention.compare("CIM 1.5")==0 && purpose.compare("Inputs Description")==0)) {
      localrc = AttPackNest("ESMF", "General", object, "ESG", "General");
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            &localrc)) return localrc;
    }
    // CIM inherits (ESMF, General)
    if (convention.compare("CIM 1.5")==0 &&
        purpose.compare("Inputs Description")==0) {
      localrc = AttPackNest("CIM 1.5", "Inputs Description", object,
                            "ESMF", "General");
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            &localrc)) return localrc;
      localrc = AttPackAddAttribute("CouplingPurpose", "CIM 1.5",
                            "Inputs Description", object);
      localrc = AttPackAddAttribute("CouplingSource", "CIM 1.5",
                            "Inputs Description", object);
      localrc = AttPackAddAttribute("CouplingTarget", "CIM 1.5",
                            "Inputs Description", object);
      localrc = AttPackAddAttribute("Frequency", "CIM 1.5",
                            "Inputs Description", object);
      localrc = AttPackAddAttribute("SpatialRegriddingMethod", "CIM 1.5",
                            "Inputs Description", object);
      localrc = AttPackAddAttribute("SpatialRegriddingDimension", "CIM 1.5",
                            "Inputs Description", object);
      localrc = AttPackAddAttribute("Technique", "CIM 1.5",
                            "Inputs Description", object);
      localrc = AttPackAddAttribute("TimeTransformationType", "CIM 1.5",
                            "Inputs Description", object);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            &localrc)) return localrc;
    }
    
  } else if (object.compare("state")==0) {
  // State standard Attribute package
    localrc = AttPackCreateCustom("ESMF", "General", object);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          &localrc)) return localrc;
    localrc = AttPackAddAttribute("Intent", "ESMF", "General", object);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          &localrc)) return localrc;

  } else if (object.compare("comp")==0) {
  // Component standard Attribute packages
    if ((convention.compare("ESG")==0 ||
         convention.compare("ESMF")==0) && purpose.compare("General")==0) {
      localrc = AttPackCreateCustom("ESG", "General", object);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            &localrc)) return localrc;
      localrc = AttPackAddAttribute("Agency", "ESG", "General", object);
      localrc = AttPackAddAttribute("Author", "ESG", "General", object);
      localrc = AttPackAddAttribute("CodingLanguage", "ESG", "General", object);
      localrc = AttPackAddAttribute("ComponentLongName", "ESG", "General", object);
      localrc = AttPackAddAttribute("ComponentShortName", "ESG", "General", object);
      localrc = AttPackAddAttribute("Discipline", "ESG", "General", object);
      localrc = AttPackAddAttribute("Institution", "ESG", "General", object);
      localrc = AttPackAddAttribute("ModelComponentFramework", "ESG", "General", object);
      localrc = AttPackAddAttribute("PhysicalDomain", "ESG", "General", object);
      localrc = AttPackAddAttribute("Version", "ESG", "General", object);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            &localrc)) return localrc;
    }
    if (convention.compare("ESMF")==0 && purpose.compare("General")==0) {
      localrc = AttPackNest("ESMF", "General", object, "ESG", "General");
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            &localrc)) return localrc;
    }
    if (convention.compare("CIM 1.5")==0 &&
        purpose.compare("Model Component Simulation Description")==0) {

      // TODO: uncomment and expand when we have better definition from CIM
      //localrc = AttPackCreateCustom("CIM 1.5",
      //                              "Scientific Property Description", object);
      //if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      //      &localrc)) return localrc;

      localrc = AttPackCreateCustom("CIM 1.5",
                                    "Platform Description", object);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            &localrc)) return localrc;

      localrc = AttPackCreateCustom("ISO 19115",
                                    "Citation Description", object);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            &localrc)) return localrc;

      localrc = AttPackCreateCustom("ISO 19115",
                                    "Responsible Party Description", object);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            &localrc)) return localrc;

      vector<string> nestconv, nestpurp;
      int nestcount = 3;  // TODO: bump to 4 when Scientific Properties enabled
      nestconv.reserve(nestcount);
      nestpurp.reserve(nestcount);
      // TODO: uncomment and expand when we have better definition from CIM
      //nestconv.push_back("CIM 1.5");
      //nestpurp.push_back("Scientific Property Description");
      nestconv.push_back("CIM 1.5");
      nestpurp.push_back("Platform Description");
      nestconv.push_back("ISO 19115");
      nestpurp.push_back("Citation Description");
      nestconv.push_back("ISO 19115");
      nestpurp.push_back("Responsible Party Description");

      localrc = AttPackNest("CIM 1.5",
                            "Model Component Simulation Description", object,
                            nestcount, nestconv, nestpurp);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            &localrc)) return localrc;
 
      //
      // Model Component attributes
      //  1 <modelComponent> in separate CIM document node, also
      //    1 within each <childComponent>
      //
      localrc = AttPackAddAttribute("Description", "CIM 1.5",
                            "Model Component Simulation Description", object);
      localrc = AttPackAddAttribute("LongName", "CIM 1.5",
                            "Model Component Simulation Description", object);
      localrc = AttPackAddAttribute("ModelType", "CIM 1.5",
                            "Model Component Simulation Description", object);
      localrc = AttPackAddAttribute("ReleaseDate", "CIM 1.5",
                            "Model Component Simulation Description", object);
      localrc = AttPackAddAttribute("ShortName", "CIM 1.5",
                            "Model Component Simulation Description", object);
      localrc = AttPackAddAttribute("URL", "CIM 1.5",
                            "Model Component Simulation Description", object);
      localrc = AttPackAddAttribute("Version", "CIM 1.5",
                            "Model Component Simulation Description", object);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            &localrc)) return localrc;

      //
      // Simulation Run attributes
      //  1 <simulationRun> in separate CIM document node
      //
      localrc = AttPackAddAttribute("SimulationDuration", "CIM 1.5",
                            "Model Component Simulation Description", object);
      localrc = AttPackAddAttribute("SimulationLongName", "CIM 1.5",
                            "Model Component Simulation Description", object);
      localrc = AttPackAddAttribute("SimulationMetadataVersion", "CIM 1.5",
                            "Model Component Simulation Description", object);
      localrc = AttPackAddAttribute("SimulationNumberOfProcessingElements",
                                                          "CIM 1.5",
                            "Model Component Simulation Description", object);
      localrc = AttPackAddAttribute("SimulationRationale", "CIM 1.5",
                            "Model Component Simulation Description", object);
      localrc = AttPackAddAttribute("SimulationShortName", "CIM 1.5",
                            "Model Component Simulation Description", object);
      localrc = AttPackAddAttribute("SimulationStartDate", "CIM 1.5",
                            "Model Component Simulation Description", object);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            &localrc)) return localrc;

      //
      // Document Relationship attributes
      //  1 <documentGenealogy> at end of <modelComponent>
      //
      localrc = AttPackAddAttribute("PreviousVersion", "CIM 1.5",
                            "Model Component Simulation Description", object);
      localrc = AttPackAddAttribute("PreviousVersionDescription", "CIM 1.5",
                            "Model Component Simulation Description", object);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            &localrc)) return localrc;

      //
      // Scientific Property attributes
      //  n <componentProperty>s in 1 <componentProperties> in
      //    <modelComponent>
      //
      // TODO: uncomment and expand when we have better definition from CIM
      //localrc = AttPackAddAttribute("ScientificPropertyLongName", "CIM 1.5",
      //                      "Scientific Property Description", object);
      //localrc = AttPackAddAttribute("ScientificPropertyShortName", "CIM 1.5",
      //                      "Scientific Property Description", object);
      //localrc = AttPackAddAttribute("ScientificPropertyValue", "CIM 1.5",
      //                      "Scientific Property Description", object);
      //if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      //      &localrc)) return localrc;

      //
      // Platform attributes
      //  1 <platform> in separate CIM document node
      //    also 1 within <deployment> within <simulationRun> CIM document node
      //
      localrc = AttPackAddAttribute("CompilerName", "CIM 1.5",
                            "Platform Description", object);
      localrc = AttPackAddAttribute("CompilerVersion", "CIM 1.5",
                            "Platform Description", object);
      localrc = AttPackAddAttribute("MachineCoresPerProcessor", "CIM 1.5",
                            "Platform Description", object);
      localrc = AttPackAddAttribute("MachineDescription", "CIM 1.5",
                            "Platform Description", object);
      localrc = AttPackAddAttribute("MachineInterconnectType", "CIM 1.5",
                            "Platform Description", object);
      localrc = AttPackAddAttribute("MachineMaximumProcessors", "CIM 1.5",
                            "Platform Description", object);
      localrc = AttPackAddAttribute("MachineName", "CIM 1.5",
                            "Platform Description", object);
      localrc = AttPackAddAttribute("MachineOperatingSystem", "CIM 1.5",
                            "Platform Description", object);
      localrc = AttPackAddAttribute("MachineProcessorType", "CIM 1.5",
                            "Platform Description", object);
      localrc = AttPackAddAttribute("MachineSystem", "CIM 1.5",
                            "Platform Description", object);
      localrc = AttPackAddAttribute("MachineVendor", "CIM 1.5",
                            "Platform Description", object);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            &localrc)) return localrc;
      //
      // Citation attributes
      //  n <citation>s in <modelComponent>
      //
      localrc = AttPackAddAttribute("Date", "ISO 19115",
                            "Citation Description", object);

      localrc = AttPackAddAttribute("DOI", "ISO 19115",
                            "Citation Description", object);

      localrc = AttPackAddAttribute("LongTitle", "ISO 19115",
                            "Citation Description", object);

      localrc = AttPackAddAttribute("PresentationForm", "ISO 19115",
                            "Citation Description", object);

      localrc = AttPackAddAttribute("ShortTitle", "ISO 19115",
                            "Citation Description", object);

      localrc = AttPackAddAttribute("URL", "ISO 19115",
                            "Citation Description", object);

      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            &localrc)) return localrc;

      //
      // Responsible Party attributes
      //  n <responsibleParty>s in <modelComponent>, others
      //
      string attPackInstanceName;
      localrc = AttPackAddAttribute("Abbreviation", "ISO 19115",
                                      "Responsible Party Description", object);

      localrc = AttPackAddAttribute("EmailAddress", "ISO 19115",
                                      "Responsible Party Description", object);

      localrc = AttPackAddAttribute("Name", "ISO 19115",
                                      "Responsible Party Description", object);

      localrc = AttPackAddAttribute("NameType", "ISO 19115",
                                      "Responsible Party Description", object);

      localrc = AttPackAddAttribute("PhysicalAddress", "ISO 19115",
                                      "Responsible Party Description", object);

      localrc = AttPackAddAttribute("ResponsiblePartyRole", "ISO 19115",
                                      "Responsible Party Description", object);

      localrc = AttPackAddAttribute("URL", "ISO 19115",
                                      "Responsible Party Description", object);

      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            &localrc)) return localrc;
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

  if (convention.compare("CIM 1.5")!=0 ||
      purpose.compare("Model Component Simulation Description")!=0) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
          "non-standard attpack type", &localrc);
        return localrc;
  }

  // create parent standard attpack ...
  stdParent = new Attribute(convention, purpose, object);
  if(!stdParent) {
    // TODO:  more detailed error message including conv,purp,object 
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_NOT_CREATED,
      "could not make the attpack", &localrc);
    return localrc;
  }
  // ... and attach to *this* attribute node
  localrc = AttPackSet(stdParent);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        &localrc)) return localrc;

  // populate parent attpack with standard attributes

  //
  // Model Component attributes
  //  1 <modelComponent> in separate CIM document node, also
  //    1 within each <childComponent>
  //
  localrc = stdParent->AttPackAddAttribute("Description");
  localrc = stdParent->AttPackAddAttribute("LongName");
  localrc = stdParent->AttPackAddAttribute("ModelType");
  localrc = stdParent->AttPackAddAttribute("ReleaseDate");
  localrc = stdParent->AttPackAddAttribute("ShortName");
  localrc = stdParent->AttPackAddAttribute("URL");
  localrc = stdParent->AttPackAddAttribute("Version");
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        &localrc)) return localrc;

  //
  // Simulation Run attributes
  //  1 <simulationRun> in separate CIM document node
  //
  localrc = stdParent->AttPackAddAttribute("SimulationDuration");
  localrc = stdParent->AttPackAddAttribute("SimulationLongName");
  localrc = stdParent->AttPackAddAttribute("SimulationMetadataVersion");
  localrc = stdParent->AttPackAddAttribute("SimulationNumberOfProcessingElements");
  localrc = stdParent->AttPackAddAttribute("SimulationRationale");
  localrc = stdParent->AttPackAddAttribute("SimulationShortName");
  localrc = stdParent->AttPackAddAttribute("SimulationStartDate");
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        &localrc)) return localrc;

  //
  // Document Relationship attributes
  //  1 <documentGenealogy> at end of <modelComponent>
  //
  localrc = stdParent->AttPackAddAttribute("PreviousVersion");
  localrc = stdParent->AttPackAddAttribute("PreviousVersionDescription");
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        &localrc)) return localrc;

  // TODO:  Scientific Property attributes
  //  n <componentProperty>s in 1 <componentProperties> in
  //    <modelComponent>
  //

  // create child standard attpacks, attach to parent attpack

  // create one Platform child attpack
  stdChild = new Attribute("CIM 1.5", "Platform Description", object);
  if(!stdChild) {
    // TODO:  more detailed error message including conv,purp,object 
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_NOT_CREATED,
      "could not make the attpack", &localrc);
    return localrc;
  }
  localrc = stdParent->AttPackSet(stdChild);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
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
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        &localrc)) return localrc;

  // for each standard nested attpack type ...
  nestAttPackInstanceNameCount = 0;
  for(i=0; i<nestCount; i++) {
    if (nestConvention[i].compare("ISO 19115") != 0 ||
        (nestPurpose[i].compare("Citation Description") != 0 &&
         nestPurpose[i].compare("Responsible Party Description") != 0)) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
        "attpack not a standard child of parent", &localrc);
      return localrc;
    }
    // ... create multiple standard child attpacks
    for(j=0; j<nestAttPackInstanceCountList[i]; j++) {
      stdChild = new Attribute(nestConvention[i], nestPurpose[i], object);
      if(!stdChild) {
        // TODO:  more detailed error message including conv,purp,object 
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_NOT_CREATED,
          "could not make the attpack", &localrc);
        return localrc;
      }
      localrc = stdParent->AttPackSet(stdChild);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
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
          nestPurpose[i].compare("Citation Description") == 0) {
        localrc = stdChild->AttPackAddAttribute("Date");
        localrc = stdChild->AttPackAddAttribute("DOI");
        localrc = stdChild->AttPackAddAttribute("LongTitle");
        localrc = stdChild->AttPackAddAttribute("PresentationForm");
        localrc = stdChild->AttPackAddAttribute("ShortTitle");
        localrc = stdChild->AttPackAddAttribute("URL");
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            &localrc)) return localrc;
      }

      //
      // Responsible Party attributes
      //  n <responsibleParty>s in <modelComponent>
      //
      if (nestConvention[i].compare("ISO 19115") == 0 &&
          nestPurpose[i].compare("Responsible Party Description") == 0) {
        localrc = stdChild->AttPackAddAttribute("Abbreviation");
        localrc = stdChild->AttPackAddAttribute("EmailAddress");
        localrc = stdChild->AttPackAddAttribute("Name");
        localrc = stdChild->AttPackAddAttribute("NameType");
        localrc = stdChild->AttPackAddAttribute("PhysicalAddress");
        localrc = stdChild->AttPackAddAttribute("ResponsiblePartyRole");
        localrc = stdChild->AttPackAddAttribute("URL");
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
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
  ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);

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
                             attPackInstanceName);
    if(!nestpack[i]) {
      // TODO:  more detailed error message including conv,purp,object
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_FOUND,
        "could not find the attpack", &localrc);
      return localrc;
    }
    if (i == 0) {
      localParent = nestpack[0]->parent;
    } else {
      if (nestpack[i]->parent != localParent) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
          "parents of nested attpacks not one and the same", &localrc);
        return localrc;
      }
    }
  }
    
  // Make the attpack
  attpack = new Attribute(convention, purpose, object);
  if(!attpack) {
    // TODO:  more detailed error message including conv,purp,object 
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_NOT_CREATED,
      "could not make the attpack", &localrc);
    return localrc;
  }
  
  // Put the attpack onto nestPack's parent
  localrc = localParent->AttPackSet(attpack);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        &localrc)) return localrc;

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
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          &localrc)) return localrc;
  }
  
  if (!done) {
    // TODO:  more detailed error message including conv,purp,object 
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_DELETED,
      "AttPackNest() failed removing the nested Attribute packages", &localrc);
    return localrc;
  }

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
      const string &convention,        // in - Attribute convention to retrieve
      const string &purpose,           // in - Attribute purpose to retrieve
      const string &object,            // in - Attribute object type to retrieve
      const string &attPackInstanceName) const { // in - attPack name
                                       // specifying which one of multiple packs
// !DESCRIPTION:
//    Get an attpack on an {\tt Attribute} given it's convention, 
//    purpose, object type, and optional attPackInstanceName
//
//EOPI

  int i;
  Attribute *ap=NULL;
  
//printf("AttPackGet(): packList.size() = %d\n", packList.size()); fflush(stdout);

  for (i=0; i<packList.size(); i++) {
    ap = packList.at(i);
    // look for the attpack on this Attribute, at this level, and return the
    // first one if any matches there, or the desired attPackInstanceName one
//printf("packList.at(%d)->attrConvention = %s\n", i, ap->attrConvention.c_str());
//printf("packList.at(%d)->attrPurpose = %s\n", i, ap->attrPurpose.c_str());
//printf("packList.at(%d)->attrObject = %s\n", i, ap->attrObject.c_str());
//printf("packList.at(%d)->attrName = %s\n", i, ap->attrName.c_str());
    if (convention.compare(ap->attrConvention) == 0 && 
        purpose.compare(ap->attrPurpose) == 0 &&
        object.compare(ap->attrObject) == 0 &&
        (attPackInstanceName.empty() ||
         (!attPackInstanceName.empty() && 
          attPackInstanceName.compare(ap->attrName) == 0))) {
          return (ap);
    }
  }

  // if not found at this level, recurse through the nested Attribute packages,
  // one level at a time, right-to-left, to find right-most package
//printf("AttPackGet(): going down a level, packList.size()=%d\n", packList.size());
  for (i=packList.size()-1; i >= 0; i--) {
//printf("i=%d\n", i);
    ap = packList.at(i)->AttPackGet(convention, purpose, object,
                                    attPackInstanceName);
    if (ap) return ap;
  }
  
  return ap;

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
      int &attPackInstanceNameCount) const { // out - # of attPack instance names
// !DESCRIPTION:
//    Get the attpack instance names given its convention, 
//    purpose, and object type.  Looks for all the instance names to be 
//    at one level in the tree.
//
//EOPI

  int i;
  Attribute *ap;

  // look for the attpacks on this Attribute, at this level
  attPackInstanceNameCount = 0;
  for (i=0; i<packList.size(); i++) {
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
  for (i=packList.size()-1; i >= 0; i--) {
    packList.at(i)->AttPackGet(convention, purpose, object,
                               attPackInstanceNameList, 
                               attPackInstanceNameCount);
    if (attPackInstanceNameCount > 0) return ESMF_SUCCESS;
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
    
  // recurse through the nested Attribute packages
  for (i=0; i<packList.size(); i++)
      attr = packList.at(i)->AttPackGetAttribute(name);
  
  // look for the Attribute on this attpack
  for (i=0; i<attrList.size(); i++) {
    if (name.compare(attrList.at(i)->attrName) == 0)
      return attrList.at(i);
  }

  return attr;

}  // end AttPackGetAttribute
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
      const string &convention,          // in - Attribute convention
      const string &purpose,             // in - Attribute purpose
      const string &object,              // in - Attribute object type
      const string &attPackInstanceName, // in - attPack name
                                       // specifying which one of multiple packs
      ESMC_Logical *present) const {     // in/out - the present flag
// 
// !DESCRIPTION:
//     Query an Attribute package for an {\tt Attribute} given its name, convention, 
//     purpose, and object type.
//
//EOPI

  unsigned int i;
  Attribute *attr, *attpack;
  
  attr = NULL; attpack = NULL;

  // get the attpack
  attpack = AttPackGet(convention, purpose, object, attPackInstanceName);
  if (!attpack) {
    *present = ESMF_FALSE;
    return ESMF_SUCCESS;
  }
  // get the attr on the attpack
  attr = attpack->AttPackGetAttribute(name);
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
  attpack = AttPackGet(convention, purpose, object, attPackInstanceName);
  if (attpack != NULL) {
    if (attpack->AttPackIsSet(inNestedAttPacks)) return true;
  }

  // package not set on *this* esmf object; if we don't want to look
  // any further down the esmf object tree, we're done
  if (!inObjectTree) return false;

  // otherwise, check all attpacks on objects in this esmf object tree
  for(int i=0; i<linkList.size(); i++) {
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
  for(int i=0; i<attrList.size(); i++) { 
    string name = attrList.at(i)->attrName;
    if (((ap = AttPackGetAttribute(name)) != NULL) &&
         (ap->parent->AttributeIsSet(name))) return true;
  }
  // otherwise check for any set attributes on nested attpacks, if requested
  if (inNestedAttPacks) {
    for(int i=0; i<packList.size(); i++) { 
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
      const string &convention,              // in - convention
      const string &purpose,                 // in - purpose
      const string &object,                  // in - object type to look for
      const string &attPackInstanceName) {   // in - attPack name
                                       // specifying which one of multiple packs
// 
// !DESCRIPTION:
//     Remove an {\tt Attribute} package

//EOPI

  int localrc;
  unsigned int i;
  Attribute *attpack, *attrparent;
  bool done = false;
  
  attpack = NULL; attrparent = NULL;

  // Initialize local return code
  localrc = ESMC_RC_NOT_IMPL;
    
  // get the attpack
  attpack = AttPackGet(convention, purpose, object, attPackInstanceName);
  if(!attpack) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_FOUND, 
      "Cannot find the Attribute package", &localrc);
    return localrc;
  }
  
  // save the parent, remove attpack from it's parent, then delete attpack
  attrparent = attpack->parent;
  for (i=0; i<attrparent->packList.size(); i++) {
    if (attrparent->packList.at(i) == attpack) {
      delete (attrparent->packList.at(i));
      attrparent->packList.erase(attrparent->packList.begin() + i);
      attrparent->structChange = ESMF_TRUE;
      done = true;
      break;
    }
  }
  
  if (!done) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_DELETED,
      "AttPackRemove could not locate the Attribute package", &localrc);
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
      const string &name,                    // in - name
      const string &convention,              // in - convention
      const string &purpose,                 // in - purpose
      const string &object,                  // in - object type to look for
      const string &attPackInstanceName) {   // in - attPack name
                                       // specifying which one of multiple packs
// 
// !DESCRIPTION:
//     Remove an {\tt Attribute} from an {\tt Attribute} package

//EOPI

  int localrc;
  unsigned int i;
  Attribute *attr, *attpack, *attrparent;
  bool done = false;

  attr = NULL; attpack = NULL; attrparent = NULL;

  // Initialize local return code
  localrc = ESMC_RC_NOT_IMPL;
  
  // get the attpack
  attpack = AttPackGet(convention, purpose, object, attPackInstanceName);
  if(!attpack) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_FOUND, 
      "Cannot find the specified Attribute package", &localrc);
    return localrc;
  }
  
  attr = attpack->AttPackGetAttribute(name);
  if(!attr) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_FOUND, 
      "Cannot find the Attribute in this Attribute Package", &localrc);
    return localrc;
  }
    
  attrparent = attr->parent;
  localrc = attrparent->AttributeRemove(name);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        &localrc)) return localrc;
  
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
      const ESMC_TypeKind &tk,       // in - typekind
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
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_FOUND, 
      "Cannot find the specified Attribute package", &localrc);
    return localrc;
  }
  
  // Find the attribute
  attr = attpack->AttPackGetAttribute(name);
  if (!attr) {
    sprintf(msgbuf, 
      "This Attribute package does not have an Attribute named %s\n",
       name.c_str());
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_FOUND, msgbuf, &localrc);
    return localrc;
  }

  // Set the Attribute
  localrc = attr->AttrModifyValue(tk, count, value);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        &localrc)) return localrc;
  
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
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_BAD, 
      "bad Attribute object", &localrc);
    return localrc;
  }

#if 0
// ERS: commented out to allow duplicate attpacks

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
	ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_BAD, 
        "BIG PROBLEM - setting attrPack on a root Attribute", &localrc);
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

    // items > 1, alloc space for a list and do the copy
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
//}

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

  // call local copy on this Attribute 
  localrc = AttributeCopy(source);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        &localrc)) return localrc;

  // copy Attributes and Attribute packages by reference
  for (i=0; i<source.attrList.size(); i++) {
    attr = NULL;
    attr = new Attribute(ESMF_FALSE);
    if (!attr) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_NOT_CREATED, 
        "Failed allocating an Attribute", &localrc);
      return localrc;
    }
    // set new attr to point to its intented destination and recurse
    (attr->attrBase) = (this->attrBase); 
    (attr->parent) = this;
    localrc = attr->AttributeCopyValue(*(source.attrList.at(i)));
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          &localrc)) return localrc;
    localrc = AttributeSet(attr);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          &localrc)) return localrc;
  }
  // copy Attribute packages by reference
  for (i=0; i<source.packList.size(); i++) {
    attr = NULL;
    attr = new Attribute(ESMF_FALSE);
    if (!attr) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_NOT_CREATED, 
        "Failed allocating an Attribute", &localrc);
      return localrc;
    }
    // set new attr to point to its intented destination and recurse
    (attr->attrBase) = (this->attrBase);
    (attr->parent) = this;
    localrc = attr->AttributeCopyValue(*(source.packList.at(i)));
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          &localrc)) return localrc;
    localrc = AttPackSet(attr);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          &localrc)) return localrc;
  }
  // copy Attribute links by value
  for (i=0; i<source.linkList.size(); i++) {
    attr = source.linkList.at(i);
    ESMC_Logical temp_linkChange = ESMF_TRUE;
    localrc = AttributeLink(attr, &temp_linkChange);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          &localrc)) return localrc;
  }

  return ESMF_SUCCESS;

 } // end AttributeCopyHybrid
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeCopyValue"
//BOPI
// !IROUTINE:  AttributeCopyValue - copy {\tt Attributes} between ESMF objects
//
// !INTERFACE:
      int Attribute::AttributeCopyValue(
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
  Attribute *attr;
  
  attr = NULL;
  
  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // call local copy on source
  localrc = AttributeCopy(source);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        &localrc)) return localrc;

  // copy base level Attributes by value
  for (i=0; i<source.attrList.size(); i++) {
    attr = NULL;
    attr = new Attribute(ESMF_FALSE);
    if (!attr) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_NOT_CREATED, 
        "Failed allocating an Attribute", &localrc);
      return localrc;
    }
    // set new attr to point to its intented destination and recurse
    (attr->attrBase) = (this->attrBase);
    (attr->parent) = this;
    localrc = attr->AttributeCopyValue(*(source.attrList.at(i)));
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          &localrc)) return localrc;
    // add newly initialized attr to destination
    localrc = AttributeSet(attr);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          &localrc)) return localrc;
  }
  // copy base level Attributes by value
  for (i=0; i<source.packList.size(); i++) {
    attr = NULL;
    attr = new Attribute(ESMF_FALSE);
    if (!attr) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_NOT_CREATED, 
        "Failed allocating an Attribute", &localrc);
      return localrc;
    }
    // set new attr to point to its intented destination and recurse
    (attr->attrBase) = (this->attrBase);
    (attr->parent) = this;
    localrc = attr->AttributeCopyValue(*(source.packList.at(i)));
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          &localrc)) return localrc;
    // add newly initialized attr to destination
    localrc = AttPackSet(attr);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          &localrc)) return localrc;
  }

  return ESMF_SUCCESS;

 } // end AttributeCopyValue
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
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        &localrc)) return localrc;

  // copy base level Attributes by value
  for (i=0; i<source->attrList.size(); i++) {
    // add each attr to destination
    localrc = AttributeSet(source->attrList.at(0));
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          &localrc)) return localrc;
    // now remove this Attribute from source, this is a swap
    source->attrList.erase(source->attrList.begin());
  }
  // copy base level Attribute packages by value
  for (i=0; i<source->packList.size(); i++) {
    // add each attr to destination
    localrc = AttPackSet(source->packList.at(0));
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          &localrc)) return localrc;
    // now remove this Attribute from source, this is a swap
    source->packList.erase(source->packList.begin());
  }
/*
  // copy the Attribute links by value
  for (i=0; i<source->linkList.size(); i++) {
    // set the links
    localrc = AttributeLink(source->linkList.at(0));
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          &localrc)) return localrc;
    // now remove the link from source, this is a swap
    localrc = source->AttributeLinkRemove(source->linkList.at(0));
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
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
  attpack = AttPackGet(convention, purpose, object, attPackInstanceName);
  if (attpack) {
    numattrs = 0;
    objcount++;
    localrc = attpack->AttributeCountTreeAttpack(objcount, numattrs);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          &localrc)) return localrc;
  }

  // Recurse the hierarchy
  for (i=0; i<linkList.size(); i++) {
    localrc = linkList.at(i)->AttributeCountTree(convention, purpose, object, 
      objcount, numattrs);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
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
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
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
  attpack = AttPackGet(convention, purpose, object, attPackInstanceName);
  if (attpack) {
    index = 0;
    localrc = attpack->AttributeCountTreeLensAttpack(index, attrLens, attrNames);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          &localrc)) return localrc;
  }

  // Recurse the hierarchy
  for (i=0; i<linkList.size(); ++i) {
    localrc = linkList.at(i)->AttributeCountTreeLens(convention, purpose, object, attrLens, attrNames);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
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
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_WRONG, 
          "Attribute package name out of order", &localrc);
        return localrc;
      }
    } else {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD, 
        "Length < 0 = no good", &localrc);
      return localrc;
    }
    // add length
    if (attrList.at(i)->items > 1) {
      ESMC_LogDefault.Write("Write items >1 not yet implemented", ESMC_LOG_INFO);
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
               ESMC_LOG_INFO);
              attrLens[index] = 10;
            }
        }
    }
    ++index;
  }
  
  // Recurse the hierarchy
  for (i=0; i<packList.size(); i++) {
    localrc = packList.at(i)->AttributeCountTreeLensAttpack(index, attrLens, attrNames);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          &localrc)) return localrc;
    }

  return ESMF_SUCCESS;

}  // end AttributeCountTreeLensAttpack
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeGet"
//BOPI
// !IROUTINE:  AttributeGet(int *) - get {\tt Attribute} from an ESMF type
//
// !INTERFACE:
      int Attribute::AttributeGet(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,                    // in - name of Attribute to retrieve
      int *count,                    // out - number of values in list
      vector<ESMC_I4> *value) const {        // out - Attribute value
// 
// !DESCRIPTION:
//    Get the {\tt ESMC_I4} valueList of an {\tt Attribute}.
//
//EOPI

  int localrc;
  Attribute *attr;
  
  attr = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Get the attribute
  attr = AttributeGet(name);
  if (!attr) {
    ESMC_LogDefault.Write(
      "Attribute not found, using default value if present", ESMC_LOG_INFO);
    return ESMF_FAILURE;
  }
  else {
    // simple sanity checks
    if (attr->tk != ESMC_TYPEKIND_I4) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_WRONGTYPE, 
        "Attribute not typekind I4", &localrc);
      return localrc;
    }

    if (count) 
      *count = attr->items;

    if (value) 
      *value = attr->vip;
  }

  return ESMF_SUCCESS;

}  // end AttributeGet(int *)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeGet"
//BOPI
// !IROUTINE:  AttributeGet(ESMC_I8 *) - get {\tt Attribute} from an ESMF type
//
// !INTERFACE:
      int Attribute::AttributeGet(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,                    // in - name of Attribute to retrieve
      int *count,                    // out - number of values in list
      vector<ESMC_I8> *value) const {        // out - Attribute value
// 
// !DESCRIPTION:
//    Get the {\tt ESMC_I8} valueList of an {\tt Attribute}.
//
//EOPI

  int localrc;
  Attribute *attr;

  attr = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Get the attribute
  attr = AttributeGet(name);
  if (!attr) {
    ESMC_LogDefault.Write(
      "Attribute not found, using default value if present", ESMC_LOG_INFO);
    return localrc;
  }
  else {
    // simple sanity checks
    if (attr->tk != ESMC_TYPEKIND_I8) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_WRONGTYPE, 
        "Attribute not typekind I8", &localrc);
      return localrc;
    }

    if (count) 
      *count = attr->items;

    if (value) 
      *value = attr->vlp;
  }

  return ESMF_SUCCESS;

}  // end AttributeGet(ESMC_I8 *)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeGet"
//BOPI
// !IROUTINE:  AttributeGet(ESMC_R4 *) - get {\tt Attribute} from an ESMF type
//
// !INTERFACE:
      int Attribute::AttributeGet(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,                    // in - name of Attribute to retrieve
      int *count,                    // out - number of values in list
      vector<ESMC_R4> *value) const {        // out - Attribute value
// 
// !DESCRIPTION:
//    Get the {\tt ESMC_R4} valueList of an {\tt Attribute}.
//
//EOPI

  int localrc;
  Attribute *attr;

  attr = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Get the attribute
  attr = AttributeGet(name);
  if (!attr) {
    ESMC_LogDefault.Write(
      "Attribute not found, using default value if present", ESMC_LOG_INFO);
    return localrc;
  }
  else {
    // simple sanity checks
    if (attr->tk != ESMC_TYPEKIND_R4) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_WRONGTYPE, 
        "Attribute not typekind R4", &localrc);
      return localrc;
    }

    if (count) 
      *count = attr->items;

    if (value) 
      *value = attr->vfp;
  }

  return ESMF_SUCCESS;

}  // end AttributeGet(ESMC_R4 *)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeGet"
//BOPI
// !IROUTINE:  AttributeGet(ESMC_R8 *) - get {\tt Attribute} from an ESMF type
//
// !INTERFACE:
      int Attribute::AttributeGet(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,                    // in - name of Attribute to retrieve
      int *count,                    // out - number of values in list
      vector<ESMC_R8> *value) const {        // out - Attribute value
// 
// !DESCRIPTION:
//    Get the {\tt ESMC_R8} valueList of an {\tt Attribute}.
//
//EOPI

  int localrc;
  Attribute *attr;

  attr = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL; 
  
  // Get the attribute
  attr = AttributeGet(name);
  if (!attr) {
    ESMC_LogDefault.Write(
      "Attribute not found, using default value if present", ESMC_LOG_INFO);
    return localrc;
  }
  else {
    // simple sanity checks
    if (attr->tk != ESMC_TYPEKIND_R8) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_WRONGTYPE, 
        "Attribute not typekind R8", &localrc);
      return localrc;
    }

    if (count) 
      *count = attr->items;

    if (value) 
      *value = attr->vdp;
  }

  return ESMF_SUCCESS;

}  // end AttributeGet(ESMC_R8 *)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeGet"
//BOPI
// !IROUTINE:  AttributeGet(ESMC_Logical *) - get {\tt Attribute} from an ESMF type
//
// !INTERFACE:
      int Attribute::AttributeGet(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,                    // in - name of Attribute to retrieve
      int *count,                    // out - number of values in list
      vector<ESMC_Logical> *value) const {   // out - Attribute value
// 
// !DESCRIPTION:
//    Get the {\tt ESMC_Logical} valueList of an {\tt Attribute}.
//
//EOPI

  int localrc;
  Attribute *attr;

  attr = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Get the attribute
  attr = AttributeGet(name);
  if (!attr) {
    ESMC_LogDefault.Write(
      "Attribute not found, using default value if present", ESMC_LOG_INFO);
    return localrc;
  }
  else {
    // simple sanity checks
    if (attr->tk != ESMC_TYPEKIND_LOGICAL) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_WRONGTYPE, 
        "Attribute not typekind LOGICAL", &localrc);
      return localrc;
    }

    if (count) 
      *count = attr->items;

    if (value) 
      *value = attr->vbp;
  }

  return ESMF_SUCCESS;

}  // end AttributeGet(ESMC_Logical *)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeGet"
//BOPI
// !IROUTINE:  AttributeGet(charlist) - get {\tt Attribute} from an ESMF type
//
// !INTERFACE:
      int Attribute::AttributeGet(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,            // in - name of Attribute to retrieve
      vector<string> *value) const {   // out - Attribute values
// 
// !DESCRIPTION:
//    Get the value of an {\tt Attribute}.
//
//EOPI

  int localrc;
  Attribute *attr;

  attr = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Get the attribute
  attr = AttributeGet(name);
  if (!attr) {
    ESMC_LogDefault.Write(
      "Attribute not found, using default value if present", ESMC_LOG_INFO);
// took this out because i think it should return success if not an error.. 
//    return localrc;
  }
  else {
    // simple sanity checks
    if (attr->tk == ESMF_NOKIND) {
      ESMC_LogDefault.Write(
        "Attribute not set, will return empty vector", ESMC_LOG_INFO);
    }
    else if (attr->tk != ESMC_TYPEKIND_CHARACTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_WRONGTYPE, 
        "Attribute not typekind CHARACTER", &localrc);
      return localrc;
    }

    *value = attr->vcpp;
  }
  
  return ESMF_SUCCESS;

}  // end AttributeGet(charlist)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeGet"
//BOPI
// !IROUTINE:  AttributeGet(name) - get {\tt Attribute} from an ESMF type
//
// !INTERFACE:
      int Attribute::AttributeGet(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,           // in - name of Attribute to retrieve
      ESMC_TypeKind *tk,            // out - typekind
      int *itemCount) const {       // out - number of values in list
// 
// !DESCRIPTION:
//    Get the {\tt void *} value of an {\tt Attribute} by name.
//
//EOPI

  int localrc;
  Attribute *attr;

  attr = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Get the attribute
  attr = AttributeGet(name);
  if (!attr) {
    ESMC_LogDefault.Write(
      "Attribute not found, using default value if present", ESMC_LOG_INFO);
    return ESMF_FAILURE;
  }
  else {
    if (tk) 
      *tk = attr->tk;

    if (itemCount)
      *itemCount = attr->items; 
  }

  return ESMF_SUCCESS;

}  // end AttributeGet(name)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeGet"
//BOPI
// !IROUTINE:  AttributeGet(num) - get {\tt Attribute} from an ESMF type
//
// !INTERFACE:
      int Attribute::AttributeGet(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      int num,                        // in - number of Attribute to retrieve
      string *name,                   // out - Attribute name
      ESMC_TypeKind *tk,              // out - typekind
      int *itemCount) const {         // out - number of values in list
// 
// !DESCRIPTION:
//    Get the {\tt void *} value of an {\tt Attribute} by number.
//
//EOPI

  int localrc;
  Attribute *attr;

  attr = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // simple sanity checks
  if (num < 0) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD, 
      "Attribute index must be >0", &localrc);
    return localrc;
  }

  // Get the attribute
  attr = AttributeGet(num);
  if (!attr) {
    ESMC_LogDefault.Write(
      "Attribute not found, using default value if present", ESMC_LOG_INFO);
    return ESMF_FAILURE;
  }
  else {
    if (name) 
      *name = attr->attrName;

    if (tk) 
      *tk = attr->tk;

    if (itemCount)
      *itemCount = attr->items;
  }

  return ESMF_SUCCESS;

}  // end AttributeGet(num)
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
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, 
      "Invalid index for AttributeGet(index)", NULL);
    return NULL;
  }

  return attrList.at(number);

}  // end AttributeGet
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeGetParent"
//BOPI
// !IROUTINE:  AttributeGetParent - get {\tt Attribute} parent from an ESMF type
//
// !INTERFACE:
      Attribute *Attribute::AttributeGetParent(
// 
// !RETURN VALUE:
//    {\tt Attribute} parent pointer or NULL on error exit.
// 
// !ARGUMENTS:
      void) const {        // none
// 
// !DESCRIPTION:
//    Get the parent of {\tt Attribute}.
//
//EOPI

  return parent;

}  // end AttributeGetParent
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeGet"
//BOPI
// !IROUTINE:  AttributeGet - get {\tt Attribute} from an ESMF type
//
// !INTERFACE:
      int Attribute::AttributeGet(
// 
// !RETURN VALUE:
//    {\tt Attribute} pointer or NULL on error exit.
// 
// !ARGUMENTS:
      const string &name,               // in - Attribute name to retrieve
      int *lens,                // in - Atttribute char* lengths to retrieve
      int count) const {        // in - number of Attribute lengths to retrieve
// 
// !DESCRIPTION:
//    Get the lengths of the strings in an {\tt Attribute}.
//
//EOPI

  int size, localrc;
  unsigned int i;
  Attribute *attr;

  attr = NULL;

  // look for the Attribute
  attr = AttributeGet(name);
  if (!attr) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_FOUND, 
      "Could not locate the Attribute", &localrc);
    return localrc;
  }
  
  // check whether this Attribute has been set
  if (!AttributeIsSet(name)) {
    // not set -> return in a sensible way
    for (i=0; i<count; i++)
      lens[i] = 0;
    return ESMF_SUCCESS;
  }
  
  // check that this is a char Attribute
  if (attr->tk != ESMC_TYPEKIND_CHARACTER) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_WRONGTYPE, 
      "Attribute is not typekind CHARACTER", &localrc);
    return localrc;
  }
  
  // check that the count is correct
  if (count < 0 || (count > attr->items)) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_ITEMSOFF, 
      "Count argument is incorrect", &localrc);
    return localrc;
  }
  
  // find the lengths of the strings on this Attribute
  if (!(attr->vcpp).empty()) {
  for (i=0; i<count; i++) 
    lens[i] = (attr->vcpp[i]).size();
  } //else if (!(attr->vcp).empty()) lens[0] = (attr->vcp).size();
  else lens[0] = 0;

  return ESMF_SUCCESS;

}  // end AttributeGet
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeGetCount"
//BOPI
// !IROUTINE:  AttributeGetCount - get an the number of {\tt Attributes}
// 
// !INTERFACE:
      int Attribute::AttributeGetCount(
// 
// !RETURN VALUE:
//    number of {\tt Attributes} in this attrList
// 
// !ARGUMENTS:
      void) const {  
// 
// !DESCRIPTION:
//      Returns number of {\tt Attributes} present
//
//EOPI

  return attrList.size();

} // end AttributeGetCount
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeGetCountPack"
//BOPI
// !IROUTINE:  AttributeGetCountPack - get an the number of {\tt Attribute} packages
// 
// !INTERFACE:
      int Attribute::AttributeGetCountPack(
// 
// !RETURN VALUE:
//    number of {\tt Attribute} packages in this attrList
// 
// !ARGUMENTS:
      void) const {  
// 
// !DESCRIPTION:
//      Returns number of {\tt Attribute} packages present
//
//EOPI

  return packList.size();

} // end AttributeGetCountPack
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeGetCountLink"
//BOPI
// !IROUTINE:  AttributeGetCountLink - get the number of {\tt Attribute} links
// 
// !INTERFACE:
      int Attribute::AttributeGetCountLink(
// 
// !RETURN VALUE:
//    number of {\tt Attribute} links in this attrList
// 
// !ARGUMENTS:
      void) const {  
// 
// !DESCRIPTION:
//      Returns number of {\tt Attribute} links present
//
//EOPI

  return linkList.size();

} // end AttributeGetCountLink
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeGetCountTotal"
//BOPI
// !IROUTINE:  AttributeGetCountTotal - get the total number of {\tt Attributes}
// 
// !INTERFACE:
      int Attribute::AttributeGetCountTotal(
// 
// !RETURN VALUE:
//    total number of {\tt Attributes} in this attrList
// 
// !ARGUMENTS:
      void) const {  
// 
// !DESCRIPTION:
//      Returns the total number of {\tt Attributes} present
//
//EOPI

  return attrList.size()+packList.size()+linkList.size();

} // end AttributeGetCountTotal
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeGetItemCount"
//BOPI
// !IROUTINE:  AttributeGetItemCount - get the item count of this {\tt Attribute}
// 
// !INTERFACE:
      int Attribute::AttributeGetItemCount(
// 
// !RETURN VALUE:
//    item count of this {\tt Attribute}
// 
// !ARGUMENTS:
      const string &name) const {       // in - name
// 
// !DESCRIPTION:
//      Returns number of items on this {\tt Attribute}
//
//EOPI

  int localrc;
  Attribute *attr;
  
  attr = NULL;

  attr = AttributeGet(name);
  if (!attr) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_FOUND, 
      "Attribute not found", &localrc);
    return localrc;
  }
  
  return attr->items;

} // end AttributeGetItemCount
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
#define ESMC_METHOD "AttributeIsSet"
//BOPI
// !IROUTINE:  AttributeIsSet - query if an {\tt Attribute} has been set
//
// !INTERFACE:
      bool Attribute::AttributeIsSet(
// 
// !RETURN VALUE:
//    Whether the value has been set
// 
// !ARGUMENTS:
      const string &name) const {            // in - Attribute name
// 
// !DESCRIPTION:
//     Query for whether an {\tt Attribute} has been set, given its name
//
//EOPI

  Attribute *attr = NULL;
  
  attr = AttributeGet(name);
  if (!attr)
    return false;   // not present
  else {
    if (attr->items > 0 && attr->tk != ESMF_NOKIND)
      return true;  // set
  }

  return false;     // not set

}  // end AttributeIsSet
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
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_LINK, 
        "AttributeLink tried to double set a link", &localrc);
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
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_FOUND, 
      "AttributeLink could not find the link to remove", &localrc);
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
      //structChange = ESMF_TRUE;
      done = true;
      break;
    }
  }
  
  if (!done) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_FOUND, 
      "Could not locate the Attribute to remove", &localrc);
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
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_BAD, 
      "Bad Attribute object", &localrc);
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
//    Set the {\tt ESMC_I4} valueList of an {\tt Attribute}.
//
//EOPI

  int localrc;
  Attribute *attr;

  attr = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new Attribute(name, ESMC_TYPEKIND_I4, count, value);  
  if (!attr) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_NOT_CREATED, 
      "Bad Attribute object", &localrc);
    return localrc;
  }
 
  localrc = AttributeSet(attr);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
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
//    Set the {\tt ESMC_I8} valueList of an {\tt Attribute}.
//
//EOPI

  int localrc;
  Attribute *attr;

  attr = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new Attribute(name, ESMC_TYPEKIND_I8, count, value);  
  if (!attr) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_NOT_CREATED, 
      "Bad Attribute object", &localrc);
    return localrc;
  }
 
  localrc = AttributeSet(attr);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
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
//    Set the {\tt ESMC_R4} valueList of an {\tt Attribute}.
//
//EOPI

  int localrc;
  Attribute *attr;

  attr = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new Attribute(name, ESMC_TYPEKIND_R4, count, value);  
  if (!attr) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_NOT_CREATED, 
      "Bad Attribute object", &localrc);
    return localrc;
  }
 
  localrc = AttributeSet(attr);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
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
//    Set the {\tt ESMC_R8} valueList of an {\tt Attribute}.
//
//EOPI

  int localrc;
  Attribute *attr;

  attr = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new Attribute(name, ESMC_TYPEKIND_R8, count, value);  
  if (!attr) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_NOT_CREATED, 
      "Bad Attribute object", &localrc);
    return localrc;
  }
 
  localrc = AttributeSet(attr);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
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
//    Set the {\tt ESMC_Logical} valueList of an {\tt Attribute}.
//
//EOPI

  int localrc;
  Attribute *attr;

  attr = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new Attribute(name, ESMC_TYPEKIND_LOGICAL, count, value);  
  if (!attr) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_NOT_CREATED, 
      "Bad Attribute object", &localrc);
    return localrc;
  }
 
  localrc = AttributeSet(attr);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
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
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_NOT_CREATED, 
      "Bad Attribute object", &localrc);
    return localrc;
  }
 
  localrc = AttributeSet(attr);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
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
      const ESMC_TypeKind &tk,              // in - typekind
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
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          &localrc)) return localrc;
  }
  }
  
  // Recurse the hierarchy
  for (i=0; i<packList.size(); i++) {
    localrc = packList.at(i)->AttributeSetObjsInTree(object,name,tk,count,value);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          &localrc)) return localrc;
  }  

  // Recurse the hierarchy
  for (i=0; i<linkList.size(); i++) {
    localrc = linkList.at(i)->AttributeSetObjsInTree(object,name,tk,count,value);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          &localrc)) return localrc;
  }  

  return ESMF_SUCCESS;

}  // end AttributeSetObjsInTree

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// READ ROUTINES:
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeRead"
//BOPI
// !IROUTINE:  AttributeRead - read Attributes from XML file
//
// !INTERFACE:
      int Attribute::AttributeRead(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      int fileNameLen,              //  in - file name length
      const char* fileName,         //  in - file name
      int schemaFileNameLen,        //  in - schema file name length
      const char* schemaFileName) { //  in - schema file name

//
// !DESCRIPTION:
//    Read the contents of an XML file into an {\tt Attribute} hierarchy.
//    Expected to be called internally.
//
//EOPI

  // Initialize local return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;
  int rc = ESMF_SUCCESS;

  // instantiate IO object; initialize with pointer to this Attribute node, to
  // place file-read attributes into.
  IO_XML *io_xml = ESMCI_IO_XMLCreate(0, NULL, 0, NULL, this, &localrc);
  ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
  if (localrc != ESMF_SUCCESS) rc = localrc;

  // read the XML file, placing contents into this Attribute node
  localrc = io_xml->read(fileNameLen, fileName, schemaFileNameLen, schemaFileName);
  ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
  if (localrc != ESMF_SUCCESS) rc = localrc;

  localrc = ESMCI_IO_XMLDestroy(&io_xml);
  ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
  if (localrc != ESMF_SUCCESS) rc = localrc;

  return rc;

 } // end AttributeRead

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// WRITE ROUTINES:
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeWriteTab"
//BOPI
// !IROUTINE:  AttributeWriteTab - write Attributes in Tab delimited format
//
// !INTERFACE:
      int Attribute::AttributeWriteTab(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      const string &convention,             //  in - convention
      const string &purpose,                //  in - purpose
      const string &object,                 //  in - object
      const string &varobj,                 //  in - variable object
      const string &basename) const{        //  in - basename
//
// !DESCRIPTION:
//    Write the contents on an {\tt Attribute} hierarchy in Tab delimited format.  
//    Expected to be called internally.
//
//EOPI

  FILE* tab;
  char msgbuf[4*ESMF_MAXSTR];
  int localrc;
  int rows, columns, index;
  unsigned int i;
  int *attrLens;
  vector<string> attrNames;
  
  rows = 0; columns = 0; index = 0;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  
  // Open a text file for writing
  sprintf(msgbuf,"%s.stdout",basename.c_str());
  if((tab=fopen(msgbuf,"w"))==NULL) {
    localrc = ESMF_FAILURE;
    sprintf(msgbuf,"Could not open the write file!");
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, 
                             msgbuf, &localrc);
    return ESMF_FAILURE;
  } 

  // determine the number of fields to write
  localrc = AttributeCountTree(convention, purpose, varobj, rows, columns);
  if (localrc != ESMF_SUCCESS) {
    sprintf(msgbuf, "AttributeWriteTab failed counting objects");
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &localrc);
    fclose(tab);
    return ESMF_FAILURE;
  }
  
  // allocate the integer array of length maxobjs
  attrLens = new int[columns];
  if (!attrLens) {
    sprintf(msgbuf, "AttributeWriteTab failed allocating attrLens");
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf,  &localrc);
    fclose(tab);
    return ESMF_FAILURE;
  }
  for (i=0; i<columns; i++) attrLens[i] = 0;
  attrNames.reserve(columns);
    
  // make a function to recurse the tree, find the max lengths, and compare names
  localrc = AttributeCountTreeLens(convention, purpose, varobj, attrLens,
    attrNames);
  if (localrc != ESMF_SUCCESS) {
    sprintf(msgbuf, "AttributeWriteTab failed CountTreeLens");
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &localrc);
    delete [] attrLens;
    attrNames.clear();
    fclose(tab);
    return ESMF_FAILURE;
  }

  // write the header
  sprintf(msgbuf, "Name: %s\t  Convention: %s\t  Purpose: %s\t\r\n\n",
    basename.c_str(),convention.c_str(),purpose.c_str());
  fprintf(tab,"%s",msgbuf);
  for (i=0; i<columns; i++) {
    sprintf(msgbuf, "%-*s\t",attrLens[i],attrNames.at(i).c_str());
    fprintf(tab,"%s",msgbuf);
  }
  sprintf(msgbuf, "\r\n");
  fprintf(tab,"%s",msgbuf);
    
  localrc = AttributeWriteTabTraverse(tab,convention,purpose,index,columns,
    attrLens,attrNames);
  if (localrc != ESMF_SUCCESS) {
    sprintf(msgbuf, "Attribute failed recursing in WriteTab");
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &localrc);
    delete [] attrLens;
    attrNames.clear();
    fclose(tab);
    return ESMF_FAILURE;
  }

  // close the file
  delete [] attrLens;
  attrNames.clear();
  fclose(tab);
  
  return ESMF_SUCCESS;

 } // end AttributeWriteTab
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeWriteTabTraverse"
//BOPI
// !IROUTINE:  AttributeWriteTabTraverse - write Attributes in Tab delimited format
//                                             recursive function
//
// !INTERFACE:
      int Attribute::AttributeWriteTabTraverse(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      FILE *tab,                                //  in - file to write
      const string &convention,                 //  in - convention
      const string &purpose,                    //  in - purpose
      int &index,                               //  in - counter
      const int &columns,                       //  in - columns
      int *attrLens,                            //  in - column widths
      const vector<string> &attrNames) const{   //  inout - column headings
//
// !DESCRIPTION:
//    Write the contents on an {\tt Attribute} hierarchy in Tab delimited format.  
//
//EOPI

  char msgbuf[4*ESMF_MAXSTR];
  int localrc;
  unsigned int i;
  Attribute *attpack;

  attpack = NULL;

  // Initialize local return code
  localrc = ESMC_RC_NOT_IMPL;
  
  string attPackInstanceName;
  attpack = AttPackGet(convention, purpose, "field", attPackInstanceName);
  if (attpack) {
    localrc = attpack->AttributeWriteTabBuffer(tab,index,columns,attrLens,attrNames);
    if (localrc != ESMF_SUCCESS) {
      sprintf(msgbuf, "AttributeWriteTabTraverse failed AttributeWriteTabBuffer");
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &localrc);
      fclose(tab);
      return ESMF_FAILURE;
    }
  }

  // Recurse the hierarchy
  for (i=0; i<linkList.size(); i++) {
    index = 0;
    localrc = linkList.at(i)->AttributeWriteTabTraverse(tab, convention, 
      purpose, index, columns, attrLens, attrNames);
  }
  
  return ESMF_SUCCESS;

 } // end AttributeWriteTabTraverse
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeWriteTabBuffer"
//BOPI
// !IROUTINE:  AttributeWriteTabBuffer - write Attributes in Tab delimited format
//                                             recursive function
//
// !INTERFACE:
      int Attribute::AttributeWriteTabBuffer(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      FILE *tab,                               //  in - file to write
      int &index,                              //  in - index counter
      const int &columns,                      //  in - columns
      int *attrLens,                           //  in - integer array of attribute lengths
      const vector<string> &attrNames) const{  //  in - attribute names
//
// !DESCRIPTION:
//    Write the contents on an {\tt Attribute} hierarchy in Tab delimited format.  
//    Expected to be called internally.
//
//EOPI

  char msgbuf[4*ESMF_MAXSTR];
  int localrc;
  int tlen;
  unsigned int i;
  
  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  
  for (i=0; i<attrList.size(); i++) {
    if(attrList.at(i)->attrName.compare(attrNames.at(index)) == 0 &&
       attrList.at(i)->attrObject.compare("field") == 0) {
      if (attrLens[index] < attrNames.at(index).size())
        tlen = attrNames.at(index).size()+2;
      else
        tlen = attrLens[index];
      if (attrList.at(i)->items == 0) {
        sprintf(msgbuf, "%-*\t",tlen);
        fprintf(tab,"%s",msgbuf);
      } else if (attrList.at(i)->items == 1) {
        if (attrList.at(i)->tk == ESMC_TYPEKIND_I4)
          sprintf(msgbuf, "%-*d\t",tlen,attrList.at(i)->vip.at(0));
        else if (attrList.at(i)->tk == ESMC_TYPEKIND_I8) 
          sprintf(msgbuf, "%-*lld\t",tlen,attrList.at(i)->vlp.at(0)); 
        else if (attrList.at(i)->tk == ESMC_TYPEKIND_R4) 
          sprintf(msgbuf, "%-*f\t",tlen,attrList.at(i)->vfp.at(0));  
        else if (attrList.at(i)->tk == ESMC_TYPEKIND_R8) 
          sprintf(msgbuf, "%-*g\t",tlen,attrList.at(i)->vdp.at(0));  
        else if (attrList.at(i)->tk == ESMC_TYPEKIND_LOGICAL) {
          if (attrList.at(i)->vbp.at(0) == ESMF_TRUE) 
            sprintf(msgbuf, "%-*s\t",tlen,"true");
          else if (attrList.at(i)->vbp.at(0) == ESMF_FALSE)
            sprintf(msgbuf, "%-*s\t",tlen,"false");
        }
        else if (attrList.at(i)->tk == ESMC_TYPEKIND_CHARACTER)
          sprintf(msgbuf, "%-*s\t",tlen,attrList.at(i)->vcpp.at(0).c_str());
        else
          sprintf(msgbuf, "%-*s\t",tlen,"N/A");
        fprintf(tab,"%s",msgbuf);
      }
      else if (attrList.at(i)->items > 1) { 
        sprintf(msgbuf,"Write items > 1 - Not yet implemented\n");
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &localrc);
        sprintf(msgbuf,"ITEMS>1");
        fprintf(tab,"%s",msgbuf);
      }
     ++index;
      if (index == columns) {
        sprintf(msgbuf, "\r\n");
        fprintf(tab,"%s",msgbuf);
      }
    }
  }

  for(i=0; i<packList.size(); ++i)
    localrc = packList.at(i)->AttributeWriteTabBuffer(tab,index,columns,
      attrLens,attrNames);

  return ESMF_SUCCESS;

 } // end AttributeWriteTabBuffer

#if 0
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeWriteXML"
//BOPI
// !IROUTINE:  AttributeWriteXML - Write contents of an {\tt Attribute} package
//
// !INTERFACE:
      int Attribute::AttributeWriteXML(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      const string &convention,        //  in - convention
      const string &purpose,           //  in - purpose
      const string &object,            //  in - object
      const string &varobj,            //  in - variable object
      const string &basename) {   //  in - basename
//
// !DESCRIPTION:
//    Print the contents of an {\tt Attribute}.  Expected to be
//    called internally.
//
//EOPI

  int localrc = ESMC_RC_NOT_IMPL;
  int rc = ESMF_SUCCESS;

  // instantiate IO object; initialize with pointer to this Attribute node, to
  // write from
  IO_XML *io_xml = ESMCI_IO_XMLCreate(0, NULL, 0, NULL, this, &localrc);
  ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
  if (localrc != ESMF_SUCCESS) rc = localrc;

  // write the XML file, from this Attribute node
  // TODO:  single call to IO_XML::write(); assumes centralized attribute tree
  //        traversal method, used from IO_XML::write().
  localrc = io_xml->write(fileNameLen, fileName);
  ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
  if (localrc != ESMF_SUCCESS) rc = localrc;

  localrc = ESMCI_IO_XMLDestroy(&io_xml);
  ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
  if (localrc != ESMF_SUCCESS) rc = localrc;

  return rc;

 } // end AttributeWriteXML
#endif

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeWriteXML"
//BOPI
// !IROUTINE:  AttributeWriteXML - Write contents of an {\tt Attribute} package
//
// !INTERFACE:
      int Attribute::AttributeWriteXML(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      const string &convention,        //  in - convention
      const string &purpose,           //  in - purpose
      const string &object,            //  in - object
      const string &varobj,            //  in - variable object
      const string &basename) const {  //  in - basename
//
// !DESCRIPTION:
//    Print the contents of an {\tt Attribute}.  Expected to be
//    called internally.
//
//EOPI

// TODO:  Rewrite this method (non-recursive) to be just a switcher to other
//        methods (recursive) based on file standard type (separate methods
//        for ESMF, CIM, WaterML, etc).  Each file type requires its own logic
//        for how to traverse the attribute tree (possibly multiple times) and
//        output the file (a form of serialization).

  char msgbuf[4*ESMF_MAXSTR];
  string modelcompname, fullname, version;
  Attribute *attr, *attpack;
  int localrc, rows, columns, fldcount;
  bool fielddone, griddone, compdone;
  ESMC_Logical presentflag;

//printf("in AttributeWriteXML()\n"); fflush(stdout);

  fielddone = false; griddone = false; compdone = false;
  rows = 0; fldcount = 0; columns = 0;
  attr = NULL; attpack = NULL;
  
  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Instantiate IO object to do the actual writing
  string fileName = basename + ".xml";
  IO_XML *io_xml = ESMCI_IO_XMLCreate(0, NULL, fileName.size(),fileName.c_str(),
                              (ESMCI::Attribute*)this, &localrc);
  ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);

  // Write ESMF header
  string comment = "Generated with ESMF Version ";
  comment += ESMF_VERSION_STRING;
  comment += ", http://www.earthsystemmodeling.org";
  localrc = io_xml->writeComment(comment);
  if (localrc == ESMF_RC_LIB_NOT_PRESENT) {
    sprintf(msgbuf, "Xerces C++ library (>= v3.1.0) not present");
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, msgbuf, &localrc);
    localrc = ESMCI_IO_XMLDestroy(&io_xml);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
    return ESMF_RC_LIB_NOT_PRESENT;
  }

  //
  // determine modelcompname, fullname, version for header
  //
  if (object.compare("comp")==0) {
    // get value of attribute 0 or set to N/A if not present
    string attPackInstanceName;
    localrc = AttPackIsPresent("ComponentShortName",convention,purpose,object,
                               attPackInstanceName, &presentflag);
    if (localrc != ESMF_SUCCESS) {
      sprintf(msgbuf, "failed finding an attribute");
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &localrc);
      localrc = ESMCI_IO_XMLDestroy(&io_xml);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
      return ESMF_FAILURE;
    }
    if (presentflag == ESMF_TRUE) {
      string attPackInstanceName;
      attr = (AttPackGet(convention, purpose, object,attPackInstanceName)->AttPackGetAttribute("ComponentShortName"));
      if (attr != NULL) {
        if (attr->vcpp.empty()) modelcompname = "N/A";
        else modelcompname = attr->vcpp.at(0);
      } else {
        sprintf(msgbuf, "failed getting attribute value");
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &localrc);
        localrc = ESMCI_IO_XMLDestroy(&io_xml);
        ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
        return ESMF_FAILURE;
      }
    }
    else {
      modelcompname="N/A";
    }
  
    // get value of attribute 1 or set to N/A if not present
    localrc = AttPackIsPresent("ComponentLongName",convention,purpose,object,
                               attPackInstanceName, &presentflag);
    if (localrc != ESMF_SUCCESS) {
      sprintf(msgbuf, "failed finding an attribute");
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &localrc);
      localrc = ESMCI_IO_XMLDestroy(&io_xml);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
      return ESMF_FAILURE;
    }
    if (presentflag == ESMF_TRUE) {
      string attPackInstanceName;
      attr = (AttPackGet(convention,purpose,object,attPackInstanceName)->AttPackGetAttribute("ComponentLongName"));
      if (attr != NULL) {
        if (attr->vcpp.empty()) fullname = "N/A";
        else fullname = attr->vcpp.at(0);
      } else {
        sprintf(msgbuf, "failed getting attribute value");
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &localrc);
        localrc = ESMCI_IO_XMLDestroy(&io_xml);
        ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
        return ESMF_FAILURE;
      }
    }
    else {
      fullname="N/A";
    }
  
    // get value of attribute 2 or set to N/A if not present
    localrc = AttPackIsPresent("Version",convention,purpose,object,
                               attPackInstanceName, &presentflag);
    if (localrc != ESMF_SUCCESS) {
      sprintf(msgbuf, "failed finding an attribute");
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &localrc);
      localrc = ESMCI_IO_XMLDestroy(&io_xml);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
      return ESMF_FAILURE;
    }
    if (presentflag == ESMF_TRUE) {
      string attPackInstanceName;
      attr = (AttPackGet(convention,purpose,object,attPackInstanceName)->AttPackGetAttribute("Version"));
      if (attr != NULL) {
        if (attr->vcpp.empty()) version = "N/A";
        else version = attr->vcpp.at(0);
      } else {
        sprintf(msgbuf, "failed getting attribute value");
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &localrc);
        localrc = ESMCI_IO_XMLDestroy(&io_xml);
        ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
        return ESMF_FAILURE;
      }
    }
    else {
      version="N/A";
    }
  
  } else if (object.compare("state")==0 ||
             object.compare("fieldbundle")==0 ||
             object.compare("field")==0 || 
             object.compare("arraybundle")==0 ||
             object.compare("array")==0 ||
             object.compare("grid")==0) {
    modelcompname="N/A";
    fullname="N/A";
    version="N/A";
  }
  else {
    sprintf(msgbuf, "AttributeWrite called from an invalid ESMF object");
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &localrc);
    return ESMF_FAILURE;
  }
  
  // TODO: replace this prototype for WaterML TimeSeries
  if (convention.compare("WaterML")==0 &&
      purpose.compare("TimeSeries")==0) {

    // Write the WaterML XML file header
    localrc = io_xml->writeStartElement("timeSeriesResponse", "", 0, 6,
           "xmlns:gml", "http://www.opengis.net/gml",
           "\n  xmlns:xlink", "http://www.w3.org/1999/xlink",
           " xmlns:xsd", "http://www.w3.org/2001/XMLSchema",
           "\n  xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance",
           " xmlns:wtr", "http://www.cuahsi.org/waterML/",
           "\n  xmlns", "http://www.cuahsi.org/waterML/1.0/");
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);

    localrc = io_xml->writeStartElement("queryInfo", "", 1, 0);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
  
    localrc = io_xml->writeElement("creationTime",
                                   "2009-01-08T15:52:17.8495Z", 2, 0);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);

    localrc = io_xml->writeStartElement("criteria", "", 2, 0);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);

    localrc = io_xml->writeElement("locationParam",
                                   "LittleBearRiver:USU-LBR-Paradise", 3, 0);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);

    localrc = io_xml->writeElement("variableParam", "LBR:USU39", 3, 0);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);

    localrc = io_xml->writeStartElement("timeParam", "", 3, 0);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);

    localrc = io_xml->writeElement("beginDateTime", "2008-04-14T13:00:00", 4, 0);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);

    localrc = io_xml->writeElement("endDateTime", "2008-04-15T12:00:00", 4, 0);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);

    localrc = io_xml->writeEndElement("timeParam", 3);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);

    localrc = io_xml->writeEndElement("criteria", 2);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);

    localrc = io_xml->writeElement("note", "OD Web Service", 2, 0);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);

    localrc = io_xml->writeEndElement("queryInfo", 1);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);

    localrc = io_xml->writeStartElement("timeSeries", "", 1, 0);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);

  } else if (!(object.compare("comp")==0 &&
               convention.compare("CIM 1.5")==0 &&
               purpose.compare("Model Component Simulation Description")==0)) {

    // Write the ESMF XML file header
    localrc = io_xml->writeStartElement("model_component", "", 1, 9,
           "name", modelcompname.c_str(),
           "full_name", fullname.c_str(),
           "version", version.c_str(),
           "\n      xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance",
           "\n      xmlns:xlink", "http://www.w3.org/1999/xlink",
           "\n      xmlns:gco", "http://www.isotc211.org/2005/gco",
           "\n      xmlns:gmd", "http://www.isotc211.org/2005/gmd",
           "\n      xmlns", "http://www.earthsystemmodeling.org",
           "\n      xsi:schemaLocation", "http://www.earthsystemmodeling.org file:/esmf_model_component.xsd");
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
#if 0
    // TODO:  write as separate elements instead of atts ?
    localrc = io_xml->writeStartElement("model_component", "", 1, 0);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
    localrc = io_xml->writeElement("name", modelcompname, 2, 0);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
    localrc = io_xml->writeElement("full_name", fullname, 2, 0);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
    localrc = io_xml->writeElement("version", version, 2, 0);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
#endif

  }

  // determine the number of fields to write
  localrc = AttributeCountTree(convention, purpose, varobj, rows, columns);
  if (localrc != ESMF_SUCCESS) {
    sprintf(msgbuf, "Attribute failed counting fields");
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &localrc);
    localrc = ESMCI_IO_XMLDestroy(&io_xml);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
    return ESMF_FAILURE;
  }
 
  // if not called from component, won't need to print that stuff
  if (object.compare("comp")!=0) {
    compdone = true;
  }
 
  // recurse the Attribute hierarchy
  if (object.compare("comp")==0 &&
      convention.compare("CIM 1.5")==0 &&
      purpose.compare("Model Component Simulation Description")==0) {
    localrc = AttributeWriteCIM(io_xml);
  } else {
    // TODO: split out WaterML, ESMF
    //   (AttributeWriteWaterML(), AttributeWriteESMF(),
    //    deprecate AttributeXML()? )
    localrc = AttributeWriteXMLtraverse(io_xml,convention,purpose,columns,
      fielddone,griddone,compdone);
  }
  if (localrc != ESMF_SUCCESS) {
    sprintf(msgbuf, "Attribute failed recursing in WriteXML");
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &localrc);
    localrc = ESMCI_IO_XMLDestroy(&io_xml);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
    return ESMF_FAILURE;
  }

  // TODO: replace this prototype for WaterML TimeSeries
  if (convention.compare("WaterML")==0 && 
      purpose.compare("TimeSeries")==0) {

    // write the WaterML footer
    localrc = io_xml->writeEndElement("timeSeries", 1);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);

    localrc = io_xml->writeEndElement("timeSeriesResponse", 0);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);

  } else if (!(object.compare("comp")==0 &&
               convention.compare("CIM 1.5")==0 &&
               purpose.compare("Model Component Simulation Description")==0)) {

    // write the ESMF XML footer
    localrc = io_xml->writeEndElement("model_component", 1);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
  }
  
  // destroy the io_xml object, which closes the file
  localrc = ESMCI_IO_XMLDestroy(&io_xml);
  ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);

  return localrc;

 } // end AttributeWriteXML
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeWriteXMLtraverse"
//BOPI
// !IROUTINE:  AttributeWriteXMLtraverse - {\tt Attribute} hierarchy traversal write
//
// !INTERFACE:
      int Attribute::AttributeWriteXMLtraverse(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      IO_XML *io_xml,                  //  in - io pointer to write
      const string &convention,        //  in - convention
      const string &purpose,           //  in - purpose
      const int &columns,              //  in - columns
      bool &fielddone,                 //  in - bool for field
      bool &griddone,                  //  in - bool for grid
      bool &compdone) const{           //  in - bool for comp
//
// !DESCRIPTION:
//    Write the contents of an {\tt Attribute}.  Expected to be
//    called internally.
//
//EOPI

  char msgbuf[4*ESMF_MAXSTR];
  int localrc;
  int index;
  unsigned int i;
  Attribute *attpack;
  
  index = 0;
  attpack = NULL;
  
  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // do component write
  if (!compdone) {
// TODO: implement attPackInstanceName search, if possible to have multiple
//       instances of attpack parents?
    string attPackInstanceName;
//printf("XMLtraverse(): looking for 1st attpack\n");
    attpack = AttPackGet(convention, purpose, "comp", attPackInstanceName);
    if (attpack != NULL) {
//    while (attpack != NULL) {
//printf("XMLtraverse(): found attPackInstanceName %s match\n",
//       attPackInstanceName);
      localrc = attpack->AttributeWriteXMLbuffer(io_xml);
      if (localrc != ESMF_SUCCESS) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
          "AttributeWriteXMLtraverse failed AttributeWriteXMLbuffer", &localrc);
        ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
      return ESMF_FAILURE;
      }
      // get next occurence of this attpack, if any
//      ordinal++;
//      attpack = AttPackGet(convention, purpose, "comp", &ordinal);
    }
//printf("XMLtraverse(): here3\n");
    compdone = true;
  }

  // do field write
  if (!fielddone) {
    // TODO: replace this prototype for WaterML TimeSeries
    if (!(convention.compare("WaterML")==0 && 
          purpose.compare("TimeSeries")==0)) {
      // write the field header
      localrc = io_xml->writeStartElement("variable_set", "", 2, 0);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
    }
    // call the field write buffer method
    localrc = AttributeWriteXMLbufferfield(io_xml, convention, purpose, index, columns);
    if (localrc != ESMF_SUCCESS) {
      sprintf(msgbuf, "AttributeWriteXMLtraverse failed AttributeWriteXMLbufferfield");
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
      return ESMF_FAILURE;
    }
    if (!(convention.compare("WaterML")==0 &&
          purpose.compare("TimeSeries")==0)) {
      // write the field footer
      localrc = io_xml->writeEndElement("variable_set", 2);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
    }
    // done with field
    fielddone = true;
  }
  
  // do grid write
  if (!griddone) {
    string attPackInstanceName;
    attpack = AttPackGet(convention, purpose, "grid", attPackInstanceName);
    if (attpack) {
      // write the grid header
      localrc = io_xml->writeStartElement("GridSpec", "", 0, 1, "name", attpack->attrBase->ESMC_BaseGetName());
      localrc = io_xml->writeStartElement("Mosaic", "", 1, 1, "name", attpack->attrBase->ESMC_BaseGetName());
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);

      localrc = attpack->AttributeWriteXMLbuffergrid(io_xml);
      if (localrc != ESMF_SUCCESS) {
        sprintf(msgbuf, "AttributeWriteXMLtraverse failed AttributeWriteXMLbuffergrid");
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &localrc);
        ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
        return ESMF_FAILURE;
      }
      // write the grid footer
      localrc = io_xml->writeEndElement("Mosaic", 1);
      localrc = io_xml->writeEndElement("GridSpec", 0);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
      griddone = true;
      return ESMF_SUCCESS;
    }
  }
  
  // recurse across all linked ESMF objects (e.g. child components, states,
  // fieldBundles, fields, grids, arrays)
  for(i=0; i<linkList.size(); i++)
    localrc = linkList.at(i)->AttributeWriteXMLtraverse(io_xml,convention,purpose,columns,
      fielddone,griddone,compdone);

  return ESMF_SUCCESS;

 } // end AttributeWriteXMLtraverse
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeWriteXMLbuffergrid"
//BOPI
// !IROUTINE:  AttributeWriteXMLbuffergrid - Write contents of an {\tt Attribute} package
//
// !INTERFACE:
      int Attribute::AttributeWriteXMLbuffergrid(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      IO_XML *io_xml) const {   //  in - io pointer to write
//
// !DESCRIPTION:
//    Print the contents of an {\tt Attribute}.  Expected to be
//    called internally.
//
//EOPI

  // TODO:  consolidate with AttributeWriteXMLbuffer() below (no difference?)

  char msgbuf[4*ESMF_MAXSTR];
  int localrc;
  unsigned int i;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

    for (i=0;  i<attrList.size(); ++i) { 
      if (attrList.at(i)->items == 1) {
        string name = attrList.at(i)->attrName; 
        ostringstream outstring;
        switch (attrList.at(i)->tk)
        {
          case ESMC_TYPEKIND_I4:
            outstring << attrList.at(i)->vip.at(0); 
            localrc = io_xml->writeElement(name, outstring.str(), 2, 0);
            ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
            break;

          case ESMC_TYPEKIND_I8:
            outstring << attrList.at(i)->vlp.at(0); 
            localrc = io_xml->writeElement(name, outstring.str(), 2, 0);
            ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
            break;

          case ESMC_TYPEKIND_R4:
            outstring << attrList.at(i)->vfp.at(0); 
            localrc = io_xml->writeElement(name, outstring.str(), 2, 0);
            ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
            break;

          case ESMC_TYPEKIND_R8:
            outstring << attrList.at(i)->vdp.at(0); 
            localrc = io_xml->writeElement(name, outstring.str(), 2, 0);
            ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
            break;

          case ESMC_TYPEKIND_LOGICAL:
            if (attrList.at(i)->vbp.at(0) == ESMF_TRUE) {
              localrc = io_xml->writeElement(name, "true", 2, 0);
              ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
            } else if (attrList.at(i)->vbp.at(0) == ESMF_FALSE) {
              localrc = io_xml->writeElement(name, "false", 2, 0);
              ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
            }
            break;

          case ESMC_TYPEKIND_CHARACTER:
            localrc = io_xml->writeElement(name, attrList.at(i)->vcpp.at(0), 2, 0);
            ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
            break;

          default:
            localrc = io_xml->writeElement(name, "N/A", 2, 0);
            ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
            break;
        }
      } else if (attrList.at(i)->items >1) { 
        sprintf(msgbuf,"Write items > 1 - Not yet implemented\n");
        ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO);
      } else if (attrList.at(i)->items == 0) {
        // do nothing
      } else {
        sprintf(msgbuf,"Items < 1, problem.");
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &localrc);
        return ESMF_FAILURE;
      }
    }

  // recurse through entire attribute tree on this ESMF object
  for(i=0; i<packList.size(); i++)
    localrc = packList.at(i)->AttributeWriteXMLbuffergrid(io_xml);

  return ESMF_SUCCESS;

 } // end AttributeWriteXMLbuffergrid
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeWriteXMLbuffer"
//BOPI
// !IROUTINE:  AttributeWriteXMLbuffer - Write contents of an {\tt Attribute} package
//
// !INTERFACE:
      int Attribute::AttributeWriteXMLbuffer(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      IO_XML *io_xml) const {   //  in - io pointer to write
//
// !DESCRIPTION:
//    Print the contents of an {\tt Attribute}.  Expected to be
//    called internally.
//
//EOPI

  // TODO: consolidate with AttributeWriteXMLbuffergrid() above (no difference?)

  char msgbuf[4*ESMF_MAXSTR];
  int localrc;
  unsigned int i;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

    for (i=0; i<attrList.size(); ++i) { 
      if (attrList.at(i)->items == 1) {
        string name = attrList.at(i)->attrName; 
        //localrc = io_xml->writeStartElement(attrPurpose, "", 2, 0);
        //localrc = io_xml->writeStartElement(name+"_set", "", 2, 0);
        //ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
        ostringstream outstring;
        switch (attrList.at(i)->tk)
        {
          case ESMC_TYPEKIND_I4:
            outstring << attrList.at(i)->vip.at(0); 
            localrc = io_xml->writeElement(name, outstring.str(), 3, 0);
            ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
            break;

          case ESMC_TYPEKIND_I8:
            outstring << attrList.at(i)->vlp.at(0); 
            localrc = io_xml->writeElement(name, outstring.str(), 3, 0);
            ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
            break;

          case ESMC_TYPEKIND_R4:
            outstring << attrList.at(i)->vfp.at(0); 
            localrc = io_xml->writeElement(name, outstring.str(), 3, 0);
            ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
            break;

          case ESMC_TYPEKIND_R8:
            outstring << attrList.at(i)->vdp.at(0); 
            localrc = io_xml->writeElement(name, outstring.str(), 3, 0);
            ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
            break;

          case ESMC_TYPEKIND_LOGICAL:
            if (attrList.at(i)->vbp.at(0) == ESMF_TRUE) {
              localrc = io_xml->writeElement(name, "true", 3, 0);
              ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
            } else if (attrList.at(i)->vbp.at(0) == ESMF_FALSE) {
              localrc = io_xml->writeElement(name, "false", 3, 0);
              ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
            }
            break;

          case ESMC_TYPEKIND_CHARACTER:
            localrc = io_xml->writeElement(name, attrList.at(i)->vcpp.at(0), 3, 0);
            ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
            break;

          default:
            localrc = io_xml->writeElement(name, "N/A", 1, 0);
            ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
            break;
      }
      //localrc = io_xml->writeEndElement(attrPurpose, 2);
      //localrc = io_xml->writeEndElement(name+"_set", 2);
      //ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
    } else if (attrList.at(i)->items >1) {
      sprintf(msgbuf,"Write items > 1 - Not yet implemented\n");
      ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO);
    } else if (attrList.at(i)->items == 0) {
      //do nothing
    } else {
      sprintf(msgbuf,"Items < 1, problem.");
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &localrc);
      return ESMF_FAILURE;
    }
  }

  // recurse through entire attribute tree on this ESMF object
  for(i=0; i<packList.size(); i++)
    localrc = packList.at(i)->AttributeWriteXMLbuffer(io_xml);

  return ESMF_SUCCESS;

 } // end AttributeWriteXMLbuffer
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeWriteCIM"
//BOPI
// !IROUTINE:  AttributeWriteCIM - Write contents of a CIM {\tt Attribute} package
//
// !INTERFACE:
      int Attribute::AttributeWriteCIM(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      IO_XML *io_xml) const {   //  in - io pointer to write
//
// !DESCRIPTION:
//    Print the contents of a CIM {\tt Attribute}.  Expected to be
//    called internally.
//
//    The output logic design for the WriteCIM*() methods is to match the CIM
//    release validation requirements, no more, no less.  It imposes the least
//    restrictions on what attributes are required to be set in order to
//    produce a valid CIM XML file.  If a required attribute is not set, a 
//    warning message is logged, and output continues with whatever else can be 
//    output.  When the user tries to validate such output, it will be invalid,
//    but between the output file, its validation errors, and the ESMF warning
//    messages in the log files, the user should be able to determine what
//    attributes to set to make the output valid.  The ESMF Fortran Reference
//    manual for Attribute packages documents what the required attributes are.
//
//EOPI
//-----------------------------------------------------------------------------
  int localrc;

  //
  // Write the CIM 1.5 XML file header
  //
  localrc = io_xml->writeStartElement("CIMDocumentSet", "", 0, 6,
         "xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance",
         "xmlns:xlink", "http://www.w3.org/1999/xlink",
         "xmlns:gco", "http://www.isotc211.org/2005/gco",
         "xmlns:gmd", "http://www.isotc211.org/2005/gmd",
         "xmlns", "http://www.purl.org/org/esmetadata/cim/1.5/schemas",
         "xsi:schemaLocation",
         "http://www.purl.org/org/esmetadata/cim/1.5/schemas/cim.xsd");
  ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
  
  //
  // write CIM document node <modelComponent>
  //
  localrc = AttributeWriteCIMmodelComp(io_xml, 1);
  if (localrc != ESMF_SUCCESS) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                          "Attribute failed recursing in WriteXML", &localrc);
    localrc = ESMCI_IO_XMLDestroy(&io_xml);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
    return ESMF_FAILURE;
  }

  //
  // write CIM document node <simulationRun>
  //
  localrc = AttributeWriteCIMsimRun(io_xml);
  if (localrc != ESMF_SUCCESS) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                          "Attribute failed recursing in WriteXML", &localrc);
    localrc = ESMCI_IO_XMLDestroy(&io_xml);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
    return ESMF_FAILURE;
  }

  //
  // write CIM document node platform>
  //
  localrc = AttributeWriteCIMplatform(io_xml);
  if (localrc != ESMF_SUCCESS) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                          "Attribute failed recursing in WriteXML", &localrc);
    localrc = ESMCI_IO_XMLDestroy(&io_xml);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
    return ESMF_FAILURE;
  }

  //
  // Write the CIM 1.5 XML file footer
  //
  localrc = io_xml->writeEndElement("CIMDocumentSet", 0);
  ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);

  return ESMF_SUCCESS;

} // end AttributeWriteCIM
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeWriteCIMmodelComp"
//BOPI
// !IROUTINE:  AttributeWriteCIMmodelComp - Write contents of a CIM {\tt Attribute} package modelComponent record
//
// !INTERFACE:
      int Attribute::AttributeWriteCIMmodelComp(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      IO_XML *io_xml,      //  in - io pointer to write
      int indent) const {  //  in - starting indent level
//
// !DESCRIPTION:
//    Print the contents of a CIM {\tt Attribute}.  Expected to be
//    called internally.
//
//EOPI

  int localrc;
  Attribute *attpack = NULL;
  static int callCount=0;
  bool inObjectTree, inThisCompTreeOnly, inNestedAttPacks;

  vector<string> valuevector;
  string value;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // distinguish between 1st call and recursive calls
  callCount++;

  string attPackInstanceName;
  attpack = AttPackGet("CIM 1.5", "Model Component Simulation Description",
                       "comp", attPackInstanceName);
  if (attpack == NULL) return ESMF_SUCCESS;  // if package not found, return 

  localrc = io_xml->writeStartElement("modelComponent", "", indent++, 0);
  ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);

  if (attpack->AttributeIsSet("ShortName")) {
    localrc = attpack->AttributeGet("ShortName", &valuevector);
    if (valuevector.size() > 1) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                          "Write items > 1 - Not yet implemented", &localrc);
    return ESMF_FAILURE;}
    value = valuevector.at(0);
    if (attpack->AttributeIsSet("Version")) {
      localrc = attpack->AttributeGet("Version", &valuevector);
      if (valuevector.size() > 1) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                          "Write items > 1 - Not yet implemented", &localrc);
        return ESMF_FAILURE;}
      value += valuevector.at(0); // append Version to ShortName
    }
    localrc = io_xml->writeElement("shortName", value, indent, 0);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
  } else {
    ESMC_LogDefault.Write("Attribute ShortName in standard attribute package "
      "(convention='CIM 1.5', purpose='Model Component Simulation Description')"
      " required to be set, to produce valid CIM XML output.",
      ESMC_LOG_WARN, ESMC_CONTEXT);
  }
  if (attpack->AttributeIsSet("LongName")) {
    localrc = attpack->AttributeGet("LongName", &valuevector);
    if (valuevector.size() > 1) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                          "Write items > 1 - Not yet implemented", &localrc);
    return ESMF_FAILURE;}
    value = valuevector.at(0);
    localrc = io_xml->writeElement("longName", value, indent, 0);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
  }
  if (attpack->AttributeIsSet("Description")) {
    localrc = attpack->AttributeGet("Description", &valuevector);
    if (valuevector.size() > 1) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                          "Write items > 1 - Not yet implemented", &localrc);
    return ESMF_FAILURE;}
    value = valuevector.at(0);
    localrc = io_xml->writeElement("description", value, indent, 0);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
  }

  // <componentProperties><componentProperty> nodes
  bool CPgeneral = AttPackIsSet("CIM 1.5", 
                     "General Component Properties Description", "comp",
                     inObjectTree=false, // only look at this comp, not children
                     inThisCompTreeOnly=true, 
                     inNestedAttPacks=false);

  bool CPfield   = AttPackIsSet("ESMF", "General", "field",
                     inObjectTree=true, inThisCompTreeOnly=true, 
                     inNestedAttPacks=true); // only look for CF/Extended
                                             // atts nested within ESMF/General,
                                             // nested within CIM/Inputs.
                                             // TODO: enforce CIM/Inputs as
                                             // top-level attpack (via pathing
                                             // mechanism?)
  if (CPgeneral || CPfield) {
    localrc = io_xml->writeStartElement("componentProperties", "", indent, 0);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);

    if (CPgeneral) {
      localrc = AttributeWriteCIMCPgeneral(io_xml, indent);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
    }
    if (CPfield) {
      localrc = AttributeWriteCIMCPfield(io_xml, indent);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
    }
 
    localrc = io_xml->writeEndElement("componentProperties", indent);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
  }

  // TODO: uncomment and expand when we have better definition from CIM
  //localrc = io_xml->writeElement("numericalProperties", "", indent, 0);
  //ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
  //
  //localrc = io_xml->writeElement("scientificProperties", "", indent, 0);
  //ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);

  // <responsibleParty> nodes
  localrc = attpack->AttributeWriteCIMRP(io_xml, indent);
  ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
 
  if (attpack->AttributeIsSet("ReleaseDate")) {
    localrc = attpack->AttributeGet("ReleaseDate", &valuevector);
    if (valuevector.size() > 1) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                          "Write items > 1 - Not yet implemented", &localrc);
    return ESMF_FAILURE;}
    value = valuevector.at(0);
    localrc = io_xml->writeElement("releaseDate", value, indent, 0);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
  }

  // <citation> nodes
  localrc = attpack->AttributeWriteCIMcitation(io_xml, indent);
  ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
 
  // <onlineResource>
  if (attpack->AttributeIsSet("URL")) {
    localrc = attpack->AttributeGet("URL", &valuevector);
    if (valuevector.size() > 1) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                          "Write items > 1 - Not yet implemented", &localrc);
    return ESMF_FAILURE;}
    value = valuevector.at(0);
    localrc = io_xml->writeStartElement("onlineResource", "", indent, 0); 
    localrc = io_xml->writeStartElement("gmd:linkage", "", ++indent, 0); 
    localrc = io_xml->writeElement("gmd:URL", value, ++indent, 0); 
    localrc = io_xml->writeEndElement("gmd:linkage", --indent); 
    localrc = io_xml->writeEndElement("onlineResource", --indent); 
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
  }

  // <composition><coupling> (all CIM fields within all child components, 
  // written only in top-level component (e.g. coupler))
  if (callCount == 1) { // for top-level component only
    if (AttPackIsSet("CIM 1.5", "Inputs Description", "field", 
                     inObjectTree=true, 
                     inThisCompTreeOnly=false,  // look at all child comps
                     inNestedAttPacks=false)) { // only look at CIM/Inputs atts,
                                                // not nested CF atts
      localrc = io_xml->writeStartElement("composition", "", 2, 0);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);

      localrc = AttributeWriteCIMcomposition(io_xml);

      localrc = io_xml->writeElement("description", "", 3, 0);
      localrc = io_xml->writeEndElement("composition", 2);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
    }
  }

  // <childComponent> tree
  for(int i=0; i<linkList.size(); i++) {
    Attribute *ap;
    for(int j=0; j<linkList.at(i)->packList.size(); j++) {
      ap = linkList.at(i)->packList.at(j);
      if (!(ap->attrConvention.compare("CIM 1.5")==0 &&
       ap->attrPurpose.compare("Model Component Simulation Description")==0 &&
       ap->attrObject.compare("comp")==0)) {
        continue; // skip non-CIM components
      } else {
        // recurse through child CIM components
        localrc = io_xml->writeStartElement("childComponent", "", indent, 0);
        ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
        localrc = linkList.at(i)->AttributeWriteCIMmodelComp(io_xml, ++indent);

        localrc = io_xml->writeEndElement("childComponent", --indent);
        ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
      }
    }
  }

  if (attpack->AttributeIsSet("SimulationNumberOfProcessingElements")) {
    localrc = attpack->AttributeGet("SimulationNumberOfProcessingElements",
                                    &valuevector);
    if (valuevector.size() > 1) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                          "Write items > 1 - Not yet implemented", &localrc);
    return ESMF_FAILURE;}
    value = valuevector.at(0);
    localrc = io_xml->writeStartElement("deployment", "", indent, 0);
    localrc = io_xml->writeStartElement("parallelisation", "", ++indent, 0);
    localrc = io_xml->writeElement("processes", value, ++indent, 0);
    localrc = io_xml->writeEndElement("parallelisation", --indent);
    localrc = io_xml->writeEndElement("deployment", --indent);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
  }

  if (attpack->AttributeIsSet("ModelType")) {
    localrc = attpack->AttributeGet("ModelType", &valuevector);
    if (valuevector.size() > 1) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                          "Write items > 1 - Not yet implemented", &localrc);
    return ESMF_FAILURE;}
    value = valuevector.at(0);
    localrc = io_xml->writeElement("type", "", indent, 2,
                                   "open", "true", 
                                   "value", value.c_str());
  } else {
    ESMC_LogDefault.Write("Attribute ModelType in standard attribute package "
      "(convention='CIM 1.5', purpose='Model Component Simulation Description')"
      " required to be set, to produce valid CIM XML output.",
      ESMC_LOG_WARN, ESMC_CONTEXT);
  }

  // TODO:  auto-generate documentID and documentVersion?
  localrc = io_xml->writeElement("documentID", 
                                 "507a5b52-a91b-11df-a484-00163e9152a5", 
                                 indent, 0);
  localrc = io_xml->writeElement("documentVersion", "1.0", indent, 0);

  // stamp the metadata source as ESMF, version x
  string metadataSource = "ESMF Version ";
  metadataSource += ESMF_VERSION_STRING;
  localrc = io_xml->writeStartElement("documentAuthor", "", indent, 0);
  localrc = io_xml->writeStartElement("gmd:individualName", "", ++indent, 0);
  localrc = io_xml->writeElement("gco:CharacterString", metadataSource.c_str(),                                  ++indent, 0);
  localrc = io_xml->writeEndElement("gmd:individualName", --indent);
  localrc = io_xml->writeStartElement("gmd:role", "", indent, 0);
  localrc = io_xml->writeElement("gmd:CI_RoleCode", "", ++indent, 2,
                                     "codeList", "",
                                     "codeListValue",
                                     "documentAuthor");
  localrc = io_xml->writeEndElement("gmd:role", --indent);
  localrc = io_xml->writeEndElement("documentAuthor", --indent);
  ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);

  // use TimeMgr for timestamping
  // TODO: also use timezone when implemented in TimeMgr
  Time dateTime(0, 0, 1, ESMC_NULL_POINTER, ESMC_CALKIND_GREGORIAN, 0);
  char dateTimeString[ESMF_MAXSTR];
  dateTime.syncToRealTime();
  dateTime.getString(dateTimeString);
  localrc = io_xml->writeElement("documentCreationDate", 
                                 dateTimeString, indent, 0);
  ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);

  // <documentGenealogy>
  if (attpack->AttributeIsSet("PreviousVersionDescription") || 
      attpack->AttributeIsSet("PreviousVersion")) {

    localrc = io_xml->writeStartElement("documentGenealogy", "", indent, 0);
    localrc = io_xml->writeStartElement("relationship", "", ++indent, 0);
    localrc = io_xml->writeStartElement("documentRelationship", "", ++indent, 2,
                                        "direction", "toTarget",
      // direction: {'toTarget', 'fromTarget'}
                                        "type", "laterVersionOf");
      // type: {'similarTo', 'other', 'laterVersionOf', 
      //        'previousVersionOf', 'fixedVersionOf'}

    if (attpack->AttributeIsSet("PreviousVersionDescription")) {
      localrc = attpack->AttributeGet("PreviousVersionDescription", &valuevector);
      if (valuevector.size() > 1) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                            "Write items > 1 - Not yet implemented", &localrc);
      return ESMF_FAILURE;}
      value = valuevector.at(0);
      localrc = io_xml->writeElement("description", value, ++indent, 0);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
    }

    if (attpack->AttributeIsSet("PreviousVersion")) {
      localrc = attpack->AttributeGet("PreviousVersion", &valuevector);
      if (valuevector.size() > 1) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                            "Write items > 1 - Not yet implemented", &localrc);
      return ESMF_FAILURE;}
      value = valuevector.at(0);
      localrc = io_xml->writeStartElement("target", "", indent, 0);
      localrc = io_xml->writeStartElement("reference", "", ++indent, 0);
      localrc = io_xml->writeElement("name", value, ++indent, 0);
      localrc = io_xml->writeEndElement("reference", --indent);
      localrc = io_xml->writeEndElement("target", --indent);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
    } else {
      ESMC_LogDefault.Write("Attribute PreviousVersion in standard attribute "
        "package (convention='CIM 1.5', "
        "purpose='Model Component Simulation Description') "
        "required to be set, when attribute PreviousVersionDescription is also "
        "set, to produce valid CIM XML output.",
        ESMC_LOG_WARN, ESMC_CONTEXT);
    }

    localrc = io_xml->writeEndElement("documentRelationship", --indent);
    localrc = io_xml->writeEndElement("relationship", --indent);
    localrc = io_xml->writeEndElement("documentGenealogy", --indent);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
  }

  localrc = io_xml->writeEndElement("modelComponent", --indent);
  ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);

  return ESMF_SUCCESS;

 } // end AttributeWriteCIMmodelComp
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeWriteCIMsimRun"
//BOPI
// !IROUTINE:  AttributeWriteCIMsimRun - Write contents of a CIM {\tt Attribute} package simulationRun record
//
// !INTERFACE:
      int Attribute::AttributeWriteCIMsimRun(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      IO_XML *io_xml) const {   //  in - io pointer to write
//
// !DESCRIPTION:
//    Print the contents of a CIM {\tt Attribute}.  Expected to be
//    called internally.
//
//EOPI

  int localrc;
  char msgbuf[4*ESMF_MAXSTR];
  Attribute *attpack = NULL;

  vector<string> valuevector;
  string value;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  string attPackInstanceName;
  attpack = AttPackGet("CIM 1.5", "Model Component Simulation Description",
                       "comp", attPackInstanceName);
  if (attpack == NULL) return ESMF_SUCCESS;

  localrc = io_xml->writeStartElement("simulationRun", "", 1, 0);
  ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);

  if (attpack->AttributeIsSet("SimulationRationale")) {
    localrc = attpack->AttributeGet("SimulationRationale", &valuevector);
    if (valuevector.size() > 1) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                          "Write items > 1 - Not yet implemented", &localrc);
    return ESMF_FAILURE;}
    value = valuevector.at(0);
    localrc = io_xml->writeElement("rationale", value, 2, 0);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
  }
  if (attpack->AttributeIsSet("SimulationShortName")) {
    localrc = attpack->AttributeGet("SimulationShortName", &valuevector);
    if (valuevector.size() > 1) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                          "Write items > 1 - Not yet implemented", &localrc);
    return ESMF_FAILURE;}
    value = valuevector.at(0);
    localrc = io_xml->writeElement("shortName", value, 2, 0);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
  } else {
    ESMC_LogDefault.Write("Attribute SimulationShortName in standard attribute "
      "package (convention='CIM 1.5', "
      "purpose='Model Component Simulation Description') "
      "required to be set, to produce valid CIM XML output.",
      ESMC_LOG_WARN, ESMC_CONTEXT);
  }
  if (attpack->AttributeIsSet("SimulationLongName")) {
    localrc = attpack->AttributeGet("SimulationLongName", &valuevector);
    if (valuevector.size() > 1) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                          "Write items > 1 - Not yet implemented", &localrc);
    return ESMF_FAILURE;}
    value = valuevector.at(0);
    localrc = io_xml->writeElement("longName", value, 2, 0);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
  } else {
    ESMC_LogDefault.Write("Attribute SimulationLongName in standard attribute "
      "package (convention='CIM 1.5', "
      "purpose='Model Component Simulation Description') "
      "required to be set, to produce valid CIM XML output.",
      ESMC_LOG_WARN, ESMC_CONTEXT);
  }

  // TODO: required elements in CIM 1.5; need atts defined in package ?
  localrc = io_xml->writeStartElement("supports", "", 2, 0);
  localrc = io_xml->writeElement("reference", "", 3, 0);
  localrc = io_xml->writeEndElement("supports", 2);
  localrc = io_xml->writeStartElement("calendar", "", 2, 0);
  localrc = io_xml->writeElement("realCalendar", "", 3, 0);
  localrc = io_xml->writeEndElement("calendar", 2);
  ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);

  localrc = io_xml->writeStartElement("dateRange", "", 2, 0);
  localrc = io_xml->writeStartElement("openDateRange", "", 3, 0);
  if (attpack->AttributeIsSet("SimulationDuration")) {
    localrc = attpack->AttributeGet("SimulationDuration", &valuevector);
    if (valuevector.size() > 1) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                          "Write items > 1 - Not yet implemented", &localrc);
    return ESMF_FAILURE;}
    value = valuevector.at(0);
    localrc = io_xml->writeElement("duration", value, 4, 0);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
  }
  if (attpack->AttributeIsSet("SimulationStartDate")) {
    localrc = attpack->AttributeGet("SimulationStartDate", &valuevector);
    if (valuevector.size() > 1) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                          "Write items > 1 - Not yet implemented", &localrc);
    return ESMF_FAILURE;}
    value = valuevector.at(0);
    localrc = io_xml->writeElement("startDate", value, 4, 0);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
  }
  localrc = io_xml->writeEndElement("openDateRange", 3);
  localrc = io_xml->writeEndElement("dateRange", 2);
  ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);

  // TODO: required elements in CIM 1.5; need atts defined in package ?
  localrc = io_xml->writeStartElement("model", "", 2, 0);
  localrc = io_xml->writeElement("reference", "", 3, 0);
  localrc = io_xml->writeEndElement("model", 2);
  // TODO:  auto-generate GUID?
  localrc = io_xml->writeElement("documentID", 
                                 "507a5b52-a91b-11df-a484-00163e9152a5", 2, 0);

  if (attpack->AttributeIsSet("SimulationMetadataVersion")) {
    localrc = attpack->AttributeGet("SimulationMetadataVersion", &valuevector);
    if (valuevector.size() > 1) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                          "Write items > 1 - Not yet implemented", &localrc);
    return ESMF_FAILURE;}
    value = valuevector.at(0);
    localrc = io_xml->writeElement("documentVersion", value, 2, 0);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
  } else {
    localrc = io_xml->writeElement("documentVersion", "1.0", 2, 0);
  }

  // use TimeMgr for timestamping
  // TODO: also use timezone when implemented in TimeMgr
  Time dateTime(0, 0, 1, ESMC_NULL_POINTER, ESMC_CALKIND_GREGORIAN, 0);
  char dateTimeString[ESMF_MAXSTR];
  dateTime.syncToRealTime();
  dateTime.getString(dateTimeString);
  localrc = io_xml->writeElement("documentCreationDate", 
                                 dateTimeString, 2, 0);
  ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);

  localrc = io_xml->writeEndElement("simulationRun", 1);
  ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);

  return ESMF_SUCCESS;

 } // end AttributeWriteCIMsimRun
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeWriteCIMplatform"
//BOPI
// !IROUTINE:  AttributeWriteCIMplatform - Write contents of a CIM {\tt Attribute} package platform record
//
// !INTERFACE:
      int Attribute::AttributeWriteCIMplatform(
// // !RETURN VALUE: //    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      IO_XML *io_xml) const {   //  in - io pointer to write
//
// !DESCRIPTION:
//    Print the contents of a CIM {\tt Attribute}.  Expected to be
//    called internally.
//
//EOPI

  int localrc;
  char msgbuf[4*ESMF_MAXSTR];
  Attribute *attpack = NULL;

  vector<string> valuevector, machineNameVector, compilerNameVector;
  string value, machineName, compilerName;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  string attPackInstanceName;
  attpack = AttPackGet("CIM 1.5", "Platform Description", "comp",
                       attPackInstanceName);
  if (attpack == NULL) return ESMF_SUCCESS;

  localrc = io_xml->writeStartElement("platform", "", 1, 0);
  ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);

  if (attpack->AttributeIsSet("MachineName")) {
    localrc = attpack->AttributeGet("MachineName", &machineNameVector);
    if (machineNameVector.size() > 1) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                          "Write items > 1 - Not yet implemented", &localrc);
      return ESMF_FAILURE;}
    machineName = machineNameVector.at(0);
  } else {
    ESMC_LogDefault.Write("Attribute MachineName in "
      "standard attribute package (convention='CIM 1.5', "
      "purpose='Platform Description') "
      "required to be set, to produce valid CIM XML output.",
      ESMC_LOG_WARN, ESMC_CONTEXT);
  }
  if (attpack->AttributeIsSet("CompilerName")) {
    localrc = attpack->AttributeGet("CompilerName", &compilerNameVector);
    if (compilerNameVector.size() > 1) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                          "Write items > 1 - Not yet implemented", &localrc);
      return ESMF_FAILURE;}
    compilerName = compilerNameVector.at(0);
  }

  if (!machineName.empty()) {
    if (compilerName.empty()) {
      localrc = io_xml->writeElement("shortName", machineName, 2, 0);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
    } else {
      localrc = io_xml->writeElement("shortName", machineName + "_" + 
                                                compilerName, 2, 0);
      localrc = io_xml->writeElement("longName", "Machine " + machineName +
                                     " and compiler " + compilerName, 2, 0);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
    }
  }

  if (attpack->AttributeIsSet("MachineDescription")) {
    localrc = attpack->AttributeGet("MachineDescription", &valuevector);
    if (valuevector.size() > 1) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                          "Write items > 1 - Not yet implemented", &localrc);
    return ESMF_FAILURE;}
    value = valuevector.at(0);
    localrc = io_xml->writeElement("description", value, 2, 0);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
  }

  localrc = io_xml->writeStartElement("unit", "", 2, 0);
  localrc = io_xml->writeStartElement("machine", "", 3, 0);
  ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);

  if (!machineName.empty()) {
    localrc = io_xml->writeElement("machineName", machineName, 4, 0);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
  }
  if (attpack->AttributeIsSet("MachineSystem")) {
    localrc = attpack->AttributeGet("MachineSystem", &valuevector);
    if (valuevector.size() > 1) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                          "Write items > 1 - Not yet implemented", &localrc);
    return ESMF_FAILURE;}
    value = valuevector.at(0);
    localrc = io_xml->writeElement("machineSystem", value, 4, 0);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
  }
  if (attpack->AttributeIsSet("MachineOperatingSystem")) {
    localrc = attpack->AttributeGet("MachineOperatingSystem", &valuevector);
    if (valuevector.size() > 1) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                          "Write items > 1 - Not yet implemented", &localrc);
    return ESMF_FAILURE;}
    value = valuevector.at(0);
    localrc = io_xml->writeElement("machineOperatingSystem", "", 4, 2,
                                   "open", "true",
                                   "value", value.c_str());
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
  }
  if (attpack->AttributeIsSet("MachineVendor")) {
    localrc = attpack->AttributeGet("MachineVendor", &valuevector);
    if (valuevector.size() > 1) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                          "Write items > 1 - Not yet implemented", &localrc);
    return ESMF_FAILURE;}
    value = valuevector.at(0);
    localrc = io_xml->writeElement("machineVendor", "", 4, 2,
                                   "open", "true",
                                   "value", value.c_str());
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
  }
  if (attpack->AttributeIsSet("MachineInterconnectType")) {
    localrc = attpack->AttributeGet("MachineInterconnectType", &valuevector);
    if (valuevector.size() > 1) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                          "Write items > 1 - Not yet implemented", &localrc);
    return ESMF_FAILURE;}
    value = valuevector.at(0);
    localrc = io_xml->writeElement("machineInterconnect", "", 4, 2,
                                   "open", "true",
                                   "value", value.c_str());
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
  }
  if (attpack->AttributeIsSet("MachineMaximumProcessors")) {
    localrc = attpack->AttributeGet("MachineMaximumProcessors", &valuevector);
    if (valuevector.size() > 1) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                          "Write items > 1 - Not yet implemented", &localrc);
    return ESMF_FAILURE;}
    value = valuevector.at(0);
    localrc = io_xml->writeElement("machineMaximumProcessors", value, 4, 0);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
  }
  if (attpack->AttributeIsSet("MachineCoresPerProcessor")) {
    localrc = attpack->AttributeGet("MachineCoresPerProcessor", &valuevector);
    if (valuevector.size() > 1) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                          "Write items > 1 - Not yet implemented", &localrc);
    return ESMF_FAILURE;}
    value = valuevector.at(0);
    localrc = io_xml->writeElement("machineCoresPerProcessor", value, 4, 0);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
  }
  if (attpack->AttributeIsSet("MachineProcessorType")) {
    localrc = attpack->AttributeGet("MachineProcessorType", &valuevector);
    if (valuevector.size() > 1) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                          "Write items > 1 - Not yet implemented", &localrc);
    return ESMF_FAILURE;}
    value = valuevector.at(0);
    localrc = io_xml->writeElement("machineProcessorType", "", 4, 2,
                                   "open", "true",
                                   "value", value.c_str());
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
  }

  localrc = io_xml->writeEndElement("machine", 3);
  ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);

  if (attpack->AttributeIsSet("CompilerName") ||
      attpack->AttributeIsSet("CompilerVersion")) {

    localrc = io_xml->writeStartElement("compiler", "", 3, 0);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);

    if (attpack->AttributeIsSet("CompilerName")) {
      localrc = attpack->AttributeGet("CompilerName", &valuevector);
      if (valuevector.size() > 1) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                            "Write items > 1 - Not yet implemented", &localrc);
        return ESMF_FAILURE;}
      value = valuevector.at(0);
      localrc = io_xml->writeElement("compilerName", value, 4, 0);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
    } else {
      ESMC_LogDefault.Write("Attribute CompilerName in "
       "standard attribute package (convention='CIM 1.5', "
       "purpose='Platform Description') "
       "required to be set, when attribute CompilerVersion is also set, "
       "to produce valid CIM XML output.",
        ESMC_LOG_WARN, ESMC_CONTEXT);
    }
    if (attpack->AttributeIsSet("CompilerVersion")) {
      localrc = attpack->AttributeGet("CompilerVersion", &valuevector);
      if (valuevector.size() > 1) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                            "Write items > 1 - Not yet implemented", &localrc);
        return ESMF_FAILURE;}
      value = valuevector.at(0);
      localrc = io_xml->writeElement("compilerVersion", value, 4, 0);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
    } else {
      ESMC_LogDefault.Write("Attribute CompilerVersion in "
       "standard attribute package (convention='CIM 1.5', "
       "purpose='Platform Description') "
       "required to be set, when attribute CompilerName is also set, "
       "to produce valid CIM XML output.",
        ESMC_LOG_WARN, ESMC_CONTEXT);
    }
    localrc = io_xml->writeEndElement("compiler", 3);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
  }

  localrc = io_xml->writeEndElement("unit", 2);
  ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);

  //localrc = io_xml->writeElement("contact", "", 2, 0);
  // TODO:  <contact><gmd:CI_ResponsibleParty>
  //localrc = io_xml->writeStartElement("contact", "", 2, 0);
  //localrc = attpack->AttributeWriteCIMRP(io_xml, 3);
  //localrc = io_xml->writeEndElement("contact", 2);
  //ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);

  // TODO:  auto-generate?
  localrc = io_xml->writeElement("documentID", 
                                 "507a5b52-a91b-11df-a484-00163e9152a5", 2, 0);
  localrc = io_xml->writeElement("documentVersion", "1.0", 2, 0);

  // use TimeMgr for timestamping
  // TODO: also use timezone when implemented in TimeMgr
  Time dateTime(0, 0, 1, ESMC_NULL_POINTER, ESMC_CALKIND_GREGORIAN, 0);
  char dateTimeString[ESMF_MAXSTR];
  dateTime.syncToRealTime();
  dateTime.getString(dateTimeString);
  localrc = io_xml->writeElement("documentCreationDate", 
                                 dateTimeString, 2, 0);
  ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);

  localrc = io_xml->writeEndElement("platform", 1);
  ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);

  return ESMF_SUCCESS;

 } // end AttributeWriteCIMplatform
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeWriteCIMRP"
//BOPI
// !IROUTINE:  AttributeWriteCIMRP - Write contents of a CIM {\tt Attribute} package ResponsibleParty record
//
// !INTERFACE:
      int Attribute::AttributeWriteCIMRP(
// // !RETURN VALUE: //    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      IO_XML *io_xml,      //  in - io pointer to write
      int indent) const {  //  in - starting indent level
//
// !DESCRIPTION:
//    Print the contents of a CIM {\tt Attribute}.  Expected to be
//    called internally.
//
//EOPI

  int localrc;
  Attribute *attpack = NULL;
  bool inNestedAttPacks;

  vector<string> valuevector;
  string value, nameType;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // write out all nested RPs
  for(int i=0; i<packList.size(); i++) {
    attpack = packList.at(i);
    if (!(attpack->attrConvention.compare("ISO 19115")==0 &&
          attpack->attrPurpose.compare("Responsible Party Description")==0))
      continue; // skip non-RPs

    // if no attributes set in this attpack instance, skip it ...
    if (!(attpack->AttPackIsSet(inNestedAttPacks=false))) continue;

    // otherwise, write it out ...

    // responsibleParty header
    localrc = io_xml->writeStartElement("responsibleParty", "", indent++, 0);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
    if (attpack->AttributeIsSet("Name")) {
      // first, determine name type:  individual, organization, or position.
      //   first choice is the setting of the NameType attribute ...
      nameType = "gmd:individualName";  // default
      if (attpack->AttributeIsSet("NameType")) {
          localrc = attpack->AttributeGet("NameType", &valuevector);
        if (valuevector.size() > 1) {
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                            "Write items > 1 - Not yet implemented", &localrc);
          return ESMF_FAILURE;}
        value = valuevector.at(0);
        transform(value.begin(), value.end(), value.begin(), ::tolower);
        if (value == "individual") {
          nameType = "gmd:individualName";
        } else if (value == "organization") {
          nameType = "gmd:organisationName";
        } else if (value == "position") {
          nameType = "gmd:positionName";
        } else {
          ESMC_LogDefault.Write("Attribute NameType in "
            "standard attribute package (convention='ISO 19115', "
            "purpose='Responsible Party Description' should be one of "
            "{Individual, Organization, Position}.",
            ESMC_LOG_WARN, ESMC_CONTEXT);
        }
      // ... otherwise guess based on the role ...
      } else if (attpack->AttributeIsSet("ResponsiblePartyRole")) {
          localrc = attpack->AttributeGet("ResponsiblePartyRole", &valuevector);
        if (valuevector.size() > 1) {
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                            "Write items > 1 - Not yet implemented", &localrc);
          return ESMF_FAILURE;}
        value = valuevector.at(0);
        if (value != "PI") transform(value.begin(), value.end(), value.begin(), ::tolower);
        if (value == "center" || value == "funder") {
          nameType = "gmd:organisationName";
        } else if (value == "PI" || value == "author" || value == "contact") {
          nameType = "gmd:individualName";
        }
      }
      // ... finally output the Name using the name type
      localrc = attpack->AttributeGet("Name", &valuevector);
      if (valuevector.size() > 1) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                      "Write items > 1 - Not yet implemented", &localrc);
        return ESMF_FAILURE;}
      value = valuevector.at(0);
      localrc = io_xml->writeStartElement(nameType, "", indent, 0);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
      localrc = io_xml->writeElement("gco:CharacterString", value, ++indent, 0);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
      localrc = io_xml->writeEndElement(nameType, --indent);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
    }

    if (attpack->AttributeIsSet("PhysicalAddress") ||
        attpack->AttributeIsSet("EmailAddress") ||
        attpack->AttributeIsSet("URL")) {

      // contactInfo header
      localrc = io_xml->writeStartElement("gmd:contactInfo", "", indent, 0);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
      localrc = io_xml->writeStartElement("gmd:CI_Contact", "", ++indent, 0);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);

      if (attpack->AttributeIsSet("PhysicalAddress") ||
          attpack->AttributeIsSet("EmailAddress")) {

        // address header
        localrc = io_xml->writeStartElement("gmd:address", "", ++indent, 0);
        ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
        localrc = io_xml->writeStartElement("gmd:CI_Address", "", ++indent, 0);
        ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
        if (attpack->AttributeIsSet("PhysicalAddress")) {
          localrc = attpack->AttributeGet("PhysicalAddress", &valuevector);
          if (valuevector.size() > 1) {
            ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                              "Write items > 1 - Not yet implemented", &localrc);
            return ESMF_FAILURE;}
          value = valuevector.at(0);
          localrc = io_xml->writeStartElement("gmd:deliveryPoint", "", ++indent, 0);
          ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);

          localrc = io_xml->writeElement("gco:CharacterString", value, ++indent, 0);
          ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
          localrc = io_xml->writeEndElement("gmd:deliveryPoint", --indent);
          ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
        }

        if (attpack->AttributeIsSet("EmailAddress")) {
          localrc = attpack->AttributeGet("EmailAddress", &valuevector);
          if (valuevector.size() > 1) {
            ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                              "Write items > 1 - Not yet implemented", &localrc);
            return ESMF_FAILURE;}
          value = valuevector.at(0);
          localrc = io_xml->writeStartElement("gmd:electronicMailAddress", "", indent, 0);
          ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
          localrc = io_xml->writeElement("gco:CharacterString", value, ++indent, 0);
          ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
          localrc = io_xml->writeEndElement("gmd:electronicMailAddress", --indent);
          ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
        }

        // address footer
        localrc = io_xml->writeEndElement("gmd:CI_Address", --indent);
        ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
        localrc = io_xml->writeEndElement("gmd:address", --indent);
        ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
      } // end if PhysicalAddress or EmailAddress

      if (attpack->AttributeIsSet("URL")) {
        localrc = attpack->AttributeGet("URL", &valuevector);
        if (valuevector.size() > 1) {
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                            "Write items > 1 - Not yet implemented", &localrc);
          return ESMF_FAILURE;}
        value = valuevector.at(0);
        localrc = io_xml->writeStartElement("gmd:onlineResource", "", indent, 0);
        ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
        localrc = io_xml->writeStartElement("gmd:CI_OnlineResource", "", ++indent, 0);
        ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
        localrc = io_xml->writeStartElement("gmd:linkage", "", ++indent, 0);
        ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);

        localrc = io_xml->writeElement("gmd:URL", value, ++indent, 0);
        ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
        localrc = io_xml->writeEndElement("gmd:linkage", --indent);
        ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
        localrc = io_xml->writeEndElement("gmd:CI_OnlineResource", --indent);
        ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
        localrc = io_xml->writeEndElement("gmd:onlineResource", --indent);
        ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
      }

      // contact footer
      localrc = io_xml->writeEndElement("gmd:CI_Contact", --indent);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
      localrc = io_xml->writeEndElement("gmd:contactInfo", --indent);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
    } // end if PhysicalAddress, EmailAddress or URL

    if (attpack->AttributeIsSet("ResponsiblePartyRole")) {
      localrc = attpack->AttributeGet("ResponsiblePartyRole", &valuevector);
      if (valuevector.size() > 1) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                          "Write items > 1 - Not yet implemented", &localrc);
        return ESMF_FAILURE;}
      value = valuevector.at(0);
      if (value != "PI") transform(value.begin(), value.end(), value.begin(), ::tolower);
      if (value == "center") value = "centre";
      localrc = io_xml->writeStartElement("gmd:role", "", indent, 0);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
      localrc = io_xml->writeElement("gmd:CI_RoleCode", "", ++indent, 2,
                                     "codeList", "",
                                     "codeListValue",
                                     value.c_str());
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
      localrc = io_xml->writeEndElement("gmd:role", --indent);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
    } else {
      ESMC_LogDefault.Write("Attribute ResponsiblePartyRole in "
        "standard attribute package (convention='ISO 19115', "
        "purpose='Responsible Party Description') "
        "required to be set, when other attributes in this package are set, "
        "to produce valid CIM XML output.",
        ESMC_LOG_WARN, ESMC_CONTEXT);
    }

    // use "Abbreviation" attribute if set ...
    if (attpack->AttributeIsSet("Abbreviation")) {
      localrc = attpack->AttributeGet("Abbreviation", &valuevector);
      if (valuevector.size() > 1) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                          "Write items > 1 - Not yet implemented", &localrc);
        return ESMF_FAILURE;}
      value = valuevector.at(0);
      localrc = io_xml->writeElement("abbreviation", value, indent, 0);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
    } else if (attpack->AttributeIsSet("Name")) {
      // ... otherwise get initials from "Name"
      localrc = attpack->AttributeGet("Name", &valuevector);
      if (valuevector.size() > 1) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                          "Write items > 1 - Not yet implemented", &localrc);
        return ESMF_FAILURE;}
      value = valuevector.at(0);
      char s[2*ESMF_MAXSTR], abbr[ESMF_MAXSTR], *p;
      int i;
      strcpy(s, value.c_str());
      p = strtok(s, " ");
      for (i=0; p != NULL && i<ESMF_MAXSTR; i++) {
        abbr[i] = p[0];
        p = strtok(NULL, " ");
      }
      abbr[i] = '\0';
      localrc = io_xml->writeElement("abbreviation", abbr, indent, 0);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
    }

    // responsibleParty footer
    localrc = io_xml->writeEndElement("responsibleParty", --indent);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);

  } // end for each nested package

  return ESMF_SUCCESS;

 } // end AttributeWriteCIMRP
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeWriteCIMCPgeneral"
//BOPI
// !IROUTINE:  AttributeWriteCIMCPgeneral - Write contents of a CIM {\tt Attribute} Inputs package as <componentProperties><componentProperty> records within a component. (fields from all components in tree)
//
// !INTERFACE:
      int Attribute::AttributeWriteCIMCPgeneral(
// // !RETURN VALUE: //    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      IO_XML *io_xml,      //  in - io pointer to write
      int indent) const {  //  in - starting indent level
//
// !DESCRIPTION:
//    Print the contents of a CIM {\tt Attribute}.  Expected to be
//    called internally.
//
//EOPI

  int localrc;
  Attribute *attpack = NULL, *ap;

  vector<string> valuevector;
  string value;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  string attPackInstanceName;
  attpack = AttPackGet("CIM 1.5", "General Component Properties Description",
                       "comp", attPackInstanceName);
  if(!attpack) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_NOT_CREATED, 
      "Cannot find the specified Attribute package\n", &localrc);
    return localrc;
  }

  // output all attributes set in this package
  for(int i=0; i<attpack->attrList.size(); i++) { 
    string name = attpack->attrList.at(i)->attrName;
    if (((ap = attpack->AttPackGetAttribute(name)) != NULL) &&
         (ap->parent->AttributeIsSet(name))) {
        localrc = ap->parent->AttributeGet(name, &valuevector);
        if (valuevector.size() > 1) {
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                          "Write items > 1 - Not yet implemented", &localrc);
          return ESMF_FAILURE;}
        value = valuevector.at(0);
        localrc = io_xml->writeStartElement("componentProperty", "", indent+1,
                                            1, "represented", "false");
        ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);

        localrc = io_xml->writeElement("shortName", name, indent+2, 0);
        ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
        localrc = io_xml->writeElement("value", value, indent+2, 0);
        ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
        localrc = io_xml->writeEndElement("componentProperty", indent+1);
        ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
    }
  }

  return ESMF_SUCCESS;

 } // end AttributeWriteCIMCPgeneral
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeWriteCIMCPfield"
//BOPI
// !IROUTINE:  AttributeWriteCIMCPfield - Write contents of a CIM {\tt Attribute} Inputs package as <componentProperties><componentProperty> records within a component. (fields from all components in tree)
//
// !INTERFACE:
      int Attribute::AttributeWriteCIMCPfield(
// // !RETURN VALUE: //    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      IO_XML *io_xml,      //  in - io pointer to write
      int indent) const {  //  in - starting indent level
//
// !DESCRIPTION:
//    Print the contents of a CIM {\tt Attribute}.  Expected to be
//    called internally.
//
//EOPI

  int localrc;
  Attribute *attpack = NULL, *ap;

  vector<string> valuevector;
  string value;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // write out all CIM field properties in this component tree
  for(int i=0; i<linkList.size(); i++) {
    // only consider objects within this component
    if (strcmp(linkList.at(i)->attrBase->ESMC_BaseGetClassName(),
        "Component")==0) continue;

    // recurse until we reach field objects
    if (strcmp(linkList.at(i)->attrBase->ESMC_BaseGetClassName(),"Field")!=0) {
      localrc = linkList.at(i)->AttributeWriteCIMCPfield(io_xml, indent);
      continue;
    }
    // found field object, now look for CIM/Inputs package
    for(int j=0; j<linkList.at(i)->packList.size(); j++) {
      attpack = linkList.at(i)->packList.at(j);
      if (!(attpack->attrConvention.compare("CIM 1.5")==0 &&
            attpack->attrPurpose.compare("Inputs Description")==0 &&
            attpack->attrObject.compare("field")==0)) {
        continue; // skip non-CIM fields and others
      } 

      // skip if no ESMF/General, CF/Extended, or CF/general  attributes set
      if ((((ap = attpack->AttPackGetAttribute("Intent")) != NULL) &&
           (!ap->parent->AttributeIsSet("Intent")) || ap == NULL) &&

          (((ap = attpack->AttPackGetAttribute("ShortName")) != NULL) &&
           (!ap->parent->AttributeIsSet("ShortName")) || ap == NULL) &&

          (((ap = attpack->AttPackGetAttribute("LongName")) != NULL) &&
           (!ap->parent->AttributeIsSet("LongName")) || ap == NULL) &&

          (((ap = attpack->AttPackGetAttribute("Units")) != NULL) &&
           (!ap->parent->AttributeIsSet("Units")) || ap == NULL) &&

          (((ap = attpack->AttPackGetAttribute("StandardName")) != NULL) &&
           (!ap->parent->AttributeIsSet("StandardName")) || ap == NULL)) { 
        continue;
      }

      // found CIM/Inputs package, now write its set attributes
      if (((ap = attpack->AttPackGetAttribute("Intent")) != NULL) &&
           (ap->parent->AttributeIsSet("Intent"))) {
        localrc = ap->parent->AttributeGet("Intent", &valuevector);
        if (valuevector.size() > 1) {
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                          "Write items > 1 - Not yet implemented", &localrc);
          return ESMF_FAILURE;}
        value = valuevector.at(0);
        // map ESMF values {Export,Import} to CIM values {out,in}
        transform(value.begin(), value.end(), value.begin(), ::tolower);
        if (value == "export") {
          value = "out";
        } else if (value == "import") {
          value = "in";
        } else {
          ESMC_LogDefault.Write("Attribute Intent in "
            "standard attribute package (convention='CIM 1.5', "
            "purpose='Inputs Description') must be one of "
            "{Export, Import} to produce valid CIM XML output.",
            ESMC_LOG_WARN, ESMC_CONTEXT);
        }
        localrc = io_xml->writeStartElement("componentProperty", "", indent+1,
                                            2, "intent", value.c_str(), 
                                            "represented", "true");
        ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
      } else {
        localrc = io_xml->writeStartElement("componentProperty", "", indent+1,
                                            1, "represented", "true");
        ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
      }
      if (((ap = attpack->AttPackGetAttribute("ShortName")) != NULL) &&
           (ap->parent->AttributeIsSet("ShortName"))) {
        localrc = ap->parent->AttributeGet("ShortName", &valuevector);
        if (valuevector.size() > 1) {
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                          "Write items > 1 - Not yet implemented", &localrc);
          return ESMF_FAILURE;}
        value = valuevector.at(0);
        localrc = io_xml->writeElement("shortName", value, indent+2, 0);
        ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
      } else {
        ESMC_LogDefault.Write("Attribute ShortName in attpack "
          "CF/General, nested within std attpack (conv='CIM 1.5', "
          "purp='Inputs Description'), required to be set, if other "
          "attributes are set in nested packages CF/General, "
          "CF/Extended, or ESMF/General, to produce valid CIM XML output.",
          ESMC_LOG_WARN, ESMC_CONTEXT);
      }
      if (((ap = attpack->AttPackGetAttribute("LongName")) != NULL) &&
           (ap->parent->AttributeIsSet("LongName"))) {
        localrc = ap->parent->AttributeGet("LongName", &valuevector);
        if (valuevector.size() > 1) {
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                          "Write items > 1 - Not yet implemented", &localrc);
          return ESMF_FAILURE;}
        value = valuevector.at(0);
        localrc = io_xml->writeElement("longName", value, indent+2, 0);
        ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
      }
      if (((ap = attpack->AttPackGetAttribute("Units")) != NULL) &&
           (ap->parent->AttributeIsSet("Units"))) {
        localrc = ap->parent->AttributeGet("Units", &valuevector);
        if (valuevector.size() > 1) {
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                          "Write items > 1 - Not yet implemented", &localrc);
          return ESMF_FAILURE;}
        value = valuevector.at(0);
        localrc = io_xml->writeElement("units", "", indent+2, 2,
                                       "open", "true", 
                                       "value", value.c_str());
        ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
      }
      if (((ap = attpack->AttPackGetAttribute("StandardName")) != NULL) &&
           (ap->parent->AttributeIsSet("StandardName"))) {
        localrc = ap->parent->AttributeGet("StandardName", &valuevector);
        if (valuevector.size() > 1) {
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                          "Write items > 1 - Not yet implemented", &localrc);
          return ESMF_FAILURE;}
        value = valuevector.at(0);
        localrc = io_xml->writeElement("standardName", "", indent+2, 2,
                                       "open", "true", 
                                       "value", value.c_str());
        ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
      }
      localrc = io_xml->writeEndElement("componentProperty", indent+1);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
    }
  }

  return ESMF_SUCCESS;

 } // end AttributeWriteCIMCPfield
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeWritecitation"
//BOPI
// !IROUTINE:  AttributeWriteCIMcitation - Write contents of a CIM {\tt Attribute} package Citation record
//
// !INTERFACE:
      int Attribute::AttributeWriteCIMcitation(
// // !RETURN VALUE: //    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      IO_XML *io_xml,      //  in - io pointer to write
      int indent) const {  //  in - starting indent level
//
// !DESCRIPTION:
//    Print the contents of a CIM {\tt Attribute}.  Expected to be
//    called internally.
//
//EOPI

  int localrc;
  Attribute *attpack = NULL;
  bool inNestedAttPacks;

  vector<string> valuevector;
  string value;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // write out all nested Citations
  for(int i=0; i<packList.size(); i++) {
    attpack = packList.at(i);
    if (!(attpack->attrConvention.compare("ISO 19115")==0 &&
          attpack->attrPurpose.compare("Citation Description")==0))
      continue; // skip non-Citations

    // if no attributes set in this attpack instance, skip it ...
    if (!(attpack->AttPackIsSet(inNestedAttPacks=false))) continue;

    // otherwise, write it out ...

    // citation header
    localrc = io_xml->writeStartElement("citation", "", indent, 0);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);

    if (attpack->AttributeIsSet("ShortTitle")) {
      localrc = attpack->AttributeGet("ShortTitle", &valuevector);
      if (valuevector.size() > 1) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                          "Write items > 1 - Not yet implemented", &localrc);
        return ESMF_FAILURE;}
      value = valuevector.at(0);
      localrc = io_xml->writeStartElement("gmd:title", "", ++indent, 0);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
      localrc = io_xml->writeElement("gco:CharacterString", value, ++indent, 0);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
      localrc = io_xml->writeEndElement("gmd:title", --indent);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
    } else {
      ESMC_LogDefault.Write("Attribute ShortTitle in "
        "standard attribute package (convention='ISO 19115', "
        "purpose='Citation Description') "
        "required to be set, when other attributes in this package are set, "
        "to produce valid CIM XML output.",
        ESMC_LOG_WARN, ESMC_CONTEXT);
    }
    if (attpack->AttributeIsSet("Date")) {
      localrc = attpack->AttributeGet("Date", &valuevector);
      if (valuevector.size() > 1) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                          "Write items > 1 - Not yet implemented", &localrc);
        return ESMF_FAILURE;}
      value = valuevector.at(0);
      localrc = io_xml->writeStartElement("gmd:date", "", indent, 0);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
      localrc = io_xml->writeStartElement("gmd:CI_Date", "", ++indent, 0);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
      localrc = io_xml->writeStartElement("gmd:date", "", ++indent, 0);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
      localrc = io_xml->writeElement("gco:Date", value, ++indent, 0);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
      localrc = io_xml->writeEndElement("gmd:date", --indent);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
      localrc = io_xml->writeStartElement("gmd:dateType", "", indent, 0);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
      localrc = io_xml->writeElement("gmd:CI_DateTypeCode", "", 
                                     ++indent, 2,
                                     "codeList", "",
                                     "codeListValue", "");
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
      localrc = io_xml->writeEndElement("gmd:dateType", --indent);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
      localrc = io_xml->writeEndElement("gmd:CI_Date", --indent);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
      localrc = io_xml->writeEndElement("gmd:date", --indent);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
    } else {
      ESMC_LogDefault.Write("Attribute Date in "
        "standard attribute package (convention='ISO 19115', "
        "purpose='Citation Description') "
        "required to be set, when other attributes in this package are set, "
        "to produce valid CIM XML output.",
        ESMC_LOG_WARN, ESMC_CONTEXT);
    }
    if (attpack->AttributeIsSet("PresentationForm")) {
      localrc = attpack->AttributeGet("PresentationForm", &valuevector);
      if (valuevector.size() > 1) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                          "Write items > 1 - Not yet implemented", &localrc);
        return ESMF_FAILURE;}
      value = valuevector.at(0);
      localrc = io_xml->writeStartElement("gmd:presentationForm", "", indent, 0);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
      localrc = io_xml->writeElement("gmd:CI_PresentationFormCode", value,
                                     ++indent,
                                     2, "codeList", "",
                                     "codeListValue", "");
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
      localrc = io_xml->writeEndElement("gmd:presentationForm", --indent);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
    }
    if (attpack->AttributeIsSet("DOI")) {
      localrc = attpack->AttributeGet("DOI", &valuevector);
      if (valuevector.size() > 1) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                          "Write items > 1 - Not yet implemented", &localrc);
        return ESMF_FAILURE;}
      value = valuevector.at(0);
      localrc = io_xml->writeStartElement("gmd:otherCitationDetails", "", 
                                          indent, 0);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
      localrc = io_xml->writeElement("gco:CharacterString", value, ++indent, 0);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
      localrc = io_xml->writeEndElement("gmd:otherCitationDetails", --indent);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
    }
    if (attpack->AttributeIsSet("LongTitle")) {
      localrc = attpack->AttributeGet("LongTitle", &valuevector);
      if (valuevector.size() > 1) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                          "Write items > 1 - Not yet implemented", &localrc);
        return ESMF_FAILURE;}
      value = valuevector.at(0);
      localrc = io_xml->writeStartElement("gmd:collectiveTitle", "", indent, 0);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
      localrc = io_xml->writeElement("gco:CharacterString", value, ++indent, 0);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
      localrc = io_xml->writeEndElement("gmd:collectiveTitle", --indent);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
    }
    // citation footer
    localrc = io_xml->writeEndElement("citation", --indent);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);

  } // end for each nested package

  return ESMF_SUCCESS;

 } // end AttributeWriteCIMcitation
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeWriteCIMcomposition"
//BOPI
// !IROUTINE:  AttributeWriteCIMcomposition - Write contents of a CIM {\tt Attribute} package composition node (fields from all components in tree)
//
// !INTERFACE:
      int Attribute::AttributeWriteCIMcomposition(
// // !RETURN VALUE: //    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      IO_XML *io_xml) const {      //  in - io pointer to write
//
// !DESCRIPTION:
//    Print the contents of a CIM {\tt Attribute}.  Expected to be
//    called internally.
//
//EOPI

  int localrc;
  char msgbuf[4*ESMF_MAXSTR];
  Attribute *attpack = NULL, *ap;
  bool inNestedAttPacks;

  vector<string> valuevector, value2vector;
  string value, value2;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // write out all CIM fields in component tree
  for(int i=0; i<linkList.size(); i++) {
    for(int j=0; j<linkList.at(i)->packList.size(); j++) {
      attpack = linkList.at(i)->packList.at(j);
      if (!(attpack->attrConvention.compare("CIM 1.5")==0 &&
            attpack->attrPurpose.compare("Inputs Description")==0 &&
            attpack->attrObject.compare("field")==0))
        continue; // skip non-CIM fields

      // if no attributes set in this attpack, skip it ...
      if (!(attpack->AttPackIsSet(inNestedAttPacks=false))) continue;

      // otherwise, write it out ...

      if (attpack->AttributeIsSet("CouplingPurpose")) {
        localrc = attpack->AttributeGet("CouplingPurpose", &valuevector);
        if (valuevector.size() > 1) {
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                        "Write items > 1 - Not yet implemented", &localrc);
          return ESMF_FAILURE;}
        value = valuevector.at(0);
        // map ESMF values {Ancillary, Boundary, Initial} to CIM 1.5 enum
        // values {ancillaryFile, boundaryCondition, initialCondition}
        transform(value.begin(), value.end(), value.begin(), ::tolower);
        if (value == "ancillary") {
          value = "ancillaryFile";
        } else if (value == "boundary") {
          value = "boundaryCondition";
        } else if (value == "initial") {
          value = "initialCondition";
        } else {
          ESMC_LogDefault.Write("Attribute CouplingPurpose in "
            "standard attribute package (convention='CIM 1.5', "
            "purpose='Inputs Description') must be one of "
            "{Ancillary, Boundary, Initial} "
            "to produce valid CIM XML output.",
            ESMC_LOG_WARN, ESMC_CONTEXT);
        }
        localrc = io_xml->writeStartElement("coupling", "", 3, 2,
                     "fullySpecified", "false", "purpose", value.c_str());
        ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
      } else {
        // Output starting <coupling> element, to match ending element
        // </coupling>, but with a blank purpose="" attr. This will produce an
        // invalid CIM 1.5 file, yet keep it well-formed XML.  Better than 
        // outputting no <coupling></coupling> pair, which would produce far
        // more validation errors, confusing a user as to what the real 
        // problem is -- that attribute CouplingPurpose is not set.
        localrc = io_xml->writeStartElement("coupling", "", 3, 2,
                     "fullySpecified", "false", "purpose", "");
        ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
        ESMC_LogDefault.Write("Attribute CouplingPurpose in "
          "standard attribute package (convention='CIM 1.5', "
          "purpose='Inputs Description') "
          "required to be set, when other attributes in this package are set, "
          "to produce valid CIM XML output.",
          ESMC_LOG_WARN, ESMC_CONTEXT);
      }
      if (attpack->AttributeIsSet("Frequency")) {
        localrc = attpack->AttributeGet("Frequency", &valuevector);
        if (valuevector.size() > 1) {
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                      "Write items > 1 - Not yet implemented", &localrc);
          return ESMF_FAILURE;}
        value = valuevector.at(0);

        // parse frequency value and units
        char s[2*ESMF_MAXSTR], empty[]="", *freq, *units;
        strcpy(s, value.c_str());
        freq = strtok(s, " ");
        units = strtok(NULL, " ");
        if (freq == NULL || units == NULL) {
          ESMC_LogDefault.Write("Attribute InputFrequency, in CIM 1.5/Inputs "
            "Description standard attribute package, must have both a time "
            "value and a units specification, e.g. '15 Minutes'.",
            ESMC_LOG_WARN, ESMC_CONTEXT);
          // prevent Xerces crash upon null ptr exception throw (with F90 main)
          if (freq == NULL) freq = empty;
          if (units == NULL) units = empty;
        }
        // CIM 1.5 enum: {seconds, minutes, hours, days, months, years,
        //                decades, centuries}
        value = units;
        transform(value.begin(), value.end(), value.begin(), ::tolower);
        if (value != "seconds" && value != "minutes" && value != "hours" &&
            value != "days" && value != "months" && value != "years" &&
            value != "decades" && value != "centuries") {
          ESMC_LogDefault.Write("Attribute InputFrequency, in CIM 1.5/Inputs "
            "Description standard attribute package, must have units as one of "
            "{Seconds, Minutes, Hours, Days, Months, Years, "
            "Decades, Centuries}, to produce valid CIM XML output.",
            ESMC_LOG_WARN, ESMC_CONTEXT);
        }
        localrc = io_xml->writeStartElement("timeProfile", "", 4, 2,
                              "units", value.c_str(), "variableRate", "false");
        localrc = io_xml->writeElement("rate", freq, 5, 0);
        localrc = io_xml->writeEndElement("timeProfile", 4);
        ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
      }
      if (attpack->AttributeIsSet("SpatialRegriddingMethod") ||
          attpack->AttributeIsSet("SpatialRegriddingDimension")) {

        if (attpack->AttributeIsSet("SpatialRegriddingDimension")) {
          localrc = attpack->AttributeGet("SpatialRegriddingDimension",&valuevector);
          if (valuevector.size() > 1) {
            ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                        "Write items > 1 - Not yet implemented", &localrc);
            return ESMF_FAILURE;}
          value = valuevector.at(0);
          // CIM 1.5 enum: {1D, 2D, 3D}
          transform(value.begin(), value.end(), value.begin(), ::toupper);
          if (value != "1D" && value != "2D" && value != "3D") {
            ESMC_LogDefault.Write("Attribute SpatialRegriddingDimension, in "
              "CIM 1.5/Inputs Description standard attribute package, must "
              "be one of {1D, 2D, 3D} to produce valid CIM XML output.",
              ESMC_LOG_WARN, ESMC_CONTEXT);
          }
          localrc = io_xml->writeStartElement("spatialRegridding", "", 4, 1,
                             "spatialRegriddingDimension", value.c_str()); 
        } else {
          // Output starting <spatialRegridding> element, to match ending 
          // element </spatialRegridding>, but with a blank 
          // spatialRegriddingDimension="" attr. This will produce an
          // invalid CIM 1.5 file, yet keep it well-formed XML.  Better than 
          // outputting no 
          // <spatialRegriddingDimension></spatialRegriddingDimension> pair, 
          // which would produce a more confusing validation error.
          localrc = io_xml->writeStartElement("spatialRegridding", "", 4, 1,
                             "spatialRegriddingDimension", ""); 
          ESMC_LogDefault.Write("Attribute SpatialRegriddingDimension in "
            "standard attribute package (convention='CIM 1.5', "
            "purpose='Inputs Description') must to be set (to one of {1D, "
            "2D, 3D}), when attribute SpatialRegriddingMethod is set, "
            "to produce valid CIM XML output.",
            ESMC_LOG_WARN, ESMC_CONTEXT);
        }
        if (attpack->AttributeIsSet("SpatialRegriddingMethod")) {
          localrc = attpack->AttributeGet("SpatialRegriddingMethod", &value2vector);
          if (value2vector.size() > 1) {
            ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                        "Write items > 1 - Not yet implemented", &localrc);
            return ESMF_FAILURE;}
          value2 = value2vector.at(0);
          // CIM 1.5 enum: {linear, near-neighbour, 
          //                cubic, conservative-first-order,
          //                conservative-second-order,
          //                conservative, non-conservative}
          transform(value2.begin(), value2.end(), value2.begin(), ::tolower);
          if (value2 == "near-neighbor") value2 = "near-neighbour";
          if (value2 != "linear" && value2 != "near-neighbour" && 
              value2 != "cubic" && value2 != "conservative-first-order" && 
              value2 != "conservative-second-order" && 
              value2 != "conservative" && value2 != "non-conservative") {
            ESMC_LogDefault.Write("Attribute SpatialRegriddingMethod, in "
              "CIM 1.5/Inputs Description standard attribute package, must be "
              "one of {Linear, Near-Neighbor, Cubic, "
              "Conservative-First-Order, Conservative-Second-Order, "
              "Conservative, Non-Conservative} to produce valid CIM "
              "XML output.",
              ESMC_LOG_WARN, ESMC_CONTEXT);
          }
          localrc = io_xml->writeElement("spatialRegriddingStandardMethod", 
                                       value2.c_str(), 5, 0);
        }
        localrc = io_xml->writeEndElement("spatialRegridding", 4);
        ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
      }
      if (attpack->AttributeIsSet("TimeTransformationType")) {
        localrc = attpack->AttributeGet("TimeTransformationType", &valuevector);
        if (valuevector.size() > 1) {
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                      "Write items > 1 - Not yet implemented", &localrc);
          return ESMF_FAILURE;}
        value = valuevector.at(0);
        localrc = io_xml->writeStartElement("timeTransformation", "", 4, 0);
        localrc = io_xml->writeElement("mappingType", "", 5, 2,
                                       "open", "true", 
                                       "value", value.c_str());
        localrc = io_xml->writeEndElement("timeTransformation", 4);
        ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
      }
      if (attpack->AttributeIsSet("CouplingSource")) {
        localrc = attpack->AttributeGet("CouplingSource", &valuevector);
        if (valuevector.size() > 1) {
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                      "Write items > 1 - Not yet implemented", &localrc);
          return ESMF_FAILURE;}
        value = valuevector.at(0);
        localrc = io_xml->writeStartElement("couplingSource", "", 4, 0);
        localrc = io_xml->writeStartElement("dataSource", "", 5, 0);
        localrc = io_xml->writeStartElement("reference", "", 6, 0);
        localrc = io_xml->writeElement("name", value, 7, 0);
        localrc = io_xml->writeEndElement("reference", 6);
        localrc = io_xml->writeEndElement("dataSource", 5);
        localrc = io_xml->writeEndElement("couplingSource", 4);
        ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
      } else {
        ESMC_LogDefault.Write("Attribute CouplingSource in "
          "standard attribute package (convention='CIM 1.5', "
          "purpose='Inputs Description') "
          "required to be set, when other attributes in this package are set, "
          "to produce valid CIM XML output.",
          ESMC_LOG_WARN, ESMC_CONTEXT);
      }
      if (attpack->AttributeIsSet("CouplingTarget")) {
        localrc = attpack->AttributeGet("CouplingTarget", &valuevector);
        if (valuevector.size() > 1) {
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                      "Write items > 1 - Not yet implemented", &localrc);
          return ESMF_FAILURE;}
        value = valuevector.at(0);
        localrc = io_xml->writeStartElement("couplingTarget", "", 4, 0);
        localrc = io_xml->writeStartElement("dataSource", "", 5, 0);
        localrc = io_xml->writeStartElement("reference", "", 6, 0);
        localrc = io_xml->writeElement("name", value, 7, 0);
        localrc = io_xml->writeEndElement("reference", 6);
        localrc = io_xml->writeEndElement("dataSource", 5);
        localrc = io_xml->writeEndElement("couplingTarget", 4);
        ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
      } else {
        ESMC_LogDefault.Write("Attribute CouplingTarget in "
          "standard attribute package (convention='CIM 1.5', "
          "purpose='Inputs Description') "
          "required to be set, when other attributes in this package are set, "
          "to produce valid CIM XML output.",
          ESMC_LOG_WARN, ESMC_CONTEXT);
      }
      if (((ap = attpack->AttPackGetAttribute("ShortName")) != NULL) &&
           (ap->parent->AttributeIsSet("ShortName"))) {
        localrc = ap->parent->AttributeGet("ShortName", &valuevector);
        if (valuevector.size() > 1) {
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "Write items > 1 - Not yet implemented", &localrc);
          return ESMF_FAILURE;}
        value = valuevector.at(0);
        localrc = io_xml->writeStartElement("connection", "", 4, 0);
        localrc = io_xml->writeStartElement("connectionTarget", "", 5, 0);
        localrc = io_xml->writeStartElement("dataSource", "", 6, 0);
        localrc = io_xml->writeStartElement("reference", "", 7, 0);
        localrc = io_xml->writeElement("name", value, 8, 0);
        localrc = io_xml->writeEndElement("reference", 7);
        localrc = io_xml->writeEndElement("dataSource", 6);
        localrc = io_xml->writeEndElement("connectionTarget", 5);
        localrc = io_xml->writeEndElement("connection", 4);
        ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
      }
      localrc = io_xml->writeEndElement("coupling", 3);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
    }
    // recurse through ESMF objects
    localrc = linkList.at(i)->AttributeWriteCIMcomposition(io_xml);
  }

  return ESMF_SUCCESS;

 } // end AttributeWriteCIMcomposition
#if 0
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeWriteCIMtraverse"
//BOPI
// !IROUTINE:  AttributeWriteCIMtraverse - Write contents of a CIM {\tt Attribute} package
//
// !INTERFACE:
      int Attribute::AttributeWriteCIMtraverse(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      IO_XML *io_xml,   //  in - io pointer to write
      ESMC_CIMRecordType cimRecType) const {
//
// !DESCRIPTION:
//    Print the contents of a CIM {\tt Attribute}.  Expected to be
//    called internally.
//
//EOPI

  int localrc;
  unsigned int i;
  Attribute *attpack = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  int ordinal=1;
  attpack = AttPackGet("CIM 1.5", "Model Component Simulation Description",
                       "comp", &ordinal);
  while (attpack != NULL) {
    localrc = attpack->AttributeWriteCIMbuffer(io_xml, cimRecType);
    if (localrc != ESMF_SUCCESS) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
        "AttributeWriteCIMtraverse failed AttributeWriteCIMbuffer", &localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
    return ESMF_FAILURE;
    }

    // get next occurence of this attpack, if any, on this component
    ordinal++;
    attpack = AttPackGet("CIM 1.5", "Model Component Simulation Description",
                         "comp", &ordinal);
  }

  // recurse across all linked ESMF objects (e.g. child components, states,
  // fieldBundles, fields, grids, arrays)
  for(i=0; i<linkList.size(); i++)
    localrc = linkList.at(i)->AttributeWriteCIMtraverse(io_xml, cimRecType);

  return ESMF_SUCCESS;

 } // end AttributeWriteCIMtraverse
#endif
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeWriteXMLbufferfield"
//BOPI
// !IROUTINE:  AttributeWriteXMLbufferfield - Write contents of an {\tt Attribute} package for field
//
// !INTERFACE:
      int Attribute::AttributeWriteXMLbufferfield(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      IO_XML *io_xml,                //  in - io pointer to write
      const string &convention,      //  in - convention
      const string &purpose,         //  in - purpose
      int &index,                    //  in - counter            
      const int &columns) const{     //  in - columns
//
// !DESCRIPTION:
//    Print the contents of an {\tt Attribute}.  Expected to be
//    called internally.
//
//EOPI

  char msgbuf[4*ESMF_MAXSTR];
  int localrc;
  unsigned int i;
  Attribute *attpack;

  attpack = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  string attPackInstanceName;
  attpack = AttPackGet(convention, purpose, "field", attPackInstanceName);
  if (attpack) {
    // TODO: replace this prototype for WaterML TimeSeries
    if (convention.compare("WaterML")==0 && 
        purpose.compare("TimeSeries")==0) {
      localrc = attpack->AttributeWriteWaterMLbuffieldT(io_xml, index, columns);
    } else {
      localrc = attpack->AttributeWriteXMLbufferfieldT(io_xml, index, columns);
    }
    if (localrc != ESMF_SUCCESS) {
      sprintf(msgbuf, "AttributeWriteXMLbufferfield failed AttributeWriteXMLbufferfieldT");
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
      return ESMF_FAILURE;
    }
  }

  for(i=0; i<linkList.size(); i++) {
    index = 0;
    localrc = linkList.at(i)->AttributeWriteXMLbufferfield(io_xml,
      convention,purpose,index,columns);
  }

  return ESMF_SUCCESS;

 } // end AttributeWriteXMLbufferfield
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeWriteXMLbufferfieldT"
//BOPI
// !IROUTINE:  AttributeWriteXMLbufferfieldT - Write contents of an {\tt Attribute} package for field
//
// !INTERFACE:
      int Attribute::AttributeWriteXMLbufferfieldT(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      IO_XML *io_xml,                //  in - io pointer to write
      int &index,                    //  in - counter            
      const int &columns) const{     //  in - columns
//
// !DESCRIPTION:
//    Print the contents of an {\tt Attribute}.  Expected to be
//    called internally.
//
//EOPI

  char msgbuf[4*ESMF_MAXSTR];
  int localrc;
  unsigned int i;
  Attribute *attpack;

  attpack = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

    for (i=0; i<attrList.size(); i++) { 
      if (index == 0) {
        localrc = io_xml->writeStartElement("variable", "", 3, 0);
        ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
      }
      string name = attrList.at(i)->attrName; 
      if (attrList.at(i)->items == 0) {
        localrc = io_xml->writeElement(name, "", 4, 0);
        ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
      } else if (attrList.at(i)->items == 1) {
        ostringstream outstring;
        switch (attrList.at(i)->tk)
        {
          case ESMC_TYPEKIND_I4:
            outstring << attrList.at(i)->vip.at(0); 
            localrc = io_xml->writeElement(name, outstring.str(), 4, 0);
            ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
            break;

          case ESMC_TYPEKIND_I8:
            outstring << attrList.at(i)->vlp.at(0); 
            localrc = io_xml->writeElement(name, outstring.str(), 4, 0);
            ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
            break;

          case ESMC_TYPEKIND_R4:
            outstring << attrList.at(i)->vfp.at(0); 
            localrc = io_xml->writeElement(name, outstring.str(), 4, 0);
            ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
            break;

          case ESMC_TYPEKIND_R8:
            outstring << attrList.at(i)->vdp.at(0); 
            localrc = io_xml->writeElement(name, outstring.str(), 4, 0);
            ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
            break;

          case ESMC_TYPEKIND_LOGICAL:
            if (attrList.at(i)->vbp.at(0) == ESMF_TRUE) {
              localrc = io_xml->writeElement(name, "true", 4, 0);
              ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
            } else if (attrList.at(i)->vbp.at(0) == ESMF_FALSE) {
              localrc = io_xml->writeElement(name, "false", 4, 0);
              ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
            }
            break;

          case ESMC_TYPEKIND_CHARACTER:
            localrc = io_xml->writeElement(name, attrList.at(i)->vcpp.at(0), 4, 0);
            ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
            break;

          default:
            localrc = io_xml->writeElement(name, "N/A", 4, 0);
            ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
            break;
        }

      } else if (attrList.at(i)->items > 1) { 
          sprintf(msgbuf,"Write items > 1 - Not yet implemented\n");
          ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO);
      } else {
        sprintf(msgbuf,"Items < 1, problem.");
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &localrc);
        return ESMF_FAILURE;
      }
      ++index;
      if (index == columns) {
        localrc = io_xml->writeEndElement("variable", 3);
        ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
      }
    }

  for(i=0; i<packList.size(); i++)
    localrc = packList.at(i)->AttributeWriteXMLbufferfieldT(io_xml,index,columns);

  return ESMF_SUCCESS;

 } // end AttributeWriteXMLbufferfieldT
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeWriteWaterMLbuffieldT"
//BOPI
// !IROUTINE:  AttributeWriteWaterMLbuffieldT - Write contents of an {\tt Attribute} package for field
//
// !INTERFACE:
      int Attribute::AttributeWriteWaterMLbuffieldT(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      IO_XML *io_xml,                //  in - io pointer to write
      int &index,                    //  in - counter            
      const int &columns) const{     //  in - columns
//
// !DESCRIPTION:
//    Print the contents of an {\tt Attribute}.  Expected to be
//    called internally.
//
//EOPI

  char msgbuf[4*ESMF_MAXSTR];
  int localrc;
  unsigned int i;
  Attribute *attpack;

  // TODO:  replace with actual field->array reference/pointer
  double array3d[]={0.1192, // 1 (name, values match those in
                    0.114,  // 2   WaterML use_test_case)
                    0.1424, // 3
                    0.1814, // 4
                    0.3841, // 5
                    0.9124, // 6
                    0.0655, // 7
                    0.4342, // 8
                    0.9543, // 9
                    0.7308, // 10
                    0.7004, // 11
                    0.6097, // 12
                    0.5544, // 13
                    0.6182, // 14
                    0.5476, // 15
                    0.5728, // 16
                    0.369,  // 17
                    0.3664, // 18
                    0.4767, // 19
                    0.3144, // 20
                    0.4517, // 21
                    0.3838, // 22
                    0.4702, // 23
                    0.389,  // 24
                    0.4949, // 25
                    0.2778, // 26
                    0.3576, // 27
                    0.3618, // 28
                    0.2803};// 29

  attpack = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // skip TimeSeries attpack (just a wrapper)
  if (attrPurpose.compare("TimeSeries")!=0) {

    // attpack header
    if (attrPurpose.compare("sourceInfo")==0) {
      localrc = io_xml->writeStartElement("sourceInfo", "", 2, 1,
                                          "xsi:type", "SiteInfoType");
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
    } else if (attrPurpose.compare("variable")==0) {
      localrc = io_xml->writeStartElement("variable", "", 2, 0);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
    }

    // print each attribute in attpack
    string xmlAttName[6], xmlAttVal[6];
    int xmlAttCount=0;
    double val;
    for (i=0; i<attrList.size(); i++) { 
      // TODO: check for #items, tk

      if (attrPurpose.compare("sourceInfo")==0) {
        if (attrList.at(i)->attrName.compare("siteCode")==0) {
          localrc = io_xml->writeElement(attrList.at(i)->attrName,
                                         attrList.at(i)->vcpp.at(0), 3, 2,
                                  "network", "LittleBearRiver", "siteID", "2");
          ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
        } else if (attrList.at(i)->attrName.compare("latitude")==0) {
          localrc = io_xml->writeStartElement("geoLocation", "", 3, 0);
          ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
          localrc = io_xml->writeStartElement("geogLocation", "", 4, 2,
                            "xsi:type", "LatLonPointType", "srs", "EPSG:4269");
          ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
          localrc = io_xml->writeElement(attrList.at(i)->attrName,
                                         attrList.at(i)->vcpp.at(0), 5, 0);
          ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
        } else if (attrList.at(i)->attrName.compare("longitude")==0) {
          localrc = io_xml->writeElement(attrList.at(i)->attrName,
                                         attrList.at(i)->vcpp.at(0), 5, 0);
          ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
          localrc = io_xml->writeEndElement("geogLocation", 4);
          ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
        } else if (attrList.at(i)->attrName.compare("X")==0) {
          localrc = io_xml->writeStartElement("localSiteXY", "", 4, 1,
                            "projectionInformation", " NAD83 / UTM zone 12N");
          ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);

          localrc = io_xml->writeElement(attrList.at(i)->attrName,
                                         attrList.at(i)->vcpp.at(0), 5, 0);
          ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
        } else if (attrList.at(i)->attrName.compare("Y")==0) {
          localrc = io_xml->writeElement(attrList.at(i)->attrName,
                                         attrList.at(i)->vcpp.at(0), 5, 0);
          ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
          localrc = io_xml->writeEndElement("localSiteXY", 4);
          ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
          localrc = io_xml->writeEndElement("geoLocation", 3);
          ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
        } else if (attrList.at(i)->attrName.compare("County")==0 ||
                   attrList.at(i)->attrName.compare("State")==0  ||
                   attrList.at(i)->attrName.compare("Site Comments")==0) {
          localrc = io_xml->writeElement("note", attrList.at(i)->vcpp.at(0), 3, 1,
                            "title", attrList.at(i)->attrName.c_str());
          ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
        } else { // siteName or verticalDatum
          localrc = io_xml->writeElement(attrList.at(i)->attrName,
                                         attrList.at(i)->vcpp.at(0), 3, 0);
          ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
        }
      } else if (attrPurpose.compare("variable")==0) {
        if (attrList.at(i)->attrName.compare("variableCode")==0) {
          localrc = io_xml->writeElement(attrList.at(i)->attrName,
                                         attrList.at(i)->vcpp.at(0), 3, 3,
                            "vocabulary", "LBR", "default", "true",
                            "variableID", "39");
          ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
        } else if (attrList.at(i)->attrName.compare("units")==0) {
          localrc = io_xml->writeElement(attrList.at(i)->attrName, 
                                         attrList.at(i)->vcpp.at(0), 3, 2,
                            "unitsAbbreviation", "mg/L", "unitsCode", "199");
          ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
        } else if (attrList.at(i)->attrName.compare("timeSupport")==0) {
          localrc = io_xml->writeStartElement(attrList.at(i)->attrName, "", 3, 1,
                            "isRegular", (attrList.at(i)->vcpp.at(0)).c_str());
          ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
          localrc = io_xml->writeEndElement(attrList.at(i)->attrName, 3);
          ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
        } else {
          localrc = io_xml->writeElement(attrList.at(i)->attrName,
                                         attrList.at(i)->vcpp.at(0), 3, 0);
          ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
        }
      } else if (attrPurpose.compare("values")==0) {
        // collect attr names, values to output at end of loop
        xmlAttName[xmlAttCount] = attrList.at(i)->attrName;
        xmlAttVal[xmlAttCount]  = attrList.at(i)->vcpp.at(0);
        xmlAttCount++;
      } else if (attrPurpose.compare((attrPurpose.size()-2),2,".1")==0) {
        // <value>
        // collect attr names, values to output at end of loop
        xmlAttName[xmlAttCount] = attrList.at(i)->attrName;
        xmlAttVal[xmlAttCount]  = attrList.at(i)->vcpp.at(0);
        xmlAttCount++;
        if (attrList.at(i)->attrName.compare("sampleID")==0) {
          // TODO: get value from array here
          val = 
            array3d[atoi((attrList.at(i)->attrPurpose.substr(5,2)).c_str())-1];
        }
      } else if (attrPurpose.compare("method")==0) {
        if (attrList.at(i)->attrName.compare("methodID")==0) {
          localrc = io_xml->writeStartElement(attrPurpose, "", 3, 1,
                        attrList.at(i)->attrName.c_str(),
                        attrList.at(i)->vcpp.at(0).c_str());
          ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
        } else {
          localrc = io_xml->writeElement(attrList.at(i)->attrName,
                                         attrList.at(i)->vcpp.at(0), 4, 0);
          ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
          localrc = io_xml->writeEndElement(attrPurpose, 3);
          ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
        }
      } else if (attrPurpose.compare("source")==0) {
        if (attrList.at(i)->attrName.compare("sourceID")==0) {
          localrc = io_xml->writeStartElement(attrPurpose, "", 3, 1,
                        attrList.at(i)->attrName.c_str(),
                        attrList.at(i)->vcpp.at(0).c_str());
          ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
        } else {
          localrc = io_xml->writeElement(attrList.at(i)->attrName,
                                         attrList.at(i)->vcpp.at(0), 4, 0);
          ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
          if (attrList.at(i)->attrName.compare("SourceDescription")==0) {
            localrc = io_xml->writeStartElement("ContactInformation", "", 4, 0);
            ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
            localrc = io_xml->writeElement("ContactName", "Amber Spackman", 5, 0);
            ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
            localrc = io_xml->writeElement("TypeOfContact", "main", 5, 0);
            ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
            localrc = io_xml->writeElement("Phone", "1-435-797-0045", 5, 0);
            ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
            localrc = io_xml->writeElement("Email", "amber.s@aggiemail.usu.edu", 5, 0);
            ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
            localrc = io_xml->writeElement("Address", 
                             "8200 Old Main Hill, Logan, Utah 84322-8200", 5, 1,
                             "xsi:type", "xsd:string");
            ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
            localrc = io_xml->writeEndElement("ContactInformation", 4);
            ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
          }
        }
      }
    }

    if (attrPurpose.compare("values")==0) {
      localrc = io_xml->writeStartElement(attrPurpose, "", 2, xmlAttCount,
         xmlAttName[0].c_str(), xmlAttVal[0].c_str(),
         xmlAttName[1].c_str(), xmlAttVal[1].c_str(),
         xmlAttName[2].c_str(), xmlAttVal[2].c_str());
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
    } else if (attrPurpose.compare((attrPurpose.size()-2),2,".1")==0) {
      // <value>
      ostringstream outstring;
      outstring << val;
      localrc = io_xml->writeElement("value", outstring.str(), 3, xmlAttCount,
         xmlAttName[0].c_str(), xmlAttVal[0].c_str(),
         xmlAttName[1].c_str(), xmlAttVal[1].c_str(),
         xmlAttName[2].c_str(), xmlAttVal[2].c_str(),
         xmlAttName[3].c_str(), xmlAttVal[3].c_str(),
         xmlAttName[4].c_str(), xmlAttVal[4].c_str(),
         xmlAttName[5].c_str(), xmlAttVal[5].c_str());
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
    } else if (attrPurpose.compare("source")==0) {
      localrc = io_xml->writeEndElement("source", 3);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
      localrc = io_xml->writeEndElement("values", 2);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
    }
    // attpack footer
    else if (attrPurpose.compare("sourceInfo")==0) {
      localrc = io_xml->writeEndElement("sourceInfo", 2);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
    } else if (attrPurpose.compare("variable")==0) {
      localrc = io_xml->writeEndElement("variable", 2);
      ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
    }

  } // end if not TimeSeries attpack wrapper

  // recurse remaining attpacks
  for(i=0; i<packList.size(); i++)
    localrc = packList.at(i)->AttributeWriteWaterMLbuffieldT(io_xml,index,columns);

  return ESMF_SUCCESS;

 } // end AttributeWriteWaterMLbuffieldT

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
      void) const {                    // could add options at some point
// 
// !DESCRIPTION:
//     Print the contents of an {\tt Attribute} object
//
//EOPI
  int localrc;
  unsigned int i;
  char msgbuf[4*ESMF_MAXSTR];

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  for (i=0; i<attrList.size(); i++) {
    sprintf(msgbuf, "   Attr %d:\n", i);
    printf("%s",msgbuf);
    ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO);
  // print name
  sprintf(msgbuf, "        name: %s\n",  attrList.at(i)->attrName.c_str());
  printf("%s",msgbuf);
  ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO);
  
  // print items if there are any
  if (attrList.at(i)->items <= 0) {
      sprintf(msgbuf, "        value: \n");
      printf("%s",msgbuf);
      ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO);
  }

  if (attrList.at(i)->items == 1) {
      sprintf(msgbuf, "        value: ");
      printf("%s",msgbuf);
      ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO);
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
                 ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_WRONGTYPE, msgbuf, &localrc);
                 return localrc;
             }
      printf("%s",msgbuf);
      ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO);
  }

  if (attrList.at(i)->items > 1) { 
      sprintf(msgbuf, "        %d items, values:\n", attrList.at(i)->items);
      printf("%s",msgbuf);
      ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO);
      for (unsigned int j=0; j<attrList.at(i)->items; j++) {
                if (attrList.at(i)->tk == ESMC_TYPEKIND_I4) {
                    sprintf(msgbuf, "          \t item %d: %d\n", j, attrList.at(i)->vip[j]); 
                } else if (attrList.at(i)->tk == ESMC_TYPEKIND_I8) {
                    sprintf(msgbuf, "          \t item %d: %lld\n", j, attrList.at(i)->vlp[j]); 
                } else if (attrList.at(i)->tk == ESMC_TYPEKIND_R4) {
                    sprintf(msgbuf, "          \t item %d: %f\n", j, attrList.at(i)->vfp[j]); 
                } else if (attrList.at(i)->tk == ESMC_TYPEKIND_R8) {
                    sprintf(msgbuf, "          \t item %d: %g\n", j, attrList.at(i)->vdp[j]); 
                } else if (attrList.at(i)->tk == ESMC_TYPEKIND_LOGICAL) {
                    sprintf(msgbuf, "          \t item %d: %s\n", j,
                      ESMC_LogicalString(attrList.at(i)->vbp[j]));
                } else if (attrList.at(i)->tk == ESMC_TYPEKIND_CHARACTER) {
                    sprintf(msgbuf, "          \t item %d: %s\n", j, attrList.at(i)->vcpp[j].c_str());
                } else{
                    sprintf(msgbuf, "          \t unknown value");
                    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &localrc);
                    return localrc;
                }
      printf("%s",msgbuf);
      ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO);
      }
  }

  // print convention
  sprintf(msgbuf, "        convention: %s\n",  attrList.at(i)->attrConvention.c_str());
  printf("%s",msgbuf);
  ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO);
  
  // print purpose
  sprintf(msgbuf, "        purpose: %s\n",  attrList.at(i)->attrPurpose.c_str());
  printf("%s",msgbuf);
  ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO);
  
  // print object
  sprintf(msgbuf, "        object: %s\n",  attrList.at(i)->attrObject.c_str());
  printf("%s",msgbuf);
  ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO);

  sprintf(msgbuf, "        attrCount: %d\n", attrList.at(i)->AttributeGetCountTotal());
  printf("%s",msgbuf);
  ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO);
  }
  
  for (i=0; i<packList.size(); i++) {
    packList.at(i)->ESMC_Print();
  }
//  for (i=0; i<linkList.size(); i++) {
//    linkList.at(i)->ESMC_Print();
//  }

  return ESMF_SUCCESS;

}  // end ESMC_Print
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

  // create unique name (within this address space)
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
      Attribute::Attribute(
//
// !RETURN VALUE:
//    {\tt Attribute} object
//
// !ARGUMENTS:
        const string &name,                // Attribute name
        const ESMC_TypeKind &typekind,    // typekind
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
        const ESMC_TypeKind &typekind,    // typekind
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
//}
 
  // if a change was made, note the new values
  if (numitems >= 1) {
    tk = typekind;
    items = numitems;
    valueChange = ESMF_TRUE;
  }

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

  localrc = AttributeCopyValue(source);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        &localrc)) delete this;

  return (*this);

}  // end AttributeOperator=
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

  unsigned int i;

  attrBase = ESMC_NULL_POINTER;
  parent = ESMC_NULL_POINTER;

        if (tk == ESMC_TYPEKIND_I4) vip.clear();
        else if (tk == ESMC_TYPEKIND_I8) vlp.clear();
        else if (tk == ESMC_TYPEKIND_R4) vfp.clear();
        else if (tk == ESMC_TYPEKIND_R8) vdp.clear();  
        else if (tk == ESMC_TYPEKIND_LOGICAL) vbp.clear();
        else if (tk == ESMC_TYPEKIND_CHARACTER) vcpp.clear();

  for (i=0; i<attrList.size(); i++)
    delete attrList.at(i);
  
  for (i=0; i<packList.size(); i++)
    delete packList.at(i);
  
  for (i=0; i<linkList.size(); i++)
    linkList.at(i) = ESMC_NULL_POINTER;
    
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
    int loffset, nbytes, chars;
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
    loffset=*offset;
    
    DESERIALIZE_VAR(buffer,loffset,chars,int);
    DESERIALIZE_VARC(buffer,loffset,attrName,temp,chars);

    DESERIALIZE_VAR(buffer,loffset,tk,ESMC_TypeKind);
    DESERIALIZE_VAR(buffer,loffset,items,int);
    DESERIALIZE_VAR(buffer,loffset,attrRoot,ESMC_Logical);
    
    DESERIALIZE_VAR(buffer,loffset,chars,int);
    DESERIALIZE_VARC(buffer,loffset,attrConvention,temp2,chars);
    DESERIALIZE_VAR(buffer,loffset,chars,int);
    DESERIALIZE_VARC(buffer,loffset,attrPurpose,temp3,chars);
    DESERIALIZE_VAR(buffer,loffset,chars,int);
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
            DESERIALIZE_VAR(buffer,loffset,chars,int);
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
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            &localrc)) return localrc;
      localrc = AttributeSet(attr);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
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
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            &localrc)) return localrc;
      localrc = AttPackSet(attr);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
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
    int loffset=*offset;
    bool cc;
    int localrc;

    // Initialize local return code; assume routine not implemented
    localrc = ESMC_RC_NOT_IMPL;
    cc = false;
    localrc = ESMC_SerializeCC(buffer,length,loffset,cc,inquireflag);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          &localrc)) return localrc;
    if (inquireflag != ESMF_INQUIREONLY) {
      cc = true;
      localrc = ESMC_SerializeCC(buffer,length,*offset,cc,inquireflag);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
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

    // Define serialization macros
#define SERIALIZE_VAR(cc,bufptr,loff,var,t) \
  if (cc) *(reinterpret_cast<t*> ((bufptr)+(loff)))=var;    \
  loff += (sizeof(t));   

#define SERIALIZE_VARC(cc,bufptr,loff,var,s) \
  if (cc) strncpy((bufptr)+(loff),(var).c_str(),s);      \
  loff += s;

    // Initialize local return code; assume routine not implemented
    localrc = ESMC_RC_NOT_IMPL;

      SERIALIZE_VAR(cc,buffer,offset,(attrName.size()),int);
      SERIALIZE_VARC(cc,buffer,offset,attrName,(attrName.size()));

      SERIALIZE_VAR(cc,buffer,offset,tk,ESMC_TypeKind);
      
      SERIALIZE_VAR(cc,buffer,offset,items,int);
      SERIALIZE_VAR(cc,buffer,offset,attrRoot,ESMC_Logical);
      
      SERIALIZE_VAR(cc,buffer,offset,(attrConvention.size()),int);
      SERIALIZE_VARC(cc,buffer,offset,attrConvention,(attrConvention.size()));
      SERIALIZE_VAR(cc,buffer,offset,(attrPurpose.size()),int);
      SERIALIZE_VARC(cc,buffer,offset,attrPurpose,(attrPurpose.size()));
      SERIALIZE_VAR(cc,buffer,offset,(attrObject.size()),int);
      SERIALIZE_VARC(cc,buffer,offset,attrObject,(attrObject.size()));
      
      SERIALIZE_VAR(cc,buffer,offset,attrPack,ESMC_Logical);
      SERIALIZE_VAR(cc,buffer,offset,attrPackHead,ESMC_Logical);
      SERIALIZE_VAR(cc,buffer,offset,attrNested,ESMC_Logical);
          
      SERIALIZE_VAR(cc,buffer,offset,attrList.size(),int);
      SERIALIZE_VAR(cc,buffer,offset,packList.size(),int);
//      SERIALIZE_VAR(cc,buffer,offset,linkList.size(),int);

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
            SERIALIZE_VAR(cc,buffer,offset,(vcpp[i].size()),int);
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
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_MEM_ALLOCATE, 
            "Buffer too short to add an Attribute hierarchy", &localrc);
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
