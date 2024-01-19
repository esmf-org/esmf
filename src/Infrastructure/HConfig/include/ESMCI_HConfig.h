// $Id$
//
// Earth System Modeling Framework
// Copyright (c) 2002-2024, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//-------------------------------------------------------------------------
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-----------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMCI_HConfig_H
#define ESMCI_HConfig_H

//-----------------------------------------------------------------------------
//BOPI
// !CLASS:  ESMCI::HConfig - Hierarchical Config
//
//
// !DESCRIPTION:
//
// The code in this file defines the C++ {\tt HConfig} class and declares
// method signatures (prototypes).  The companion file {\tt ESMCI_HConfig.C}
// contains the full code (bodies) for the {\tt HConfig} methods.
//
//EOPI
//-------------------------------------------------------------------------

namespace ESMCI {
  class HConfig;
}

#include "ESMCI_Config.h"
#include <string>
#include <vector>

#ifdef ESMF_YAMLCPP
#include "yaml-cpp/yaml.h"
#endif

//-------------------------------------------------------------------------

namespace ESMCI {

  // classes and structs

  class HConfig{

    private:
#ifdef ESMF_YAMLCPP
      std::vector<YAML::Node> *doc = NULL;  // a NULL indicates this is iterator
      YAML::iterator iter;        // if iterator, then this member is iterator
      YAML::NodeType::value type; // what is being iterated over: map / sequence
#endif

    public:

      static HConfig create(int *rc=NULL);
      template <typename T> static HConfig create(T content, int *rc=NULL);
      template <typename T> static HConfig create(T *content, int count, int *rc=NULL);
      static int destroy(HConfig *hconfig);

      template <typename T> inline static YAML::Node find(T self, HConfig *key);

      inline static std::string tagRef(YAML::Node &self);
      inline static std::string tag(YAML::Node self);

      int load(const std::string& content);
      int loadFile(const std::string& filename, int *docIndex=NULL);
      int saveFile(const std::string& filename, int *docIndex=NULL);

      HConfig createAt(int *index=NULL, int *docIndex=NULL, int *rc=NULL);
      HConfig createAtKey(HConfig *key, int *docIndex=NULL, int *rc=NULL);
      HConfig createAtMapKey(int *index=NULL, int *docIndex=NULL, int *rc=NULL);
      HConfig createAtMapKeyKey(HConfig *key, int *docIndex=NULL, int *rc=NULL);
      HConfig createAtMapVal(int *index=NULL, int *docIndex=NULL, int *rc=NULL);
      HConfig createAtMapValKey(HConfig *key, int *docIndex=NULL, int *rc=NULL);

      int add(HConfig *value);
      int add(HConfig *key, HConfig *value);
      int addMapKey(HConfig *value);
      int addMapKey(HConfig *key, HConfig *value);
      int addMapVal(HConfig *value);
      int addMapVal(HConfig *key, HConfig *value);

      int getDocCount(int *rc=NULL);

      int getSize(int *rc=NULL);
      int getSizeMapKey(int *rc=NULL);
      int getSizeMapVal(int *rc=NULL);

      std::string getTag(int *rc=NULL);
      std::string getTagMapKey(int *rc=NULL);
      std::string getTagMapVal(int *rc=NULL);

      int isNull(bool *flag);
      int isScalar(bool *flag);
      int isSequence(bool *flag);
      int isMap(bool *flag);
      int isDefined(bool *flag);

      int isNullMapKey(bool *flag);
      int isScalarMapKey(bool *flag);
      int isSequenceMapKey(bool *flag);
      int isMapMapKey(bool *flag);
      int isDefinedMapKey(bool *flag);

      int isNullMapVal(bool *flag);
      int isScalarMapVal(bool *flag);
      int isSequenceMapVal(bool *flag);
      int isMapMapVal(bool *flag);
      int isDefinedMapVal(bool *flag);

      int isIterator(bool *flag);
      int isSequenceIterator(bool *flag);
      int isMapIterator(bool *flag);

      HConfig iterBegin(int *rc=NULL);
      HConfig iterEnd(int *rc=NULL);
      HConfig iterBeginMapKey(int *rc=NULL);
      HConfig iterEndMapKey(int *rc=NULL);
      HConfig iterBeginMapVal(int *rc=NULL);
      HConfig iterEndMapVal(int *rc=NULL);
      int iterNext();

      template<typename T> T as(bool *asOkay, int *rc=NULL);
      template<typename T> T asMapKey(bool *asOkay, int *rc=NULL);
      template<typename T> T asMapVal(bool *asOkay, int *rc=NULL);

      int set(HConfig *value);
      int setMapKey(HConfig *value);
      int setMapVal(HConfig *value);

      int remove(int index);
      int remove(const std::string& keyString);

      int toConfig(ESMCI_Config **config);
  };   // class HConfig

} // namespace ESMCI

#endif  // ESMCI_HConfig_H
