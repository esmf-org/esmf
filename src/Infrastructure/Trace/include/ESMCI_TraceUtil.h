// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// Trace include file for C++

#ifndef ESMCI_TRACEUTIL_H
#define ESMCI_TRACEUTIL_H

#include <string>

namespace ESMCI { 

  static vector<string> split(const string& s, const string& delim, const bool keep_empty = true) {
    vector<string> result;
    if (delim.empty()) {
      result.push_back(s);
      return result;
    }
    string::const_iterator substart = s.begin(), subend;
    while (true) {
      subend = search(substart, s.end(), delim.begin(), delim.end());
      string temp(substart, subend);
      if (keep_empty || !temp.empty()) {
	result.push_back(temp);
      }
      if (subend == s.end()) {
	break;
      }
      substart = subend + delim.size();
    }
    return result;
  }
  
  static string trim(const string& str) {
    size_t first = str.find_first_not_of(' ');
    if (string::npos == first) {
      return str;
    }
    size_t last = str.find_last_not_of(' ');
    return str.substr(first, (last - first + 1));
  }
  
  
}

#endif
