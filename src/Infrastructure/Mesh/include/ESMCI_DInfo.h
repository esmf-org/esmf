// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2022, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================

//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------

#ifndef ESMCI_DInfo_H
#define ESMCI_DInfo_H

#include <vector>

#include "ESMCI_Macros.h"
#include "ESMCI_F90Interface.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_VM.h"

#include <Mesh/include/Legacy/ESMCI_Exception.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
// static const char *const version = "$Id$";
//-----------------------------------------------------------------------------


//==============================================================================
//
// A distributed information class used for setting and getting information
// associated with a numeric id. The id should be relatively
/// dense between it's min and max, because the ids are spread evenly across
// procs between it's min and max. 
//
//==============================================================================



using namespace ESMCI;

template <class IDTYPE, class INFOTYPE>
class DInfo {


  //private:
public:

  class DInfoEntry{
  public:
    IDTYPE id;
    INFOTYPE info;

    DInfoEntry(IDTYPE _id, INFOTYPE _info): id(_id), info(_info) {}

    // Less 
    bool operator<(const DInfoEntry &rhs) const {
      if (id != rhs.id) {
        return id < rhs.id;
      } 
      return info < rhs.info;
    }

    // Do we need this? 
    bool operator==(const DInfoEntry &rhs) const {
      return (id == rhs.id &&
              info == rhs.info);
    }

    
  };
  
  std::vector<DInfoEntry> staging; // A place to accumulate ids and info before setting
                                   // up for searching
  std::vector<DInfoEntry> searchable; // After committing this will contain a sorted list
                                      // for searching

  bool is_committed; // has this DInfo object been committed, so it's searchable
  IDTYPE gmin,gmax; // global min and max of id in staging (only valid when committed)

  
public:
  
  // Full constructor
  DInfo(): is_committed(false) {}

  // Reserve space in staging area
  void reserve(int _num) {
    staging.reserve(_num);
  }

  // Set up to enable searching
  void commit();

  // Add an entry
  void add(IDTYPE _id, INFOTYPE _info) {
    staging.push_back(DInfoEntry(_id,_info));
  }


  // Reset back to empty (pre-commit) state, but like a vector hold onto any memory that happens to be there
  void clear() {
    staging.clear();
    searchable.clear();
    is_committed=false;
  }
  
  
  // Search info in directory
  // (Because the search involves communication it's more efficient to do it in a chunk))
  void search(int num_search,
              IDTYPE *search_ids,
              bool  error_on_not_found, 
              INFOTYPE not_found_info_val,
              INFOTYPE *out_search_info);
  
  // Output searchable list (probably mostly for debugging)
  void print_searchable();
  
};


#endif // ESMCI_DInfo_H
