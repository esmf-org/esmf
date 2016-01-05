// $Id$
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_MIGRATOR_H_
#define ESMCI_MIGRATOR_H_

#include <Mesh/include/ESMCI_SparseMsg.h>
#include <Mesh/include/ESMCI_MeshTypes.h>
#include <Mesh/include/ESMCI_DDir.h>
#include <Mesh/include/ESMCI_ParEnv.h>

namespace ESMCI {

/*
 * Generic object migration.  Transposes the parallel distribution of an object.
 * The object type here is some set of data that is associated with global ids.
 * That is, for each global id there are (possibly multiple) copies of some data type (this matrix row,
 * struct, etc...) on one or more processors.  The gid specific objects
 * are the 'elements' below.
 * We give the migration object a new distribution of the global ids (presumably 
 * different than that the object currently has).  This migration is able to move
 * the said object to the new location.  
 * 
 * The object to migrate must either implement the typedefs and functions
 * in MigTraits, or it must specialize the object itself.
 * 
 * It is possible that during this migration an element could be lost,
 * for instance if no one needs it.  Therefore, by default, if an element
 * has no send addresses, we maintain it on the current processor.
 */

/* 
 * Default migration traits.  Overide for objects with
 * more specific needs.  Otherwise, simply include the necessary
 * typedefs/functions as laid out below (T:: and t. 's)
 */
template <typename T>
struct MigTraits {

  // global id iterator.  *i returns gid
  typedef typename T::element_iterator element_iterator;
  
  // type for element
  typedef typename T::element_type element_type;
  
  // default pack/unpack objects
  typedef SparsePack<element_type> element_pack;
  
  // Size to pack element.  May or may not depend on particular element
  static UInt element_pack_size(element_type &) { return element_pack::size(); }
  
  typedef SparseUnpack<element_type> element_unpack;
  
  static UInt get_id(element_type &e) { return e.get_id(); }
  
  // first global id iterator)
  static element_iterator element_begin(T &t) { return t.element_begin(); }
  static element_iterator element_end(T &t) { return t.element_end(); }
  
  // Insert element el into t at local id (if helpful) lid
  static void insert_element(T &t, UInt lid, element_type &el) { t.insert(lid, el); }
  
  static void resize_object(T &t, UInt n) { t.resize(n); }
  
  static void clear_object(T &t) { t.clear(); }
};

/*
 * Template class to migrate compatible objects (implicit interface).
 */
class Migrator {
public:
  

  /*
   * Send in the final distribution (dest) and the current
   * distribution (src).  The constructor creates a distributed
   * directory and finds the communication path from here to there.
   * ASSUM: when the source object is passed in migrate, we expect
   * the element_iterator to traverse the list in the order of
   * the src_gids here (this avoids a lookup for each gid).
   */
  Migrator(UInt ndest_gids, const UInt dest_gids[],
      const UInt *dest_lids, // may be null
      UInt nsrc_gids, const UInt src_gids[]);
  
  /* 
   * Migrate object t to object r.  The pack/unpack routines must be
   * compatible.
   */
  template <typename T, typename R>
  void Migrate(T &t, R &r, bool obj_equal = false);
  
  /* 
   * Migrate an object to a redistributed copy of itself.
   */
  template <typename T>
  void Migrate(T &t) {
    Migrate(t, t, true);
  }
  
private:
  DDir<> dir;
  std::vector<DDir<>::dentry> response;
  UInt ndest_gid;
};

/*---------------------------------------------*/
// Mig object implementation (template functions)
/*---------------------------------------------*/

template <typename T, typename R>
void Migrator::Migrate(T &t, R &r, bool obj_equal) {
  Trace __trace("Migrator::Migrate(T &t, R &r, bool obj_equal)");
  
  typedef MigTraits<T> MIGT;
  typedef MigTraits<R> MIGR;
  
  // Sizing loop;
  UInt csize = Par::Size();
  std::vector<UInt> to_proc;
  std::vector<UInt> send_size_all(csize, 0); // need vector proc can look up into (Map?)
  
  typename MIGT::element_iterator ib = MIGT::element_begin(t),
      ie = MIGT::element_end(t);

  // We require that the element iterator and the response are in the
  // same order, though there may be 0-n responses for each gid.
  // Use this fact for efficiency;
  
  std::vector<DDir<>::dentry>::iterator ri =
    response.begin(), rs, re = response.end();
  
  
  for (; ib != ie && ri != re;) {
    
    UInt gid_save = MIGT::get_id(*ib);
 

    rs = ri; // checkpoint beginning of gid
    
    while (rs != re && ib != ie && MIGT::get_id(*ib) == gid_save) {

      ri = rs; // reset ri to beginning of run
   
//Par::Out() << "ib gid=" << MIGT::get_id(*ib) << ", ri->gid=" << ri->gid << std::endl;      
      // Just a sanity check. ib <= ri
      ThrowRequire(ri == re || MIGT::get_id(*ib) <= ri->gid); 
      
      // We have one or more matches;
      while (ri != re && ri->gid == MIGT::get_id(*ib)) {
    
        UInt proc = ri->origin_proc;
//Par::Out() << "\tri->gid=" << ri->gid << ", proc=" << proc << std::endl;        
        std::vector<UInt>::iterator plb =
          std::lower_bound(to_proc.begin(), to_proc.end(), proc);
        
        if (plb == to_proc.end() || *plb != proc)
          to_proc.insert(plb, proc);
        
        // element
        send_size_all[proc] += MIGT::element_pack_size(*ib);
        
        // local id
        send_size_all[proc] += SparsePack<UInt>::size();
        
        ++ri;
      }
      
      ++ib;
    } // ib same gid
    
  } // ib
    
    SparseMsg msg;
    
    UInt nsend = to_proc.size();
    
    std::vector<UInt> sizes(nsend, 0);
    
    for (UInt i = 0; i < nsend; i++)
      sizes[i] = send_size_all[to_proc[i]];
    
    msg.setPattern(nsend, nsend == 0 ? NULL : &to_proc[0]);
    
    msg.setSizes(nsend == 0 ? NULL : &sizes[0]);
    
    // Now pack data
    ib = MIGT::element_begin(t);

    ri = response.begin();
    
    for (; ib != ie && ri != re; ) {
      
      UInt gid_save = MIGT::get_id(*ib);
      
      rs = ri; // checkpoint beginning of gid
      
      while (rs != re && ib != ie && MIGT::get_id(*ib) == gid_save) {
        
        ri = rs; // reset ri to beginning of run
        
        // We have one or more matches;
        while (ri != re && ri->gid == MIGT::get_id(*ib)) {
          
          UInt proc = ri->origin_proc;
          
          SparseMsg::buffer &b = *msg.getSendBuffer(proc);
          
          // Element
          typename MIGT::element_pack(b, *ib);
          
          // lid
          SparsePack<UInt>(b, ri->origin_lid);
          
          ++ri;
        }
      
        ++ib;
      
      }
    
    } // for ib
    if (!msg.filled()) Throw() << "Did not fill migrate message";
    
    // Object is completely packed, so clear it
    if (obj_equal) {
      MIGT::clear_object(t);
      MIGT::resize_object(t, ndest_gid);
    }
    
    msg.communicate();
  
    // Now unpack the object
    for (std::vector<UInt>::iterator p = msg.inProc_begin(); p != msg.inProc_end(); p++) {
      
      UInt proc = *p;
      SparseMsg::buffer &b = *msg.getRecvBuffer(proc);
      
      while (!b.empty()) {
        
        typename MIGR::element_type el;
        
        // Element
        typename MIGR::element_unpack(b, el);
        
        // Lid
        UInt lid;
        SparseUnpack<UInt>(b, lid);
        
        MIGR::insert_element(t, lid, el);
        
      }
      
    } // for p
    if (!msg.empty()) Throw() << "Did not empty migrate message";
   
}

} // namespace

#endif /*ESMC_MIGRATOR_H_*/
