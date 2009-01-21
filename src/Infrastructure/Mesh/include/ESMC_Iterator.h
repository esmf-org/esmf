// $Id: ESMC_Iterator.h,v 1.2.2.2 2009/01/21 21:25:22 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.


// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMC_Iterator_h
#define ESMC_Iterator_h

namespace ESMCI {
namespace MESH {

// Adapts a map to a linear iterator, so that
// *it yields *it->second.  This both dereferences the right
// hand side and gets rid of the pesky first,second.
template<typename MapType, typename Map_iterator, typename T, typename Ref, typename Ptr>
class Map_Ptr_Adapt_iterator {
public:
typedef std::forward_iterator_tag iterator_category;
typedef std::ptrdiff_t difference_type;
typedef typename Map_iterator::value_type::second_type value_type;
typedef Map_Ptr_Adapt_iterator<MapType, typename MapType::iterator, T, T&, T*> iterator;
typedef Map_Ptr_Adapt_iterator<MapType, typename MapType::const_iterator, T, const T&, const T*> const_iterator;
typedef Ref reference;
typedef Ptr pointer;
typedef Map_Ptr_Adapt_iterator<MapType, Map_iterator,T, Ref, Ptr> self;

template<typename A, typename B, typename C, typename D, typename E> friend class Map_Ptr_Adapt_iterator;

Map_Ptr_Adapt_iterator(const iterator &rhs) :
 mi(rhs.mi) 
{}
Map_Ptr_Adapt_iterator(const Map_iterator &rhs) :
mi(rhs) 
{}
self &operator=(const Map_iterator &rhs) {
  if (this == &rhs) return *this;
  mi = rhs.mi;
}

bool operator==(const self &rhs) const {
  return mi == rhs.mi;
}

bool operator !=(const self &rhs) const {
  return !(*this == rhs);
}

self &operator++() {
  mi++;
  return *this;
} 
self &operator++(int) {
  self tmp = *this;
  this->operator++();
  return tmp;
}
reference operator*() const {
  return *mi->second;
}
pointer operator->() const {
  return mi->second;
}

private:
Map_iterator mi;
};

} // namespace
} // namespace

#endif
