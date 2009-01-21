// $Id: ESMC_MeshObj.h,v 1.2.2.2 2009/01/21 21:25:22 cdeluca Exp $
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

#ifndef ESMC_MeshObj_h
#define ESMC_MeshObj_h


#include <ESMC_MeshTypes.h>
#include <ESMC_List.h>
#include <ESMC_Tree.h>
#include <ESMC_SmallAlloc.h>

#include <stdlib.h>
#include <stdio.h>
#include <utility>

#include <string>


#include <list>
#include <vector>
#include <map>
#include <exception>
#include <iostream>
#include <limits>



// A basic container for all mesh objects (used to connect different types)
//  Some container types
namespace ESMCI {
namespace MESH {

class MeshObj;
typedef List<MeshObj> MeshObjList;


class Kernel;  // Only so the object can provide an
                    // accesor.  Do not include details here,
                    // becuase this would create a circular
                    // dependency.

class _fieldStore; // similar story here.  We only hold a pointer to this
                   // class. We should never include its header, since this
                   // would cause a circular dependency.

typedef long MeshObj_id_type;


/**
 * A class to represent a basic object in a mesh such as a node, face, edge,
 * element, etc..  This class provides a container for the various relations
 * between mesh objects such as USES, CHILD, etc...
 * A main goal of this class is to be as small as possible, since there
 * very well may be millions of these present.
*/
class MeshObj :  public ListNode<MeshObj>, public TreeNode<MeshObj_id_type> {
public:
  static void *operator new(std::size_t);
  static void operator delete(void *p, std::size_t);
class Relation {
public:
  Relation() : obj(NULL), ordinal(0), type(0), polarity(0), rotation(0) {}
  MeshObj *obj;
  typedef UShort ordinal_type;
  typedef UChar type_type;
  typedef bool polarity_type;
  typedef UChar rotation_type;
  ordinal_type ordinal;
  type_type type; // ha ha
  polarity_type polarity; // true = normal, false = reversed
  rotation_type rotation;
  bool operator<(const Relation &r) const {
    if (obj->get_type() != r.obj->get_type()) return obj->get_type() < r.obj->get_type();
    if (type != r.type) return type < r.type;
    if (ordinal != r.ordinal) return ordinal < r.ordinal;
  // objects can change id, so this is
  // not a reliable sort criterion  if (obj->get_id() != r.obj->get_id()) return obj->get_id() < r.obj->get_id();
    return false;
  }
  bool operator==(const Relation &r) const {
    return obj == r.obj &&
           ordinal == r.ordinal &&
           polarity == r.polarity &&
           type == r.type &&
           rotation == r.rotation;
  }
};
 typedef MeshObj_id_type id_type;
 friend class Mesh;
 friend class MeshDB;
 friend class Kernel;
 typedef enum {NONE = 0x0, NODE = 0x01, EDGE=0x02, FACE=0x04, ELEMENT=0x08, INTERP= 0x10, ANY=0xFF } MeshObjType;
 typedef enum {USES=0x01, USED_BY=0x02, PARENT=0x04, CHILD=0x08} RelationType;
/*
 MeshObj() :
   TreeNode<MeshObj_id_type>(0),
   type(NONE), data_index(0), meshset(NULL) {}
*/
 MeshObj(UChar _type, int _id, long _data_index=-1, int _owner=std::numeric_limits<int>::max());
 ~MeshObj();
 typedef std::vector<Relation> RelationList;
 RelationList Relations;
 // find the given object (if exists)
 bool operator <(const MeshObj &rhs) {return KeyOfValue() < rhs.KeyOfValue();}
 MeshObj::id_type get_id() const {return KeyOfValue();}
 const Kernel *GetKernel() const { return meshset;}
 Kernel *GetKernel() { return meshset;}
 void printdata() const;
 void printrelations(std::ostream &) const;
 //const MeshObjTopo * get_topo() const {return topo;}
 // Find the element opposite to this one's side 'side_ordinal'.  Return the oridnal
 // in the other elements numbering, rotation and polarity.  Return NULL if nothing there.
 UInt get_type() const {return type;}
 UInt get_owner() const { return owner;}

 // Long story; 
 UInt *get_owner_ptr() { return &owner; }
 void set_owner(UInt _owner) { owner = _owner;}
 long get_data_index() const { return data_index;}
 bool operator==(const MeshObj &r) const { return &r == this;}
 bool operator!=(const MeshObj &r) const { return &r != this;}
 void AssignStore(_fieldStore *s, UInt idx);
 const std::pair<_fieldStore *,UInt> &GetStore() const { return fstore;}
private:
 MeshObj(const MeshObj &rhs);
 MeshObj &operator=(const MeshObj &rhs);
 UChar type;
 long data_index;
 Kernel *meshset; // point back to mesh set
 UInt owner;  // owning proc
 std::pair<_fieldStore*,UInt> fstore;
 friend std::ostream &operator<<(std::ostream &os, const MeshObj &dc);

};

typedef MeshObj::RelationList MeshObjRelationList;

// Add a relation properly sorted
MeshObjRelationList::iterator AddMeshObjRelation(MeshObj &obj, const MeshObj::Relation &r);

std::string MeshObjTypeString(UInt);
std::string RelationTypeString(UInt);

const UInt NumMeshObjTypes = 4;
const UInt MeshObjTypes[] = {
  MeshObj::NODE,
  MeshObj::EDGE,
  MeshObj::FACE,
  MeshObj::ELEMENT
};

// Return converse of obj relation type
UInt MeshObjRelationConverse(UInt rel_type);

} //namespace
} //namespace


#endif
