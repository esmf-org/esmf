// $Id: ESMC_MeshObj.C,v 1.2.2.2 2009/01/21 21:25:23 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <ESMC_MeshObj.h>
#include <iostream>
#include <set>
#include <algorithm>
#include <ESMC_ParEnv.h>

namespace ESMCI {
namespace MESH {

MeshObj::MeshObj(UChar _type, int _id, long _data_index, int _owner) :
TreeNode<MeshObj_id_type>(_id), 
type(_type),
data_index(_data_index), 
meshset(NULL), 
owner(_owner),
fstore(NULL,0)
{
}


MeshObj::~MeshObj() {
  // Nothing to delete.  
}

void MeshObj::printrelations(std::ostream &os) const {
  os << "\tRelations:" << std::endl;
  for (MeshObjRelationList::const_iterator it = Relations.begin(); it != Relations.end(); it++) {
    std::string stype;
    const MeshObj::Relation &rel = (*it);
    stype = MeshObjTypeString(rel.obj->type);
    os << "\tid=" << rel.obj->get_id() << ", type=" << stype << ", ordinal=" << rel.ordinal <<
         ", polarity:" << rel.polarity << ", rotation:" << static_cast<int>(rel.rotation)
         << ", type:" << RelationTypeString(rel.type) << std::endl;
  }
}

void MeshObj::AssignStore(_fieldStore *s, UInt idx) {
  fstore.first = s;
  fstore.second = idx;
}

std::string MeshObjTypeString(UInt type) {
    switch(type) {
      case MeshObj::NODE:
        return "NODE";
      case MeshObj::EDGE:
        return "EDGE";
      case MeshObj::FACE:
         return "FACE";
      case MeshObj::ELEMENT:
         return "ELEMENT";
      default:
         return "UNKNOWN";
    }
}

std::string RelationTypeString(UInt type) {
    switch(type) {
      case MeshObj::USES:
        return "USES";
      case MeshObj::USED_BY:
        return "USED_BY";
      case MeshObj::PARENT:
         return "PARENT";
      case MeshObj::CHILD:
         return "CHILD";
      default:
         return "UNKNOWN";
    }
}

MeshObjRelationList::iterator AddMeshObjRelation(MeshObj &obj, const MeshObj::Relation &r) {
  MeshObjRelationList::iterator ip =
    std::lower_bound(obj.Relations.begin(), obj.Relations.end(), r);

  // Just make sure:  We could have several relations that
  // have the same obj_type, ordinal, type (think several
  // parents of a node ordinal 5)
  // To be certain that we do not have the relation already, we must traverse
  // these objects to look for our relation
  bool found = false;
  while(!found && ip != obj.Relations.end() 
      && ip->obj->get_type() == r.obj->get_type() 
      && ip->ordinal == r.ordinal 
      && ip->type == r.type) 
  {
    if (ip->obj == r.obj) found = true;
      else ++ip;
  }

  // A duplicate
  if (found) return ip;

  return obj.Relations.insert(ip, r);
}


void *MeshObj::operator new(std::size_t size) {
  return ObjPool<MeshObj>::instance()->Allocate(size);
}

void MeshObj::operator delete(void *p, std::size_t size) {
  ObjPool<MeshObj>::instance()->Deallocate(p, size);
}

UInt MeshObjRelationConverse(UInt rel_type) {
  switch(rel_type) {
    case MeshObj::CHILD:
      return MeshObj::PARENT;
    case MeshObj::PARENT:
      return MeshObj::CHILD;
    case MeshObj::USES:
      return MeshObj::USED_BY;
    case MeshObj::USED_BY:
      return MeshObj::USES;
    default:
      return 0;
  }
}

} //namespacd
} //namespacd
