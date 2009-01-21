// $Id: ESMC_MeshObjConn.h,v 1.2.2.2 2009/01/21 21:25:22 cdeluca Exp $
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

#ifndef ESMC_MeshObjConn_h
#define ESMC_MeshObjConn_h

#include <ESMC_MeshObj.h>
#include <ESMC_CommRel.h>

#include <set>


// This file abstracts things that depend on a mesh object and a meshh object topology.
// This is necessary since the MeshObj Cannot directly access its topo.

namespace ESMCI {
namespace MESH {

namespace MeshObjConn {

MeshObj *opposite_element(const MeshObj &obj, const int side_ordinal);

// Return the side element oppostie a face, along with its polarity, rotation WRT
// the STANDARD orientation of the side from this element.  Note:the actual relation
// to a face may or may not have THAT orientation....  This function is used
// mainly when adding a face, so we know the face has the standard orientation, since
// it was added as such.
MeshObj *opposite_element(const MeshObj &obj, const int side_ordinal, int &ordinal,
             int &polarity, int &rotation);

MeshObjRelationList::const_iterator find_relation(const MeshObj &obj, UInt objtype, int ordinal, int rel_type);
MeshObjRelationList::iterator find_relation(MeshObj &obj, UInt objtype, int ordinal, int rel_type);

MeshObjRelationList::const_iterator find_relation(const MeshObj &obj, UInt objtype, int ordinal);
MeshObjRelationList::iterator find_relation(MeshObj &obj, UInt objtype, int ordinal);

// Return the first of a given type
MeshObjRelationList::const_iterator find_relation(const MeshObj &obj, UInt objtype);
MeshObjRelationList::iterator find_relation(MeshObj &obj, UInt objtype);

// Get all nodes connected to an object and the elements around this element
void PatchNodes(const MeshObj &elem, std::set<const MeshObj*> &nodes);

// Return (in out_obj) a list of all objects that have a rel_type to all objects in in_obj.
// For instance, to find all elements that use a set of nodes (rl_type=USES),
template<typename obj_iter>
void common_objs(obj_iter in_obj_begin, obj_iter in_obj_end, // set of object to have in common
                 UInt rel_type, // what relation the object must have to in_obj
                 UInt out_obj_type, // type of out object to select
                 std::vector<MeshObj*> &out_obj // list of results (vector size is set by routine)
                 );

// For a list of nodes and elems, return the ordinals and polarity of the elem
// wrt the edge, with standard edge polarity given by the ordering in nodes.
// Does not use edge relations to accomplish this, so the actual relations may
// have different orientations, depending on how the nodes are ordering in nodes.
// if must_find = true, throws when nodes are not an edge, else
// returns ordinal -1 if nodes not an edge of an element in list.
template<typename obj_iter>
void edge_info(obj_iter node_begin, obj_iter node_end,
               obj_iter elem_begin, obj_iter elem_end,
               int *ordinal, // out
               int *polarity, // out
               bool must_find = true
               );

// For a list of nodes and elems (1 or 2), return the ordinals and polarity of the elem
// wrt the face, with standard edge polarity given by the ordering in nodes.
// Does not use face relations to accomplish this, so the actual relations may
// have different orientations, depending on how the nodes are ordering in nodes.
// If must_find == false, then ordinal = -1 if not found on element.
template<typename obj_iter>
void face_info(obj_iter node_begin, obj_iter node_end, 
               obj_iter elem_begin, obj_iter elem_end,
               int *ordinal, // out
               int *polarity, // out
               int *rotation, // out
               bool must_find = true // if true, throws when not found, else uses ret value
               );

// For a child node, find the lowest order topology and nodes
// supporting the node; i.e. if the node is on an edge, send back the edge
// nodes and edge topo supporting the node, if face, etc...
void get_node_support(const MeshObj &node, // in
                       const MeshObjTopo *&topo, // out
                       UInt &ordinal, // out
                       std::vector<MeshObj*> &nodes // out
                      );

// Return the nodes an object USES (by querying topology, etc...).  Uses any
// parent that is found, and corrects orientation so that the nodes returned
// are guarenteed to be of the orientation of the object.
// if must_find = false, then returns true if nodes found, false if problems
bool get_obj_nodes(const MeshObj &obj, std::vector<MeshObj*> &nodes, bool must_find = true);

// Get the processors that might share this object by
// using its nodal USES relations.  Does not return the current proc.
// For a node, we get the smallest topology supporting the node, and run the
// intersection test on that.
// If the object is already resolved an should be in comm, just go straight to comm
void get_shared_procs(const MeshObj &obj, const CommRel &node_sym_spec, std::vector<UInt> &procs, bool obj_in_comm=false);

// Return the processors (other than this) that a node is shared with (different from above)
void get_node_sharing(MeshObj &node, const CommRel &node_sym_spec, std::vector<UInt> &procs);

// Remove USES and CHILD relations from all objects this obj has USED_BY, PARENT relations.
// Extracts a lower dimensional topological object from higher dimension objects, such as nodes
// from elements
void remove_back_relations(MeshObj &obj);

// Removes any USED_BY, PARENT relations from objects this guy USES/CHILD's.  Used to
// take out, for instance, and element, removing the relations on nodes.
// if child_only, doesn't remove USED back rels, only PARENT back rels
void remove_forward_relations(MeshObj &obj, bool child_only = false);

bool verify_parent_child_relations(MeshObj &obj);

// Return true if there is a USED_BY/USED pair for the object
bool obj_used(MeshObj &obj);

// Return the ordinal of the relation from obj to rel_obj or -1 if none
int get_ordinal(MeshObj &obj, MeshObj &rel_obj);

// Verify the polarity relations for edges
bool verify_edge_relations(Mesh &mesh);

// Verify the polarity relations for faces
bool verify_face_relations(Mesh &mesh);

} // namespace 

} // namespace
} // namespace

#endif
