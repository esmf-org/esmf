// $Id$
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_RefineTopo_h
#define ESMCI_RefineTopo_h

#include <Mesh/include/Legacy/ESMCI_MeshTypes.h>
#include <Mesh/include/Legacy/ESMCI_MeshObjTopo.h>
#include <Mesh/include/Legacy/ESMCI_Exception.h>

namespace ESMCI {

class RefineTopo {
public:
  virtual ~RefineTopo() {}
  virtual bool is_homogeneous() const = 0;
  virtual UInt NumChild() const = 0;
  virtual const UInt* ChildNode(UInt child) const = 0;
  virtual const MeshObjTopo *ChildTopo(UInt child) const = 0;
  virtual const RefineTopo *FaceRTopo(UInt face) const = 0;
  virtual const RefineTopo *EdgeRTopo(UInt face) const = 0;
  friend const RefineTopo *GetHomoRefineTopo(const MeshObjTopo*);
  const MeshObjTopo *GetParentTopo() const { return parent_topo; }
protected:
  RefineTopo(const MeshObjTopo *_tp) : parent_topo(_tp) {}
private:
  RefineTopo(const RefineTopo&);
  RefineTopo &operator=(const RefineTopo &);
  const MeshObjTopo *parent_topo;
};

// Class to describe a homogeneous refinement strategy
class HomoRefineTopo : public RefineTopo {
public:
  HomoRefineTopo(
                 const MeshObjTopo *parent_topo,
                 UInt _numChild,
                 const  MeshObjTopo *_ctopo,
                 const UInt *_childNode,
                 const  RefineTopo *_ftopo,
                 const RefineTopo *_etopo
                 ) :
  RefineTopo(parent_topo),
  numChild(_numChild),
  nchildNode(_ctopo->num_nodes),
  childNode(_childNode),
  ctopo(_ctopo),
  ftopo(_ftopo),
  etopo(_etopo)
  {
  }

  bool is_homogeneous() const { return true; }
  UInt NumChild() const { return numChild; }
  const UInt* ChildNode(UInt child) const
  { ThrowRequire(child < numChild); 
    return &childNode[child*nchildNode];
  }
  const MeshObjTopo *ChildTopo(UInt) const { return ctopo; }
  const RefineTopo *FaceRTopo(UInt) const { return ftopo; }
  const RefineTopo *EdgeRTopo(UInt) const { return etopo; }
private:
UInt numChild;
UInt nchildNode; // how many nodes a child has
const UInt *childNode;
const MeshObjTopo *ctopo;
const RefineTopo *ftopo;
const RefineTopo *etopo;
};

const RefineTopo *GetHomoRefineTopo(const MeshObjTopo*);

} // namespace

#endif
