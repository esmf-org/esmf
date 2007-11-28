// $Id: ESMC_RefineTopo.h,v 1.2 2007/11/28 16:23:22 dneckels Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMC_RefineTopo_h
#define ESMC_RefineTopo_h

#include <mesh/ESMC_MeshTypes.h>
#include <mesh/ESMC_MeshObjTopo.h>
#include <mesh/ESMC_Exception.h>

namespace ESMC {

class RefineTopo {
public:
  virtual ~RefineTopo() {}
  virtual bool is_homogeneous() const = 0;
  virtual UInt NumChild() const = 0;
  virtual const UInt* ChildNode(UInt child) const = 0;
  virtual const MeshObjTopo *ChildTopo(UInt child) const = 0;
  virtual const RefineTopo *FaceRTopo(UInt face) const = 0;
  virtual const RefineTopo *EdgeRTopo(UInt face) const = 0;
private:
};

// Class to describe a homogeneous refinement strategy
class HomoRefineTopo : public RefineTopo {
public:
  HomoRefineTopo(UInt _numChild,
                 const  MeshObjTopo *_ctopo,
                 const UInt *_childNode,
                 const  RefineTopo *_ftopo,
                 const RefineTopo *_etopo
                 ) :
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

const RefineTopo *GetHomoRefineTopo(const std::string &tname);

} // namespace

#endif
