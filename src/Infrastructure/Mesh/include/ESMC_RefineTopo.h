// $Id: ESMC_RefineTopo.h,v 1.1.2.2 2009/01/21 21:25:22 cdeluca Exp $
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

#ifndef ESMC_RefineTopo_h
#define ESMC_RefineTopo_h

#include <ESMC_MeshTypes.h>
#include <ESMC_MeshObjTopo.h>
#include <ESMC_Exception.h>

namespace ESMCI {
namespace MESH {

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
} // namespace

#endif
