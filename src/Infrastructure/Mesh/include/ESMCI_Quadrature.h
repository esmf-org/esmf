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
#ifndef ESMCI_Quadrature_h
#define ESMCI_Quadrature_h

#include <Mesh/include/ESMCI_MeshTypes.h>
#include <Mesh/include/ESMCI_Exception.h>

#include <string>
#include <algorithm>
#include <map>
#include <vector>
// Quadrature algorithms

namespace ESMCI { 

class MeshObjTopo;

// Generic interface/management of locs, wgts arrays
class intgRule {
public:
  intgRule(UInt _q, UInt _n, UInt _pdim);
  virtual ~intgRule();
  // Return the number of quadrature points
  UInt npoints() const {return n;}

  UInt order() const { return q; }

  UInt parametric_dim() const { return pdim; }

  // parametric locations
  const double *locations() const { return locs; }
  // weights
  const double *weights() const { return wgts; }

  virtual const intgRule *side_rule() const = 0;

  virtual const std::string &iname() const = 0;

  // Swap out for a rule of same type, different order.
  virtual const intgRule *ChangeOrder(UInt q) const = 0;
  
protected:
  UInt q;
  UInt n;
  UInt pdim;
  double *locs;
  double *wgts;
};

// Assign arbitrary parametric coords, null weights
class arbq : public intgRule {
public:
  arbq(UInt _pdim, UInt nq, const double pcoord[], const double *_wgts=NULL);
  ~arbq() {}
  const std::string &iname() const {return name;}

  const intgRule *side_rule() const {
    Throw() << "No side rule for arbq";
  }

  const intgRule *ChangeOrder(UInt q) const {
    Throw() << "Arbq doesnt swap order";
  }
  private:
  const std::string name;
};

// Gauss Legendre on [-1,1]
class barq : public intgRule {
public:
  static barq &instance(UInt q);
  ~barq();
  const std::string name;
  const std::string &iname() const {return name;}
  const intgRule *ChangeOrder(UInt q) const {
    return &instance(q);
  }
  const intgRule *side_rule() const {
    Throw() << "No side rule for bar";
  }
private:
  barq(UInt ord);  // order of quadrature
};

// Gauss Legendre tensor on [-1,1]^2
class quadq : public intgRule {
public:
  static quadq &instance(UInt order);
  ~quadq();
  const std::string name;
  const std::string &iname() const {return name;}
  const intgRule *ChangeOrder(UInt q) const {
    return &instance(q);
  }
  const intgRule *side_rule() const {
    return &barq::instance(q);
  }
private:
  quadq(UInt ord);  // order of quadrature
};

class triq : public intgRule {
public:
  static triq &instance(UInt q);
  ~triq();
  const std::string name;
  const std::string &iname() const {return name;}
  const intgRule *ChangeOrder(UInt q) const {
    return &instance(q);
  }
  const intgRule *side_rule() const {
    return &barq::instance(q);
  }
private:
  triq(UInt q);  // order of quadrature
};

// Gauss Legendre tensor on [-1,1]^3
class hexq : public intgRule {
public:
  static hexq &instance(UInt q);
  ~hexq();
  const std::string name;
  const std::string &iname() const {return name;}
  const intgRule *ChangeOrder(UInt q) const {
    return &instance(q);
  }
  const intgRule *side_rule() const {
    return &quadq::instance(q);
  }
private:
  hexq(UInt q);  // order of quadrature
};

class tetraq : public intgRule {
public:
  static tetraq &instance(UInt q);
  ~tetraq();
  const std::string name;
  const std::string &iname() const {return name;}
  const intgRule *ChangeOrder(UInt q) const {
    return &instance(q);
  }
  const intgRule *side_rule() const {
    return &triq::instance(q);
  }
private:
  tetraq(UInt q);  // order of quadrature
};


// Return the n point gauss-legendre quadrature points and weights.
// 1d, on [-1,1]
void gauss_legendre(UInt n, double locs[], double *wghts = NULL);

// Factory
struct Topo2Intg {
intgRule *operator()(UInt q, const std::string &toponame);
};

/**
 * An set of integration rule factories for
 * various topology types and lower dimensional
 * integration rules.
 */
class sideIntgFactory {
public:
  const intgRule *GetSideRule(UInt side_num) const {
    return side_rules[side_num];
  }

  /**
   * Create a factory given the topology type and the base (lower dimensional rule)
   */
  static const sideIntgFactory *instance(const std::string &toponame, const intgRule *base_rule);

  typedef std::map<std::pair<std::string,const intgRule*>, sideIntgFactory*> InstanceMap;
private:
  sideIntgFactory(const MeshObjTopo *topo, const intgRule *irule);
  sideIntgFactory(const sideIntgFactory&);
  sideIntgFactory &operator=(const sideIntgFactory&);

  std::vector<const intgRule*> side_rules;
  const MeshObjTopo *topo;
  const intgRule *base;
};

} // namespace

#endif
