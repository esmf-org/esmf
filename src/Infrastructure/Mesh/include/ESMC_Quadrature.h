// $Id: ESMC_Quadrature.h,v 1.1.2.2 2009/01/21 21:25:22 cdeluca Exp $
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

#ifndef ESMC_Quadrature_h
#define ESMC_Quadrature_h
#include <string>

#include <ESMC_MeshTypes.h>
#include <ESMC_Exception.h>
#include <algorithm>
#include <map>
// Quadrature algorithms

namespace ESMCI { 
namespace MESH { 

// Generic interface/management of locs, wgts arrays
class intgRule {
public:
  intgRule(UInt _q, UInt _n, UInt _pdim);
  virtual ~intgRule();
  // Return the number of quadrature points
  UInt npoints() const {return n;}

  UInt order() const { return q; }

  // parametric locations
  const double *locations() const { return locs; }
  // weights
  const double *weights() const { return wgts; }

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
  arbq(UInt _pdim, UInt nq, const double pcoord[]) :
    intgRule(nq,nq,_pdim) {
    std::copy(pcoord, pcoord+(nq*pdim), locs);
  }
  ~arbq() {}
  const std::string &iname() const {return name;}

  const intgRule *ChangeOrder(UInt q) const {
    Throw() << "Arbq doesnt swap order";
  }
  private:
  static const std::string name;
};

// Gauss Legendre on [-1,1]
class barq : public intgRule {
public:
  static barq &instance(UInt q);
  ~barq();
  static const std::string name;
  const std::string &iname() const {return name;}
  const intgRule *ChangeOrder(UInt q) const {
    return &instance(q);
  }
private:
  barq(UInt ord);  // order of quadrature
  static std::map<UInt,barq*> classInstances;
};

// Gauss Legendre tensor on [-1,1]^2
class quadq : public intgRule {
public:
  static quadq &instance(UInt order);
  ~quadq();
  static const std::string name;
  const std::string &iname() const {return name;}
  const intgRule *ChangeOrder(UInt q) const {
    return &instance(q);
  }
private:
  quadq(UInt ord);  // order of quadrature
  static std::map<UInt,quadq*> classInstances;
};

class triq : public intgRule {
public:
  static triq &instance(UInt q);
  ~triq();
  static const std::string name;
  const std::string &iname() const {return name;}
  const intgRule *ChangeOrder(UInt q) const {
    return &instance(q);
  }
private:
  triq(UInt q);  // order of quadrature
  static std::map<UInt, triq*> classInstances;
};

// Gauss Legendre tensor on [-1,1]^3
class hexq : public intgRule {
public:
  static hexq &instance(UInt q);
  ~hexq();
  static const std::string name;
  const std::string &iname() const {return name;}
  const intgRule *ChangeOrder(UInt q) const {
    return &instance(q);
  }
private:
  hexq(UInt q);  // order of quadrature
  static std::map<UInt, hexq*> classInstances;
};

class tetraq : public intgRule {
public:
  static tetraq &instance(UInt q);
  ~tetraq();
  static const std::string name;
  const std::string &iname() const {return name;}
  const intgRule *ChangeOrder(UInt q) const {
    return &instance(q);
  }
private:
  tetraq(UInt q);  // order of quadrature
  static std::map<UInt, tetraq*> classInstances;
};


// Return the n point gauss-legendre quadrature points and weights.
// 1d, on [-1,1]
void gauss_legendre(UInt n, double locs[], double *wghts = NULL);

// Factory
struct Topo2Intg {
intgRule *operator()(UInt q, const std::string &toponame);
};

} // namespace
} // namespace

#endif
