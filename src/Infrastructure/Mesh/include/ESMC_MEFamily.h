// $Id: ESMC_MEFamily.h,v 1.1.2.2 2009/01/21 21:25:22 cdeluca Exp $
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

#ifndef ESMC_MEFamily_h
#define ESMC_MEFamily_h

#include <ESMC_MasterElement.h>
#include <ESMC_MasterElementV.h>

#include <ESMC_Quadrature.h>

#include <map>
#include <string>

namespace ESMCI {
namespace MESH {

// A topo->me class switcher
class MEFamily {
public:
MEFamily() {}
virtual ~MEFamily() {}
// Get standard specific trait versions
virtual MasterElement<METraits<> > *getME(const std::string &toponame, METraits<>) const = 0;
MasterElement<METraits<> > *getME(const std::string &toponame) const;

MasterElement<METraits<double,fad_type> > *getME(const std::string &toponame, METraits<double,fad_type> ) const;
MasterElement<METraits<fad_type,double> > *getME(const std::string &toponame, METraits<fad_type,double> ) const;

// True if the dofs live strictly on the nodes. e.g. lagrangian
virtual bool is_nodal() const = 0;

// Default implementation using me for order, topo for type.
virtual const intgRule *GetIntg(const std::string &toponame) const;

// true if dofs live strictly on element. e.g. discontinous galerkin
virtual bool is_elemental() const = 0;

virtual const std::string &name() const = 0;
private:
MEFamily(const MEFamily&);
};

// A default MEfamily
class MEFamilyStd : public MEFamily {
MEFamilyStd();
static MEFamilyStd *classInstance;
public:
MasterElement<METraits<> > *getME(const std::string &toponame, METraits<>) const;
static const MEFamilyStd &instance();
bool is_nodal() const { return true;} // lagrange
bool is_elemental() const { return false;}


const std::string &name() const { return fname;}
private:
MEFamilyStd(const MEFamilyStd&);
const std::string fname;
};

// A Low order default
class MEFamilyLow : public MEFamily {
MEFamilyLow();
static MEFamilyLow *classInstance;
public:
bool is_nodal() const { return true;} // lagrange
bool is_elemental() const { return false;} 
static const MEFamilyLow &instance();

MasterElement<METraits<> > *getME(const std::string &toponame, METraits<>) const;
const std::string &name() const { return fname;}
private:
MEFamilyLow(const MEFamilyLow&);
const std::string fname;

};

// An element field me
class MEFamilyDG0 : public MEFamily {
MEFamilyDG0();
static MEFamilyDG0 *classInstance;
public:
bool is_nodal() const { return false;} 
bool is_elemental() const { return true;} 
static const MEFamilyDG0 &instance();

MasterElement<METraits<> > *getME(const std::string &toponame, METraits<>) const;
const std::string &name() const { return fname;}
private:
MEFamilyDG0(const MEFamilyDG0&);
const std::string fname;

};

// Hierarchical
class MEFamilyHier : public MEFamily {
MEFamilyHier(UInt order);
static std::map<UInt, MEFamilyHier *> classInstances;
public:
bool is_nodal() const { return false;} 
bool is_elemental() const { return false;} 
static const MEFamilyHier &instance(UInt order);

MasterElement<METraits<> > *getME(const std::string &toponame, METraits<>) const;
const std::string &name() const { return fname;}
private:
MEFamilyHier(const MEFamilyHier&);
const std::string fname;
UInt order;
// Store the base element, since these are not instanced.
std::map<std::string, MasterElement<METraits<> >*> meMap;

};

} // namespace
} // namespace

#endif
