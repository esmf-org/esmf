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
#ifndef ESMCI_MEFamily_h
#define ESMCI_MEFamily_h

#include <Mesh/include/ESMCI_MasterElement.h>

#include <Mesh/include/ESMCI_Quadrature.h>

#include <map>
#include <string>

namespace ESMCI {


/**
 * The basic role of this class is to provide for master elements
 * and mesh heterogeneity to interact suitably.  It also provides
 * dimension independence for a master element.
 *
 * This class provides a mesh topology (QUAD, HEX, TRI) to master
 * element mapping.  Thus we can register a field that lives over
 * say, a mesh of quads and tri's, and treat the field as a single
 * entity, rather than two distinct fields.  
 * Also, we can register an ME type that is dimension independant,
 * i.e. 10th order lagrange.  Provided the master element is
 * implemented, the MEFamily will hand back the correct element, and
 * the users code will not have to switch on this element type.
 */
class MEFamily {
public:
MEFamily() {}
virtual ~MEFamily() {}
// Get standard specific trait versions
virtual MasterElement<METraits<> > *getME(const std::string &toponame, METraits<>) const = 0;

template<typename METRAITS>
MasterElement<METRAITS> *getME(const std::string &toponame, METRAITS) const {
  return getME(toponame, METraits<>())->operator()(METRAITS());
}

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

// High order lagrange
class MEFLagrange : public MEFamily {
MEFLagrange(UInt order);
public:
bool is_nodal() const { return false;} 
bool is_elemental() const { return false;} 
static const MEFLagrange &instance(UInt order);

MasterElement<METraits<> > *getME(const std::string &toponame, METraits<>) const;
const std::string &name() const { return fname;}
private:
MEFLagrange(const MEFLagrange&);
const std::string fname;
UInt order;

};

// High order lagrange, DG
class MEFLagrangeDG : public MEFamily {
MEFLagrangeDG(UInt order);
public:
bool is_nodal() const { return false;} 
bool is_elemental() const { return true;} 
static const MEFLagrangeDG &instance(UInt order);

MasterElement<METraits<> > *getME(const std::string &toponame, METraits<>) const;
const std::string &name() const { return fname;}
private:
MEFLagrangeDG(const MEFLagrangeDG&);
const std::string fname;
UInt order;

};

} // namespace

#endif
