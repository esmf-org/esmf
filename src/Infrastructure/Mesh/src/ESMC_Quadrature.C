// $Id: ESMC_Quadrature.C,v 1.1.2.2 2009/01/21 21:25:23 cdeluca Exp $
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
#include <ESMC_Quadrature.h>
#include <ESMC_Exception.h>

#include <cmath>

#include <iostream>
#include <vector>

#include <sstream>

namespace ESMCI {
namespace MESH {

std::string int2string(UInt i) {
  std::ostringstream oss;
  oss << i;
  return oss.str();
}

// Straight from numerical recipes in C
void gauss_legendre(UInt n, double locs[], double *wgts) {
  UInt m;
  double z, z1, pp, p1, p2, p3;
  
  m = (n+1)/2;
  for (UInt i = 1; i <= m; i++) {
    z = cos(M_PI*(i-0.25)/(n+0.5));
    do {

      p1 = 1.0; p2 = 0.0;
      for (UInt j = 1; j <= n; j++) {
        p3 = p2;
        p2 = p1;
        p1 = ((2.0*j-1.0)*z*p2-(j-1)*p3)/j;
      }

      pp = n*(z*p1-p2)/(z*z-1.0);
      z1 = z;
      z=z1-p1/pp;
//std::cout << "z-z1=" << z-z1 << std::endl;
    } while(std::fabs(z-z1)> 1e-10);

    locs[i-1] = -z;
    locs[n+1-i-1] = z;
    if (wgts) {
      wgts[i-1] = 2.0/((1.0-z*z)*pp*pp);
      wgts[n+1-i-1] = wgts[i-1];
    }
  } // for i
}


intgRule::intgRule(UInt _q, UInt _n, UInt _pdim) :
q(_q),
n(_n),
pdim(_pdim),
locs(new double[n*pdim]),
wgts(new double[n])
{
}

intgRule::~intgRule() {
  delete [] locs;
  delete [] wgts;
}

const std::string arbq::name("arbq");

// ** 1D bar
std::map<UInt,barq*> barq::classInstances;

const std::string barq::name("barq");

barq &barq::instance(UInt q) {
  std::map<UInt,barq*>::iterator qi = classInstances.find(q);
  barq *qp;
  if (qi == classInstances.end()) {
    qp = new barq(q);
    classInstances[q] = qp;
  } else qp = qi->second;
  
  return *qp;
}

barq::barq(UInt q) :
intgRule(q, q, 1)
{
  gauss_legendre(n, locs, wgts);
}

barq::~barq() {
}

// ** 2D quadrilateral
std::map<UInt,quadq*> quadq::classInstances;

const std::string quadq::name("quadq");

quadq &quadq::instance(UInt q) {
  std::map<UInt,quadq*>::iterator qi = classInstances.find(q);
  quadq *qp;
  if (qi == classInstances.end()) {
    qp = new quadq(q);
    classInstances[q] = qp;
  } else qp = qi->second;
  
  return *qp;
}

quadq::quadq(UInt tq) :
intgRule(tq, tq*tq, 2)
{
  std::vector<double> tlocs(q), twgts(q);
  gauss_legendre(q, &tlocs[0], &twgts[0]);

  for (UInt i = 0; i < q; i++) {
    for (UInt j = 0; j < q; j++) {
      locs[(i*q+j)*pdim] = tlocs[i];
      locs[(i*q+j)*pdim+1] = tlocs[j];
      wgts[i*q+j] = twgts[i]*twgts[j];
    }
  }
}

quadq::~quadq() {
}

// ** 2D triangle
std::map<UInt,triq*> triq::classInstances;

const std::string triq::name("triq");

triq &triq::instance(UInt q) {
  std::map<UInt,triq*>::iterator qi = classInstances.find(q);
  triq *qp;
  if (qi == classInstances.end()) {
    qp = new triq(q);
    classInstances[q] = qp;
  } else qp = qi->second;
  
  return *qp;
}

static UInt tri_order2q(UInt o) {
  UInt mp[] = {
  0, 1, 3, 4, 6, 7, 12, 13
  };

  return mp[o];
}

triq::triq(UInt tq) :
intgRule(tq, tri_order2q(tq), 2)
{

  if( tq == 1){
    wgts[0]  =  0.50000000000000000;
    locs[0]     =  0.33333333333333333;
    locs[1]     =  0.33333333333333333;
  }
  else if( tq == 2){
    wgts[0]  =  0.16666666666666667;
    wgts[1]  =  0.16666666666666667;
    wgts[2]  =  0.16666666666666667;
    locs[0]     =  0.16666666666666667;
    locs[1]     =  0.16666666666666667;
    locs[2]     =  0.66666666666666667;
    locs[3]     =  0.16666666666666667;
    locs[4]     =  0.16666666666666667;
    locs[5]     =  0.66666666666666667;
  }
  else if( tq == 3){
    wgts[0]  = -0.28125000000000000;
    wgts[1]  =  0.26041666666666663;
    wgts[2]  =  0.26041666666666663;
    wgts[3]  =  0.26041666666666663;
    locs[0]     =  0.33333333333333333;
    locs[1]     =  0.33333333333333333;
    locs[2]     =  0.20000000000000000;
    locs[3]     =  0.20000000000000000;
    locs[4]     =  0.60000000000000000;
    locs[5]     =  0.20000000000000000;
    locs[6]     =  0.20000000000000000;
    locs[7]     =  0.60000000000000000;
  }
  else if( tq == 4){
    wgts[0]  =  0.0549758718276610;
    wgts[1]  =  0.0549758718276610;
    wgts[2]  =  0.0549758718276610;
    wgts[3]  =  0.1116907948390055;
    wgts[4]  =  0.1116907948390055;
    wgts[5]  =  0.1116907948390055;
    locs[0]     =  0.091576213509771;
    locs[1]     =  0.091576213509771;
    locs[2]     =  0.816847572980459;
    locs[3]     =  0.091576213509771;
    locs[4]     =  0.091576213509771;
    locs[5]     =  0.816847572980459;
    locs[6]     =  0.445948490915965;
    locs[7]     =  0.445948490915965;
    locs[8]     =  0.108103018168070;
    locs[9]     =  0.445948490915965;
    locs[10]    =  0.445948490915965;
    locs[11]    =  0.108103018168070;
  }
  else if( tq == 5){
    wgts[0]  =  0.1125000000000000;
    wgts[1]  =  0.0629695902724135;
    wgts[2]  =  0.0629695902724135;
    wgts[3]  =  0.0629695902724135;
    wgts[4]  =  0.066197076394253;
    wgts[5]  =  0.066197076394253;
    wgts[6]  =  0.066197076394253;
    locs[0]     =  0.333333333333333;
    locs[1]     =  0.333333333333333;
    locs[2]     =  0.101286507323456;
    locs[3]     =  0.101286507323456;
    locs[4]     =  0.797426985353087;
    locs[5]     =  0.101286507323456;
    locs[6]     =  0.101286507323456;
    locs[7]     =  0.797426985353087;
    locs[8]     =  0.470142064105115;
    locs[9]     =  0.470142064105115;
    locs[10]    =  0.059715871789770;
    locs[11]    =  0.470142064105115;
    locs[12]    =  0.470142064105115;
    locs[13]    =  0.059715871789770;
  }
  else if( tq == 6){
    wgts[0]  =  0.0254224531851035;
    wgts[1]  =  0.0254224531851035;
    wgts[2]  =  0.0254224531851035;
    wgts[3]  =  0.0583931378631895;
    wgts[4]  =  0.0583931378631895;
    wgts[5]  =  0.0583931378631895;
    wgts[6]  =  0.041425537809187;
    wgts[7]  =  0.041425537809187;
    wgts[8]  =  0.041425537809187;
    wgts[9]  =  0.041425537809187;
    wgts[10] =  0.041425537809187;
    wgts[11] =  0.041425537809187;
    locs[0]     =  0.063089014491502;
    locs[1]     =  0.063089014491502;
    locs[2]     =  0.873821971016996;
    locs[3]     =  0.063089014491502;
    locs[4]     =  0.063089014491502;
    locs[5]     =  0.873821971016996;
    locs[6]     =  0.249286745170910;
    locs[7]     =  0.249286745170910;
    locs[8]     =  0.501426509658179;
    locs[9]     =  0.249286745170910;
    locs[10]    =  0.249286745170910;
    locs[11]    =  0.501426509658179;
    locs[12]    =  0.310352451033785;
    locs[13]    =  0.053145049844816;
    locs[14]    =  0.053145049844816;
    locs[15]    =  0.310352451033785;
    locs[16]    =  0.636502499121399;
    locs[17]    =  0.053145049844816;
    locs[18]    =  0.053145049844816;
    locs[19]    =  0.636502499121399;
    locs[20]    =  0.636502499121399;
    locs[21]    =  0.310352451033785;
    locs[22]    =  0.310352451033785;
    locs[23]    =  0.636502499121399;
  }
  else if( tq == 7){
    wgts[0]  = -0.074785022233835;
    wgts[1]  =  0.087807628716602;
    wgts[2]  =  0.087807628716602;
    wgts[3]  =  0.087807628716602;
    wgts[4]  =  0.0266736178044195;
    wgts[5]  =  0.0266736178044195;
    wgts[6]  =  0.0266736178044195;
    wgts[7]  =  0.0385568804451285;
    wgts[8]  =  0.0385568804451285;
    wgts[9]  =  0.0385568804451285;
    wgts[10] =  0.0385568804451285;
    wgts[11] =  0.0385568804451285;
    wgts[12] =  0.0385568804451285;
    locs[0]     =  0.333333333333333;
    locs[1]     =  0.333333333333333;
    locs[2]     =  0.260345966079038;
    locs[3]     =  0.260345966079038;
    locs[4]     =  0.479308067841923;
    locs[5]     =  0.260345966079038;
    locs[6]     =  0.260345966079038;
    locs[7]     =  0.479308067841923;
    locs[8]     =  0.065130102902216;
    locs[9]     =  0.065130102902216;
    locs[10]    =  0.869739794195568;
    locs[11]    =  0.065130102902216;
    locs[12]    =  0.065130102902216;
    locs[13]    =  0.869739794195568;
    locs[14]    =  0.312865496004874;
    locs[15]    =  0.048690315425316;
    locs[16]    =  0.048690315425316;
    locs[17]    =  0.312865496004874;
    locs[18]    =  0.638444188569809;
    locs[19]    =  0.048690315425316;
    locs[20]    =  0.048690315425316;
    locs[21]    =  0.638444188569809;
    locs[22]    =  0.638444188569809;
    locs[23]    =  0.312865496004874;
    locs[24]    =  0.312865496004874;
    locs[25]    =  0.638444188569809;
  }
  else {
    Throw() << "Quadrature of order "<<q<<" not available on triangles";
  }
}

triq::~triq() {
}

// ** 3D hexahedron
std::map<UInt,hexq*> hexq::classInstances;

const std::string hexq::name("hexq");

hexq &hexq::instance(UInt q) {
  std::map<UInt,hexq*>::iterator qi = classInstances.find(q);
  hexq *qp;
  if (qi == classInstances.end()) {
    qp = new hexq(q);
    classInstances[q] = qp;
  } else qp = qi->second;
  
  return *qp;
}

hexq::hexq(UInt tq) :
intgRule(tq, tq*tq*tq, 3)
{
  std::vector<double> tlocs(q), twgts(q);
  gauss_legendre(q, &tlocs[0], &twgts[0]);

  for (UInt i = 0; i < q; i++) {
    for (UInt j = 0; j < q; j++) {
      for (UInt k = 0; k < q; k++) {
        locs[((i*q+j)*q + k)*pdim] = tlocs[i];
        locs[((i*q+j)*q + k)*pdim+1] = tlocs[j];
        locs[((i*q+j)*q + k)*pdim+2] = tlocs[k];
        wgts[(i*q + j)*q + k] = twgts[i]*twgts[j]*twgts[k];
      }
    }
  }
}

hexq::~hexq() {
}

// ** 3D Tetrahedron
std::map<UInt,tetraq*> tetraq::classInstances;

const std::string tetraq::name("tetraq");

tetraq &tetraq::instance(UInt q) {
  std::map<UInt,tetraq*>::iterator qi = classInstances.find(q);
  tetraq *qp;
  if (qi == classInstances.end()) {
    qp = new tetraq(q);
    classInstances[q] = qp;
  } else qp = qi->second;
  
  return *qp;
}

static UInt tetra_order2q(UInt o) {
  UInt mp[] = {
  0, 1, 4, 5, 11, 15
  };

  return mp[o];
}

tetraq::tetraq(UInt tq) :
intgRule(tq, tetra_order2q(tq), 3)
{

  if( tq == 1){
    wgts[0]  =  0.1666666666666667;
    locs[0]     =  0.2500000000000000;
    locs[1]     =  0.2500000000000000;
    locs[2]     =  0.2500000000000000;
  }
  else if( tq == 2){
    wgts[0]  =  0.0416666666666667;
    wgts[1]  =  0.0416666666666667;
    wgts[2]  =  0.0416666666666667;
    wgts[3]  =  0.0416666666666667;

    locs[0]     =  0.1381966011250150;
    locs[1]     =  0.1381966011250150;
    locs[2]     =  0.1381966011250150;

    locs[3]     =  0.5854101966249685;
    locs[4]     =  0.1381966011250150;
    locs[5]     =  0.1381966011250150;

    locs[6]     =  0.1381966011250150;
    locs[7]     =  0.5854101966249685;
    locs[8]     =  0.1381966011250150;

    locs[9]     =  0.1381966011250150;
    locs[10]    =  0.1381966011250150;
    locs[11]    =  0.5854101966249685;
  }
  else if( tq == 3){
    wgts[0]  = -0.1333333333333333;
    wgts[1]  =  0.0750000000000000;
    wgts[2]  =  0.0750000000000000;
    wgts[3]  =  0.0750000000000000;
    wgts[4]  =  0.0750000000000000;
    locs[0]     =  0.2500000000000000;
    locs[1]     =  0.2500000000000000;
    locs[2]     =  0.2500000000000000;
    locs[3]     =  0.1666666666666667;
    locs[4]     =  0.1666666666666667;
    locs[5]     =  0.1666666666666667;
    locs[6]     =  0.5000000000000000;
    locs[7]     =  0.1666666666666667;
    locs[8]     =  0.1666666666666667;
    locs[9]     =  0.1666666666666667;
    locs[10]    =  0.5000000000000000;
    locs[11]    =  0.1666666666666667;
    locs[12]    =  0.1666666666666667;
    locs[13]    =  0.1666666666666667;
    locs[14]    =  0.5000000000000000;
  }
  else if( tq == 4){
    wgts[0]  = -0.013155555555555555;
    wgts[1]  =  0.00762222222222222222;
    wgts[2]  =  0.00762222222222222222;
    wgts[3]  =  0.00762222222222222222;
    wgts[4]  =  0.00762222222222222222;
    wgts[5]  =  0.0248888888888888880;
    wgts[6]  =  0.0248888888888888880;
    wgts[7]  =  0.0248888888888888880;
    wgts[8]  =  0.0248888888888888880;
    wgts[9]  =  0.0248888888888888880;
    wgts[10] =  0.0248888888888888880;
    locs[0]     =  0.2500000000000000;
    locs[1]     =  0.2500000000000000;
    locs[2]     =  0.2500000000000000;
    locs[3]     =  0.0714285714285714285;
    locs[4]     =  0.0714285714285714285;
    locs[5]     =  0.0714285714285714285;
    locs[6]     =  0.785714285714285714;
    locs[7]     =  0.0714285714285714285;
    locs[8]     =  0.0714285714285714285;
    locs[9]     =  0.0714285714285714285;
    locs[10]    =  0.785714285714285714;
    locs[11]    =  0.0714285714285714285;
    locs[12]    =  0.0714285714285714285;
    locs[13]    =  0.0714285714285714285;
    locs[14]    =  0.785714285714285714;
    locs[15]    =  0.399403576166799219;
    locs[16]    =  0.100596423833200785;
    locs[17]    =  0.100596423833200785;
    locs[18]    =  0.100596423833200785;
    locs[19]    =  0.399403576166799219;
    locs[20]    =  0.100596423833200785;
    locs[21]    =  0.100596423833200785;
    locs[22]    =  0.100596423833200785;
    locs[23]    =  0.399403576166799219;
    locs[24]    =  0.399403576166799219;
    locs[25]    =  0.399403576166799219;
    locs[26]    =  0.100596423833200785;
    locs[27]    =  0.399403576166799219;
    locs[28]    =  0.100596423833200785;
    locs[29]    =  0.399403576166799219;
    locs[30]    =  0.100596423833200785;
    locs[31]    =  0.399403576166799219;
    locs[32]    =  0.399403576166799219;
  }
  else if( tq == 5){
    wgts[0]  =  0.0302836780970891856;
    wgts[1]  =  0.00602678571428571597;
    wgts[2]  =  0.00602678571428571597;
    wgts[3]  =  0.00602678571428571597;
    wgts[4]  =  0.00602678571428571597;
    wgts[5]  =  0.0116452490860289742;
    wgts[6]  =  0.0116452490860289742;
    wgts[7]  =  0.0116452490860289742;
    wgts[8]  =  0.0116452490860289742;
    wgts[9]  =  0.0109491415613864534;
    wgts[10] =  0.0109491415613864534;
    wgts[11] =  0.0109491415613864534;
    wgts[12] =  0.0109491415613864534;
    wgts[13] =  0.0109491415613864534;
    wgts[14] =  0.0109491415613864534;
    locs[0]     =  0.2500000000000000;
    locs[1]     =  0.2500000000000000;
    locs[2]     =  0.2500000000000000;
    locs[3]     =  0.3333333333333333;
    locs[4]     =  0.3333333333333333;
    locs[5]     =  0.3333333333333333;
    locs[6]     =  0.0000000000000000;
    locs[7]     =  0.3333333333333333;
    locs[8]     =  0.3333333333333333;
    locs[9]     =  0.3333333333333333;
    locs[10]    =  0.0000000000000000;
    locs[11]    =  0.3333333333333333;
    locs[12]    =  0.3333333333333333;
    locs[13]    =  0.3333333333333333;
    locs[14]    =  0.0000000000000000;
    locs[15]    =  0.0909090909090909091;
    locs[16]    =  0.0909090909090909091;
    locs[17]    =  0.0909090909090909091;
    locs[18]    =  0.727272727272727273;
    locs[19]    =  0.0909090909090909091;
    locs[20]    =  0.0909090909090909091;
    locs[21]    =  0.0909090909090909091;
    locs[22]    =  0.727272727272727273;
    locs[23]    =  0.0909090909090909091;
    locs[24]    =  0.0909090909090909091;
    locs[25]    =  0.0909090909090909091;
    locs[26]    =  0.727272727272727273;
    locs[27]    =  0.0665501535736642813;
    locs[28]    =  0.433449846426335728;
    locs[29]    =  0.433449846426335728;
    locs[30]    =  0.433449846426335728;
    locs[31]    =  0.0665501535736642813;
    locs[32]    =  0.433449846426335728;
    locs[33]    =  0.433449846426335728;
    locs[34]    =  0.433449846426335728;
    locs[35]    =  0.0665501535736642813;
    locs[36]    =  0.0665501535736642813;
    locs[37]    =  0.0665501535736642813;
    locs[38]    =  0.433449846426335728;
    locs[39]    =  0.0665501535736642813;
    locs[40]    =  0.433449846426335728;
    locs[41]    =  0.0665501535736642813;
    locs[42]    =  0.433449846426335728;
    locs[43]    =  0.0665501535736642813;
    locs[44]    =  0.0665501535736642813;
  }
  else {
    Throw() << "Quadrature of order "<<q<<" not available on tetra";
  }
}

tetraq::~tetraq() {
}

// Factory
intgRule *Topo2Intg::operator()(UInt q, const std::string &name) {

  // _L selects the higher order mapping, the low order field

  if (name == "QUAD" || name == "QUAD4" || name == "QUAD_3D") {
      return &quadq::instance(q);
  }
  if (name == "QUAD9") {
      return &quadq::instance(q);
  } else if (name == "TRI" || name == "TRI3" || name == "TRI3_3D") {
      return &triq::instance(q);
  }
  else if (name == "SHELL3" || name == "SHELL3_L") {
      return &triq::instance(q);
  }
  else if (name == "SHELL" || name == "SHELL4" || name == "SHELL_L" || name == "SHELL4_L") {
      return &quadq::instance(q);
  }
  else if (name == "SHELL9_L") {
      return &quadq::instance(q);
  }
  else if (name == "SHELL9") {
      return &quadq::instance(q);
  }
  else if (name == "HEX" || name == "HEX_L" || name == "HEX8") {
    return &hexq::instance(q);
  }
  else if (name == "TETRA" || name == "TETRA4") {
    return &tetraq::instance(q);
  }
  else if (name == "BAR2_3D" || name == "BAR3_3D" || name == "BAR2_2D" || name == "BAR2") {
    return &barq::instance(q);
  }

  Throw() << "Can't find intg  for topo " << name << ", integration=" << q;
}

} // namespace
} // namespace
