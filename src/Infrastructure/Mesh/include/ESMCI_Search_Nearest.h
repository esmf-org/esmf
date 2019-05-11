// $Id$
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_SearchNearest_h
#define ESMCI_SearchNearest_h

#include <list>

#include <Mesh/include/ESMCI_OTree.h>
#include <Mesh/include/Regridding/ESMCI_Mapping.h>
#include <Mesh/include/Regridding/ESMCI_SearchFlags.h>
#include <Mesh/include/Regridding/ESMCI_WMat.h>
#include "PointList/include/ESMCI_PointList.h"

#include <vector>


namespace ESMCI {


struct Search_nearest_node_result {
  int dst_gid;
  double pcoord[3];
};

struct Search_nearest_result {
  std::vector<Search_nearest_node_result> nodes;

  UInt dst_gid;
  UInt src_gid;
};


typedef std::vector<Search_nearest_result*> SearchNearestResultList;

void SearchNearestSrcToDst(const PointList &src_pl, const PointList &dst_pl, int unmappedaction, SearchNearestResultList &result, bool set_dst_status, WMat &dst_status);

void ParSearchNearestSrcToDst(const PointList &src_pl, const PointList &dst_pl, int unmappedaction, SearchNearestResultList &result, bool set_dst_status, WMat &dst_status);

void SearchNearestSrcToDstNPnts(const PointList &src_pl, const PointList &dst_pl, int num_pnts, int unmappedaction, SearchNearestResultList &result, bool set_dst_status, WMat &dst_status);

void ParSearchNearestSrcToDstNPnts(const PointList &src_pl, const PointList &dst_pl, int num_pnts,  int unmappedaction, SearchNearestResultList &result, bool set_dst_status, WMat &dst_status);
// 
// void SearchNearestDstToSrc(const Mesh &src, const Mesh &dst, int unmappedaction, SearchNearestResultList &result);
// 
// void ParSearchNearestDstToSrc(const Mesh &src, const Mesh &dst, int unmappedaction, SearchNearestResultList &result);

} //namespace

#endif
