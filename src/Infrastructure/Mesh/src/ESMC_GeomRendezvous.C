//==============================================================================
// $Id: ESMC_GeomRendezvous.C,v 1.1 2007/08/27 17:42:59 dneckels Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <ESMC_GeomRendezvous.h>
#include <ESMC_Exception.h>

namespace ESMCI {
namespace MESH {

GeomRend::GeomRend(const Mesh &_srcmesh, const Mesh &_dstmesh, constDstCfg &cfg) :
srcmesh(_srcmesh),
dstmesh(_dstmesh),
dcfg(cfg),
srcComm(),
dstComm(),
built(false)
{
	
}

void 	GeomRend::Build() {
	ThrowRequire(built == false);
	built = true;
	
	/*
	 * There are basically a few cases.
	 *   case 1: We want to provide a cover of nodes or nodes + interp
	 *   case 2: We must cover the cells themselves.
	 */
	  
}

} // namespace MESH
} // namespace ESMCI