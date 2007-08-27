//==============================================================================
// $Id: ESMC_GeomRendezvous.h,v 1.1 2007/08/27 17:42:59 dneckels Exp $
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
#ifndef ESMC_GeomRendezvous_h
#define ESMC_GeomRendezvous_h

namespace ESMCI {
namespace MESH {

class CommReg;
class Mesh;


	
class GeomRend {
public:
	/*
	 * Configurendezvous configuration for the source grid object.
	 * The source mesh interpolation region is created by looping through
	 * the iter_obj_type over the region specified by Context.
	 * Within these areas, all subordinate objects of obj_type are added
	 * to the geometric rendezvous.
	 */
	struct DstCfg {
		UInt iter_obj_type; // Object to iterate when building intersection 
		UInt obj_type; // One of node, node + interp, interp
		Context ctxt; // Context to match when iterating 
	};
	GeomRend(const Mesh &srcmesh, const Mesh &dstmesh, const DstCfg &config);
	~GeomRend();
	
	/*
	 * Build the geometric Rendezvous.
	 */
	void Build();
	
private:
	const Mesh &srcmesh;
	const Mesh &dstmesh;
	
	DstConfig dcfg;
	
	// The communication Registers from mesh to rendezvous mesh.
	CommReg &srcComm;
	CommReg &dstComm;
	bool built;
};
	
} // namespace
} // namespace

#endif
