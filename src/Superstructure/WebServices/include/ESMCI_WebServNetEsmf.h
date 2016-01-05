// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//-------------------------------------------------------------------------
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMCI_WebServNetEsmf_H
#define ESMCI_WebServNetEsmf_H


namespace ESMCI
{

// Default port number for socket service... really shouldn't use this
#define NET_ESMF_PORT	27001

// Request identifiers
#define NET_ESMF_EXIT				10
#define NET_ESMF_NEW	   			15
#define NET_ESMF_INIT				20
#define NET_ESMF_RUN					21
#define NET_ESMF_FINAL				22
#define NET_ESMF_STATE				23
#define NET_ESMF_FILES				24
#define NET_ESMF_INIT_DONE			25
#define NET_ESMF_RUN_DONE			26
#define NET_ESMF_FINAL_DONE		27
#define NET_ESMF_DATA_DESC			28
#define NET_ESMF_DATA				29
#define NET_ESMF_TIMESTEP			30
#define NET_ESMF_TIMESTEP_DONE	31
#define NET_ESMF_END					40
#define NET_ESMF_UNKN				50
#define NET_ESMF_PING				99

// State values
#define NET_ESMF_STAT_IDLE				0
#define NET_ESMF_STAT_READY			1
#define NET_ESMF_STAT_BUSY				2
#define NET_ESMF_STAT_INITIALIZING	3
#define NET_ESMF_STAT_RUNNING			4
#define NET_ESMF_STAT_FINALIZING		5
#define NET_ESMF_STAT_INIT_DONE		6
#define NET_ESMF_STAT_RUN_DONE		7
#define NET_ESMF_STAT_FINAL_DONE		8
#define NET_ESMF_STAT_DONE				9
#define NET_ESMF_STAT_SUBMITTED		10
#define NET_ESMF_STAT_TIMESTEP_DONE	11
#define NET_ESMF_STAT_ERROR			99


#define COMP_SVR_STATE_PENDING			"PENDING"
#define COMP_SVR_STATE_READY				"READY"


} // end namespace

#endif 	// ESMCI_WebServNetEsmf_H
