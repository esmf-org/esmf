// $Id: c_ESMF_TimeInstant.C,v 1.3 2002/10/30 19:53:49 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the GPL.
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// < insert a paragraph or two explaining what you'll find in this file >
//
//-----------------------------------------------------------------------------
//

#include <ESMC_Types.h>
#include <ESMC_TimeInstant.h>

extern "C"
{
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_esmf_timeinstantinit_
//
// !INTERFACE:
	void c_esmf_timeinstantinit_(ESMC_TimeInstant *ti, int64 *S, int32 *Sn, int32 *Sd,
				     ESMC_Calendar **cal, int *tz)
//
// !RETURN VALUE:


//
// !ARGUMENTS:
//	ESMC_TimeInstant *ti
//	int64 *S
//	int32 *Sn
//	int32 *Sd
//	ESMC_Calendar **cal
//	int *tz

//
// !DESCRIPTION:
//
//
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

	{
		ti->ESMC_TimeInstInit(*S, *Sn, *Sd, *cal, *tz);
	} // end c_esmf_timeinstantinit_
} // end extern "C"
