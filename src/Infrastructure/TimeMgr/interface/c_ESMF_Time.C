// $Id: c_ESMF_Time.C,v 1.5 2003/02/11 18:38:59 eschwab Exp $
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

#include <ESMC_Time.h>

extern "C"
{
#if 0
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_esmf_timeinit_
//
// !INTERFACE:
	void c_esmf_timeinit_(ESMC_Time *ti, ESMF_IKIND_I8 *S, int *Sn, int *Sd,
				          ESMC_Calendar **cal, int *tz)
//
// !RETURN VALUE:


//
// !ARGUMENTS:
//	ESMC_Time *ti
//	ESMF_IKIND_I8 *S
//	int *Sn
//	int *Sd
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
		ti->ESMC_TimeInit(*S, *Sn, *Sd, *cal, *tz);
	} // end c_esmf_timeinit_
#endif
} // end extern "C"
