// $Id: c_ESMF_Time.C,v 1.3 2002/10/30 19:07:45 svasquez Exp $
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
#include <ESMC_Time.h>
#include <stdio.h>

extern "C"
{
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_esmf_timeinit_
//
// !INTERFACE:
	void c_esmf_timeinit_(ESMC_Time *t, int64 *S, int32 *Sn, int32 *Sd)
//
// !RETURN VALUE:


//
// !ARGUMENTS:
//	ESMC_Time *t
//	int64 *S
//	int32 *Sn
//	int32 *Sd

//
// !DESCRIPTION:
//      
//     
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

	{
		printf("c_esmf_timeinit_: S, Sn, Sd = %lld, %ld, %ld\n", *S, *Sn, *Sd);
		t->Init(*S, *Sn, *Sd);
	} // end c_esmf_timeinit_

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_esmf_timeprint_
//
// !INTERFACE:
	void c_esmf_timeprint_(ESMC_Time *t, int64 *S, int32 *Sn, int32 *Sd)

//
// !RETURN VALUE:


//
// !ARGUMENTS:
//	ESMC_Time *t
//	 int64 *S
//	 int32 *Sn
//	 int32 *Sd
//

// !DESCRIPTION:
//
//
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n


	{
		printf("c_esmf_timeprint_ entered\n");
		t->ESMC_TimePrint(S, Sn, Sd);
	} // end c_esmf_timeprint_


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_esmf_timereads_
//
// !INTERFACE:
	void c_esmf_timereads_(ESMC_Time *t, int64 *S)

//
// !RETURN VALUE:


//
// !ARGUMENTS:
//	ESMC_Time *t
//	int64 *S
//

// !DESCRIPTION:
//
//
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

	{
		printf("c_esmf_timereads_ entered\n");
		t->Read_S(S);
	} // end c_esmf_timereads_


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_esmf_timewrites_
//
// !INTERFACE:
	void c_esmf_timewrites_(ESMC_Time *t, int64 *S)


//
// !RETURN VALUE:


//
// !ARGUMENTS:
//	ESMC_Time *t
//	int64 *S
//

// !DESCRIPTION:
//
//
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

	{
		printf("c_esmf_timewrites_ entered\n");
		t->Write_S(*S);
	} // end c_esmf_timewrites_

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_esmf_timesum_
//
// !INTERFACE:
	void c_esmf_timesum_(ESMC_Time *time1, ESMC_Time *time2, ESMC_Time *sum)


//
// !RETURN VALUE:


//
// !ARGUMENTS:
//	ESMC_Time *time1
//	 ESMC_Time *time2
//	 ESMC_Time *sum

// !DESCRIPTION:
//
//
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

	{
		printf("c_esmf_timesum_ entered\n");
		*sum = *time1 + *time2;
	} // end  c_esmf_timesum_

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_esmf_timediff_
//
// !INTERFACE:
	void c_esmf_timediff_(ESMC_Time *time1, ESMC_Time *time2, ESMC_Time *diff)

//
// !RETURN VALUE:


//
// !ARGUMENTS:
//	ESMC_Time *time1
//	ESMC_Time *time2
//	ESMC_Time *diff
// !DESCRIPTION:
//
//
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

	{
		printf("c_esmf_timediff_ entered\n");
		*diff = *time1 - *time2;
	}// end c_esmf_timediff_



} // end extern "C"
