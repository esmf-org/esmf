// $Id: c_ESMF_BaseTime.C,v 1.1 2003/02/11 18:38:59 eschwab Exp $
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

#include <ESMC_BaseTime.h>
#include <stdio.h>

extern "C"
{
#if 0
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_esmf_basetimeinit_
//
// !INTERFACE:
	void c_esmf_basetimeinit_(ESMC_BaseTime *t, ESMF_IKIND_I8 *S, int *Sn, int *Sd)
//
// !RETURN VALUE:


//
// !ARGUMENTS:
//	ESMC_BaseTime *t
//	ESMF_IKIND_I8 *S
//	int *Sn
//	int *Sd

//
// !DESCRIPTION:
//      
//     
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

	{
		printf("c_esmf_basetimeinit_: S, Sn, Sd = %lld, %ld, %ld\n", *S, *Sn, *Sd);
		t->ESMC_BaseTimeInit(*S, *Sn, *Sd);
	} // end c_esmf_basetimeinit_
#endif

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_esmf_basetimesum_
//
// !INTERFACE:
	void c_esmf_basetimesum_(ESMC_BaseTime *time1, ESMC_BaseTime *time2, ESMC_BaseTime *sum)


//
// !RETURN VALUE:


//
// !ARGUMENTS:
//	ESMC_BaseTime *time1
//	ESMC_BaseTime *time2
//	ESMC_BaseTime *sum

// !DESCRIPTION:
//
//
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

	{
		printf("c_esmf_basetimesum_ entered\n");
		*sum = *time1 + *time2;
	} // end  c_esmf_basetimesum_

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_esmf_basetimediff_
//
// !INTERFACE:
	void c_esmf_basetimediff_(ESMC_BaseTime *time1, ESMC_BaseTime *time2, ESMC_BaseTime *diff)

//
// !RETURN VALUE:


//
// !ARGUMENTS:
//	ESMC_BaseTime *time1
//	ESMC_BaseTime *time2
//	ESMC_BaseTime *diff
// !DESCRIPTION:
//
//
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

	{
		printf("c_esmf_basetimediff_ entered\n");
		*diff = *time1 - *time2;
	}// end c_esmf_basetimediff_

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_esmf_baseprint_
//
// !INTERFACE:
	void c_esmf_baseprint_(ESMC_BaseTime *t, ESMF_IKIND_I8 *S, int *Sn, int *Sd)

//
// !RETURN VALUE:


//
// !ARGUMENTS:
//	ESMC_BaseTime *t
//	 INT64 *S
//	 int *Sn
//	 int *Sd
//

// !DESCRIPTION:
//
//
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n


	{
		printf("c_esmf_baseprint_ entered\n");
		t->ESMC_BasePrint(S, Sn, Sd);
	} // end c_esmf_baseprint_

} // end extern "C"
