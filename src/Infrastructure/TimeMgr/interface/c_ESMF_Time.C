#include <ESMC_Types.h>
#include <ESMC_Time.h>
#include <stdio.h>

extern "C"
{
	void c_esmf_timeinit_(ESMC_Time *t, int64 *S, int32 *Sn, int32 *Sd)
	{
		printf("c_esmf_timeinit_: S, Sn, Sd = %lld, %ld, %ld\n", *S, *Sn, *Sd);
		t->Init(*S, *Sn, *Sd);
	}

	void c_esmf_timeprint_(ESMC_Time *t, int64 *S, int32 *Sn, int32 *Sd)
	{
		printf("c_esmf_timeprint_ entered\n");
		t->ESMC_TimePrint(S, Sn, Sd);
	}

	void c_esmf_timereads_(ESMC_Time *t, int64 *S)
	{
		printf("c_esmf_timereads_ entered\n");
		t->Read_S(S);
	}

	void c_esmf_timewrites_(ESMC_Time *t, int64 *S)
	{
		printf("c_esmf_timewrites_ entered\n");
		t->Write_S(*S);
	}

	void c_esmf_timesum_(ESMC_Time *time1, ESMC_Time *time2, ESMC_Time *sum)
	{
		printf("c_esmf_timesum_ entered\n");
		*sum = *time1 + *time2;
	}

	void c_esmf_timediff_(ESMC_Time *time1, ESMC_Time *time2, ESMC_Time *diff)
	{
		printf("c_esmf_timediff_ entered\n");
		*diff = *time1 - *time2;
	}
}
