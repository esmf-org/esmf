#include <ESMC_Types.h>
#include <ESMC_TimeInstant.h>

extern "C"
{
	void c_esmf_timeinstantinit_(ESMC_TimeInstant *ti,
								 int64 *S, int32 *Sn, int32 *Sd,
								 ESMC_Calendar **cal, int *tz)
	{
		ti->Init(*S, *Sn, *Sd, *cal, *tz);
	}
}
