#include <ESMC_Types.h>
#include <ESMC_Util.h>
#include <ESMC_TimeInterval.h>

//-------------------------------------------------------------------------
// Class ESMC_TimeInterval Methods
//-------------------------------------------------------------------------

ESMC_TimeInterval::ESMC_TimeInterval(void)
{
	// uses default base class constructor
}

ESMC_TimeInterval::ESMC_TimeInterval(int64 S, int32 Sn, int32 Sd) :
	 ESMC_Time(S, Sn, Sd)
{
}

ESMC_TimeInterval::~ESMC_TimeInterval(void)
{
}

int ESMC_TimeInterval::Init(int64 S, int32 Sn, int32 Sd)
{
	ESMC_Time::Init(S, Sn, Sd);
	return(ESMC_SUCCESS);
}
