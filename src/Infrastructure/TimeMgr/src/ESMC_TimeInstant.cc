#include <ESMC_Types.h>
#include <ESMC_Time.h>
#include <ESMC_Calendar.h>
#include <ESMC_Util.h>
#include <ESMC_TimeInstant.h>
#include <stdio.h>

/*
#include <iostream>
//#include <stdlib.h>
using std::cout;
using std::endl;
*/

//-------------------------------------------------------------------------
// Class ESMC_TimeInstant Methods
//-------------------------------------------------------------------------

ESMC_TimeInstant::ESMC_TimeInstant(void)
{
	Init(0, 0, 1, NULL, 0); 	// what is default calendar ??
}

ESMC_TimeInstant::ESMC_TimeInstant(int64 S, int32 Sn, int32 Sd,
								   ESMC_Calendar *Cal, int Tz)
{
	Init(S, Sn, Sd, Cal, Tz);
}

ESMC_TimeInstant::~ESMC_TimeInstant(void)
{
}

int ESMC_TimeInstant::Init(int64 S, int32 Sn, int32 Sd, ESMC_Calendar *Cal,
						   int Tz)
{
	// use base class Init()
	if (ESMC_Time::Init(S, Sn, Sd) == ESMC_SUCCESS)
	{
		this->Calendar = Cal;
		this->Timezone = Tz;

		return(ESMC_SUCCESS);
	}
	else return(ESMC_FAILURE);
}

// for persistence/checkpointing
int ESMC_TimeInstant::Dump(int64 *S, int32 *Sn, int32 *Sd)
{
	// use base class Dump() first
	return(ESMC_Time::Dump(S, Sn, Sd));
}

// for testing/debugging
int ESMC_TimeInstant::Dump(void)
{
	// use base class Dump
	ESMC_Time::Dump();

	return(ESMC_SUCCESS);
}
