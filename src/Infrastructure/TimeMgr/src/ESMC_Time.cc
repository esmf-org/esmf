#include <ESMC_Types.h>
#include <ESMC_Time.h>
#include <ESMC_Util.h>
#include <stdio.h>
#include <stdlib.h>
/*
#include <iostream>
using std::cout;
using std::endl;
*/

//-------------------------------------------------------------------------
// Class ESMC_Time Methods
//-------------------------------------------------------------------------

ESMC_Time::ESMC_Time(void)
{
	Init(0, 0, 1);
}

ESMC_Time::ESMC_Time(int64 S, int32 Sn, int32 Sd)
{
	Init(S, Sn, Sd);
}

ESMC_Time::~ESMC_Time(void)
{
}

int ESMC_Time::Init(int64 S, int32 Sn, int32 Sd)
{
	printf("ESMC_Time::Init(): S, Sn, Sd = %lld, %ld, %ld\n", S, Sn, Sd);

	// S, Sn must be either both positive or both negative;
	//    Sd always positive and >= 1
	if ( ((S >= 0 && Sn >= 0) || (S <= 0 && Sn <= 0)) && Sd >= 1 )
	{
		this->S = S;
		this->Sn = Sn;
		this->Sd = Sd;

		printf("ESMC_Time::Init(): S, Sn, Sd = %lld, %ld, %ld\n",
					this->S, this->Sn, this->Sd);

		// normalize (share logic with += ?? )
		int32 w;
		if (labs((w = this->Sn/this->Sd)) >= 1)
		{
		 	this->S += w;
		 	this->Sn = this->Sn % this->Sd;
		}

		return(ESMC_SUCCESS);
	}
	else return(ESMC_FAILURE);
}

// for persistence/checkpointing
int ESMC_Time::Dump(int64 *S, int32 *Sn, int32 *Sd)
{
	if (S != NULL && Sn != NULL & Sd != NULL)
	{
		*S = this->S;
		*Sn = this->Sn;
		*Sd = this->Sd;

		return(ESMC_SUCCESS);
	}
	else return(ESMC_FAILURE);
}

// for testing/debugging
int ESMC_Time::Dump(void)
{
/*
	cout << "S = "  << S  << endl;
	cout << "Sn = " << Sn << endl;
	cout << "Sd = " << Sd << endl << endl;
*/

	return(ESMC_SUCCESS);
}

ESMC_Time ESMC_Time::operator+(ESMC_Time &Time)
{
	ESMC_Time sum = *this;

	// assume positive values for now ??
	// fractional part addition -- LCD (assume same denominator for now) ??
	sum.Sn += Time.Sn;

	// normalize (share logic with Init() ?? )
	int32 w;
	if (labs((w = sum.Sn/sum.Sd)) >= 1)
	{
		 sum.S += w;
		 sum.Sn = sum.Sn % sum.Sd;
	}

	// whole part addition
	sum.S += Time.S;

	return(sum);
}

ESMC_Time ESMC_Time::operator-(ESMC_Time &Time)
{
	ESMC_Time diff = *this;

	// assume positive values for now ??
	// assume this > Time and both normalized for now ??
	// fractional part subtraction -- LCD (assume same denominator for now) ??

	// fractional part subtraction
	if (diff.Sn < Time.Sn)
	{
		// borrow
		diff.Sn += diff.Sd;
		diff.S--;
	}
	diff.Sn -= Time.Sn;

	// whole part subtraction 
	diff.S -= Time.S;

	return(diff);
}

ESMC_Time& ESMC_Time::operator+=(ESMC_Time &Time)
{
	// assume positive values for now ??
	// fractional part addition -- LCD (assume same denominator for now) ??
	Sn += Time.Sn;

	// normalize (share logic with Init() ?? )
	int32 w;
	if (labs((w = Sn/Sd)) >= 1)
	{
		 S += w;
		 Sn = Sn % Sd;
	}

	// whole part addition
	S += Time.S;

	return(*this);
}

ESMC_Time& ESMC_Time::operator-=(ESMC_Time &Time)
{
	// assume positive values for now ??
	// assume this > Time and both normalized for now ??
	// fractional part subtraction -- LCD (assume same denominator for now) ??

	// fractional part subtraction
	if (Sn < Time.Sn)
	{
		// borrow
		Sn += Sd;
		S--;
	}
	Sn -= Time.Sn;

	// whole part subtraction 
	S -= Time.S;

	return(*this);
}

// direct, one-to-one access to each core element (no conversions)
int ESMC_Time::Read_S (int64 *S)
{
	if (S != NULL)
	{
		*S = this->S;

		return(ESMC_SUCCESS);
	}
	else return (ESMC_FAILURE);
}

int ESMC_Time::Read_Sn (int32 *Sn)
{
	if (Sn != NULL)
	{
		*Sn = this->Sn;

		return(ESMC_SUCCESS);
	}
	else return (ESMC_FAILURE);
}

int ESMC_Time::Read_Sd (int32 *Sd)
{
	if (Sd != NULL)
	{
		*Sd = this->Sd;

		return(ESMC_SUCCESS);
	}
	else return (ESMC_FAILURE);
}

int ESMC_Time::Write_S(int64 S)
{
	this->S = S;

	return(ESMC_SUCCESS);
}

int ESMC_Time::Write_Sn(int32 Sn)
{
	this->Sn = Sn;

	return(ESMC_SUCCESS);
}

int ESMC_Time::Write_Sd(int32 Sd)
{
	// denominator must always be positive and >= 1
	if (Sd >= 1)
	{
		this->Sd = Sd;

		return(ESMC_SUCCESS);
	}
	else return(ESMC_FAILURE);
}

// individual get/set methods which perform signed conversion
int ESMC_Time::Get_H(int *H){return(ESMC_SUCCESS);}
int ESMC_Time::Set_H(int  H){return(ESMC_SUCCESS);}

int ESMC_Time::Get_M(int *M){return(ESMC_SUCCESS);}
int ESMC_Time::Set_M(int  M){return(ESMC_SUCCESS);}

int ESMC_Time::Get_S(int *S){return(ESMC_SUCCESS);}
int ESMC_Time::Set_S(int  S){return(ESMC_SUCCESS);}

int ESMC_Time::Get_Sn(int *Sn){return(ESMC_SUCCESS);}
int ESMC_Time::Set_Sn(int  Sn){return(ESMC_SUCCESS);}
