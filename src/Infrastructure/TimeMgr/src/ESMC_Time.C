// $Id: ESMC_Time.C,v 1.3 2002/10/15 03:26:27 eschwab Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC Time method code (body) file

//-------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ Time methods declared
// in the companion file ESMC_Time.h
//
//-------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
 #include <ESMC_Util.h>
 #include <stdio.h>
 #include <stdlib.h>
 /*
 #include <iostream>
 //#include <stdlib.h>
 using std::cout;
 using std::endl;
 */

 // associated class definition file
 #include <ESMC_Time.h>

//-------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Time.C,v 1.3 2002/10/15 03:26:27 eschwab Exp $";
//-------------------------------------------------------------------------

//
//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
//
// This section includes all the Time routines
//
//

//-------------------------------------------------------------------------
// Class ESMC_Time Methods
//-------------------------------------------------------------------------

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInit - shallow class initializer 1
//
// !INTERFACE:
      int ESMC_Time::ESMC_TimeInit(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int64 S,              // in - integer seconds
      int32 Sn,             // in - fractional seconds, numerator
      int32 Sd ) {          // in - fractional seconds, denominator
//
// !DESCRIPTION:
//      Initialzes a Time with given values
//
//EOP
// !REQUIREMENTS:  

	printf("ESMC_Time::ESMC_TimeInit(): S, Sn, Sd = %lld, %ld, %ld\n",
           S, Sn, Sd);

	// S, Sn must be either both positive or both negative;
	//    Sd always positive and >= 1
	if ( ((S >= 0 && Sn >= 0) || (S <= 0 && Sn <= 0)) && Sd >= 1 )
	{
		this->S = S;
		this->Sn = Sn;
		this->Sd = Sd;

		printf("ESMC_Time::ESMC_TimeInit(): S, Sn, Sd = %lld, %ld, %ld\n",
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

}  // end ESMC_TimeInit

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimePrint - return Time state
//
// !INTERFACE:
      int ESMC_Time::ESMC_TimePrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int64 *S,              // out - integer seconds
      int32 *Sn,             // out - fractional seconds, numerator
      int32 *Sd) const {     // out - fractional seconds, denominator
//
// !DESCRIPTION:
//      return Time state for persistence/checkpointing
//
//EOP
// !REQUIREMENTS:  

	if (S != NULL && Sn != NULL & Sd != NULL)
	{
		*S = this->S;
		*Sn = this->Sn;
		*Sd = this->Sd;

		return(ESMC_SUCCESS);
	}
	else return(ESMC_FAILURE);

}  // end ESMC_TimePrint

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimePrint - print Time state
//
// !INTERFACE:
      int ESMC_Time::ESMC_TimePrint(void) const {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      print Time state for testing/debugging
//
//EOP
// !REQUIREMENTS:  

/*
	cout << "S = "  << S  << endl;
	cout << "Sn = " << Sn << endl;
	cout << "Sd = " << Sd << endl << endl;
*/
	return(ESMC_SUCCESS);

}  // end ESMC_TimePrint

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Time(+) - increment Time
//
// !INTERFACE:
      ESMC_Time ESMC_Time::operator+(
//
// !RETURN VALUE:
//    ESMC_Time result
//
// !ARGUMENTS:
      ESMC_Time &Time) {   // in - ESMC_Time increment
//
// !DESCRIPTION:
//      Increment current object's (this) Time with given Time, return result
//
//EOP
// !REQUIREMENTS:  

	ESMC_Time sum = *this;

	// assume positive values for now ??
	// fractional part addition -- LCD (assume same denominator for now) ??
	sum.Sn += Time.Sn;

	// normalize (share logic with ESMC_TimeInit() ?? )
	int32 w;
	if (labs((w = sum.Sn/sum.Sd)) >= 1)
	{
		 sum.S += w;
		 sum.Sn = sum.Sn % sum.Sd;
	}

	// whole part addition
	sum.S += Time.S;

	return(sum);

}  // end ESMC_Time::operator+

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Time(-) - decrement Time
//
// !INTERFACE:
      ESMC_Time ESMC_Time::operator-(
//
// !RETURN VALUE:
//    ESMC_Time result
//
// !ARGUMENTS:
      ESMC_Time &Time) {   // in - ESMC_Time decrement
//
// !DESCRIPTION:
//      Decrement current object's (this) Time with given Time, return result
//
//EOP
// !REQUIREMENTS:  

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

}  // end ESMC_Time::operator-

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Time(+=) - increment Time
//
// !INTERFACE:
      ESMC_Time& ESMC_Time::operator+=(
//
// !RETURN VALUE:
//    ESMC_Time& result
//
// !ARGUMENTS:
      ESMC_Time &Time) {   // in - ESMC_Time increment
//
// !DESCRIPTION:
//      Increment current object's (this) Time with given Time
//
//EOP
// !REQUIREMENTS:  

	// assume positive values for now ??
	// fractional part addition -- LCD (assume same denominator for now) ??
	Sn += Time.Sn;

	// normalize (share logic with ESMC_TimeInit() ?? )
	int32 w;
	if (labs((w = Sn/Sd)) >= 1)
	{
		 S += w;
		 Sn = Sn % Sd;
	}

	// whole part addition
	S += Time.S;

	return(*this);

}  // end ESMC_Time::operator+=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Time(-=) - decrement Time
//
// !INTERFACE:
      ESMC_Time& ESMC_Time::operator-=(
//
// !RETURN VALUE:
//    ESMC_Time& result
//
// !ARGUMENTS:
      ESMC_Time &Time) {   // in - ESMC_Time decrement
//
// !DESCRIPTION:
//      Decrement current object's (this) Time with given Time
//
//EOP
// !REQUIREMENTS:  

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

}  // end ESMC_Time::operator-=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeRead_S - Get Time seconds
//
// !INTERFACE:
      int ESMC_Time::ESMC_TimeRead_S (
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int64 *S) {   // out - integer seconds
//
// !DESCRIPTION:
//      returns integer seconds part of Time value (no conversions)
//
//EOP
// !REQUIREMENTS:  

	if (S != NULL)
	{
		*S = this->S;

		return(ESMC_SUCCESS);
	}
	else return (ESMC_FAILURE);

}  // end ESMC_TimeRead_S

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeRead_Sn - Get Time fractional seconds (numerator)
//
// !INTERFACE:
      int ESMC_Time::ESMC_TimeRead_Sn (
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int32 *Sn) {  // out - integer fractional seconds (numerator)
//
// !DESCRIPTION:
//      returns fractional integer seconds part (numerator) of Time value
//      (no conversions)
//
//EOP
// !REQUIREMENTS:  

	if (Sn != NULL)
	{
		*Sn = this->Sn;

		return(ESMC_SUCCESS);
	}
	else return (ESMC_FAILURE);

}  // end ESMC_TimeRead_Sn

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeRead_Sd - Get Time fractional seconds (denominator)
//
// !INTERFACE:
      int ESMC_Time::ESMC_TimeRead_Sd (
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int32 *Sd) {  // out - integer fractional seconds (denominator)
//
// !DESCRIPTION:
//      returns fractional integer seconds part of Time value (denominator)
//      (no conversions)
//
//EOP
// !REQUIREMENTS:  

	if (Sd != NULL)
	{
		*Sd = this->Sd;

		return(ESMC_SUCCESS);
	}
	else return (ESMC_FAILURE);

}  // end ESMC_TimeRead_Sd

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeWrite_S - Set Time seconds
//
// !INTERFACE:
      int ESMC_Time::ESMC_TimeWrite_S(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int64 S) {   // in - integer seconds
//
// !DESCRIPTION:
//      sets integer seconds part of Time value (no conversions)
//
//EOP
// !REQUIREMENTS:  

	this->S = S;

	return(ESMC_SUCCESS);

}  // end ESMC_TimeWrite_S

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeWrite_Sn - Set Time fractional seconds (numerator)
//
// !INTERFACE:
      int ESMC_Time::ESMC_TimeWrite_Sn(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int32 Sn) {  // in - fractional seconds (numerator)
//
// !DESCRIPTION:
//      sets fractional seconds part (numerator) of Time value (no conversions)
//
//EOP
// !REQUIREMENTS:  

	this->Sn = Sn;

	return(ESMC_SUCCESS);

}  // end ESMC_TimeWrite_Sn

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeWrite_Sd - Set Time fractional seconds (denominator)
//
// !INTERFACE:
      int ESMC_Time::ESMC_TimeWrite_Sd(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int32 Sd) {  // in - fractional seconds (denominator)
//
// !DESCRIPTION:
//    sets fractional seconds part (denominator) of Time value (no conversions)
//
//EOP
// !REQUIREMENTS:  

	// denominator must always be positive and >= 1
	if (Sd >= 1)
	{
		this->Sd = Sd;

		return(ESMC_SUCCESS);
	}
	else return(ESMC_FAILURE);

}  // end ESMC_TimeWrite_Sd

// individual get/set methods which perform signed conversion

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeGet_H - get Time converted to hours
//
// !INTERFACE:
      int ESMC_Time::ESMC_TimeGet_H(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int *H) {  // out - Time converted to hours
//
// !DESCRIPTION:
//      converts Time value into hours
//
//EOP
// !REQUIREMENTS:  

    return(ESMC_SUCCESS);

}  // end ESMC_TimeGet_H

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeSet_H - set Time converted from hours
//
// !INTERFACE:
      int ESMC_Time::ESMC_TimeSet_H(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int H) {  // int - hours to convert into Time
//
// !DESCRIPTION:
//      converts hours into Time value
//
//EOP
// !REQUIREMENTS:  

     return(ESMC_SUCCESS);

}  // end ESMC_TimeSet_H

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeGet_M - get Time converted to minutes
//
// !INTERFACE:
      int ESMC_Time::ESMC_TimeGet_M(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int *M) {  // out - Time converted to minutes
//
// !DESCRIPTION:
//      converts Time value into minutes
//
//EOP
// !REQUIREMENTS:  

     return(ESMC_SUCCESS);

}  // end ESMC_TimeGet_M

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeSet_M - set Time converted from minutes
//
// !INTERFACE:
      int ESMC_Time::ESMC_TimeSet_M(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int M) {  // int - minutes to convert into Time
//
// !DESCRIPTION:
//      converts minutes into Time value
//
//EOP
// !REQUIREMENTS:  

     return(ESMC_SUCCESS);

}  // end ESMC_TimeSet_M

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeGet_S - get Time converted to seconds
//
// !INTERFACE:
      int ESMC_Time::ESMC_TimeGet_S(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int *S) {  // out - Time converted to seconds
//
// !DESCRIPTION:
//      converts Time value into seconds
//
//EOP
// !REQUIREMENTS:  

     return(ESMC_SUCCESS);

}  // end ESMC_TimeGet_S

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeSet_S - set Time converted from seconds
//
// !INTERFACE:
      int ESMC_Time::ESMC_TimeSet_S(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int S) {  // int - seconds to convert into Time
//
// !DESCRIPTION:
//      converts seconds into Time value
//
//EOP
// !REQUIREMENTS:  

     return(ESMC_SUCCESS);

}  // end ESMC_TimeSet_S

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeGet_Sn - get Time converted to fractional seconds
//                               (numerator)
// !INTERFACE:
      int ESMC_Time::ESMC_TimeGet_Sn(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int *Sn) {  // out - Time converted to fractional seconds (numerator)
//
// !DESCRIPTION:
//      converts Time value into fractional seconds (numerator)
//
//EOP
// !REQUIREMENTS:  

     return(ESMC_SUCCESS);

}  // end ESMC_TimeGet_Sn

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeSet_Sn - set Time converted from fractional seconds
//                               (numerator)
// !INTERFACE:
      int ESMC_Time::ESMC_TimeSet_Sn(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int Sn) {  // int - fractional seconds (numerator) to convert into Time
//
// !DESCRIPTION:
//      converts fractional seconds (numerator) into Time value
//
//EOP
// !REQUIREMENTS:  

     return(ESMC_SUCCESS);

}  // end ESMC_TimeSet_Sn

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Time - native default C++ constructor
//
// !INTERFACE:
      ESMC_Time::ESMC_Time(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Initializes a ESMC_Time with defaults via ESMC_TimeInit
//
//EOP
// !REQUIREMENTS:  

	ESMC_TimeInit(0, 0, 1);

}  // end ESMC_Time

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Time - native C++ constructor
//
// !INTERFACE:
      ESMC_Time::ESMC_Time(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
      int64 S,              // in - integer seconds
      int32 Sn,             // in - fractional seconds, numerator
      int32 Sd) {           // in - fractional seconds, denominator
//
// !DESCRIPTION:
//      Initializes a ESMC_Time via ESMC_TimeInit
//
//EOP
// !REQUIREMENTS:  

	ESMC_TimeInit(S, Sn, Sd);

}  // end ESMC_Time

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_Time - native default C++ destructor
//
// !INTERFACE:
      ESMC_Time::~ESMC_Time(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Default ESMC_Time destructor
//
//EOP
// !REQUIREMENTS:  

} // end ~ESMC_Time
