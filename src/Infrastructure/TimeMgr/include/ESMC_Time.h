// $Id: ESMC_Time.h,v 1.3 2002/10/11 01:52:27 eschwab Exp $
#ifndef ESMC_TIME_H
#define ESMC_TIME_H

#include <ESMC_Types.h>
#include <pthread.h>

class ESMC_Time
{
//-------------------------------------------------------------------------
//BOP
//
// !CLASS: ESMC_Time
//
// !SUPERCLASSES:
//
// !AGGREGATE CLASSES:
//
// !ASSOCIATE CLASSES:
//
// !FRIEND CLASSES:
//
// !PUBLIC DATA MEMBERS:
//
// !PUBLIC MEMBER FUNCTIONS:
  public:

    ESMC_Time(void);
    ESMC_Time(int64 S, int32 Sn, int32 Sd);
    ~ESMC_Time(void);

    int Init(int64 S, int32 Sn, int32  Sd);

	// for persistence/checkpointing
	int ESMC_TimePrint(int64 *S, int32 *Sn, int32 *Sd) const;

	// for testing/debugging
	int ESMC_TimePrint(void) const;

    // direct, one-to-one access to each core element (no conversions)
    int Read_S (int64 *S);
    int Write_S(int64  S);

    int Read_Sn (int32 *Sn);
    int Write_Sn(int32  Sn);

    int Read_Sd (int32 *Sd);
    int Write_Sd(int32  Sd);

    // individual get/set methods which perform signed conversion
	//  (TMG 1.1, 1.2, 2.1)
	int Get_D(int *D);
	int Set_D(int  D);

    int Get_H(int *H);
    int Set_H(int  H);

    int Get_M(int *M);
    int Set_M(int  M);

    int Get_S(int *S);
    int Set_S(int  S);

    int Get_Sn(int *Sn);
    int Set_Sn(int  Sn);

    int Get_Sd(int *Sd);
    int Set_Sd(int  Sd);

    int Get_MS(int *MS);
    int Set_MS(int  MS);

    int Get_US(int *MS);
    int Set_US(int  MS);

    int Get_NS(int *NS);
    int Set_NS(int  NS);

    int Get_d(double *d);
    int Set_d(double  d);

    int Get_h(double *h);
    int Set_h(double  h);

    int Get_m(double *m);
    int Set_m(double  m);

    int Get_s(double *s);
    int Set_s(double  s);

    int Get_ms(double *ms);
    int Set_ms(double  ms);

    int Get_us(double *us);
    int Set_us(double  us);

    int Get_ns(double *ns);
    int Set_ns(double  ns);

    // comparison methods (TMG 1.5.3, 2.4.3, 7.2)
    bool operator==(const ESMC_Time &) const; 
    bool operator!=(const ESMC_Time &) const; 
    bool operator< (const ESMC_Time &) const; 
    bool operator> (const ESMC_Time &) const; 
    bool operator<=(const ESMC_Time &) const; 
    bool operator>=(const ESMC_Time &) const; 

    // increment, decrement methods (TMG 1.5.4, 2.4.4, 2.4.5, 2.4.6, 5.1, 5.2,
    //                                   7.2)
    ESMC_Time& operator+=(ESMC_Time &);
    ESMC_Time  operator+ (ESMC_Time &);
    ESMC_Time& operator-=(ESMC_Time &);
    ESMC_Time  operator- (ESMC_Time &);

// !DESCRIPTION:
//      ESMF C++ Time Manager Object Class Definitions
//
//      Core representation of time for time intervals and instances
//       - minimum elements; maximum range, resolution and flexibility
//       - each element is bounded to the next higher one (ie. fractional
//         seconds are < 1.0).
//       - For arithmetic consistency both whole seconds and the numerator of
//         fractional seconds must carry the same sign (both positve or both
//         negative), except, of course, for zero values.
//       - conversions and other dependencies are done by interface methods
//       - fractional math should be handled by an open-source package if
//         available. (see ESMC\_TimeInterval.h also)
//
//  NOTES:
//      Core representation meets TMG 1.3, 1.4, 2.2, 5.4
//
//      TMG 1.5.2, 2.4.2: Copy from one object to another is handled
//      via the class default memberwise assignment method, which uses
//      the overloaded equals (=) operator.  E.g.  *obj1 = *obj2 which
//      can be called from a C wrapper function mapping F90 to C++
//
//      TMG 1.5.1, 2.4.1, 3.1, 4.1: Each class has an Init() function,
//      which is used in lieu of a constructor since it can
//      return an error code.
//
// !BUGS:
//
// !SEE ALSO:
//
// !REVISION HISTORY:
//
//  10Jun02   Earl Schwab  Initial code.
//
//EOP
//-------------------------------------------------------------------------
  protected:

    int64 S;    // Integer seconds (signed)
    int32 Sn;	// Integer fractional seconds (exact) n/d; numerator (signed)
    int32 Sd;  	// Integer fractional seconds (exact) n/d; denominator

    pthread_mutex_t TimeMutex; // (TMG 7.5)
};

#endif // ESMC_TIME_H
