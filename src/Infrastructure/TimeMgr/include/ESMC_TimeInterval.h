// $Id: ESMC_TimeInterval.h,v 1.2 2002/10/08 18:18:01 eschwab Exp $
#ifndef ESMC_TIME_INTERVAL_H
#define ESMC_TIME_INTERVAL_H

#include <ESMC_Types.h>
#include <ESMC_Fraction.h>
#include <ESMC_Time.h>

class ESMC_TimeInterval : public ESMC_Time
{ 
//-------------------------------------------------------------------------
//BOP
//
// !CLASS: ESMC_TimeInterval
//
// !SUPERCLASSES:
//	ESMC_Time
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

    ESMC_TimeInterval(void);
	ESMC_TimeInterval(int64 S, int32 Sn, int32 Sd);
    ~ESMC_TimeInterval(void);

	int Init(int64 S, int32 Sn, int32 Sd);
    int Init(const char *TimeList, ...);

    // all get/set routines perform signed conversions, where applicable;
    //   direct, one-to-one access to core time elements is provided by the
    //   ESMC_Time base class

    // generic interface -- via variable argument lists
    //   can map to F90 named-optional-arguments interface
    //   adv:     flexible -- can specify any combination w/o source changes
    //            elegant -- only list those time items needed
    //   disadv:  parsing overhead, but limited to just those items specified
    //            no arg type checking -- user may pass-in bad args.
	//   

	// (TMG 1.1)
    int Get(const char *TimeList, ...);   // e.g. Get("D:S",(int *)D, (int *)S);
    int Set(const char *TimeList, ...);   // e.g. Set("s" , (double) s);

    // -- AND/OR -- individual/combo get/set
    //   adv:    fastest -- no parsing
    //             args type checked
    //   disadv: limited combinations, must modify source to add more

    // shortcut interfaces (TMG 1.1, 1.2, 1.5.1)
    int Get_S_nd(int64 *S, int32 *Sn, int32 *Sd);
    int Set_S_nd(int64  S, int32  Sn, int32  Sd);

    int Get_D_S(int32 *D, int *S);
    int Set_D_S(int32  D, int  S);

    int Get_D_H_M_S_MS(int32 *D, int *H, int *M, int *S, int *MS);
    int Set_D_H_M_S_MS(int32  D, int  H, int  M, int  S, int  MS);

    // division (TMG 1.5.5)
     // return fraction _nd ??
    ESMC_Fraction& operator/(ESMC_TimeInterval &);

    // subdivision (TMG 1.5.6, 5.3, 7.2)
    ESMC_TimeInterval& operator/=(int &);
    ESMC_TimeInterval& operator/ (int &);

    // multiplication (TMG 1.5.7, 7.2)
    ESMC_TimeInterval& operator*=(int &);
    ESMC_TimeInterval& operator* (int &);
    ESMC_TimeInterval& operator*=(ESMC_Fraction &);
    ESMC_TimeInterval& operator* (ESMC_Fraction &);
    ESMC_TimeInterval& operator*=(double &);
    ESMC_TimeInterval& operator* (double &);

    // return in string format (TMG 1.5.9)
    int GetString(char *Ts);

    // return positive value (TMG 1.5.8)
    ESMC_TimeInterval *GetAbsValue(ESMC_TimeInterval *);

    // return negative value (TMG 1.5.8)
    ESMC_TimeInterval *GetNegAbsValue(ESMC_TimeInterval *);

// !DESCRIPTION:
//       - For arithmetic consistency both whole seconds and the numerator of
//         fractional seconds must carry the same sign (both positve or both 
//         negative), except, of course, for zero values.
//       - fractional math should be handled by an open-source package if
//         available (see ESMC\_Time.h also)
//       - Calendar intervals are dependent on a calendar and so represent
//         a specialized case of a TimeInterval.  A derived class
//         CalendarInterval will be defined to inherit from TimeInterval
//         and specialize it for use with Calendars.
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
  private:

	// inherited from ESMC_Time

};

#endif // ESMC_TIME_INTERVAL_H
