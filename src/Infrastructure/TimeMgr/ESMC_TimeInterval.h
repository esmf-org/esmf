// $Id: ESMC_TimeInterval.h,v 1.2 2002/09/20 18:03:38 eschwab Exp $
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

    int Get(const char *TimeList, ...);   // e.g. Get("D:S",(int *)D, (int *)S);
    int Set(const char *TimeList, ...);   // e.g. Set("s" , (double) s);

    // -- AND/OR -- individual/combo get/set
    //   adv:    fastest -- no parsing
    //             args type checked
    //   disadv: limited combinations, must modify source to add more

    // shortcut interfaces
    int Get_S_nd(int64 *S, int32 *Sn, int32 *Sd);
    int Set_S_nd(int64  S, int32  Sn, int32  Sd);

    int Get_D_S(int32 *D, int *S);
    int Set_D_S(int32  D, int  S);

    int Get_D_H_M_S_MS(int32 *D, int *H, int *M, int *S, int *MS);
    int Set_D_H_M_S_MS(int32  D, int  H, int  M, int  S, int  MS);

    // division
     // return fraction _nd ??
    ESMC_Fraction& operator/(ESMC_TimeInterval &);

    // subdivision
    ESMC_TimeInterval& operator/=(int &);
    ESMC_TimeInterval& operator/ (int &);

    // multiplication
    ESMC_TimeInterval& operator*=(int &);
    ESMC_TimeInterval& operator* (int &);
    ESMC_TimeInterval& operator*=(ESMC_Fraction &);
    ESMC_TimeInterval& operator* (ESMC_Fraction &);
    ESMC_TimeInterval& operator*=(double &);
    ESMC_TimeInterval& operator* (double &);

    // return in string format
    int GetString(char *Ts);

    // magnitude (scalar int or double -- not with S_nd) ??
    //   return positive value
    int GetAbsValue(ESMC_TimeInterval *);

// !DESCRIPTION:
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
