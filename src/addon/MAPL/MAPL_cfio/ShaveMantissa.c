 /*  +-======-+ 
  Copyright (c) 2003-2007 United States Government as represented by 
  the Admistrator of the National Aeronautics and Space Administration.  
  All Rights Reserved.
  
  THIS OPEN  SOURCE  AGREEMENT  ("AGREEMENT") DEFINES  THE  RIGHTS  OF USE,
  REPRODUCTION,  DISTRIBUTION,  MODIFICATION AND REDISTRIBUTION OF CERTAIN 
  COMPUTER SOFTWARE ORIGINALLY RELEASED BY THE UNITED STATES GOVERNMENT AS 
  REPRESENTED BY THE GOVERNMENT AGENCY LISTED BELOW ("GOVERNMENT AGENCY").  
  THE UNITED STATES GOVERNMENT, AS REPRESENTED BY GOVERNMENT AGENCY, IS AN 
  INTENDED  THIRD-PARTY  BENEFICIARY  OF  ALL  SUBSEQUENT DISTRIBUTIONS OR 
  REDISTRIBUTIONS  OF THE  SUBJECT  SOFTWARE.  ANYONE WHO USES, REPRODUCES, 
  DISTRIBUTES, MODIFIES  OR REDISTRIBUTES THE SUBJECT SOFTWARE, AS DEFINED 
  HEREIN, OR ANY PART THEREOF,  IS,  BY THAT ACTION, ACCEPTING IN FULL THE 
  RESPONSIBILITIES AND OBLIGATIONS CONTAINED IN THIS AGREEMENT.
  
  Government Agency: National Aeronautics and Space Administration
  Government Agency Original Software Designation: GSC-15354-1
  Government Agency Original Software Title:  GEOS-5 GCM Modeling Software
  User Registration Requested.  Please Visit http://opensource.gsfc.nasa.gov
  Government Agency Point of Contact for Original Software:  
  			Dale Hithon, SRA Assistant, (301) 286-2691
  
 +-======-+   */

#include <stdio.h>
#include <stdlib.h>
#include <math.h> 
#include "ShaveMantissa.h"   /* protype */

#define MAXBITS 20

#define EXPMSK 0x7f800000  //  bits set at exponent locations

#define isZNN(E)  ((E)==0x00000000) // E=0;   zero or unnorm
#define isNAN(E)  ((E)==EXPMSK    ) // E=255; nan  or inf

//#ifdef FAST_ISUNDEF
#if 1
#  define isUndef(A)  ((A) == undef) /* cheap but not robust */
#else
#  define isUndef(A)  ((float32)fabs(undef-(A))<tol) /* robust but expensive */
#endif

#define SKIP(I,A)  (isZNN(I) || isUndef(A) || isNAN(I)) 

//========================================

float32 SetOffset(float32 minv, float32 maxv)
{
  float32   midv, mnabs,  range;

  range  = (maxv-minv);
  midv   = (maxv+minv)*0.5;
  mnabs  = fabs(maxv)>fabs(minv) ? fabs(minv) : fabs(maxv);

  return (range<mnabs) ? midv : midv*(mnabs/range);
}

//========================================

/*
//---------------------------------------------------------------------------
//BOP
//
// !ROUTINE: ShaveMantissa32 - Degrades Precison of 32-bit Float Array 
//
// !INTERFACE:
*/
int ShaveMantissa32 ( float32 a[], float32 ain[], int32 len, int xbits, int has_undef, float32 undef, int32 chunksize )

/*
// !INPUT PARAMETERS:
*/

// int32   len;        /* size of a[] */
// int     xbits;      /* number of bits to excludes from mantissa */
// int     has_undef;  /* whether field has missing (undef) values */ 
// int32   chunksize;  /* find mid range over chunksize chunks     */ 
// float32 undef;      /* missing (undefined) value */
// float32 ain[];      /* input array */

/*
// !OUTPUT PARAMETERS:
*/

// float32 a[];    // output "shaved" array; can share storage with ain[]

/*
// !DESCRIPTION:  
//
//  This routine returns a lower precision version of the input array {\tt a}.
//
//  This is done by clearing the low-order {\tt xbits} bits of the mantissa of
//  a suitably offset version of the data, and then reshifting it to the original 
//  offset. The offsetting procedure prevents the shaving from "throwing out
//  the baby with the bathwater", for variables, such as Kelvin temperatures
//  or geopotentials, that may have large offsets.
//
//  The number of bits retained is {\tt nbits = 24 - xbits}, given that
//  32-bit floats in IEEE representation reserves only 24 bits for the
//  mantissa. The purpose of this precision degradation is to promote
//  internal GZIP compression by HDF-4.
//
//  For variables without large offsets, this algorithm produces very
//  similar results as the standard GRIB encoding with fixed number of bits  
//  ({\tt nbits = 24 - xbits}) and power of 2 binary scaling. For most files,
//  it produces comparable compression to GRIB while using only standard 
//  compression techniques (e.g. GZIP).
//
// !REVISION HISTORY:
//
//  08Dec2006  Suarez    First version.
//  09Dec2006  da Silva  Minor revisions for IA64 build, prologue.
//  11Dec2006  da Silva  Merged with Max's newest version handling Inf, NAN
//                       and bug fixes. 
//  18Dec2006  Suarez    Eliminated test for overflow, which I verified was
//                       unnecessary. Treat zeros like undef. Eliminated a
//                       leftover conversion of nan to undef. Corrected macro
//                       for inf.  MAJOR correction to keep code from hanging
//                       when it encountered an invalid value.
//  26Dec2006  Suarez    Added selection of offset based on range and zero
//                       offset. Restored treatment of fields beginning with
//                       invalids and of all constant fields. Fixed bug that
//                       was not copying input to output arrays.
//  10Mar2009  Suarez    Used a union for the shaving. Also changed the
//                       SKIP  checks and protected the max and min.
//  24oct2009  da Silva  Changed abs() to fabs() in SetOffset; moved float32 
//                       defs to header so that it can be used with prototype.
//  28oct2010  da Silva  Changed another occurence of abs() -> fabs()
//EOP
//---------------------------------------------------------------------------
*/

{
  float32   maxv, minv,  offset, *b, *c, *begnxt, *last, tol;
  uint32    round, mask, e;

  union{
    float32 x;
    uint32  i;
  } aa;

  /* sanity checks */

  if ( len < 1 || xbits < 1 ) {
    fprintf(stderr,
	    "ShaveMantissa32: Bad length of mask bits: len= %d, xbits= %d\n", 
	    len, xbits );
    return 1;
  }

  if ( xbits > MAXBITS ) {
    fprintf(stderr,
	    "ShaveMantissa32: Shaving too many bits: %d; maximum allowed is %d\n",
	    xbits, MAXBITS );
    return 2;
  }

  /* if user has not chosen an undef, pick one */

  if ( !has_undef ) undef = (float32) HUGE_VAL;

  /* miscelaneous static combinations */

  tol   = 0.0001*undef;
  mask  = 0xFFFFFFFF<<xbits--;
  round = 1         <<xbits  ;
  last  = &a[len-1];

  // Do not allow overlapping input and output buffers
  //   unless they are the same. If not the same, copy
  //   input to output

  b = a;
  if(ain!=a) {
    if(fabs(ain-a)<len) {
      fprintf(stderr,"ShaveMantissa32: Overlapping arrays");
      return 3;
    }
    while(a<=last) *a++=*ain++;
  }

  // Loop over chunks

  while(b<=last) {

    // The beginning of the chunk after the current one

    begnxt = b + chunksize;
    if(begnxt>last) begnxt = last+1;

    // Move to first valid value in chunk and initialize min and max

    a = b-1;
    while(++a < begnxt) {
      aa.x = *a;
      e    = aa.i & EXPMSK;
      if(!SKIP(e,aa.x)) {maxv=aa.x; minv=aa.x; c=a; break;}
    }

    // Empty chunk; go to next chunk

    if(a==begnxt) {b=begnxt; continue;}

    // Find man and max valid values of chunk

    while(++a<begnxt) {
      aa.x = *a;
      e    = aa.i & EXPMSK;
      if(!SKIP(e,aa.x)) {
	if(aa.x<minv) minv=aa.x;
	if(aa.x>maxv) maxv=aa.x;
      }
    }

    // Constant chunk; no need to shave; go to next chunck.

    if(minv==maxv) {b=begnxt; continue;}

    // Find optimum offset

    offset = SetOffset(minv,maxv);

    // Shave chunk beginning at first valid value

    a = c-1;
    while(++a<begnxt) {
      aa.x = *a;
      e    = aa.i & EXPMSK;
      if(!SKIP(e,aa.x)) {
	aa.x -= offset;
	aa.i  = ((aa.i + round) & mask);
	aa.x += offset;
	if(aa.x>maxv) aa.x=maxv;
	if(aa.x<minv) aa.x=minv;
	*a    = aa.x;
      }
    }

    // Prepare for next chunk

    b = begnxt;

  } // End chunk loop

  return 0;
}

//========================================

//    Simple hook for FORTRAN interface.

int SHAVEMANTISSA32 (float32 *a, float32 *ain, int32 *len, int *xbits, 
                     int *has_undef, float32 *undef, int32 *chunksize)
{return (int)ShaveMantissa32(a,ain,*len,*xbits,*has_undef,*undef,*chunksize);}

int SHAVEMANTISSA32_ (float32 *a, float32 *ain, int32 *len, int *xbits, 
                      int *has_undef, float32 *undef, int32 *chunksize)
{return (int)ShaveMantissa32(a,ain,*len,*xbits,*has_undef,*undef,*chunksize);}

int shavemantissa32 (float32 *a, float32 *ain, int32 *len, int *xbits, 
                     int *has_undef, float32 *undef, int32 *chunksize)
{return (int)ShaveMantissa32(a,ain,*len,*xbits,*has_undef,*undef,*chunksize);}

int shavemantissa32_ (float32 *a, float32 *ain, int32 *len, int *xbits, 
                      int *has_undef, float32 *undef, int32 *chunksize)
{return (int)ShaveMantissa32(a,ain,*len,*xbits,*has_undef,*undef,*chunksize);}





