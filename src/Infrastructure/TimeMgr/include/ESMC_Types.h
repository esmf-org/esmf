// $Id: ESMC_Types.h,v 1.3 2002/10/28 23:34:13 nscollins Exp $
//
// < Something here from legal about the status of the code, like:
//  This code developed by NASA/NCAR/ESMC whatever, and is covered by
//  the terms of the GNU public license.  See license file for more details. >
//
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMF_TYPE_H
#define ESMF_TYPE_H

#include <stdio.h>
#include "ESMC.h"
//-------------------------------------------------------------------------
//
// !DESCRIPTION:
//  
// ESMF platform-independent data types.  Needs higher level, ESMF-wide home ??
//
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//
//-------------------------------------------------------------------------

#ifndef ESMF_HAS_INT64
#define int64  long long
// why not typedef long long int64?
#endif
#ifndef ESMF_HAS_UINT64
#define uint64 unsigned long long
#endif
#ifndef ESMF_HAS_INT32
#define int32  long
#endif
#ifndef ESMF_HAS_UINT32
#define uint32 unsigned long
#endif

#endif // ESMF_TYPE_H
