/*$Id: fix.h,v 1.1 2002/12/11 23:35:00 nscollins Exp $*/

/*
    This fixes various things in system files that are incomplete, for 
  instance many systems don't properly prototype all system functions.
  It is not intended to DUPLICATE anything in the system include files;
  if the compiler reports a conflict between a prototye in a system file
  and this file then the prototype in this file should be removed.

    This is included by files in src/sys/src
*/

#if !defined(MF_FIX_H)
#define MF_FIX_H

#endif


