/**
 * MOAB, a Mesh-Oriented datABase, is a software component for creating,
 * storing and accessing finite element mesh data.
 * 
 * Copyright 2004 Sandia Corporation.  Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S. Government
 * retains certain rights in this software.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 */

#include <stdarg.h>
#include <stdio.h>
#include <string.h>

#include "status.h"

#ifdef _MSC_VER
# define vsnprintf(A,B,C,D) _vsnprintf((A),(B),(C),(D))
#endif

int mhdf_isError( mhdf_Status const* status )
{
  return !!status->message[0];
}

const char* mhdf_message( mhdf_Status const* status )
{
  return status->message;
}

void mhdf_setOkay( mhdf_Status* status )
{
  if (status) status->message[0] = '\0';
}

void mhdf_setFail( mhdf_Status* status, const char* fmt, ... )
{
  if (status)
  {
    va_list args;
    va_start( args, fmt );
    vsnprintf( status->message, MHDF_MESSAGE_BUFFER_LEN, fmt, args );
    va_end(args);
    if (!status->message[0])
      strncpy( status->message, "(Uknown error)", MHDF_MESSAGE_BUFFER_LEN );
  }
}

