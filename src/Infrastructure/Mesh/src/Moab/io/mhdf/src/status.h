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

#ifndef MHDF_STATUS_INTERNAL_H
#define MHDF_STATUS_INTERNAL_H

#include "mhdf.h"

void mhdf_setOkay( mhdf_Status* );

void mhdf_setFail( mhdf_Status*, const char*, ... );

#endif
