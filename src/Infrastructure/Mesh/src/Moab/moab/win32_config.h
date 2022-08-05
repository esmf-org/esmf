#ifndef MOAB_WIN32_CONFIG_H
#define MOAB_WIN32_CONFIG_H

#if defined WIN32 && defined _MSC_VER
#include "MOAB_export.h"

#define _USE_MATH_DEFINES // for C++  
#include <cmath> 

#else
#define MOAB_EXPORT
#endif

#endif
