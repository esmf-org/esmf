/* $Id: ESMO_App.h,v 1.1 2002/11/15 21:21:41 jwolfe Exp $ */
#ifndef ESMC_APP_H
#define ESMC_APP_H

#include "ESMO_BasicUtil.h"
#include "ESMO_Machine.h"
#include "ESMO_Log.h"

typedef struct
{
  int placeholder;
  ESMC_Machine machine;
  ESMC_Log logSTD;
} ESMC_AppClass, *ESMC_App;


#ifdef __cplusplus
extern "C" {
#endif

int ESMC_AppNew(ESMC_App *app);
int ESMC_AppConstruct(ESMC_App app);
int ESMC_AppDelete(ESMC_App app);
int ESMC_AppDestruct(ESMC_App app);

#ifdef __cplusplus
}
#endif

#endif
