#ifndef ESMC_APP_H
#define ESMC_APP_H

#include "ESMC_BasicUtil.h"
#include "ESMC_Machine.h"
#include "ESMC_Log.h"

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
