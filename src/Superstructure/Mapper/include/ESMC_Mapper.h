#ifndef ESMC_Mapper_H
#define ESMC_Mapper_H

#include "ESMC_Util.h"
#include "ESMC_VM.h"
#ifdef __cplusplus
extern "C"{
#endif

typedef struct {
  void *ptr;
} ESMC_Mapper;

ESMC_Mapper ESMC_MapperCreate(ESMC_VM vm,
              int config_fname_len, const char *config_fname, int *rc);

/*
int ESMC_MapperSetCompInfo();

int ESMC_MapperOptimize();

int ESMC_MapperGetOptimal();
*/

int ESMC_MapperDestroy(ESMC_Mapper mapper);


#ifdef __cplusplus
} // extern "C"
#endif

#endif // ESMC_Mapper_H
