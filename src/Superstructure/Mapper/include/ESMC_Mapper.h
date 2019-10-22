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
int ESMC_MapperSetConstraints(ESMC_Mapper mapper);
int ESMC_MapperSetCompConstraints(ESMC_Mapper mapper,
      int comp_name_len, const char *comp_name,
      int phase_name_len, const char *phase_name);
int ESMC_MapperOptimize(ESMC_Mapper mapper, int *opt_threshold_reached);
int ESMC_MapperPrint(ESMC_Mapper mapper);

int ESMC_MapperSetCompInfo(ESMC_Mapper mapper,
      int comp_name_len, const char *comp_name,
      int phase_name_len, const char *phase_name,
      int comp_pet_range_start, int comp_pet_range_end,
      double comp_time_intvl_start, double comp_time_intvl_end);
int ESMC_MapperGetCompInfo(ESMC_Mapper mapper,
      int comp_name_len, const char *comp_name,
      int phase_name_len, const char *phase_name,
      int *comp_pet_range_start, int *comp_pet_range_end);
int ESMC_MapperDestroy(ESMC_Mapper mapper);


#ifdef __cplusplus
} // extern "C"
#endif

#endif // ESMC_Mapper_H
