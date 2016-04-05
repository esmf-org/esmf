
#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"

#undef  GET_POINTER
#define GET_POINTER     ESMFL_StateGetPointerToData
#undef  MAPL_GetPointer
#define MAPL_GetPointer ESMFL_StateGetPointerToData

#ifdef  GR8

#define MAPL_real  real(MAPL_R8)

#else

#ifdef  GR4
#define MAPL_real  real(MAPL_R4)
#else
#define MAPL_real  real(MAPL_RN)
#endif

#endif

#define    MAPL_SSX(A)   MAPL_SSX_(A)
#define    MAPL_SSX_(A)  SS ## A
