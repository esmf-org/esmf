#if 0
ESMF Log include file for both  C++ and Fortran
#endif

#if 0
!BOP
ESMF\_SINGLE\_FILE: write log output to a single file
!EOP
#endif
#define ESMF_SINGLE_LOG_FILE 8

#if 0
!BOP
ESMF\_MULT\_LOG\_FILE: write log output to multiple files
!EOP
#endif
#define ESMF_MULT_LOG_FILE 9

#if 0
!BOP
ESMF\_LOG\_TRUE: integer to signify true statement
!EOP
#endif
#define ESMF_LOG_TRUE 1

#if 0
!BOP
ESMF\_LOG\_FALSE: integer to signify false statement
!EOP
#endif
#define ESMF_LOG_FALSE 0

#if 0
!BOP 
ESMF\_LOG\_UNIT\_NUMBER: used to create fortran unit numbers
!EOP
#endif
#define ESMF_LOG_FORT_UNIT_NUMBER 50

#if 0
!BOP
ESMF\_LOG\_FORT\_STDOUT: standard out for Fortran
!EOP
#endif
#define ESMF_LOG_FORT_STDOUT 6

#if 0
!BOP
ESMF\_LOG\_UPPER: upper bound for fortran unit number
!EOP
#endif
#define ESMF_LOG_UPPER 100
