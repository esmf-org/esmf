/*============================================================================*  
 * ESMC_Error.h
 *
 * Error handling codes.
 *============================================================================*/


#if !defined(ESMC_ERROR_H)
#define ESMC_ERROR_H

#include "ESMC_BasicUtil.h"

/*
   Defines the directory where the compiled source is located; used
   in printing error messages. Each makefile has an entry 
   LOCDIR	  =  thedirectory
   and build/common includes in CCPPFLAGS -D__SDIR__='"${LOCDIR}"'
   which is a flag passed to the C/C++ compilers.
*/

#if !defined(__SDIR__)
#define __SDIR__ "unknowndirectory/"
#endif

/*
   Defines the function where the compiled source is located; used 
   in printing error messages.
*/
#if !defined(__FUNC__)
#define __FUNC__ "unknownfunction"
#endif

typedef enum {ESMC_ERR_RETURN,
              ESMC_ERR_EXIT,
              ESMC_ERR_USER_DEFINED} ESMC_ErrHandlerType;

struct ErrHandlerClass{
  ESMC_ErrHandlerType type;
  void (*p)(void *);
}; 

typedef struct ErrHandlerClass *ESMC_ErrHandler;
                
typedef struct ErrHandlerClass ESMC_ErrHandlerClass;

/*----------------------------------------------------------------------------*
 *   These are the generic error codes.  The user is free to add
 *   additional messages within the source code.
 *----------------------------------------------------------------------------*/

#define ESMC_ERR_MEM             55   /* unable to allocate requested memory */
#define ESMC_ERR_SUP             56   /* no support for requested operation */
#define ESMC_ERR_SIG             59   /* signal received */
#define ESMC_ERR_FP              72   /* floating point exception */
#define ESMC_ERR_COR             74   /* corrupted MF object */
#define ESMC_ERR_LIB             76   /* error in library called by MF */
#define ESMC_ERR_PLIB            77   /* MF library generated inconsistent data */
#define ESMC_ERR_MEMC            78   /* memory corruption */
#define ESMC_ERR_DATE            90   /* invalid date generated */
#define ESMC_ERR_BUSY            91   /* Resource is busy */

#define ESMC_ERR_ARG_SIZ         60   /* nonconforming object sizes used in operation */
#define ESMC_ERR_ARG_IDN         61   /* two arguments not allowed to be the same */
#define ESMC_ERR_ARG_WRONG       62   /* wrong argument (but object probably ok) */
#define ESMC_ERR_ARG_CORRUPT     64   /* null or corrupted MF object as argument */
#define ESMC_ERR_ARG_OUTOFRANGE  63   /* input argument, out of range */
#define ESMC_ERR_ARG_BADPTR      68   /* invalid pointer argument */
#define ESMC_ERR_ARG_NOTSAMETYPE 69   /* two args must be same object type */
#define ESMC_ERR_ARG_NOTSAMECOMM 80   /* two args must be same communicators */
#define ESMC_ERR_ARG_WRONGSTATE  73   /* object in argument is in wrong state, e.g. unassembled mat */
#define ESMC_ERR_ARG_INCOMP      75   /* arguments are incompatible */

#define ESMC_ERR_FILE_OPEN       65   /* unable to open file */
#define ESMC_ERR_FILE_READ       66   /* unable to read from file */
#define ESMC_ERR_FILE_WRITE      67   /* unable to write to file */
#define ESMC_ERR_FILE_UNEXPECTED 79   /* unexpected data in file */

#define ESMC_ERR_FILE_CLOSE      81   /* unable to close file */

#if defined(ESMC_DEBUG) 

#define ESMC_ERRA(num,dum,str)        {return ESMC_Err(__LINE__, __FUNC__, __FILE__, \
						   __SDIR__, num, dum, str);}

#define ESMC_ERRA1(num,dum,str,a1)    {return ESMC_Err(__LINE__, __FUNC__, __FILE__, \
						   __SDIR__, num, dum, str, a1);}

#define ESMC_ERRA2(num,dum,str,a1,a2) {return ESMC_Err(__LINE__, __FUNC__, __FILE__, \
						   __SDIR__, num, dum, str, a1, a2);}

#else

#define ESMC_ERRA(n,p,s) ;
#define ESMC_ERRA1(n,p,s,a1) ;
#define ESMC_ERRA2(n,p,s,a1,a2) ;

#endif

#if defined(__cplusplus)
#define EXTERN_C_BEGIN extern "C" {
#define EXTERN_C_END }
#else
#define EXTERN_C_BEGIN 
#define EXTERN_C_END 
#endif


/*----------------------------------------------------------------------*
 * Test evaluation
 *----------------------------------------------------------------------*/

#define ESMC_ERROR_TEST(eval,str)      {if(eval){printf("PASS test: "); \
                                     printf(str); \
                                     printf("\n");} \
                                     else {printf("FAIL test "); \
                                     printf(str); \
                                     printf("\n\n");}}

/*============================================================================*
 * Function prototypes
 *============================================================================*/

extern void ESMC_ErrHandlerSetType(ESMC_ErrHandlerType type); 

extern void ESMC_ErrExit(int line, char *func, char *file, char *dir, int rc, int p, 
			   char *lbuf);

extern void ESMC_ErrReturn(int line, char *func, char *file, char *dir, int rc, int p, 
			    char *lbuf);

extern void ESMC_GetErrString(int rc, char **rs); 

#endif
