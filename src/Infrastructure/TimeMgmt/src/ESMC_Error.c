/* $Id: ESMC_Error.c,v 1.1 2003/03/11 03:17:50 cdeluca Exp $ */

#include <stdarg.h>
#include "ESMO_Error.h"

/*============================================================================*
 * Global error handler structure and pointer
 *============================================================================*/
  ESMC_ErrHandlerClass mc_errHandler;
  ESMC_Bool mc_errHandlerSet = ESMC_FALSE;

/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_ErrHandlerSetType"

void ESMC_ErrHandlerSetType(ESMC_ErrHandlerType type) 
{
  mc_errHandlerSet = ESMC_TRUE;
  
  switch(type){     
  case(ESMC_ERR_RETURN):
    mc_errHandler.type=ESMC_ERR_RETURN;
    mc_errHandler.p=(void(*)(void *))(&ESMC_ErrReturn);
    break;
  case(ESMC_ERR_EXIT):
    mc_errHandler.type=ESMC_ERR_EXIT;
    mc_errHandler.p=(void(*)(void *))(&ESMC_ErrExit);
    break;
  default: 
    mc_errHandler.type=ESMC_ERR_EXIT;
    mc_errHandler.p=(void(*)(void *))(&ESMC_ErrExit);
  }
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_Err"

int ESMC_Err(int line, char *func, char *file, char *dir, int rc, int p, 
	    char *msg, ...) 
{
  va_list argp;
  char buf[2048], *lbuf=ESMC_NULL;

  if(*msg) {
    va_start(argp, msg);
    vsprintf(buf, msg, argp);
    va_end(argp);
    lbuf = buf;
  }  

  /* If an error handler function has been specified, use it.  
     Otherwise, use exit on error as the default. */
 
  if(mc_errHandlerSet) {
    (*((void(*)(int,char *,char *,char *,int,int,char *))(mc_errHandler.p)))
    (line,func,file,dir,rc,p,lbuf);
  }
  else{
    ESMC_ErrExit(line,func,file,dir,rc,p,lbuf);
  }

  return(rc);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_ErrExit"

void ESMC_ErrExit(int line, char *func, char *file, char *dir, int rc, int p, 
	    char *lbuf) 
{
  char rs[256];
  char *rsbuf = rs;
  if(!lbuf) lbuf = " ";
  
  printf("MF ERROR: %s() line %d in %s%s\n",func,line,dir,file);  
  ESMC_GetErrString(rc, &rsbuf); 
  printf("MF ERROR: %s, %s", rsbuf, lbuf);  

  exit(rc);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_ErrReturn"

void ESMC_ErrReturn(int line, char *func, char *file, char *dir, int rc, int p, 
	    char *lbuf) 
{
  return;
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_GetErrString"

void ESMC_GetErrString(int rc, char **rs) 
{
  switch(rc){
  case(ESMC_ERR_DATE):
    strcpy(*rs, "Invalid date generated");
    break;
  case(ESMC_ERR_ARG_OUTOFRANGE):
    strcpy(*rs, "Argument out of range");
    break;
  default:
    strcpy(*rs, "Does not match any known error codes");
    break;
  }
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_ErrPrint"

void ESMC_ErrPrint(int rc) 
{
  char rs[256];
  char *rsbuf = rs;

  ESMC_GetErrString(rc, &rsbuf); 
  printf("MF ERROR: %s\n", rsbuf);  

  return;
}























