/* $Id: ESMO_TOD.h,v 1.1 2002/11/15 21:26:16 jwolfe Exp $ */

#ifndef ESMC_TOD_H
#define ESMC_TOD_H

#include "ESMO_BasicUtil.h"

/* 
!BOP
! !ROUTINE: Time of Day (TOD) Class
\begin{verbatim}
*/

enum TODType{ ESMC_TOD_TYPE_UNDEFINED,   /* Abbreviations */
	      ESMC_TOD_INT_SEC,          /* IS = integer seconds */
	      ESMC_TOD_INT_MSEC};        /* MS = integer milliseconds */

typedef enum TODType ESMC_TODType;

struct TODClass{
  ESMC_TODType type;
  int sec;
  int msec;
};

/* 
\end{verbatim}
!EOP
*/

typedef struct TODClass *ESMC_TOD;

typedef struct TODClass ESMC_TODClass;

/*============================================================================*
 * Public methods
 *============================================================================*/

extern int ESMC_TODConstructIS(ESMC_TOD this, int seconds);

extern int ESMC_TODConstructUndefined(ESMC_TOD this);

extern int ESMC_TODIncrement(ESMC_TOD this, ESMC_TOD inc, ESMC_TOD resultTOD, 
			    int *resultDays);

extern int ESMC_TODDecrement(ESMC_TOD this, ESMC_TOD dec, ESMC_TOD resultTOD, 
			    int *resultDays);

extern int ESMC_TODIsLater(ESMC_TOD earlyTOD, ESMC_TOD lateTOD, ESMC_Bool *isLater);

extern int ESMC_TODPrint(ESMC_TOD this);

#endif



















