# $Id: makefile,v 1.6 2003/09/18 16:00:38 flanigan Exp $

# makefile template for the examples directory.

ALL:  examples

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  =

SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
LOCDIR	  = src/Infrastructure/Route/examples

EXAMPLES_BUILD   = $(ESMF_EXDIR)/ESMC_RouteEx \
                   $(ESMF_EXDIR)/ESMF_RouteEx

EXAMPLES_RUN     = run_ESMC_RouteEx \
                   run_ESMF_RouteEx

EXAMPLES_RUN_UNI = run_ESMC_RouteEx_uni \
                   run_ESMF_RouteEx_uni


include ${ESMF_DIR}/makefile

DIRS =


CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod *.txt *.stdout NULL core $(EXAMPLES_BUILD)
CLOBBERDIRS =


#
# ESMC_RouteEx
#
run_ESMC_RouteEx:
	-${MPIRUN} -np 4 $(ESMF_EXDIR)/ESMC_RouteEx > $(ESMF_EXDIR)/ESMC_RouteEx.stdout

run_ESMC_RouteEx_uni:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMC_RouteEx > $(ESMF_EXDIR)/ESMC_RouteEx_uni.stdout


#
# ESMF_RouteEx
#
run_ESMF_RouteEx:
	-${MPIRUN} -np 4 $(ESMF_EXDIR)/ESMF_RouteEx > $(ESMF_EXDIR)/ESMF_RouteEx.stdout

run_ESMF_RouteEx_uni:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMF_RouteEx > $(ESMF_EXDIR)/ESMF_RouteEx_uni.stdout

