# $Id: makefile,v 1.6 2004/09/23 17:40:28 nscollins Exp $ 

ALL:  build_examples

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  =

SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
LOCDIR	  = src/Infrastructure/ArrayComm/examples



EXAMPLES_BUILD   =  $(ESMF_EXDIR)/ESMF_ArrayCommEx 


EXAMPLES_RUN     =  run_ESMF_ArrayCommEx


EXAMPLES_RUN_UNI =  run_ESMF_ArrayCommEx_uni 



include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod *.txt *.stdout NULL core ESMF_LogFile $(EXAMPLES_BUILD)
CLOBBERDIRS =



#
# ESMF_ArrayCommEx
#
run_ESMF_ArrayCommEx:
	-${MPIRUN} -np 4 $(ESMF_EXDIR)/ESMF_ArrayCommEx > $(ESMF_EXDIR)/ESMF_ArrayCommEx.stdout


run_ESMF_ArrayCommEx_uni:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMF_ArrayCommEx > $(ESMF_EXDIR)/ESMF_ArrayCommEx.stdout

