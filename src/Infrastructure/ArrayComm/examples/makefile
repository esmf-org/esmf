# $Id: makefile,v 1.3 2004/06/08 09:27:14 nscollins Exp $ 

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
CLEANFILES  = *.o *.mod *.txt *.stdout NULL core $(EXAMPLES_BUILD)
CLOBBERDIRS =



#
# ESMF_ArrayCommEx
#
run_ESMF_ArrayCommEx:
	-${MPIRUN} -np 4 $(ESMF_EXDIR)/ESMC_ArrayCommEx > $(ESMF_EXDIR)/ESMC_ArrayCommEx.stdout


run_ESMF_ArrayCommEx_uni:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMC_ArrayCommEx > $(ESMF_EXDIR)/ESMC_ArrayCommEx_uni.stdout

