# $Id: makefile,v 1.2 2003/10/07 21:15:59 nscollins Exp $ 

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
LOCDIR	  = src/Infrastructure/Array/examples



EXAMPLES_BUILD   =  $(ESMF_EXDIR)/ESMF_ArrayCreateEx 


EXAMPLES_RUN     =  run_ESMF_ArrayCreateEx


EXAMPLES_RUN_UNI =  run_ESMF_ArrayCreateEx_uni 



include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod *.txt *.stdout NULL core $(EXAMPLES_BUILD)
CLOBBERDIRS =



#
# ESMF_ArrayCreateEx
#
run_ESMF_ArrayCreateEx:
	-${MPIRUN} -np 4 $(ESMF_EXDIR)/ESMC_ArrayCreateEx > $(ESMF_EXDIR)/ESMC_ArrayCreateEx.stdout


run_ESMF_ArrayCreateEx_uni:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMC_ArrayCreateEx > $(ESMF_EXDIR)/ESMC_ArrayCreateEx_uni.stdout

