# $Id: makefile,v 1.4 2004/02/11 19:03:47 nscollins Exp $ 

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
	-${MPIRUN} -np 4 $(ESMF_EXDIR)/ESMF_ArrayCreateEx > $(ESMF_EXDIR)/ESMF_ArrayCreateEx.stdout


run_ESMF_ArrayCreateEx_uni:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMF_ArrayCreateEx > $(ESMF_EXDIR)/ESMF_ArrayCreateEx_uni.stdout

