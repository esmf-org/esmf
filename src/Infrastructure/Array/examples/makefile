# $Id: makefile,v 1.6 2004/08/03 19:33:40 nscollins Exp $ 

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



EXAMPLES_BUILD   =  $(ESMF_EXDIR)/ESMF_ArrayCreateEx \
                    $(ESMF_EXDIR)/ESMF_ArrayGetEx 


EXAMPLES_RUN     =  run_ESMF_ArrayCreateEx \
                    run_ESMF_ArrayGetEx


EXAMPLES_RUN_UNI =  run_ESMF_ArrayCreateEx_uni \
                    run_ESMF_ArrayGetEx_uni 



include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod *.txt *.stdout NULL core ESMF_LogFile $(EXAMPLES_BUILD)
CLOBBERDIRS =



#
# ESMF_ArrayCreateEx
#
run_ESMF_ArrayCreateEx:
	-${MPIRUN} -np 4 $(ESMF_EXDIR)/ESMF_ArrayCreateEx > $(ESMF_EXDIR)/ESMF_ArrayCreateEx.stdout


run_ESMF_ArrayCreateEx_uni:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMF_ArrayCreateEx > $(ESMF_EXDIR)/ESMF_ArrayCreateEx_uni.stdout


#
# ESMF_ArrayGetEx
#
run_ESMF_ArrayGetEx:
	-${MPIRUN} -np 4 $(ESMF_EXDIR)/ESMF_ArrayGetEx > $(ESMF_EXDIR)/ESMF_ArrayGetEx.stdout


run_ESMF_ArrayGetEx_uni:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMF_ArrayGetEx > $(ESMF_EXDIR)/ESMF_ArrayGetEx_uni.stdout

