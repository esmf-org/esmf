# $Id: makefile,v 1.15 2003/09/17 22:28:00 flanigan Exp $

ALL: examples


CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  = 
SOURCEH	  = 
OBJSC     = 
OBJSF     = 
LOCDIR	  = src/Infrastructure/CommMem/examples


EXAMPLES_BUILD   =  $(ESMF_EXDIR)/ESMC_CommMemEx \
                    $(ESMF_EXDIR)/ESMF_DELayout_F1Ex \
                    $(ESMF_EXDIR)/ESMF_DELayout_F2Ex \
                    $(ESMF_EXDIR)/ESMF_DElayout_F3Ex


EXAMPLES_RUN     =  run_ESMC_CommMemEx \
                    run_ESMF_DeLayout_F1Ex \
                    run_ESMF_DeLayout_F2Ex \
                    run_ESMF_DeLayout_F3Ex


EXAMPLES_RUN_UNI =  run_ESMC_CommMemEx_uni \
                    run_ESMF_DeLayout_F1Ex_uni \
                    run_ESMF_DeLayout_F2Ex_uni \
                    run_ESMF_DeLayout_F3Ex_uni 


include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod *.txt *.stdout NULL core $(EXAMPLES_BUILD)
CLOBBERDIRS =


#
#  ESMC_CommMemEx
#
run_ESMC_CommMemEx:
	-${MPIRUN} -np 4 $(ESMF_EXDIR)/ESMC_CommMemEx > $(ESMF_EXDIR)/ESMC_CommMemEx.stdout


run_ESMC_CommMemEx_uni:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMC_CommMemEx > $(ESMF_EXDIR)/ESMC_CommMemEx_uni.stdout

#
#  ESMF_DeLayout_F1Ex
#
run_ESMF_DeLayout_F1Ex:
	-${MPIRUN} -np 4 $(ESMF_EXDIR)/ESMF_DeLaout_F1Ex > $(ESMF_EXDIR)/ESMF_DeLayout_F1Ex.stdout

run_ESMF_DeLayout_F1Ex_uni:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMF_DeLaout_F1Ex > $(ESMF_EXDIR)/ESMF_DeLayout_F1Ex.stdout


#
#  ESMF_DeLayout_F2Ex
#
run_ESMF_DeLayout_F2Ex:
	-${MPIRUN} -np 4 $(ESMF_EXDIR)/ESMF_DeLaout_F2Ex > $(ESMF_EXDIR)/ESMF_DeLayout_F2Ex.stdout

run_ESMF_DeLayout_F2Ex_uni:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMF_DeLaout_F2Ex > $(ESMF_EXDIR)/ESMF_DeLayout_F2Ex.stdout


#
#  ESMF_DeLayout_F3Ex
#
run_ESMF_DeLayout_F3Ex:
	-${MPIRUN} -np 4 $(ESMF_EXDIR)/ESMF_DeLaout_F3Ex > $(ESMF_EXDIR)/ESMF_DeLayout_F3Ex.stdout

run_ESMF_DeLayout_F3Ex_uni:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMF_DeLaout_F3Ex > $(ESMF_EXDIR)/ESMF_DeLayout_F3Ex.stdout




