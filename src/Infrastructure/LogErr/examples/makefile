# $Id: makefile,v 1.9 2003/09/17 21:17:17 flanigan Exp $ 

ALL: 

CFLAGS    =
FFLAGS    = 
SOURCEC	  = 
SOURCEF	  =

SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
LOCDIR	  = src/Infrastructure/LogErr/examples


EXAMPLES_BUILD = $(ESMF_EXDIR)/ESMC_TestErr1Ex \
                 $(ESMF_EXDIR)/ESMC_TestErr2Ex \
                 $(ESMF_EXDIR)/ESMC_TestErr3Ex \
                 $(ESMF_EXDIR)/ESMF_TestErr1Ex \
                 $(ESMF_EXDIR)/ESMF_TestErr2Ex \
                 $(ESMF_EXDIR)/ESMF_TestErr3Ex \
                 $(ESMF_EXDIR)/ESMF_TestLog1Ex \
                 $(ESMF_EXDIR)/ESMF_TestLog2Ex 


EXAMPLES_RUN = run_ESMC_TestErr1Ex \
               run_ESMC_TestErr2Ex \
               run_ESMC_TestErr3Ex \
               run_ESMF_TestErr1Ex \
               run_ESMF_TestErr2Ex \
               run_ESMF_TestErr3Ex \
               run_ESMF_TestLog1Ex \
               run_ESMF_TestLog2Ex 


UNI_EXAMPLES_RUN = run_ESMC_TestErr1Ex_uni \
                   run_ESMC_TestErr2Ex_uni \
                   run_ESMC_TestErr3Ex_uni \
                   run_ESMF_TestErr1Ex_uni \
                   run_ESMF_TestErr2Ex_uni \
                   run_ESMF_TestErr3Ex_uni \
                   run_ESMF_TestLog1Ex_uni \
                   run_ESMF_TestLog2Ex_uni 


include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod *.txt *.stdout NULL core $(EXAMPLES_BUILD)
CLOBBERDIRS =

#
#  ESMC_TestErr1Ex
#
run_ESMC_TestErr1Ex:
	-${MPIRUN} -np 4 $(ESMF_EXDIR)/ESMC_TestErr1Ex > $(ESMF_EXDIR)/ESMC_TestErr1Ex.stdout


run_ESMC_TestErr1Ex_uni:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMC_TestErr1Ex > $(ESMF_EXDIR)/ESMC_TestErr1Ex_uni.stdout


#
#  ESMC_TestErr2Ex
#
run_ESMC_TestErr2Ex:
	-${MPIRUN} -np 4 $(ESMF_EXDIR)/ESMC_TestErr2Ex > $(ESMF_EXDIR)/ESMC_TestErr2Ex.stdout


run_ESMC_TestErr2Ex_uni:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMC_TestErr2Ex > $(ESMF_EXDIR)/ESMC_TestErr2Ex_uni.stdout


#
#  ESMC_TestErr3Ex
#
run_ESMC_TestErr3Ex:
	-${MPIRUN} -np 4 $(ESMF_EXDIR)/ESMC_TestErr3Ex > $(ESMF_EXDIR)/ESMC_TestErr3Ex.stdout


run_ESMC_TestErr3Ex_uni:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMC_TestErr3Ex > $(ESMF_EXDIR)/ESMC_TestErr3Ex_uni.stdout


#
#  ESMF_TestErr1Ex
#
run_ESMF_TestErr1Ex:
	-${MPIRUN} -np 4 $(ESMF_EXDIR)/ESMF_TestErr1Ex > $(ESMF_EXDIR)/ESMF_TestErr1Ex.stdout


run_ESMF_TestErr1Ex_uni:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMF_TestErr1Ex > $(ESMF_EXDIR)/ESMF_TestErr1Ex_uni.stdout


#
#  ESMF_TestErr2Ex
#
run_ESMF_TestErr2Ex:
	-${MPIRUN} -np 4 $(ESMF_EXDIR)/ESMF_TestErr2Ex > $(ESMF_EXDIR)/ESMF_TestErr2Ex.stdout


run_ESMF_TestErr2Ex_uni:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMF_TestErr2Ex > $(ESMF_EXDIR)/ESMF_TestErr2Ex_uni.stdout


#
#  ESMF_TestErr3Ex
#
run_ESMF_TestErr3Ex:
	-${MPIRUN} -np 4 $(ESMF_EXDIR)/ESMF_TestErr3Ex > $(ESMF_EXDIR)/ESMF_TestErr3Ex.stdout


run_ESMF_TestErr3Ex_uni:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMF_TestErr3Ex > $(ESMF_EXDIR)/ESMF_TestErr3Ex_uni.stdout


#
#  ESMF_TestLog1Ex
#
run_ESMF_TestLog1Ex:
	-${MPIRUN} -np 4 $(ESMF_EXDIR)/ESMF_TestLog1Ex > $(ESMF_EXDIR)/ESMF_TestLog1Ex.stdout


run_ESMF_TestLog1Ex_uni:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMF_TestLog1Ex > $(ESMF_EXDIR)/ESMF_TestLog1Ex_uni.stdout


#
#  ESMF_TestLog2Ex
#
run_ESMF_TestLog2Ex:
	-${MPIRUN} -np 4 $(ESMF_EXDIR)/ESMF_TestLog2Ex > $(ESMF_EXDIR)/ESMF_TestLog2Ex.stdout


run_ESMF_TestLog2Ex_uni:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMF_TestLog2Ex > $(ESMF_EXDIR)/ESMF_TestLog2Ex_uni.stdout


