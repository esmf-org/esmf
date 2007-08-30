# $Id: makefile,v 1.12 2007/08/30 05:06:44 cdeluca Exp $ 

ALL:      build_examples
run:      run_examples
run_uni:  run_examples_uni

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  =

SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
LOCDIR	  = src/Superstructure/State/examples


EXAMPLES_BUILD = $(ESMF_EXDIR)/ESMF_StateEx
# TODO:FIELDINTEGRATION Restore State reconcile example. 
#                 $(ESMF_EXDIR)/ESMF_StateReconcileEx 
#                 $(ESMF_EXDIR)/ESMC_StateEx 

                 
EXAMPLES_RUN   = run_ESMF_StateEx 
#                 run_ESMF_StateReconcileEx 
#                 run_ESMC_StateEx 
                 

EXAMPLES_RUN_UNI = run_ESMF_StateEx_uni 
#                   run_ESMF_StateReconcileEx_uni 
#                   run_ESMC_StateEx_uni 
                   
include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   = 
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS =


#
# ESMC_StateEx
#
run_ESMC_StateEx:
	$(MAKE) EXNAME=State NP=4 excrun

run_ESMC_StateEx_uni:
	$(MAKE) EXNAME=State NP=1 excrun

#
# ESMF_StateEx
#
run_ESMF_StateEx:
	$(MAKE) EXNAME=State NP=4 exfrun

run_ESMF_StateEx_uni:
	$(MAKE) EXNAME=State NP=1 exfrun


#
# ESMF_StateReconcileEx
#
run_ESMF_StateReconcileEx:
	$(MAKE) EXNAME=StateReconcile NP=4 exfrun

run_ESMF_StateReconcileEx_uni:
	$(MAKE) EXNAME=StateReconcile NP=1 exfrun



