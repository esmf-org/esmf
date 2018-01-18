# $Id$

ALL: build_examples
run: run_examples
run_uni: run_examples_uni

LOCDIR	  = src/Infrastructure/Route/examples

.NOTPARALLEL:
EXAMPLES_BUILD    = $(ESMF_EXDIR)/ESMF_RHandleBitForBitEx \
                    $(ESMF_EXDIR)/ESMF_RHandleFromRHandleEx \
                    $(ESMF_EXDIR)/ESMF_RHandleDynamicMaskingEx
                    
EXAMPLES_RUN      = run_ESMF_RHandleBitForBitEx \
                    run_ESMF_RHandleFromRHandleEx \
                    run_ESMF_RHandleDynamicMaskingEx
                    
EXAMPLES_RUN_UNI  = 


include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   = 
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS =

#
#  Run targets
#
run_ESMF_RHandleBitForBitEx:
	$(MAKE) EXNAME=RHandleBitForBit NP=4 exfrun

run_ESMF_RHandleFromRHandleEx:
	$(MAKE) EXNAME=RHandleFromRHandle NP=4 exfrun

run_ESMF_RHandleDynamicMaskingEx:
	$(MAKE) EXNAME=RHandleDynamicMasking NP=4 exfrun
