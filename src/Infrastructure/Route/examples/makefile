# $Id$

ALL: build_examples
run: run_examples
run_uni: run_examples_uni

LOCDIR	  = src/Infrastructure/Route/examples

.NOTPARALLEL:
EXAMPLES_BUILD    = $(ESMF_EXDIR)/ESMF_RHandleBitForBitEx \
                    $(ESMF_EXDIR)/ESMF_RHandleFromRHandleEx \
                    $(ESMF_EXDIR)/ESMF_RHandleFromFileEx \
                    $(ESMF_EXDIR)/ESMF_RHandleReusabilityEx \
                    $(ESMF_EXDIR)/ESMF_RHandleDynamicMaskingEx
                    
EXAMPLES_RUN      = run_ESMF_RHandleBitForBitEx \
                    run_ESMF_RHandleFromRHandleEx \
                    run_ESMF_RHandleFromFileEx \
                    run_ESMF_RHandleReusabilityEx \
                    run_ESMF_RHandleDynamicMaskingEx
                    
EXAMPLES_RUN_UNI  = run_ESMF_RHandleReusabilityEx_uni \
                    run_ESMF_RHandleFromFileEx_uni \
                    run_ESMF_RHandleDynamicMaskingEx_uni


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

run_ESMF_RHandleFromFileEx:
	$(MAKE) EXNAME=RHandleFromFile NP=4 exfrun

run_ESMF_RHandleFromFileEx_uni:
	$(MAKE) EXNAME=RHandleFromFile NP=1 exfrun

run_ESMF_RHandleReusabilityEx:
	$(MAKE) EXNAME=RHandleReusability NP=4 exfrun

run_ESMF_RHandleReusabilityEx_uni:
	$(MAKE) EXNAME=RHandleReusability NP=1 exfrun

run_ESMF_RHandleDynamicMaskingEx:
	$(MAKE) EXNAME=RHandleDynamicMasking NP=4 exfrun

run_ESMF_RHandleDynamicMaskingEx_uni:
	$(MAKE) EXNAME=RHandleDynamicMasking NP=1 exfrun
