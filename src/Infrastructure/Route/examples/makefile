# $Id$

ALL: build_examples
run: run_examples
run_uni: run_examples_uni

LOCDIR	  = src/Infrastructure/Route/examples


EXAMPLES_BUILD    = $(ESMF_EXDIR)/ESMF_RHandleBitForBitEx

EXAMPLES_RUN      = run_ESMF_RHandleBitForBitEx

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

