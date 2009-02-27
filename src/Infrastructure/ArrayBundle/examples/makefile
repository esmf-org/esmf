# $Id: makefile,v 1.3 2009/02/27 22:48:02 svasquez Exp $

ALL:      build_examples
run:      run_examples
run_uni:  run_examples_uni

LOCDIR	  = src/Infrastructure/ArrayBundle/examples


EXAMPLES_BUILD    = $(ESMF_EXDIR)/ESMF_ArrayBundleEx

EXAMPLES_RUN      = run_ESMF_ArrayBundleEx

EXAMPLES_RUN_UNI  = 


include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   = 
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS =

#
#  Run targets
#
run_ESMF_ArrayBundleEx:
	$(MAKE) EXNAME=ArrayBundle NP=4 exfrun


