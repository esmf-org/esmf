# $Id$

ALL:      build_examples
run:      run_examples
run_uni:  run_examples_uni

LOCDIR	  = src/Infrastructure/Grid/examples

.NOTPARALLEL:
EXAMPLES_BUILD    = $(ESMF_EXDIR)/ESMF_GridUsageEx \
	            $(ESMF_EXDIR)/ESMF_GridCreateRegFromDGEx

EXAMPLES_RUN      = run_ESMF_GridUsageEx \
	            run_ESMF_GridCreateRegFromDGEx

EXAMPLES_RUN_UNI  = run_ESMF_GridUsageEx_uni \
	            run_ESMF_GridCreateRegFromDGEx_uni

include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   = 
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS =

#
#  ESMF_GridUsage
#
run_ESMF_GridUsageEx:
	mkdir -p $(ESMF_EXDIR)/data
	cp -f data/*.nc $(ESMF_EXDIR)/data
	cp -f ../../Field/tests/data/*.nc $(ESMF_EXDIR)/data
	chmod u+rw $(ESMF_EXDIR)/data/*
	$(MAKE) EXNAME=GridUsage NP=4 exfrun

run_ESMF_GridUsageEx_uni:
	mkdir -p $(ESMF_EXDIR)/data
	cp -f data/*.nc $(ESMF_EXDIR)/data
	cp -f ../../Field/tests/data/*.nc $(ESMF_EXDIR)/data
	chmod u+rw $(ESMF_EXDIR)/data/*
	$(MAKE) EXNAME=GridUsage NP=1 exfrun

#
#  ESMF_GridCreateRegFromDG
#
run_ESMF_GridCreateRegFromDGEx:
	$(MAKE) EXNAME=GridCreateRegFromDG NP=4 exfrun

run_ESMF_GridCreateRegFromDGEx_uni:
	$(MAKE) EXNAME=GridCreateRegFromDG NP=1 exfrun
