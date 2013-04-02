# $Id$ 

ALL:      build_examples
run:      run_examples
run_uni:  run_examples_uni

LOCDIR	  = src/Superstructure/WebServices/examples


EXAMPLES_BUILD = $(ESMF_EXDIR)/ESMF_WebServicesEx 

EXAMPLES_RUN   = run_ESMF_WebServicesEx

EXAMPLES_RUN_UNI = run_ESMF_WebServicesEx_uni
                   
include $(ESMF_DIR)/makefile


CLEANDIRS   = 
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS =

DIRS = 

#
# ESMF_WebServicesEx
#
run_ESMF_WebServicesEx:
	$(MAKE) EXNAME=WebServices NP=4 exfrun

run_ESMF_WebServicesEx_uni:
	$(MAKE) EXNAME=WebServices NP=1 exfrun
