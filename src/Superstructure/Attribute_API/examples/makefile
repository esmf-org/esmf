# $Id: makefile,v 1.2 2008/09/26 05:18:09 cdeluca Exp $

ALL:      build_examples
run:      run_examples
run_uni:  run_examples_uni

LOCDIR	  = src/Superstructure/Attribute_API/examples


EXAMPLES_BUILD    = $(ESMF_EXDIR)/ESMF_AttributeEx

EXAMPLES_RUN      = run_ESMF_AttributeEx

EXAMPLES_RUN_UNI  = run_ESMF_AttributeEx_uni

include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   = 
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS =

#
#  ESMF_Attribute
#
run_ESMF_AttributeEx:
	$(MAKE) EXNAME=Attribute NP=4 exfrun

run_ESMF_AttributeEx_uni:
	$(MAKE) EXNAME=Attribute NP=1 exfrun
