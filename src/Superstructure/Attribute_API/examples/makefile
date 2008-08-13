# $Id: makefile,v 1.1 2008/08/13 14:48:47 rokuingh Exp $

ALL:      build_examples
run:      run_examples
run_uni:  run_examples_uni

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  = 
SOURCEH	  = 
OBJSC     = 
OBJSF     = 
TEXFILES  = 
MANSEC	  = 
LIBBASE	  = 
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
