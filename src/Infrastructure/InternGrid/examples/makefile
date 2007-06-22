# $Id: makefile,v 1.1 2007/06/22 23:21:35 cdeluca Exp $

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
LOCDIR	  = src/Infrastructure/InternGrid/examples


EXAMPLES_BUILD    = $(ESMF_EXDIR)/ESMF_InternGridCreateEx

EXAMPLES_RUN      = run_ESMF_InternGridCreateEx

EXAMPLES_RUN_UNI  = run_ESMF_InternGridCreateEx_uni


include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   = 
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS =

#
#  ESMF_InternGridCreate
#
run_ESMF_InternGridCreateEx:
	$(MAKE) EXNAME=InternGridCreate NP=4 exfrun

run_ESMF_InternGridCreateEx_uni:
	echo "PASS: ESMF_InternGridCreateEx not set up to run on one processor." > $(ESMF_EXDIR)/ESMF_InternGridCreateEx.stdout




