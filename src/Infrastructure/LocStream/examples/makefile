# $Id: makefile,v 1.2 2008/05/07 06:00:40 oehmke Exp $

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
LOCDIR	  = src/Infrastructure/LocStream/examples


EXAMPLES_BUILD    = $(ESMF_EXDIR)/ESMF_LocStreamEx


EXAMPLES_RUN      = run_ESMF_LocStreamEx

EXAMPLES_RUN_UNI  = run_ESMF_LocStreamEx_uni


include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   = 
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS =

#
#  ESMF_LocStream
#
run_ESMF_LocStreamEx:
	$(MAKE) EXNAME=LocStream NP=4 exfrun

run_ESMF_LocStreamEx_uni:
	$(MAKE) EXNAME=LocStream NP=1 exfrun





