# $Id: makefile,v 1.8 2003/09/18 15:37:45 flanigan Exp $

# makefile template for the examples directory.

ALL: examples

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
LOCDIR	  = src/Infrastructure/Grid/examples

EXAMPLES_BUILD    = $(ESMF_EXDIR)/ESMF_GridCreateEx

EXAMPLES_RUN      = run_ESMF_GridCreateEx

EXAMPLES_RUN_UNI  = run_ESMF_GridCreateEx_uni

include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod *.txt *.stdout NULL core $(EXAMPLES_BUILD)
CLOBBERDIRS =

#
#  ESMF_GridCreate
#
run_ESMF_GridCreateEx:
	-${MPIRUN} -np 4 $(ESMF_EXDIR)/ESMF_GridCreateEx > $(ESMF_EXDIR)/ESMF_GridCreateEx.stdout

run_ESMF_GridCreateEx_uni:
	echo "ESMF_GridCreateEx not set up to run on one processor."






