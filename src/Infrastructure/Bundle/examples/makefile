# $Id: makefile,v 1.11 2006/01/30 20:10:06 nscollins Exp $ 

ALL: build_examples

run: run_examples

CFLAGS    =
FFLAGS    =
SOURCEC   = 
SOURCEF   =

SOURCEH   = 
OBJSC     =  
OBJSF     = 
MANSEC    = 
LIBBASE   = 
LOCDIR    = src/Infrastructure/Bundle/examples

EXAMPLES_BUILD = $(ESMF_EXDIR)/ESMF_BundleCreateEx 

EXAMPLES_RUN = run_ESMF_BundleCreateEx 

EXAMPLES_RUN_UNI = run_ESMF_BundleCreateEx_uni 


include $(ESMF_DIR)/makefile


DIRS      = 

CLEANDIRS   = 
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS =


#
# ESMF_BundleCreateEx
#
run_ESMF_BundleCreateEx:
	$(MAKE) EXNAME=BundleCreate NP=4 exfrun

run_ESMF_BundleCreateEx_uni:
	$(MAKE) EXNAME=BundleCreate NP=1 exfrun

