# $Id: makefile,v 1.1.2.3 2008/07/17 21:57:34 feiliu Exp $ 

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
LOCDIR    = src/Infrastructure/FieldBundle/examples

EXAMPLES_BUILD = $(ESMF_EXDIR)/ESMF_FieldBundleCreateEx 

EXAMPLES_RUN = run_ESMF_FieldBundleCreateEx 

EXAMPLES_RUN_UNI = run_ESMF_FieldBundleCreateEx_uni 


include $(ESMF_DIR)/makefile


DIRS      = 

CLEANDIRS   = 
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS =


#
# ESMF_FieldBundleCreateEx
#
run_ESMF_FieldBundleCreateEx:
	$(MAKE) EXNAME=FieldBundleCreate NP=4 exfrun

run_ESMF_FieldBundleCreateEx_uni:
	$(MAKE) EXNAME=FieldBundleCreate NP=1 exfrun

