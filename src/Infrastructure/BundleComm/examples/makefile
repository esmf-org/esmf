# $Id: makefile,v 1.8 2006/01/30 20:10:06 nscollins Exp $

ALL:  build_examples

run: run_examples

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  =

SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
LOCDIR	  = src/Infrastructure/BundleComm/examples


#EXAMPLES_BUILD   =  $(ESMF_EXDIR)/ESMF_BundleCommEx
EXAMPLES_BUILD   =


#EXAMPLES_RUN     =  run_ESMF_BundleCommEx
EXAMPLES_RUN     =


#EXAMPLES_RUN_UNI =  run_ESMF_BundleCommEx_uni
EXAMPLES_RUN_UNI =


include ${ESMF_DIR}/makefile

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS =


run_ESMF_BundleCommEx:
	$(MAKE) EXNAME=BundleComm NP=4 exfrun

run_ESMF_BundleCommEx_uni:
	$(MAKE) EXNAME=BundleComm NP=1 exfrun

