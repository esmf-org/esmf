# $Id: makefile,v 1.8.4.1 2008/04/02 20:07:05 cdeluca Exp $

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
LOCDIR	  = src/Infrastructure/FieldBundleComm/examples


#EXAMPLES_BUILD   =  $(ESMF_EXDIR)/ESMF_FieldBundleCommEx
EXAMPLES_BUILD   =


#EXAMPLES_RUN     =  run_ESMF_FieldBundleCommEx
EXAMPLES_RUN     =


#EXAMPLES_RUN_UNI =  run_ESMF_FieldBundleCommEx_uni
EXAMPLES_RUN_UNI =


include ${ESMF_DIR}/makefile

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS =


run_ESMF_FieldBundleCommEx:
	$(MAKE) EXNAME=FieldBundleComm NP=4 exfrun

run_ESMF_FieldBundleCommEx_uni:
	$(MAKE) EXNAME=FieldBundleComm NP=1 exfrun

