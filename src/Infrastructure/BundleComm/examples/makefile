# $Id: makefile,v 1.6 2004/08/03 19:33:44 nscollins Exp $

ALL:  build_examples

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

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod *.txt *.stdout NULL core ESMF_LogFile $(EXAMPLES_BUILD)
CLOBBERDIRS =


run_ESMF_BundleCommEx:
	-${MPIRUN} -np 4 $(ESMF_EXDIR)/ESMF_BundleCommEx > $(ESMF_EXDIR)/ESMF_BundleCommEx.stdout

run_ESMF_BundleCommEx_uni:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMF_BundleCommEx > $(ESMF_EXDIR)/ESMF_BundleCommEx.stdout

