# $Id: makefile,v 1.4 2003/09/18 16:49:38 flanigan Exp $ 

ALL: examples

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


EXAPMLES_RUN_UNI = run_ESMF_BundleCreateEx_uni 


include $(ESMF_DIR)/makefile


DIRS      = 

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod *.txt *.stdout NULL core $(EXAMPLES_BUILD)
CLOBBERDIRS =


#
# ESMF_BundleCreateEx
#
run_ESMF_BundleCreateEx:
	-${MPIRUN} -np 4 $(ESMF_EXDIR)/ESMF_BundleCreateEx > $(ESMF_EXDIR)/ESMF_BundleCreateEx.stdout

run_ESMF_BundleCreateEx_uni:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMF_BundleCreateEx > $(ESMF_EXDIR)/ESMF_BundleCreateEx_uni.stdout

