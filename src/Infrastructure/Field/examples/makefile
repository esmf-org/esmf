# $Id: makefile,v 1.8 2003/09/18 16:29:59 flanigan Exp $ 

ALL:  examples

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  =

SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
LOCDIR	  = src/Infrastructure/Field/examples

EXAMPLES_BUILD = $(ESMF_EXDIR)/ESMC_FieldCreateEx \
                 $(ESMF_EXDIR)/ESMF_FieldCreateEx

EXAMPLES_RUN = run_ESMC_FieldCreateEx \
               run_ESMF_FieldCreateEx 

EXAPMLES_RUN_UNI = run_ESMC_FieldCreateEx_uni \
                   run_ESMF_FieldCreateEx_uni


include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   =
CLEANFILES  = *.o *.mod ESMF_FieldCreateEx ESMC_FieldCreateEx
CLOBBERDIRS =

#
# ESMC_FieldCreateEx
#
run_ESMC_FieldCreateEx:
	-${MPIRUN} -np 4 $(ESMF_EXDIR)/ESMC_FieldCreateEx > $(ESMF_EXDIR)/ESMC_FieldCreateEx.stdout

run_ESMC_FieldCreateEx_uni:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMC_FieldCreateEx > $(ESMF_EXDIR)/ESMC_FieldCreateEx_uni.stdout

#
# ESMF_FieldCreateEx
#
run_ESMF_FieldCreateEx:
	-${MPIRUN} -np 4 $(ESMF_EXDIR)/ESMF_FieldCreateEx > $(ESMF_EXDIR)/ESMF_FieldCreateEx.stdout

run_ESMF_FieldCreateEx_uni:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMF_FieldCreateEx > $(ESMF_EXDIR)/ESMF_FieldCreateEx_uni.stdout




