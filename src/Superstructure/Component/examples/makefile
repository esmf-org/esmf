# $Id: makefile,v 1.11 2003/09/18 17:12:13 flanigan Exp $ 

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
LOCDIR	  = src/Superstructure/Component/examples


EXAMPLES_BUILD = $(ESMF_EXDIR)/ESMC_AppMainEx \
                 $(ESMF_EXDIR)/ESMC_CplEx \
                 $(ESMF_EXDIR)/ESMC_GCompEx \
                 $(ESMF_EXDIR)/ESMC_StateEx \
                 $(ESMF_EXDIR)/ESMC_XformEx \
                 $(ESMF_EXDIR)/ESMF_AppMainEx \
                 $(ESMF_EXDIR)/ESMF_CplEx \
                 $(ESMF_EXDIR)/ESMF_GCompEx \
                 $(ESMF_EXDIR)/ESMF_StateEx \
                 $(ESMF_EXDIR)/ESMF_XformEx 


EXAMPLES_RUN   = run_ESMC_AppMainEx \
                 run_ESMC_CplEx \
                 run_ESMC_GCompEx \
                 run_ESMC_StateEx \
                 run_ESMC_XformEx \
                 run_ESMF_AppMainEx \
                 run_ESMF_CplEx \
                 run_ESMF_GCompEx \
                 run_ESMF_StateEx \
                 run_ESMF_XformEx   


EXAPMLES_RUN_UNI = run_ESMC_AppMainEx_uni \
                   run_ESMC_CplEx_uni \
                   run_ESMC_GCompEx_uni \
                   run_ESMC_StateEx_uni \
                   run_ESMC_XformEx_uni \
                   run_ESMF_AppMainEx_uni \
                   run_ESMF_CplEx_uni \
                   run_ESMF_GCompEx_uni \
                   run_ESMF_StateEx_uni \
                   run_ESMF_XformEx_uni   

include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod *.txt *.stdout NULL core $(EXAMPLES_BUILD)
CLOBBERDIRS =


#
# ESMC_AppMainEx
#
run_ESMC_AppMainEx:
	-${MPIRUN} -np 4 $(ESMF_EXDIR)/ESMC_AppMainEx > $(ESMF_EXDIR)/ESMC_AppMainEx.stdout

run_ESMC_AppMainEx_uni:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMC_AppMainEx > $(ESMF_EXDIR)/ESMC_AppMainEx.stdout

#
# ESMC_CplEx
#
run_ESMC_CplEx:
	-${MPIRUN} -np 4 $(ESMF_EXDIR)/ESMC_CplEx > $(ESMF_EXDIR)/ESMC_CplEx.stdout

run_ESMC_CplEx_uni:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMC_CplEx > $(ESMF_EXDIR)/ESMC_CplEx.stdout

#
# ESMC_GCompEx
#
run_ESMC_GCompEx:
	-${MPIRUN} -np 4 $(ESMF_EXDIR)/ESMC_GCompEx > $(ESMF_EXDIR)/ESMC_GCompEx.stdout

run_ESMC_GCompEx_uni:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMC_GCompEx > $(ESMF_EXDIR)/ESMC_GCompEx.stdout

#
# ESMC_StateEx
#
run_ESMC_StateEx:
	-${MPIRUN} -np 4 $(ESMF_EXDIR)/ESMC_StateEx > $(ESMF_EXDIR)/ESMC_StateEx.stdout

run_ESMC_StateEx_uni:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMC_StateEx > $(ESMF_EXDIR)/ESMC_StateEx.stdout

#
# ESMC_XformEx
#
run_ESMC_XformEx:
	-${MPIRUN} -np 4 $(ESMF_EXDIR)/ESMC_XformEx > $(ESMF_EXDIR)/ESMC_XformEx.stdout

run_ESMC_XformEx_uni:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMC_XformEx > $(ESMF_EXDIR)/ESMC_XformEx.stdout


#
# EMSF_AppMainEx
#
run_EMSF_AppMainEx:
	-${MPIRUN} -np 4 $(ESMF_EXDIR)/EMSF_AppMainEx > $(ESMF_EXDIR)/EMSF_AppMainEx.stdout

run_EMSF_AppMainEx_uni:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/EMSF_AppMainEx > $(ESMF_EXDIR)/EMSF_AppMainEx.stdout

#
# EMSF_CplEx
#
run_EMSF_CplEx:
	-${MPIRUN} -np 4 $(ESMF_EXDIR)/EMSF_CplEx > $(ESMF_EXDIR)/EMSF_CplEx.stdout

run_EMSF_CplEx_uni:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/EMSF_CplEx > $(ESMF_EXDIR)/EMSF_CplEx.stdout

#
# EMSF_GCompEx
#
run_EMSF_GCompEx:
	-${MPIRUN} -np 4 $(ESMF_EXDIR)/EMSF_GCompEx > $(ESMF_EXDIR)/EMSF_GCompEx.stdout

run_EMSF_GCompEx_uni:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/EMSF_GCompEx > $(ESMF_EXDIR)/EMSF_GCompEx.stdout

#
# EMSF_StateEx
#
run_EMSF_StateEx:
	-${MPIRUN} -np 4 $(ESMF_EXDIR)/EMSF_StateEx > $(ESMF_EXDIR)/EMSF_StateEx.stdout

run_EMSF_StateEx_uni:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/EMSF_StateEx > $(ESMF_EXDIR)/EMSF_StateEx.stdout

#
# EMSF_XformEx
#
run_EMSF_XformEx:
	-${MPIRUN} -np 4 $(ESMF_EXDIR)/EMSF_XformEx > $(ESMF_EXDIR)/EMSF_XformEx.stdout

run_EMSF_XformEx_uni:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/EMSF_XformEx > $(ESMF_EXDIR)/EMSF_XformEx.stdout
