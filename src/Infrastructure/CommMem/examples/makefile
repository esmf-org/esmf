# $Id: makefile,v 1.14 2003/09/17 22:00:15 flanigan Exp $

ALL: examples


CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  = 
SOURCEH	  = 
OBJSC     = 
OBJSF     = 
LOCDIR	  = src/Infrastructure/CommMem/examples


EXAMPLES_BUILD   =  $(ESMF_EXDIR)/ESMF_TimeEx \
                    $(ESMF_EXDIR)/ESMF_TimeIntervalEx \
                    $(ESMF_EXDIR)/ESMF_CalendarEx \
                    $(ESMF_EXDIR)/ESMF_ClockEx \
		    $(ESMF_EXDIR)/ESMC_ClockEx \
                    $(ESMF_EXDIR)/ESMF_ClockAdvEx

EXAMPLES_RUN     =  run_ESMF_TimeEx \
                    run_ESMF_TimeIntervalEx \
                    run_ESMF_CalendarEx \
                    run_ESMF_ClockEx \
		    run_ESMC_ClockEx \
                    run_ESMF_ClockAdvEx

EXAMPLES_RUN_UNI =  run_ESMF_TimeEx_uni \
                    run_ESMF_TimeIntervalEx_uni \
                    run_ESMF_CalendarEx_uni \
                    run_ESMF_ClockEx_uni \
		    run_ESMC_ClockEx_uni \
                    run_ESMF_ClockAdvEx_uni



include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = *.o *.mod
CLOBBERDIRS =

ESMF_DELayout_FEx: ESMF_DELayout_FEx.o chkopts
	-${F90CXXLD} -o ESMF_DELayout_FEx ESMF_DELayout_FEx.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB}
	${RM} ESMF_DELayout_FEx.o

ESMF_DELayout_FEx2: ESMF_DELayout_FEx2.o chkopts
	-${F90CXXLD} -o ESMF_DELayout_FEx2 ESMF_DELayout_FEx2.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB}
	${RM} ESMF_DELayout_FEx2.o

ESMF_DELayout_FEx3: ESMF_DELayout_FEx3.o chkopts
	-${F90CXXLD} -o ESMF_DELayout_FEx3 ESMF_DELayout_FEx3.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB}
	${RM} ESMF_DELayout_FEx3.o

ESMC_CommMemEx: ESMC_CommMemEx.o  chkopts
	-${CXXF90LD} -o ESMC_CommMemEx ESMC_CommMemEx.o -lesmf ${CXXF90LIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} -lm
	${RM} ESMC_CommMemEx.o





