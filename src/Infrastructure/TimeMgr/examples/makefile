# $Id: makefile,v 1.9 2003/04/24 05:32:40 eschwab Exp $ 

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
LOCDIR	  = src/Infrastructure/TimeMgr/examples

TESTEXAMPLES_3  = \
	ESMF_TimeEx RUN_ESMF_TimeEx ESMF_TimeEx.rm \
	ESMF_CalendarEx RUN_ESMF_CalendarEx ESMF_CalendarEx.rm \
	ESMF_CalRangeEx RUN_ESMF_CalRangeEx ESMF_CalRangeEx.rm \
	ESMF_ClockEx RUN_ESMF_ClockEx ESMF_ClockEx.rm \
	ESMC_ClockEx RUN_ESMC_ClockEx ESMC_ClockEx.rm

EXAMPLESF       = ESMF_TimeEx.F90 ESMF_CalendarEx.F90 ESMF_CalRangeEx.F90 \
			ESMF_ClockEx
EXAMPLESC       = ESMC_ClockEx.C

include ${ESMF_DIR}/makefile

DIRS = 

examples: ESMF_TimeEx ESMF_CalendarEx ESMF_CalRangeEx ESMF_ClockEx ESMC_ClockEx

ESMF_TimeMgrDir:
	mkdir -p ${ESMC_TESTDIR}

ESMF_TimeEx: ESMF_TimeEx.o  chkopts
	-${SL_F_LINKER} -g -o ESMF_TimeEx ESMF_TimeEx.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} $(SL_LINKOPTS)
	${RM} ESMF_TimeEx.o

RUN_ESMF_TimeEx:
	-@rm -f testlog stdlog.* logfile.* ESMF_TimeEx.tmps
	ESMF_TimeEx > ESMF_TimeEx.stdout

ESMF_TimeEx.rm:
	echo "ESMF_TimeEx.rm target called here."

ESMF_CalendarEx: ESMF_CalendarEx.o  chkopts
	-${SL_F_LINKER} -g -o ESMF_CalendarEx ESMF_CalendarEx.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} $(SL_LINKOPTS)
	${RM} ESMF_CalendarEx.o

RUN_ESMF_CalendarEx:
	-@rm -f testlog stdlog.* logfile.* ESMF_CalendarEx.tmps
	ESMF_CalendarEx > ESMF_CalendarEx.stdout

ESMF_CalendarEx.rm:
	echo "ESMF_CalendarEx.rm target called here."

ESMF_CalRangeEx: ESMF_CalRangeEx.o  chkopts
	-${SL_F_LINKER} -g -o ESMF_CalRangeEx ESMF_CalRangeEx.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} $(SL_LINKOPTS)
	${RM} ESMF_CalRangeEx.o

RUN_ESMF_CalRangeEx:
	-@rm -f testlog stdlog.* logfile.* ESMF_CalRangeEx.tmps
	ESMF_CalRangeEx > ESMF_CalRangeEx.stdout

ESMF_CalRangeEx.rm:
	echo "ESMF_CalRangeEx.rm target called here."

ESMF_ClockEx: ESMF_ClockEx.o  chkopts
	-${SL_F_LINKER} -g -o ESMF_ClockEx ESMF_ClockEx.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} $(SL_LINKOPTS)
	${RM} ESMF_ClockEx.o

RUN_ESMF_ClockEx:
	-@rm -f testlog stdlog.* logfile.* ESMF_ClockEx.tmps
	ESMF_ClockEx > ESMF_ClockEx.stdout

ESMF_ClockEx.rm:
	echo "ESMF_ClockEx.rm target called here."

ESMC_ClockEx: ESMC_ClockEx.o  chkopts
	-${SL_C_LINKER} -g -o ESMC_ClockEx ESMC_ClockEx.o -lesmf ${CXXF90LIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} $(SL_LINKOPTS)
	${RM} ESMC_ClockEx.o

RUN_ESMC_ClockEx:
	-@rm -f testlog stdlog.* logfile.* ESMC_ClockEx.tmps
	ESMC_ClockEx > ESMC_ClockEx.stdout

ESMC_ClockEx.rm:
	echo "ESMC_ClockEx.rm target called here."
