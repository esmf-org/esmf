# $Id: makefile,v 1.5 2003/04/11 16:40:46 eschwab Exp $ 

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
	ESMF_ClockEx RUN_ESMF_ClockEx ESMF_ClockEx.rm \
	ESMC_ClockEx RUN_ESMC_ClockEx ESMC_ClockEx.rm

EXAMPLESF       = ESMF_ClockEx.F90
EXAMPLESC       = ESMC_ClockEx.C

include ${ESMF_DIR}/makefile

DIRS = 

examples: ESMF_ClockEx ESMC_ClockEx

ESMF_TimeMgrDir:
	mkdir -p ${ESMC_TESTDIR}

ESMF_ClockEx: ESMF_ClockEx.o  chkopts
	${SL_F_LINKER} -g -o ESMF_ClockEx ESMF_ClockEx.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} $(SL_LINKOPTS)
	${RM} ESMF_ClockEx.o

RUN_ESMF_ClockEx:
	-@rm -f testlog stdlog.* logfile.* ESMF_ClockEx.tmps
	ESMF_ClockEx > ESMF_ClockEx.stdout

ESMF_ClockEx.rm:
	echo "ESMF_ClockEx.rm target called here."

ESMC_ClockEx: ESMC_ClockEx.o  chkopts
	${SL_C_LINKER} -g -o ESMC_ClockEx ESMC_ClockEx.o -lesmf ${CXXF90LIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} $(SL_LINKOPTS)
	${RM} ESMC_ClockEx.o

RUN_ESMC_ClockEx:
	-@rm -f testlog stdlog.* logfile.* ESMC_ClockEx.tmps
	ESMC_ClockEx > ESMC_ClockEx.stdout

ESMC_ClockEx.rm:
	echo "ESMC_ClockEx.rm target called here."
