# $Id: makefile,v 1.2 2003/02/27 17:54:53 flanigan Exp $ 

ALL: 

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  =
SOURCEH	  = 
OBJSC	  = 
OBJSF	  =
MANSEC	  = 
LIBBASE	  = 
DIRS	  = 
LOCDIR	  = src/LowLevelUtils/TimeMgmt/examples/C

TESTEXAMPLES_1  = ESMC_TimeEx.Alice RUN_ESMC_TimeEx ESMC_TimeEx.rm \
	 ESMC_DateEx.Alice RUN_ESMC_DateEx ESMC_DateEx.rm ESMC_TimeMgrEx.Alice \
	 RUN_ESMC_TimeMgrEx ESMC_TimeMgrEx.rm

TESTEXAMPLES_4 = ${TESTEXAMPLES_1}

EXAMPLESC       = ESMC_TimeEx.c ESMC_DateEx.c ESMC_TimeMgrEx.c

include $(ESMF_DIR)/makefile

DIRS = 

MFLOGDIR=${ESMC_TESTDIR}

ESMC_TimeEx: ESMC_TimeEx.o  chkopts
	-${CLINKER} -o ESMC_TimeEx ESMC_TimeEx.o ${ESMC_TIME_LIB} -lesmf ${MPI_LIB} ${MP_LIB} ${THREAD_LIB}
	${RM} -f ESMC_TimeEx.o

ESMC_DateEx: ESMC_DateEx.o  chkopts
	-${CLINKER} -o ESMC_DateEx ESMC_DateEx.o ${ESMC_TIME_LIB} -lesmf -lm ${MPI_LIB} ${MP_LIB} ${THREAD_LIB}
	${RM} -f ESMC_DateEx.o

ESMC_TimeMgrEx: ESMC_TimeMgrEx.o  chkopts
	-${CLINKER} -o ESMC_TimeMgrEx ESMC_TimeMgrEx.o ${ESMC_TIME_LIB} -lesmf ${MPI_LIB} ${MP_LIB} ${THREAD_LIB}
	${RM} -f ESMC_TimeMgrEx.o

RUN_ESMC_TimeEx:
	${MPIRUN} -np 1 ESMC_TimeEx > ${MFLOGDIR}/ESMC_TimeEx.log

RUN_ESMC_DateEx:
	${MPIRUN} -np 1 ESMC_DateEx > ${MFLOGDIR}/ESMC_DateEx.log

RUN_ESMC_TimeMgrEx:
	${MPIRUN} -np 1 ESMC_TimeMgrEx > ${MFLOGDIR}/ESMC_TimeMgrEx.log

