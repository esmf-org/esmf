# $Id: makefile,v 1.3 2003/04/15 20:48:12 svasquez Exp $ 

ALL: 

CFLAGS    =
FFLAGS    =

# Do not uses source, obj for examples.  These pertain to library sources
# and objects.
SOURCEC	  = 
SOURCEF	  =
SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
DIRS	  = 
LOCDIR	  = src/Infrastructure/TimeMgmt/examples/F90

TESTEXAMPLES_3  = \
	ESMF_TimeEx RUN_ESMF_TimeEx ESMF_TimeEx.rm \
	ESMF_DateEx RUN_ESMF_DateEx ESMF_DateEx.rm \
	ESMF_TimeMgrEx RUN_ESMF_TimeMgrEx ESMF_TimeMgrEx.rm \
	ESMF_AlarmEx RUN_ESMF_AlarmEx ESMF_AlarmEx.rm 

TESTEXAMPLES_9  = \
	ESMF_TimeEx RUN_ESMF_TimeEx ESMF_TimeEx.rm \
	ESMF_DateEx RUN_ESMF_DateEx ESMF_DateEx.rm \
	ESMF_TimeMgrEx RUN_ESMF_TimeMgrEx ESMF_TimeMgrEx.rm \
	ESMF_AlarmEx RUN_ESMF_AlarmEx ESMF_AlarmEx.rm 

EXAMPLESF       = ESMF_TimeEx.f ESMF_DateEx.f ESMF_TimeMgrEx.f ESMF_AlarmEx.f

include $(ESMF_DIR)/makefile

DIRS = 

ESMF_TimeEx: ESMF_TimeEx.o  chkopts
	-${FLINKER} -o ESMF_TimeEx ESMF_TimeEx.o ${ESMC_TIME_LIB} -lesmf ${MPI_LIB} ${MP_LIB} ${THREAD_LIB}
	${RM} -f ESMF_TimeEx.o

ESMF_DateEx: ESMF_DateEx.o  chkopts
	-${FLINKER} -o ESMF_DateEx ESMF_DateEx.o ${ESMC_TIME_LIB} -lesmf ${MPI_LIB} ${MP_LIB} ${THREAD_LIB}
	${RM} -f ESMF_DateEx.o

ESMF_TimeMgrEx: ESMF_TimeMgrEx.o  chkopts
	-${FLINKER} -o ESMF_TimeMgrEx ESMF_TimeMgrEx.o ${ESMC_TIME_LIB} -lesmf ${MPI_LIB} ${MP_LIB} ${THREAD_LIB}
	${RM} -f ESMF_TimeMgrEx.o

ESMF_AlarmEx: ESMF_AlarmEx.o  chkopts
	-${FLINKER} -o ESMF_AlarmEx ESMF_AlarmEx.o ${ESMC_TIME_LIB} -lesmf ${MPI_LIB} ${MP_LIB} ${THREAD_LIB}
	${RM} -f ESMF_AlarmEx.o

RUN_ESMF_TimeEx:
	-${MPIRUN} -np 1 ESMF_TimeEx > ${ESMC_TESTDIR}/ESMF_TimeEx.log

RUN_ESMF_DateEx:
	-${MPIRUN} -np 1 ESMF_DateEx > ${ESMC_TESTDIR}/ESMF_DateEx.log

RUN_ESMF_TimeMgrEx:
	-${MPIRUN} -np 1 ESMF_TimeMgrEx > ${ESMC_TESTDIR}/ESMF_TimeMgrEx.log

RUN_ESMF_AlarmEx:
	-${MPIRUN} -np 1 ESMF_AlarmEx > ${ESMC_TESTDIR}/ESMF_AlarmEx.log
