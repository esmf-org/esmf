# $Id: makefile,v 1.1 2003/02/26 01:18:02 nscollins Exp $ 

ALL:  test

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
LOCDIR	  = src/SystemTests/63029

TESTEXAMPLES_3  = \
	ESMF_SysTest63029  RUN_ESMF_SysTest63029  ESMF_SysTest63029.rm 

TESTEXAMPLES_9  = \
	ESMF_SysTest63029  RUN_ESMF_SysTest63029UNI  ESMF_SysTest63029.rm 

EXAMPLESF       = user_model.F90 ESMF_SysTest63029.F90

EXAMPLESC       = 

test:	ESMF_SysTest63029

include ${ESMF_DIR}/build/${ESMF_ARCH}/base


ESMF_SysTest63029: ESMF_SysTest63029.o  user_model.F90 chkopts
	-${F90CXXLD} -o ESMF_SysTest63029 ESMF_SysTest63029.o user_model.o \
	    -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB}
	${RM} ESMF_SysTest63029.o

RUN_ESMF_SysTest63029:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest63029.tmps
	${MPIRUN} -np 2 ESMF_SysTest63029 > ESMF_SysTest63029.stdout

RUN_ESMF_SysTest63029UNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest63029.tmps
	${MPIRUN} -np 1 ESMF_SysTest63029 > ESMF_SysTest63029.stdout

ESMF_SysTest63029.rm:
	echo "ESMF_SysTest63029.rm target called here."


