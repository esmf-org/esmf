# $Id: makefile,v 1.10 2003/04/15 20:09:31 svasquez Exp $ 

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

TESTEXAMPLES_1  = \
	ESMC_AppMainEx RUN_ESMC_AppMainEx ESMC_AppMainEx.rm \
	ESMC_CplEx RUN_ESMC_CplEx ESMC_CplEx.rm \
	ESMC_GCompEx RUN_ESMC_GCompEx ESMC_GCompEx.rm \
	ESMC_StateEx RUN_ESMC_StateEx ESMC_StateEx.rm \
	ESMC_XformEx RUN_ESMC_XformEx ESMC_XformEx.rm 

TESTEXAMPLES_3  = \
	ESMF_AppMainEx RUN_ESMF_AppMainEx ESMF_AppMainEx.rm \
	ESMF_CplEx RUN_ESMF_CplEx ESMF_CplEx.rm \
	ESMF_GCompEx RUN_ESMF_GCompEx ESMF_GCompEx.rm \
	ESMF_StateEx RUN_ESMF_StateEx ESMF_StateEx.rm \
	ESMF_XformEx RUN_ESMF_XformEx ESMF_XformEx.rm 

TESTEXAMPLES_4  = \
	ESMC_AppMainEx RUN_ESMC_AppMainExUNI ESMC_AppMainEx.rm \
	ESMC_CplEx RUN_ESMC_CplExUNI ESMC_CplEx.rm \
	ESMC_GCompEx RUN_ESMC_GCompExUNI ESMC_GCompEx.rm \
	ESMC_StateEx RUN_ESMC_StateExUNI ESMC_StateEx.rm \
	ESMC_XformEx RUN_ESMC_XformExUNI ESMC_XformEx.rm 

TESTEXAMPLES_9  = \
	ESMF_AppMainEx RUN_ESMF_AppMainEx ESMF_AppMainEx.rm \
	ESMF_CplEx RUN_ESMF_CplEx ESMF_CplEx.rm \
	ESMF_GCompEx RUN_ESMF_GCompEx ESMF_GCompEx.rm \
	ESMF_StateEx RUN_ESMF_StateEx ESMF_StateEx.rm \
	ESMF_XformEx RUN_ESMF_XformEx ESMF_XformEx.rm 


EXAMPLESF  = ESMF_AppMainEx.F90 ESMF_CplEx.F90 ESMF_GCompEx.F90 \
             ESMF_StateEx.F90 ESMF_XformEx.F90

EXAMPLESC  = ESMC_AppMainEx.C ESMC_CplEx.C ESMC_GCompEx.C \
             ESMC_StateEx.C ESMC_XformEx.C



examples:  ESMF_AppMainEx ESMF_CplEx ESMF_GCompEx ESMF_StateEx ESMF_XformEx \
           ESMC_AppMainEx ESMC_CplEx ESMC_GCompEx ESMC_StateEx ESMC_XformEx


include $(ESMF_DIR)/makefile

DIRS	  = 


#------------------------
# Fortran examples
#------------------------

#------------------------
ESMF_AppMainCreateDir:
	mkdir -p ${ESMC_TESTDIR}

ESMF_AppMainEx: ESMF_AppMainEx.o  
	-${F90CXXLD} -g -o ESMF_AppMainEx ESMF_AppMainEx.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} 
	${RM} ESMF_AppMainEx.o

RUN_ESMF_AppMainEx:
	-@rm -f testlog stdlog.* logfile.* ESMF_AppMainEx.tmps
	-${MPIRUN} -np 4 ESMF_AppMainEx > ESMF_AppMainEx.stdout

RUN_ESMF_AppMainExUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_AppMainEx.tmps
	-${MPIRUN} -np 1 ESMF_AppMainEx > ESMF_AppMainEx.stdout

ESMF_AppMainEx.rm:
	echo "ESMF_AppMainEx.rm target called here."


#------------------------
ESMF_CplCreateDir:
	mkdir -p ${ESMC_TESTDIR}

ESMF_CplEx: ESMF_CplEx.o  
	-${F90CXXLD} -g -o ESMF_CplEx ESMF_CplEx.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} 
	${RM} ESMF_CplEx.o

RUN_ESMF_CplEx:
	-@rm -f testlog stdlog.* logfile.* ESMF_CplEx.tmps
	-${MPIRUN} -np 4 ESMF_CplEx > ESMF_CplEx.stdout

RUN_ESMF_CplExUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_CplEx.tmps
	-${MPIRUN} -np 1 ESMF_CplEx > ESMF_CplEx.stdout

ESMF_CplEx.rm:
	echo "ESMF_CplEx.rm target called here."


#------------------------
ESMF_GCompCreateDir:
	mkdir -p ${ESMC_TESTDIR}

ESMF_GCompEx: ESMF_GCompEx.o  
	-${F90CXXLD} -g -o ESMF_GCompEx ESMF_GCompEx.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} 
	${RM} ESMF_GCompEx.o

RUN_ESMF_GCompEx:
	-@rm -f testlog stdlog.* logfile.* ESMF_GCompEx.tmps
	-${MPIRUN} -np 4 ESMF_GCompEx > ESMF_GCompEx.stdout

RUN_ESMF_GCompExUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_GCompEx.tmps
	-${MPIRUN} -np 1 ESMF_GCompEx > ESMF_GCompEx.stdout

ESMF_GCompEx.rm:
	echo "ESMF_GCompEx.rm target called here."


#------------------------
ESMF_StateCreateDir:
	mkdir -p ${ESMC_TESTDIR}

ESMF_StateEx: ESMF_StateEx.o  
	-${F90CXXLD} -g -o ESMF_StateEx ESMF_StateEx.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} 
	${RM} ESMF_StateEx.o

RUN_ESMF_StateEx:
	-@rm -f testlog stdlog.* logfile.* ESMF_StateEx.tmps
	-${MPIRUN} -np 4 ESMF_StateEx > ESMF_StateEx.stdout

RUN_ESMF_StateExUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_StateEx.tmps
	-${MPIRUN} -np 1 ESMF_StateEx > ESMF_StateEx.stdout

ESMF_StateEx.rm:
	echo "ESMF_StateEx.rm target called here."


#------------------------
ESMF_XformCreateDir:
	mkdir -p ${ESMC_TESTDIR}

ESMF_XformEx: ESMF_XformEx.o  
	-${F90CXXLD} -g -o ESMF_XformEx ESMF_XformEx.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} 
	${RM} ESMF_XformEx.o

RUN_ESMF_XformEx:
	-@rm -f testlog stdlog.* logfile.* ESMF_XformEx.tmps
	-${MPIRUN} -np 4 ESMF_XformEx > ESMF_XformEx.stdout

RUN_ESMF_XformExUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_XformEx.tmps
	-${MPIRUN} -np 1 ESMF_XformEx > ESMF_XformEx.stdout

ESMF_XformEx.rm:
	echo "ESMF_XformEx.rm target called here."


#------------------------
# C++ examples
#------------------------

#------------------------
ESMC_AppMainCreateDir:
	mkdir -p ${ESMC_TESTDIR}

ESMC_AppMainEx: ESMC_AppMainEx.o  
	-${F90CXXLD} -g -o ESMC_AppMainEx ESMC_AppMainEx.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} 
	${RM} ESMC_AppMainEx.o

RUN_ESMC_AppMainEx:
	-@rm -f testlog stdlog.* logfile.* ESMC_AppMainEx.tmps
	-${MPIRUN} -np 4 ESMC_AppMainEx > ESMC_AppMainEx.stdout

RUN_ESMC_AppMainExUNI:
	-@rm -f testlog stdlog.* logfile.* ESMC_AppMainEx.tmps
	-${MPIRUN} -np 1 ESMC_AppMainEx > ESMC_AppMainEx.stdout

ESMC_AppMainEx.rm:
	echo "ESMC_AppMainEx.rm target called here."


#------------------------
ESMC_CplCreateDir:
	mkdir -p ${ESMC_TESTDIR}

ESMC_CplEx: ESMC_CplEx.o  
	-${F90CXXLD} -g -o ESMC_CplEx ESMC_CplEx.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} 
	${RM} ESMC_CplEx.o

RUN_ESMC_CplEx:
	-@rm -f testlog stdlog.* logfile.* ESMC_CplEx.tmps
	-${MPIRUN} -np 4 ESMC_CplEx > ESMC_CplEx.stdout

RUN_ESMC_CplExUNI:
	-@rm -f testlog stdlog.* logfile.* ESMC_CplEx.tmps
	-${MPIRUN} -np 1 ESMC_CplEx > ESMC_CplEx.stdout

ESMC_CplEx.rm:
	echo "ESMC_CplEx.rm target called here."


#------------------------
ESMC_GCompCreateDir:
	mkdir -p ${ESMC_TESTDIR}

ESMC_GCompEx: ESMC_GCompEx.o  
	-${F90CXXLD} -g -o ESMC_GCompEx ESMC_GCompEx.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} 
	${RM} ESMC_GCompEx.o

RUN_ESMC_GCompEx:
	-@rm -f testlog stdlog.* logfile.* ESMC_GCompEx.tmps
	-${MPIRUN} -np 4 ESMC_GCompEx > ESMC_GCompEx.stdout

RUN_ESMC_GCompExUNI:
	-@rm -f testlog stdlog.* logfile.* ESMC_GCompEx.tmps
	-${MPIRUN} -np 1 ESMC_GCompEx > ESMC_GCompEx.stdout

ESMC_GCompEx.rm:
	echo "ESMC_GCompEx.rm target called here."


#------------------------
ESMC_StateCreateDir:
	mkdir -p ${ESMC_TESTDIR}

ESMC_StateEx: ESMC_StateEx.o  
	-${F90CXXLD} -g -o ESMC_StateEx ESMC_StateEx.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} 
	${RM} ESMC_StateEx.o

RUN_ESMC_StateEx:
	-@rm -f testlog stdlog.* logfile.* ESMC_StateEx.tmps
	-${MPIRUN} -np 4 ESMC_StateEx > ESMC_StateEx.stdout

RUN_ESMC_StateExUNI:
	-@rm -f testlog stdlog.* logfile.* ESMC_StateEx.tmps
	-${MPIRUN} -np 1 ESMC_StateEx > ESMC_StateEx.stdout

ESMC_StateEx.rm:
	echo "ESMC_StateEx.rm target called here."


#------------------------
ESMC_XformCreateDir:
	mkdir -p ${ESMC_TESTDIR}

ESMC_XformEx: ESMC_XformEx.o  
	-${F90CXXLD} -g -o ESMC_XformEx ESMC_XformEx.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} 
	${RM} ESMC_XformEx.o

RUN_ESMC_XformEx:
	-@rm -f testlog stdlog.* logfile.* ESMC_XformEx.tmps
	-${MPIRUN} -np 4 ESMC_XformEx > ESMC_XformEx.stdout

RUN_ESMC_XformExUNI:
	-@rm -f testlog stdlog.* logfile.* ESMC_XformEx.tmps
	-${MPIRUN} -np 1 ESMC_XformEx > ESMC_XformEx.stdout

ESMC_XformEx.rm:
	echo "ESMC_XformEx.rm target called here."

