# $Id: makefile,v 1.1 2003/09/17 22:01:38 cdeluca Exp $ 

ALL: 

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

TESTEXAMPLES_3  = \
        ESMF_BundleCreateEx RUN_ESMF_BundleCreateEx ESMF_BundleCreateEx.rm 

TESTEXAMPLES_9  = \
        ESMF_BundleCreateEx RUN_ESMF_BundleCreateExUNI ESMF_BundleCreateEx.rm 

EXAMPLESF       = ESMF_BundleCreateEx.F90
EXAMPLESC       = 

include $(ESMF_DIR)/makefile

DIRS      = 

CLEANDIRS   =
CLEANFILES  = *.o *.mod ESMF_BundleCreateEx 
CLOBBERDIRS =


examples: ESMF_BundleCreateEx 


ESMF_BundleCreateDir:
        mkdir -p ${ESMC_TESTDIR}

ESMF_BundleCreateEx: ESMF_BundleCreateEx.o  chkopts
        -${F90CXXLD} -o ESMF_BundleCreateEx ESMF_BundleCreateEx.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB}
        ${RM} ESMF_BundleCreateEx.o

RUN_ESMF_BundleCreateEx:
        -@rm -f testlog stdlog.* logfile.* ESMF_BundleCreateEx.tmps
        -${MPIRUN} -np 4 ESMF_BundleCreateEx > ESMF_BundleCreateEx.stdout

RUN_ESMF_BundleCreateExUNI:
        -@rm -f testlog stdlog.* logfile.* ESMF_BundleCreateEx.tmps
        -${MPIRUN} -np 1 ESMF_BundleCreateEx > ESMF_BundleCreateEx.stdout

ESMF_BundleCreateEx.rm:
        echo "ESMF_BundleCreateEx.rm target called here."

