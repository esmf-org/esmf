# $Id: makefile,v 1.3 2002/12/09 23:16:39 nscollins Exp $ 

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
LOCDIR	  = src/Infrastructure/Data/examples

TESTEXAMPLES_3  = \
	ESMF_ArrayCreateEx RUN_ESMF_ArrayCreateEx ESMF_ArrayCreateEx.rm

TESTEXAMPLES_9  = \
	ESMF_ArrayCreateEx RUN_ESMF_ArrayCreateExUNI ESMF_ArrayCreateEx.rm

EXAMPLESF       = ESMF_ArrayCreateEx.F90

include ${ESMF_DIR}/build/${ESMF_ARCH}/base

ESMF_ArrayCreateDir:
	mkdir -p ${ESMC_TESTDIR}

ESMF_ArrayCreateEx: ESMF_ArrayCreateEx.o  chkopts
	${F90CXXLD} -v -o ESMF_ArrayCreateEx ESMF_ArrayCreateEx.o ${ESMC_TIME_LIB} -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} 
	${RM} -f ESMF_ArrayCreateEx.o

RUN_ESMF_ArrayCreateEx:
	-@rm -f testlog stdlog.* logfile.* ESMF_ArrayCreateEx.tmps
	${MPIRUN} -np 4 ESMF_ArrayCreateEx > ESMF_ArrayCreateEx.stdout
	#@sort -t, -k2 testlog | cut -d, -f4- > ESMF_ArrayCreateEx.tmp
	#@if (diff ESMF_ArrayCreateEx.tmp ${ESMF_DIR}/${LOCDIR}/output/ESMF_ArrayCreateEx.out) then \
	#echo "PASS ArrayCreateger test" > ${ESMC_TESTDIR}/ESMF_ArrayCreateEx.log; else \
	#echo "FAIL ArrayCreateger test" > ${ESMC_TESTDIR}/ESMF_ArrayCreateEx.log; fi
	#@rm -f testlog stdlog.* logfile.* ESMF_ArrayCreateEx.tmp optlog

RUN_ESMF_ArrayCreateExUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_ArrayCreateEx.tmps
	${MPIRUN} -np 1 ESMF_ArrayCreateEx > ESMF_ArrayCreateEx.stdout
	#@sort -t, -k2 testlog | cut -d, -f4- > ESMF_ArrayCreateEx.tmp
	#@if (diff ESMF_ArrayCreateEx.tmp ${ESMF_DIR}/${LOCDIR}/output/ESMF_ArrayCreateEx.outuni) then \
	#echo "PASS ArrayCreateEx test" > ${ESMC_TESTDIR}/ESMF_ArrayCreateEx.log; else \
	#echo "FAIL ArrayCreateEx test" > ${ESMC_TESTDIR}/ESMF_ArrayCreateEx.log; fi
	#@rm -f testlog stdlog.* logfile.* ESMF_ArrayCreateEx.tmp optlog

ESMF_ArrayCreateEx.rm:
	echo "ESMF_ArrayCreateEx.rm target called here."


# local for halem
doit:
	rm -f ESMF_ArrayCreateEx.o ESMF_ArrayCreateEx
	f90 -c -I/u1/nancy/esmf/esmf/mod/modg/alpha -g -assume gfullpath  -free -cpp -I/u1/nancy/esmf/esmf/src/Infrastructure/Data/examples -I/u1/nancy/esmf/esmf/src/Infrastructure/Data/examples/../include -I/u1/nancy/esmf/esmf -I/u1/nancy/esmf/esmf/src/include -I/usr/local/include -I/u1/nancy/esmf/esmf/build/alpha -I/u1/nancy/esmf/esmf/include -I/u1/nancy/esmf/esmf/mod/modg/alpha  -I/u1/nancy/esmf/esmf/timing /u1/nancy/esmf/esmf/src/Infrastructure/Data/examples/ESMF_ArrayCreateEx.F90
	f90 -L/u1/nancy/esmf/esmf/lib/libg/alpha -v -o ESMF_ArrayCreateEx ESMF_ArrayCreateEx.o -L/u1/nancy/esmf/esmf/lib/libg/alpha -lesmf -L/usr/ccs/lib/cmplrs/cxx -lcxx  -L/usr/opt/mpi/lib -lmpi    


#	/usr/bin/cc -L/u1/nancy/esmf/esmf/lib/libg/alpha -v -o ESMC_ArrayCreateEx ESMC_ArrayCreateEx.o -L/u1/nancy/esmf/esmf/lib/libg/alpha -L/usr/ccs/lib/cmplrs/cxx -L/usr/opt/mpi/lib /usr/lib/cmplrs/fort90/for_main.o -O4 -lesmf -lcxx -lmpi -qlshpf -lUfor -lfor -lFutil -lm -lots -lm_c32 -lmld -lexc 
#	/usr/lib/cmplrs/cc/ld -o ESMC_ArrayCreateEx -L/u1/nancy/esmf/esmf/lib/libg/alpha -L/u1/nancy/esmf/esmf/lib/libg/alpha -L/usr/ccs/lib/cmplrs/cxx -L/usr/opt/mpi/lib -g0 -O4 -call_shared /usr/lib/cmplrs/cc/crt0.o ESMC_ArrayCreateEx.o /usr/lib/cmplrs/fort90/for_main.o -lesmf -lcxx -lmpi -qlshpf -lUfor -lfor -lFutil -lm -lots -lm_c32 -lmld -lexc -lc 

