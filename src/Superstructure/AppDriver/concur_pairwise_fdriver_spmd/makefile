# $Id: makefile,v 1.1 2004/12/09 23:08:38 nscollins Exp $ 

# by default, this makefile builds the executable named "Application"
ALL:  executable

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  =

SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
LOCDIR	  = src/Superstructure/AppDriver/concur_pairwise_fdriver_spmd

# this includes all the ESMF definitions that we use below.
include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = *.o *.mod loadmap.txt
CLOBBERDIRS =

# object files which need to be compiled into the executable.
APPOBJS = CplCompTemplate.o GridComp1Template.o GridComp2Template.o \
          ParentGridCompTemplate.o AppDriver.o

#
# Sample application for which we supply skeleton files to use as a 
# starting point for a larger application.
#

# build the executable.
executable: $(APPOBJS)
	-$(SL_F_LINKER) -o Application $(APPOBJS) -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} ${NETCDF_LIB} $(SL_LINKOPTS)
	${RM} -f $(CLEANFILES)

# run the executable in parallel.
run:  Application
	-${MPIRUN} -np 4 ./Application

# run only a single copy.
rununi: Application
	-${MPIRUN} -np 1 ./Application


# files to be deleted when cleaning up.
clean:
	${RM} -f Application $(CLEANFILES)

