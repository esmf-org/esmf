# $Id: makefile,v 1.2 2003/11/06 21:05:51 nscollins Exp $ 

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
LOCDIR	  = src/Superstructure/AppDriver/seq_pairwise_fdriver_spmd

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = *.o *.mod 
CLOBBERDIRS =

APPOBJS = CouplerComponentTemplate.o GridComponentTemplate.o \
          ParentGridComponentTemplate.o AppDriver.o
APPMODS = USERCPLCOMPMOD.mod USERGRIDCOMPMOD.mod USERPARENTGRIDCOMPMOD.mod

#
#
# Components
#
executable: $(APPOBJS)
	-$(SL_F_LINKER) -o Application $(APPOBJS) -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} $(SL_LINKOPTS)
	${RM} -f $(APPOBJS) $(APPMODS)

run:  Application
	-${MPIRUN} -np 4 ./Application

rununi: Application
	-${MPIRUN} -np 1 ./Application

