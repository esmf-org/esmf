# $Id: makefile,v 1.6 2007/06/23 04:00:49 cdeluca Exp $ 

# by default, this makefile builds the executable named "Application"
ALL:  executable

LOCDIR	  = src/Superstructure/AppDriver/concur_pairwise_fdriver_spmd

# this includes all the ESMF definitions that we use below.
include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = 
CLOBBERDIRS =

# object files which need to be compiled into the executable.
APPOBJS = CplCompTemplate.o IGridComp1Template.o IGridComp2Template.o \
          ParentIGridCompTemplate.o AppDriver.o

#
# Sample application for which we supply skeleton files to use as a 
# starting point for a larger application.
#

# build the executable.
executable: $(APPOBJS)
	$(FLINKER) $(LINKOPTS) -o Application $(APPOBJS) $(FLINKLIBS)

# run the executable in parallel.
run:  Application
	${ESMF_MPIRUN} -np 4 ./Application

# run only a single copy.
rununi: Application
	${ESMF_MPIRUN} -np 1 ./Application


# files to be deleted when cleaning up.
clean:
	${ESMF_RM} -f Application $(CLEANFILES)

