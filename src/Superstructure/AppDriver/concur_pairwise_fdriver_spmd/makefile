# $Id: makefile,v 1.3 2005/05/23 17:00:42 nscollins Exp $ 

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
APPOBJS = CplCompTemplate.o GridComp1Template.o GridComp2Template.o \
          ParentGridCompTemplate.o AppDriver.o

#
# Sample application for which we supply skeleton files to use as a 
# starting point for a larger application.
#

# build the executable.
executable: $(APPOBJS)
	$(FLINKER) $(LINKOPTS) -o Application $(APPOBJS) $(FLINKLIBS)

# run the executable in parallel.
run:  Application
	${MPIRUN} -np 4 ./Application

# run only a single copy.
rununi: Application
	${MPIRUN} -np 1 ./Application


# files to be deleted when cleaning up.
clean:
	${RM} -f Application $(CLEANFILES)

