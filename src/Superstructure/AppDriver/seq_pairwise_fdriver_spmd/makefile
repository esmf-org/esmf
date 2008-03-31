# $Id: makefile,v 1.14.2.1 2008/03/31 21:47:25 feiliu Exp $ 

# by default, this makefile builds the executable named "Application"
ALL:  executable

LOCDIR	  = src/Superstructure/AppDriver/seq_pairwise_fdriver_spmd

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
	$(ESMF_F90COMPILER) $(ESMF_F90LINKOPTS) -o Application $(APPOBJS) $(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) $(ESMF_F90ESMFLINKLIBS)

# run the executable in parallel.
run:  Application
	${ESMF_MPIRUN} -np 4 ./Application

# run only a single copy.
rununi: Application
	${ESMF_MPIRUN} -np 1 ./Application


# files to be deleted when cleaning up.
clean:
	${ESMF_RM} -f Application $(CLEANFILES)

