# $Id: makefile,v 1.24 2004/08/03 19:33:50 nscollins Exp $

ALL:  build_examples
#
#  makefile for examples directory.
#
# All of the makefile targets and rules for building and linking
# examples are located in the build system makefile fragments.
# The run targets are located in this makefile.
#
# Applications and their standard output files are created in
# $(ESMF_EXDIR).  $(ESMF_EXDIR) is defined in build/common.mk.
#
# Targets:
#
#    examples:         Will first build the ESMF library, then will
#                      build the example applications and finally
#                      will run the examples
#
#    examples_uni:     Same as examples target, but runs examples on
#                      a single processor.
#
#    build_examples:   Builds example applications in $(ESMF_EXDIR)
#
#    run_examples:     Runs applications built from source in this
#                      directory.  Output is saved in file in $(ESMF_EXDIR).
#
#    run_examples_uni: Same as run_examples target, but runs examples on
#                      only one processor.
#


CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  =

SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
LOCDIR	  = src/Infrastructure/TimeMgr/examples

#
# Set EXAMPLES_BUILD to full path of the examples to be
# built.  The compiling from source code to object code is
# handle by the same compile rules used to build the objects
# used in the library. The linking of the objects is done with
# specific linking rules in build/common.mk.
#
# The examples executable files follow this naming convention:
#
#      $(ESMF_EXDIR)/ESM?_<Comp>Ex
#
# Where the ? is ether F or C.  The example linking rules
# depend on this naming convention being followed.
#
EXAMPLES_BUILD   =  $(ESMF_EXDIR)/ESMF_TimeEx \
                    $(ESMF_EXDIR)/ESMF_TimeIntervalEx \
                    $(ESMF_EXDIR)/ESMF_CalendarEx \
                    $(ESMF_EXDIR)/ESMF_ClockEx \
                    $(ESMF_EXDIR)/ESMF_AlarmEx
#		    $(ESMF_EXDIR)/ESMC_ClockEx

#
# List the example run targets in EXAMPLES_RUN and
# EXAMPLES_RUN_UNI.  All of the run targets should be defined
# in this makefile.
#
# The examples are executed using an mpirun script. However the
# examples do not use mpi, the script is used only to set the
# environment variables correctly.

EXAMPLES_RUN     =  run_ESMF_TimeEx \
                    run_ESMF_TimeIntervalEx \
                    run_ESMF_CalendarEx \
                    run_ESMF_ClockEx \
                    run_ESMF_AlarmEx
#		    run_ESMC_ClockEx

EXAMPLES_RUN_UNI =  run_ESMF_TimeEx_uni \
                    run_ESMF_TimeIntervalEx_uni \
                    run_ESMF_CalendarEx_uni \
                    run_ESMF_ClockEx_uni \
                    run_ESMF_AlarmEx_uni
#		    run_ESMC_ClockEx_uni

include ${ESMF_DIR}/makefile

DIRS = 

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod *.txt *.stdout NULL core ESMF_LogFile $(EXAMPLES_BUILD)
CLOBBERDIRS =

#
#  ESMF_TimeEx
#
run_ESMF_TimeEx_uni:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMF_TimeEx > $(ESMF_EXDIR)/ESMF_TimeEx.stdout

run_ESMF_TimeEx:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMF_TimeEx > $(ESMF_EXDIR)/ESMF_TimeEx.stdout


#
# ESMF_TimeIntervalEx
#
run_ESMF_TimeIntervalEx_uni:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMF_TimeIntervalEx > $(ESMF_EXDIR)/ESMF_TimeIntervalEx.stdout

run_ESMF_TimeIntervalEx:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMF_TimeIntervalEx > $(ESMF_EXDIR)/ESMF_TimeIntervalEx.stdout


#
# ESMF_CalendarEx
#
run_ESMF_CalendarEx_uni:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMF_CalendarEx > $(ESMF_EXDIR)/ESMF_CalendarEx.stdout

run_ESMF_CalendarEx:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMF_CalendarEx > $(ESMF_EXDIR)/ESMF_CalendarEx.stdout


#
# ESMF_ClockEx
#
run_ESMF_ClockEx_uni:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMF_ClockEx > $(ESMF_EXDIR)/ESMF_ClockEx.stdout

run_ESMF_ClockEx:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMF_ClockEx > $(ESMF_EXDIR)/ESMF_ClockEx.stdout

#
# ESMC_ClockEx
#
#run_ESMC_ClockEx_uni:
#	-${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMC_ClockEx > $(ESMF_EXDIR)/ESMC_ClockEx.stdout

#run_ESMC_ClockEx:
#	-${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMC_ClockEx > $(ESMF_EXDIR)/ESMC_ClockEx.stdout

# ESMF_AlarmEx
#
run_ESMF_AlarmEx_uni:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMF_AlarmEx > $(ESMF_EXDIR)/ESMF_AlarmEx.stdout

run_ESMF_AlarmEx:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMF_AlarmEx > $(ESMF_EXDIR)/ESMF_AlarmEx.stdout
