# $Id: makefile,v 1.15 2006/09/22 23:55:42 theurich Exp $

ALL: build_examples
run: run_examples

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
OBJSC     = 
OBJSF     = 
TEXFILES  = 
MANSEC	  = 
LIBBASE	  = 
LOCDIR	  = src/Infrastructure/Array/examples

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

EXAMPLES_BUILD    = $(ESMF_EXDIR)/ESMF_ArrayFarrayEx $(ESMF_EXDIR)/ESMF_ArrayFarrayHaloEx $(ESMF_EXDIR)/ESMF_ArrayLarrayEx $(ESMF_EXDIR)/ESMF_ArrayEx

#
# List the example run targets in EXAMPLES_RUN and 
# EXAMPLES_RUN_UNI.  All of the run targets should be defined
# in this makefile.
#

EXAMPLES_RUN      = run_ESMF_ArrayFarrayEx run_ESMF_ArrayFarrayHaloEx run_ESMF_ArrayLarrayEx run_ESMF_ArrayEx

# TODO: once we have a test script tag that allows to indicate that some
#       example files should only be run in multi-pet mode the following
#       line (and the code) must be cleaned up.

EXAMPLES_RUN_UNI  = run_ESMF_ArrayFarrayEx_uni run_ESMF_ArrayFarrayHaloEx_uni run_ESMF_ArrayLarrayEx_uni run_ESMF_ArrayEx_uni


include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   = 
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS =

#
#  Run targets
#
run_ESMF_ArrayEx:
	-${ESMF_MPIRUN} -np 4 $(ESMF_EXDIR)/ESMF_ArrayEx > $(ESMF_EXDIR)/ESMF_ArrayEx.stdout

run_ESMF_ArrayEx_uni:
	-${ESMF_MPIRUN} -np 1 $(ESMF_EXDIR)/ESMF_ArrayEx > $(ESMF_EXDIR)/ESMF_ArrayEx.stdout


run_ESMF_ArrayLarrayEx:
	-${ESMF_MPIRUN} -np 4 $(ESMF_EXDIR)/ESMF_ArrayLarrayEx > $(ESMF_EXDIR)/ESMF_ArrayLarrayEx.stdout

run_ESMF_ArrayLarrayEx_uni:
	-${ESMF_MPIRUN} -np 1 $(ESMF_EXDIR)/ESMF_ArrayLarrayEx > $(ESMF_EXDIR)/ESMF_ArrayLarrayEx.stdout

run_ESMF_ArrayFarrayHaloEx:
	-${ESMF_MPIRUN} -np 4 $(ESMF_EXDIR)/ESMF_ArrayFarrayHaloEx > $(ESMF_EXDIR)/ESMF_ArrayFarrayHaloEx.stdout

run_ESMF_ArrayFarrayHaloEx_uni:
	-${ESMF_MPIRUN} -np 1 $(ESMF_EXDIR)/ESMF_ArrayFarrayHaloEx > $(ESMF_EXDIR)/ESMF_ArrayFarrayHaloEx.stdout

run_ESMF_ArrayFarrayEx:
	-${ESMF_MPIRUN} -np 4 $(ESMF_EXDIR)/ESMF_ArrayFarrayEx > $(ESMF_EXDIR)/ESMF_ArrayFarrayEx.stdout

run_ESMF_ArrayFarrayEx_uni:
	-${ESMF_MPIRUN} -np 1 $(ESMF_EXDIR)/ESMF_ArrayFarrayEx > $(ESMF_EXDIR)/ESMF_ArrayFarrayEx.stdout
