# $Id: makefile,v 1.2 2003/11/04 23:48:46 jwolfe Exp $

ALL: examples

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
LOCDIR	  = src/Infrastructure/DELayout/examples


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

EXAMPLES_BUILD   =  $(ESMF_EXDIR)/ESMC_CommMemEx \
                    $(ESMF_EXDIR)/ESMF_DELayout_F1Ex \
                    $(ESMF_EXDIR)/ESMF_DELayout_F2Ex \
                    $(ESMF_EXDIR)/ESMF_DELayout_F3Ex

#
# List the example run targets in EXAMPLES_RUN and 
# EXAMPLES_RUN_UNI.  All of the run targets should be defined
# in this makefile.
#
EXAMPLES_RUN     =  run_ESMC_CommMemEx \
                    run_ESMF_DELayout_F1Ex \
                    run_ESMF_DELayout_F2Ex \
                    run_ESMF_DELayout_F3Ex


EXAMPLES_RUN_UNI =  run_ESMC_CommMemEx_uni \
                    run_ESMF_DELayout_F1Ex_uni \
                    run_ESMF_DELayout_F2Ex_uni \
                    run_ESMF_DELayout_F3Ex_uni 


include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod *.txt *.stdout NULL core $(EXAMPLES_BUILD)
CLOBBERDIRS =


#
#  ESMC_CommMemEx
#
run_ESMC_CommMemEx:
	-${MPIRUN} -np 2 $(ESMF_EXDIR)/ESMC_CommMemEx > $(ESMF_EXDIR)/ESMC_CommMemEx.stdout


run_ESMC_CommMemEx_uni:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMC_CommMemEx > $(ESMF_EXDIR)/ESMC_CommMemEx_uni.stdout

#
#  ESMF_DELayout_F1Ex
#
run_ESMF_DELayout_F1Ex:
	-${MPIRUN} -np 2 $(ESMF_EXDIR)/ESMF_DELayout_F1Ex > $(ESMF_EXDIR)/ESMF_DELayout_F1Ex.stdout

run_ESMF_DELayout_F1Ex_uni:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMF_DELayout_F1Ex > $(ESMF_EXDIR)/ESMF_DELayout_F1Ex.stdout


#
#  ESMF_DELayout_F2Ex
#
run_ESMF_DELayout_F2Ex:
	-${MPIRUN} -np 6 $(ESMF_EXDIR)/ESMF_DELayout_F2Ex > $(ESMF_EXDIR)/ESMF_DELayout_F2Ex.stdout

run_ESMF_DELayout_F2Ex_uni:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMF_DELayout_F2Ex > $(ESMF_EXDIR)/ESMF_DELayout_F2Ex.stdout


#
#  ESMF_DELayout_F3Ex
#
run_ESMF_DELayout_F3Ex:
	-${MPIRUN} -np 6 $(ESMF_EXDIR)/ESMF_DELayout_F3Ex > $(ESMF_EXDIR)/ESMF_DELayout_F3Ex.stdout

run_ESMF_DELayout_F3Ex_uni:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMF_DELayout_F3Ex > $(ESMF_EXDIR)/ESMF_DELayout_F3Ex.stdout




