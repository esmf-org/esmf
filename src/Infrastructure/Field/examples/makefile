# $Id: makefile,v 1.18.2.1 2005/02/10 18:56:48 nscollins Exp $ 

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
LOCDIR	  = src/Infrastructure/Field/examples

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

EXAMPLES_BUILD = $(ESMF_EXDIR)/ESMC_FieldCreateEx \
                 $(ESMF_EXDIR)/ESMF_FieldCreateEx \
                 $(ESMF_EXDIR)/ESMF_FieldFromUserEx \
                 $(ESMF_EXDIR)/ESMF_FieldGlobalEx \
                 $(ESMF_EXDIR)/ESMF_FieldWriteEx

#
# List the example run targets in EXAMPLES_RUN and 
# EXAMPLES_RUN_UNI.  All of the run targets should be defined
# in this makefile.
#

EXAMPLES_RUN = run_ESMC_FieldCreateEx \
               run_ESMF_FieldCreateEx \
               run_ESMF_FieldFromUserEx \
               run_ESMF_FieldGlobalEx \
               run_ESMF_FieldWriteEx 

EXAMPLES_RUN_UNI = run_ESMC_FieldCreateEx_uni \
                   run_ESMF_FieldCreateEx_uni \
                   run_ESMF_FieldFromUserEx_uni \
                   run_ESMF_FieldGlobalEx_uni \
                   run_ESMF_FieldWriteEx_uni

include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod *.txt *.stdout NULL core ESMF_LogFile \
              foo.nc $(EXAMPLES_BUILD) 0 1 2 3
CLOBBERDIRS =


#
# ESMC_FieldCreateEx
#
run_ESMC_FieldCreateEx:
	-${MPIRUN} -np 4 $(ESMF_EXDIR)/ESMC_FieldCreateEx > $(ESMF_EXDIR)/ESMC_FieldCreateEx.stdout

run_ESMC_FieldCreateEx_uni:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMC_FieldCreateEx > $(ESMF_EXDIR)/ESMC_FieldCreateEx.stdout

#
# ESMF_FieldCreateEx
#
run_ESMF_FieldCreateEx:
	-${MPIRUN} -np 4 $(ESMF_EXDIR)/ESMF_FieldCreateEx > $(ESMF_EXDIR)/ESMF_FieldCreateEx.stdout

run_ESMF_FieldCreateEx_uni:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMF_FieldCreateEx > $(ESMF_EXDIR)/ESMF_FieldCreateEx.stdout

#
# ESMF_FieldFromUserEx
#
EXAMPLE_FieldFromUser_OBJS = UserCodeMod.o
ESMF_FieldFromUserEx.o	: UserCodeMod.o

run_ESMF_FieldFromUserEx:
	-${MPIRUN} -np 6 $(ESMF_EXDIR)/ESMF_FieldFromUserEx > $(ESMF_EXDIR)/ESMF_FieldFromUserEx.stdout

run_ESMF_FieldFromUserEx_uni:
	echo "PASS: ESMF_FieldFromUserEx not set up to run on one processor." > $(ESMF_EXDIR)/ESMF_FieldFromUserEx.stdout

#
# ESMF_FieldGlobalEx
#
run_ESMF_FieldGlobalEx:
	-${MPIRUN} -np 6 $(ESMF_EXDIR)/ESMF_FieldGlobalEx > $(ESMF_EXDIR)/ESMF_FieldGlobalEx.stdout

run_ESMF_FieldGlobalEx_uni:
	echo "PASS: ESMF_FieldGlobalEx not set up to run on one processor." > $(ESMF_EXDIR)/ESMF_FieldGlobalEx.stdout

#
# ESMF_FieldWriteEx
#
 run_ESMF_FieldWriteEx:
	-${MPIRUN} -np 4 $(ESMF_EXDIR)/ESMF_FieldWriteEx > $(ESMF_EXDIR)/ESMF_FieldWriteEx.stdout

 run_ESMF_FieldWriteEx_uni:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMF_FieldWriteEx > $(ESMF_EXDIR)/ESMF_FieldWriteEx.stdout




