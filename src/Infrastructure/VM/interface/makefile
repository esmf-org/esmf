# $Id: makefile,v 1.8 2005/09/22 18:12:10 nscollins Exp $

# leave alone.
ALL: build_here

# Don't set these unless absolutely necessary - they've got to be
# architecture and compiler independent and should be inherited
# from the upper level makefile if possible.
CFLAGS    =
FFLAGS    =

ifeq ($(ESMF_COMM),lam)
# only include the casefix files for lam communications.
# they might be useful other places, but we used the lam header files
# as the template for the interfaces, so it may need to be altered to
# match another implementation of mpi.
SOURCEC	  = ESMC_VM_F.C mpi_lcasefix.C mpi_ucasefix.C
else
SOURCEC	  = ESMC_VM_F.C
endif

SOURCEF	  = ESMF_VM.F90
SOURCEH	  = 
STOREH	  = 

# Leave these alone.
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

MANSEC	  = VMobj
LIBBASE	  = libesmf

# The following line must be set to the current subdir name relative
# to the top level esmf directory. (e.g. Base/src, TimeMgr/interface, etc)
LOCDIR	  = src/Infrastructure/VM/interface

CLEANDIRS  =
CLEANFILES =
CLOBBERDIS =

include ${ESMF_DIR}/makefile

# Leave the following line blank.
DIRS	  = 



# If you have class files which need to be compiled in a particular
# order, list the dependencies below, with no rules following.
#
# e.g.:
#
# ESMF_Field.o:   ESMF_DataArray.o
#
# ESMF_Bundle.o:  ESMF_Field.o
#




