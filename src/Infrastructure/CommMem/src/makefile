
# $Id: makefile,v 1.14 2003/04/24 14:11:37 nscollins Exp $

# leave alone.
ALL: lib 

# don't set these unless absolutely necessary - they've got to be
#  architecture and compiler independent and should be inherited
#  from the upper level makefile if possible.
#  TODO:  move platform define to central esmf/build
CFLAGS    = -D${ESMF_ARCH} ${MPI_INCLUDE}
FFLAGS    =

# if you have C files in this directory, list them all on the line below.
SOURCEC	  = ESMC_PE.C ESMC_DE.C ESMC_PEList.C ESMC_DELayout.C ESMC_Comm.C
# if you have F90 files in this directory, list them all on the line below.
SOURCEF	  = 
# if you have .h include files in this dir, list them all on the line below.
STOREH	  = ESMC_Comm.h ESMC_CommMem.h ESMC_DE.h ESMC_DELayout.h \
            ESMC_PE.h ESMC_PEList.h ESMF_CommMem.h

# leave these alone.
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

# is MANSEC something about documentation?
MANSEC	  = CommMemobj
LIBBASE	  = libesmf

# the following line must be set to the current subdir name relative
# to the top level esmf directory. (e.g. Base/src, TimeMgr/interface, etc)
LOCDIR	  = src/Infrastructure/CommMem/src

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =


# if you have class files which need to be compiled in a particular
# order, list the dependencies below, with no rules following.
#
# e.g.:
#
# ESMF_Field.o:   ESMF_DataArray.o
#
# ESMF_Bundle.o:  ESMF_Field.o
#




