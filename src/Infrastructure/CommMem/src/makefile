# $Id: makefile,v 1.7 2003/01/09 15:25:58 nscollins Exp $

# leave alone.
ALL: lib 

# don't set these unless absolutely necessary - they've got to be
#  architecture and compiler independent and should be inherited
#  from the upper level makefile if possible.
#  TODO:  move platform define to central esmf/build
CFLAGS    = -D${ESMF_ARCH} ${MPI_INCLUDE}
FFLAGS    =

# if you have C files in this directory, list them all on the line below.
SOURCEC	  = ESMC_Machine.C ESMC_PE.C ESMC_DE.C ESMC_PEList.C ESMC_Layout.C ESMC_Comm.C
# if you have F90 files in this directory, list them all on the line below.
SOURCEF	  = 
# if you have .h include files in this dir, list them all on the line below.
STOREH	  = ESMC_Comm.h ESMC_CommMem.h ESMC_DE.h ESMC_Layout.h ESMC_Machine.h \
            ESMC_PE.h ESMC_PEList.h ESMF_CommMem.h

# leave these alone.
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))
TEXFILES  = $(addsuffix .tex, $(basename $(SOURCEF)))

# is MANSEC something about documentation?
MANSEC	  = CommMemobj
LIBBASE	  = libesmf

# leave the following line blank.
DIRS	  = 

# the following line must be set to the current subdir name relative
# to the top level esmf directory. (e.g. Base/src, TimeMgr/interface, etc)
LOCDIR	  = src/Infrastructure/CommMem/src

# leave these alone 
CLEANFILES = $(LIBNAME) $(addprefix doc/, $(addsuffix .tex, $(basename $(SOURCEF))))

include ${ESMF_DIR}/build/${ESMF_ARCH}/base


# if you have class files which need to be compiled in a particular
# order, list the dependencies below, with no rules following.
#
# e.g.:
#
# ESMF_Field.o:   ESMF_DataArray.o
#
# ESMF_Bundle.o:  ESMF_Field.o
#




