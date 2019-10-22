# $Id$

ALL: build_here

SOURCEC  =
SOURCEF  = dcopy.F90  ddot.F90  dger.F90  dnrm2.F90  drot.F90  dscal.F90  dswap.F90  dtrmm.F90 \
           dtrmv.F90  idamax.F90 dgemv.F90 dgemm.F90 dtrsm.F90 daxpy.F90

SOURCEH  =

# List all .h files which should be copied to common include dir
STOREH	  =

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/Mesh/src/BLAS

CLEANDIRS   =
CLEANFILES  = 
CLOBBERDIRS =

include ${ESMF_DIR}/makefile

DIRS	  = 



