# $Id: makefile,v 1.3 2011/12/16 06:06:02 theurich Exp $

ALL: build_here

SOURCEC  =
SOURCEF  = dcopy.f  ddot.f  dger.f  dnrm2.f  drot.f  dscal.f  dswap.f  dtrmm.f \
           dtrmv.f  idamax.f dgemv.f dgemm.f dtrsm.f daxpy.f

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



