# $Id: makefile,v 1.2 2011/12/15 23:20:20 peggyli Exp $

ALL: build_here

SOURCEC  =
SOURCEF  = \
      dcopy.f  ddot.f  dger.f  dnrm2.f  drot.f  dscal.f  dswap.f  dtrmm.f  dtrmv.f  idamax.f

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



