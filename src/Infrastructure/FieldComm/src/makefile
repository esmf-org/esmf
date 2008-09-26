# $Id: makefile,v 1.12 2008/09/26 05:53:54 cdeluca Exp $

ALL: build_here 

AUTOGEN   = ESMF_FieldGather.F90 ESMF_FieldScatter.F90
SOURCEC	  = 
SOURCEF	  =  ESMF_FieldComm.F90 $(AUTOGEN) ESMF_FieldRedist.F90 ESMF_FieldSMM.F90
SOURCEH	  = 

# List all .h files which should be copied to common include dir
STOREH	  = 

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/FieldComm/src

CLEANDIRS  =
CLEANFILES = $(AUTOGEN)
CLOBBERDIS =

include ${ESMF_DIR}/makefile

DIRS	  = 

