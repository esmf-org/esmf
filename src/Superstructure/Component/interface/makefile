# $Id: makefile,v 1.18 2003/10/22 20:09:41 cdeluca Exp $

# leave alone.
ALL: lib 

# don't set these unless absolutely necessary - they've got to be
#  architecture and compiler independent and should be inherited
#  from the upper level makefile if possible.
CFLAGS    = ${MPI_INCLUDE}
FFLAGS    =

# if you have C files in this directory, list them all on the line below.
SOURCEC	  = ESMC_FTable_F.C ESMC_Xform.C \
            ESMC_Comp.C ESMC_Comp_F.C ESMC_GridComp.C ESMC_CplComp.C
# if you have F90 files in this directory, list them all on the line below.
SOURCEF	  =  ESMF_Comp_C.F90 ESMF_Xform_C.F90
# if you have .h include files in this dir, list them all on the line below.
SOURCEH	  = 
# list .h files to be copied to higher level common include directory
STOREH	  = 

# leave these alone.
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

# is MANSEC something about documentation?
MANSEC	  = Componentobj
LIBBASE	  = libesmf


# the following line must be set to the current subdir name relative
# to the top level esmf directory. (e.g. Base/src, TimeMgr/interface, etc)
LOCDIR	  = src/Superstructure/Component/interface


include $(ESMF_DIR)/makefile

# leave the following line blank.
DIRS	  = 

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =


