# $Id: makefile,v 1.15 2003/04/28 17:37:43 nscollins Exp $

# leave alone.
ALL: lib 

# don't set these unless absolutely necessary - they've got to be
#  architecture and compiler independent and should be inherited
#  from the upper level makefile if possible.
CFLAGS    = ${MPI_INCLUDE}
FFLAGS    =

# if you have C files in this directory, list them all on the line below.
SOURCEC	  = ESMC_FTable.C
# if you have F90 files in this directory, list them all on the line below.
SOURCEF	  = ESMF_Xform.F90 ESMF_State.F90 ESMF_Comp.F90 \
            ESMF_GridComp.F90 ESMF_CplComp.F90 ESMF_AppComp.F90 ESMF.F90
# if you have .h include files in this dir, list them all on the line below.
SOURCEH	  = 

# leave these alone.
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

# is MANSEC something about documentation?
MANSEC	  = Componentobj
LIBBASE	  = libesmf



# the following line must be set to the current subdir name relative
# to the top level esmf directory. (e.g. Base/src, TimeMgr/interface, etc)
LOCDIR	  = src/Superstructure/Component/src


include $(ESMF_DIR)/makefile

# leave the following line blank.
DIRS	  = 

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

