# $Id: makefile,v 1.12 2003/02/27 21:28:26 nscollins Exp $

# leave alone.
ALL: lib 

# don't set these unless absolutely necessary - they've got to be
#  architecture and compiler independent and should be inherited
#  from the upper level makefile if possible.
CFLAGS    =
FFLAGS    =

# if you have C files in this directory, list them all on the line below.
SOURCEC	  = ESMC_FTable.C ESMC_FTable_F.C ESMC_State.C ESMC_Xform.C \
            ESMC_Comp.C ESMC_Comp_F.C 
# if you have F90 files in this directory, list them all on the line below.
SOURCEF	  =  ESMF_Comp_C.F90 ESMF_State_C.F90 ESMF_Xform_C.F90
# if you have .h include files in this dir, list them all on the line below.
SOURCEH	  = 
# list .h files to be copied to higher level common include directory
STOREH	  = 

# leave these alone.
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))
TEXFILES  = $(addsuffix .tex, $(basename $(SOURCEF)))

# is MANSEC something about documentation?
MANSEC	  = Componentobj
LIBBASE	  = libesmf


# the following line must be set to the current subdir name relative
# to the top level esmf directory. (e.g. Base/src, TimeMgr/interface, etc)
LOCDIR	  = src/Superstructure/Component/interface

# leave these alone 
CLEANFILES = $(LIBNAME) $(addprefix doc/, $(addsuffix .tex, $(basename $(SOURCEF))))

include $(ESMF_DIR)/makefile

# leave the following line blank.
DIRS	  = 