# $Id: makefile,v 1.8 2004/04/30 23:31:02 slswift Exp $

ALL: build_here

CFLAGS    =
FFLAGS    = 
SOURCEC   = 
SOURCEF   = wrf_io.F90 field_routines.F90 # ESMF_IO.F90
CODE      = ext_ncd_get_dom_ti.code ext_ncd_get_var_td.code ext_ncd_get_var_ti.code ext_ncd_put_dom_ti.code ext_ncd_put_var_td.code ext_ncd_put_var_ti.code transpose.code
#STOREH    = ESMC_IO.h

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

MANSEC    = IOobj
LIBBASE   = libesmf

# This needs to be the path of this directory relative to the
# top level esmf directory.
LOCDIR    = src/Infrastructure/IO/src

# Leave this line alone.
include $(ESMF_DIR)/makefile

# The DIRS line needs to contain all subdirectories which exist 
# directly below this directory, and have either library,
# example/test code, or documents which need to be generated.
DIRS =

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

wrf_io.F:		wrf_io.F90 $(CODE)
			sed -e '1,/BEGINIOFLAGS/d' -e '/ENDIOFLAGS/,$$d' $< > wrf_io_flags.h
			$(CPP) -U__FILE__ -D__FILE__='"'wrf_io.F90'"' -x f77-cpp-input -E -P -I./ $< | $(M4)  -Uinclude -Uindex -Ulen - > $@

field_routines.F:	field_routines.F90 wrf_io.F90
			$(CPP) -U__FILE__ -D__FILE__='"'field_routines.F90'"' -x f77-cpp-input -E -P -I./ $< > $@

# ESMF_IO.o:		wrf_io.F90 field_routines.F90

